# -*- coding: utf-8 -*-
"""1990–2023 年度率序列：纯 ARIMA 预测 2024–2035，95%预测区间及滚动验证。

安装依赖：pip install numpy pandas pmdarima statsmodels joblib
运行：python arima_forecast_2024_2035.py
滚动验证：python arima_forecast_2024_2035.py --run-rolling --rolling-horizons 1 3 5
如果只想做滚动验证：添加 --rolling-only

说明：
* 每条序列仅使用原始尺度 ARIMA；使用 AICc 自动选 (p,d,q)。
* 预测区间由 ARIMA 模型预测误差方差得到，不使用固定残差标准差。
* 对预测值与区间的下限施加非负截断（因而边界处覆盖率不保证精确95%）。
* 不在正式预测中以 Naive 兜底；拟合失败被记录，避免悄悄混用模型。
* 将 Naive / Drift 仅作为滚动验证中的比较基线。
* 按输入目录下 BRI-long-location.csv 的 location 列筛选，仅分析两者共有的国家。
"""

import argparse
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
import pmdarima as pm
from joblib import Parallel, delayed
from statsmodels.stats.diagnostic import acorr_ljungbox
from statsmodels.tsa.stattools import adfuller

FILES = {
    "ASDR": "全球STD-ASDR1990-2023.csv",
    "ASIR": "全球STD-ASIR1990-2023.csv",
    "ASPR": "全球STD-ASPR1990-2023.csv",
    "ASMR": "全球STD-ASMR1990-2023.csv",
}
TRAIN_START = 1990
TRAIN_END = 2023
FORECAST_END = 2035
MIN_TRAIN = 15


def load_data(input_dir):
    bri_path = input_dir / "BRI-long-location.csv"
    if not bri_path.is_file():
        raise FileNotFoundError(f"国家名单文件不存在：{bri_path}")
    bri = pd.read_csv(bri_path, encoding="utf-8-sig")
    if "location" not in bri.columns:
        raise ValueError(f"{bri_path.name} 缺少列：location")
    bri_countries = set(bri["location"].dropna().astype(str).str.strip()) - {""}
    if not bri_countries:
        raise ValueError(f"{bri_path.name} 中没有有效的国家名称")

    tables = []
    for indicator, filename in FILES.items():
        path = input_dir / filename
        if not path.is_file():
            raise FileNotFoundError(f"输入文件不存在：{path}")
        df = pd.read_csv(path, encoding="utf-8-sig")
        needed = {"location", "year", "val"}
        if not needed.issubset(df.columns):
            raise ValueError(f"{path.name} 缺少列：{sorted(needed - set(df.columns))}")
        df = df[["location", "year", "val"]].copy()
        df["location"] = df["location"].astype("string").str.strip()
        df = df.loc[df["location"].isin(bri_countries)].copy()
        df["Indicator"] = indicator
        tables.append(df)

    data = pd.concat(tables, ignore_index=True)
    if data.empty:
        raise ValueError("输入文件与 BRI-long-location.csv 没有匹配的国家")
    data["year"] = pd.to_numeric(data["year"], errors="coerce")
    data["val"] = pd.to_numeric(data["val"], errors="coerce")
    data = data.loc[data["year"].between(TRAIN_START, TRAIN_END)].copy()
    data = data.dropna(subset=["location", "year", "val"])
    data["year"] = data["year"].astype(int)
    data["location"] = data["location"].astype(str).str.strip()
    data = data[np.isfinite(data["val"])].copy()
    if (data["val"] < 0).any():
        raise ValueError("发现负的疾病负担率，请检查原始数据。")
    if data.duplicated(["location", "Indicator", "year"]).any():
        examples = data.loc[data.duplicated(["location", "Indicator", "year"], keep=False),
                            ["location", "Indicator", "year"]].head()
        raise ValueError(f"同一地区-指标-年份存在重复记录：\n{examples}")
    data["Key"] = data["location"] + " | " + data["Indicator"]
    return data.sort_values(["Key", "year"])


def extract_series(group):
    """ARIMA 按连续年度步长建模；缺年则报告失败，而不是把相隔多年当作相邻步。"""
    s = group.set_index("year")["val"].sort_index().astype(float)
    if not s.index.is_unique:
        raise ValueError("年份重复")
    if len(s) and not np.array_equal(s.index.to_numpy(), np.arange(s.index.min(), s.index.max() + 1)):
        raise ValueError("时间序列存在缺失年份")
    return s


def fit_arima(train):
    """AICc 自动选择阶数；趋势/漂移由 auto_arima 的默认规则决定。"""
    if len(train) < 8:
        raise ValueError("有效数据少于8年")
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        model = pm.auto_arima(
            np.asarray(train, dtype=float),
            seasonal=False,
            stepwise=True,
            suppress_warnings=True,
            error_action="ignore",
            information_criterion="aicc",
            max_p=3, max_q=3, max_d=2,
            max_order=None,
        )
    return model


def predict_arima(model, steps):
    """模型给出的 horizon-specific 95% 预测区间。"""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        fc, ci = model.predict(n_periods=steps, return_conf_int=True, alpha=0.05)
    fc = np.asarray(fc, dtype=float)
    ci = np.asarray(ci, dtype=float)
    if fc.shape != (steps,) or ci.shape != (steps, 2):
        raise ValueError("ARIMA 预测结果维度不正确")
    if not (np.isfinite(fc).all() and np.isfinite(ci).all()):
        raise ValueError("ARIMA 产生非有限预测值/区间")
    neg_count = int((fc < 0).sum())
    fc = np.maximum(fc, 0.0)
    lo = np.maximum(ci[:, 0], 0.0)
    hi = np.maximum(ci[:, 1], fc)
    # 数值误差/截断下仍确保 lower <= forecast <= upper
    lo = np.minimum(lo, fc)
    return fc, lo, hi, neg_count


def diagnostics(model, original):
    resid = np.asarray(model.resid(), dtype=float)
    resid = resid[np.isfinite(resid)]
    try:
        adf_stat, adf_p, *_ = adfuller(np.asarray(original, dtype=float), autolag="AIC")
    except Exception:
        adf_stat, adf_p = np.nan, np.nan
    lag = min(10, max(1, len(resid) // 5))
    try:
        lb = acorr_ljungbox(resid, lags=[lag], model_df=sum(model.order[::2]), return_df=True)
        lb_stat = float(lb.iloc[0]["lb_stat"])
        lb_p = float(lb.iloc[0]["lb_pvalue"])
    except Exception:
        lb_stat, lb_p = np.nan, np.nan
    return {
        "ARIMA_order": str(model.order),
        "AICc": float(model.aicc()),
        "stationarity_ADF_statistic": adf_stat,
        "stationarity_ADF_pvalue": adf_p,
        "resid_mean": float(np.mean(resid)) if len(resid) else np.nan,
        "resid_std": float(np.std(resid, ddof=1)) if len(resid) > 1 else np.nan,
        "LjungBox_lag": lag,
        "LjungBox_statistic": lb_stat,
        "LjungBox_pvalue": lb_p,
        "residual_adequate_p_gt_0_05": bool(lb_p > 0.05) if np.isfinite(lb_p) else None,
    }


def forecast_one(key, group, end_year):
    location = str(group["location"].iloc[0])
    indicator = str(group["Indicator"].iloc[0])
    try:
        s = extract_series(group)
        if s.index.min() != TRAIN_START or s.index.max() != TRAIN_END:
            raise ValueError(f"要求覆盖{TRAIN_START}-{TRAIN_END}，实际为{s.index.min()}-{s.index.max()}")
        model = fit_arima(s.values)
        years = np.arange(TRAIN_END + 1, end_year + 1)
        fc, lo, hi, neg_count = predict_arima(model, len(years))
        preds = pd.DataFrame({
            "Key": key, "Country": location, "Indicator": indicator,
            "Year": years, "Forecast": fc, "Lower95": lo, "Upper95": hi,
            "Model": "ARIMA", "ARIMA_order": str(model.order),
            "AICc": float(model.aicc()), "n_clipped": neg_count,
        })
        diag = {"Key": key, "Country": location, "Indicator": indicator,
                "n_clipped": neg_count, **diagnostics(model, s.values)}
        return preds, diag, None
    except Exception as exc:
        return None, None, {"Key": key, "Country": location,
                            "Indicator": indicator, "Error": str(exc)}


def baseline(train, h, method):
    last = float(train[-1])
    if method == "Naive":
        return np.full(h, last)
    if method == "Drift":
        slope = (last - float(train[0])) / (len(train) - 1)
        return np.maximum(last + slope * np.arange(1, h + 1), 0.0)
    raise ValueError(method)


def rolling_one(key, group, horizon, min_train):
    """扩展窗口。对所有能观察到真实值的各步长计算误差及ARIMA区间覆盖。"""
    try:
        s = extract_series(group)
        years = s.index.to_numpy()
        y = s.to_numpy(dtype=float)
        if len(y) < min_train + 1:
            return [], [{"Key": key, "Horizon_max": horizon, "Error": "历史长度不足"}]
        out, errors = [], []
        for i in range(min_train, len(y)):
            available_h = min(horizon, len(y) - i)
            train = y[:i]
            try:
                model = fit_arima(train)
                fc, lo, hi, _ = predict_arima(model, available_h)
            except Exception as exc:
                errors.append({"Key": key, "Horizon_max": horizon,
                               "Origin": int(years[i - 1]), "Error": str(exc)})
                # ARIMA 失败的 origin 不纳入任何方法的对比，保证相同测试样本
                continue
            na = baseline(train, available_h, "Naive")
            dr = baseline(train, available_h, "Drift")
            for j in range(available_h):
                actual = float(y[i + j])
                for name, val in (("ARIMA", float(fc[j])), ("Naive", float(na[j])),
                                  ("Drift", float(dr[j]))):
                    row = {"Key": key, "Origin": int(years[i - 1]),
                           "TargetYear": int(years[i + j]), "h": j + 1,
                           "Model": name, "actual": actual, "forecast": val,
                           "error": actual - val}
                    if name == "ARIMA":
                        row.update({"Lower95": float(lo[j]), "Upper95": float(hi[j]),
                                    "covered95": int(lo[j] <= actual <= hi[j])})
                    out.append(row)
        return out, errors
    except Exception as exc:
        return [], [{"Key": key, "Horizon_max": horizon, "Error": str(exc)}]


def summarize_rolling(records):
    """逐步长、逐序列统计；PI coverage 只对 ARIMA 有定义。"""
    rows = []
    for (key, model, h), d in records.groupby(["Key", "Model", "h"], sort=True):
        err = d["error"].to_numpy(dtype=float)
        actual = d["actual"].to_numpy(dtype=float)
        nonzero = actual != 0
        rows.append({
            "Key": key, "Model": model, "h": int(h), "n_origin": len(d),
            "RMSE": float(np.sqrt(np.mean(err ** 2))),
            "MAE": float(np.mean(np.abs(err))),
            "MAPE_percent": float(np.mean(np.abs(err[nonzero] / actual[nonzero])) * 100)
            if nonzero.any() else np.nan,
            "MAPE_nonzero_n": int(nonzero.sum()),
            "PI95_coverage": float(d["covered95"].mean()) if model == "ARIMA" else np.nan,
            "PI95_mean_width": float((d["Upper95"] - d["Lower95"]).mean())
            if model == "ARIMA" else np.nan,
        })
    result = pd.DataFrame(rows)
    loc_ind = result["Key"].str.rsplit(" | ", n=1, expand=True)
    result.insert(1, "Country", loc_ind[0])
    result.insert(2, "Indicator", loc_ind[1])
    return result


def save_csv(df, path):
    df.to_csv(path, index=False, encoding="utf-8-sig")
    print(f"已写入 {path}（{len(df)}行）")


def main():
    parser = argparse.ArgumentParser(description="单一ARIMA模型年度预测与多步滚动验证")
    parser.add_argument("--input-dir", type=Path, default=Path("."))
    parser.add_argument("--output-dir", type=Path, default=Path("."))
    parser.add_argument("--n-jobs", type=int, default=-1, help="并行任务数，-1表示全部核心")
    parser.add_argument("--run-rolling", action="store_true")
    parser.add_argument("--rolling-only", action="store_true")
    parser.add_argument("--rolling-horizons", type=int, nargs="+", default=[1, 3, 5])
    parser.add_argument("--min-train", type=int, default=MIN_TRAIN)
    args = parser.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    data = load_data(args.input_dir)
    groups = list(data.groupby("Key", sort=True))
    print(f"已读取 {len(data)} 条观测，{len(groups)} 条地区×指标序列")

    if not args.rolling_only:
        print("开始ARIMA批量预测2024—2035...")
        results = Parallel(n_jobs=args.n_jobs)(
            delayed(forecast_one)(key, grp, FORECAST_END) for key, grp in groups)
        predictions = [p for p, _, _ in results if p is not None]
        diags = [d for _, d, _ in results if d is not None]
        failed = [e for _, _, e in results if e is not None]
        if predictions:
            pred_df = pd.concat(predictions, ignore_index=True)
            save_csv(pred_df, args.output_dir / "forecast_2024_2035_arima.csv")
            print("预测为负后截断的点数：", int(pred_df["n_clipped"].sum() / (FORECAST_END - TRAIN_END)))
        else:
            print("没有成功预测的序列")
        if diags:
            save_csv(pd.DataFrame(diags), args.output_dir / "model_diagnostics_arima.csv")
        save_csv(pd.DataFrame(failed, columns=["Key", "Country", "Indicator", "Error"]),
                 args.output_dir / "failed_series_arima.csv")
        print(f"预测成功 {len(predictions)} 条序列，失败 {len(failed)} 条")

    if args.run_rolling or args.rolling_only:
        hmax = max(args.rolling_horizons)
        if min(args.rolling_horizons) < 1:
            parser.error("预测步长必须是正整数")
        print(f"开始滚动验证：模型一次预测1～{hmax}步，再输出{args.rolling_horizons}步的结果")
        # 每个 origin 只拟合一次，避免为不同 horizon 重复拟合相同模型。
        all_results = Parallel(n_jobs=args.n_jobs)(
            delayed(rolling_one)(key, grp, hmax, args.min_train) for key, grp in groups)
        rows = [row for rr, _ in all_results for row in rr]
        errs = [e for _, ee in all_results for e in ee]
        save_csv(pd.DataFrame(errs), args.output_dir / "rolling_failures_arima.csv")
        if rows:
            raw = pd.DataFrame(rows)
            raw = raw[raw["h"].isin(args.rolling_horizons)].copy()
            save_csv(raw, args.output_dir / "rolling_predictions_arima.csv")
            save_csv(summarize_rolling(raw), args.output_dir / "rolling_metrics_arima.csv")
        else:
            print("没有足够的有效滚动评估记录")
    print("完成。")


if __name__ == "__main__":
    main()
