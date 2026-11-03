# -*- coding: utf-8 -*-
"""
plot8 new.py —— 重绘 Figure 8（B 路线：每个指标各取 2035 年预测前 6 国，四面板各自带图例）

与 plot8.py 的区别
------------------
1. 删除整张图右侧的共享图例，改成「每个面板自己有图例」（解决 R2-1 / R1-3）。
2. 自动在几个常见目录里查找预测 CSV 与 4 个历史数据文件，不再要求工作目录唯一。
3. 统一国家名（Türkiye→Turkiye、Côte d'Ivoire→Ivory Coast），避免历史线与预测线被拆开。
4. 若读到的不是手稿那一版预测 CSV（South Africa IR 2035 ≠ 20,192.19），会打印醒目的版本警告。
5. 面板标题用 IR / PR / MR / DR，并加 A–D 标签，与正文和图注一致。
6. 输出文件名改为 top6_forecast_new.png，不覆盖原有图。

运行：python "plot8 new.py"
"""

import os
import numpy as np
import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from matplotlib.lines import Line2D

# PDF 用 TrueType 字体嵌入（期刊要求 Type 42，避免 Type 3）
matplotlib.rcParams["pdf.fonttype"] = 42
matplotlib.rcParams["ps.fonttype"] = 42

# ============================================================
# 0. 配置
# ============================================================
HERE = os.path.dirname(os.path.abspath(__file__))

FORECAST_NAMES = ["forecast_2024_2035_arima.csv", "forcast_2024_2035_arima.csv"]
FORECAST_DIRS = [
    HERE,
    os.path.join(HERE, "..", "青少年性病一带一路", "新版PY图"),
    r"C:\Users\tianq\Desktop\青少年性病一带一路\新版PY图",
]

HIST_FILES = {
    "ASDR": "全球、21大区和国家STD-ASDR1990-2023.csv",
    "ASIR": "全球、21大区和国家STD-ASIR1990-2023.csv",
    "ASPR": "全球、21大区和国家STD-ASPR1990-2023.csv",
    "ASMR": "全球、21大区和国家STD-ASMR1990-2023.csv",
}
HIST_DIRS = [
    HERE,
    os.path.join(HERE, "..", "青少年性病"),
    r"C:\Users\tianq\Desktop\青少年性病",
]

METRICS = ["ASIR", "ASPR", "ASMR", "ASDR"]
LABELS = {"ASIR": "IR", "ASPR": "PR", "ASMR": "MR", "ASDR": "DR"}
USE_K = {"ASIR": True, "ASPR": True, "ASMR": False, "ASDR": True}

# 只在 A 面板标注 Historical / Predicted 线型说明（图注里已有文字说明）
SHOW_LINETYPE_IN_ALL_PANELS = False

# 各面板图例位置：统一放右上角
LEGEND_LOC = {"ASIR": "upper right", "ASPR": "upper right",
              "ASMR": "upper right", "ASDR": "upper right"}

# 手稿用的那一版预测（arima(2)，19:24）。若 CSV 对不上，脚本会提示。
MANUSCRIPT_SA_IR_2035 = 20192.19

OUT_PNG = os.path.join(HERE, "top6_forecast_new.png")
OUT_PDF = os.path.join(HERE, "top6_forecast_new.pdf")

NAME_FIX = {
    "United Republic of Tanzania": "Tanzania",
    "Türkiye": "Turkiye",
    "Côte d'Ivoire": "Ivory Coast",
    "Cote d'Ivoire": "Ivory Coast",
}


# ============================================================
# 1. 找文件
# ============================================================
def find_forecast():
    for d in FORECAST_DIRS:
        for n in FORECAST_NAMES:
            p = os.path.join(d, n)
            if os.path.isfile(p):
                return p
    raise FileNotFoundError(
        "没有找到预测 CSV（forecast_2024_2035_arima.csv）。\n"
        "已查找目录：\n  " + "\n  ".join(FORECAST_DIRS)
    )


def find_hist(name):
    for d in HIST_DIRS:
        p = os.path.join(d, name)
        if os.path.isfile(p):
            return p
    return None


# ============================================================
# 2. 读预测数据
# ============================================================
fc_path = find_forecast()
print(f"[forecast] {fc_path}")
raw = pd.read_csv(fc_path, encoding="utf-8-sig")

need = {"Country", "Indicator", "Year", "Forecast", "Lower95", "Upper95"}
lack = need - set(raw.columns)
if lack:
    raise ValueError(f"预测 CSV 缺少列：{sorted(lack)}（实际列：{list(raw.columns)}）")

fc = raw.rename(columns={
    "Country": "location", "Indicator": "metric", "Year": "year",
    "Forecast": "value", "Lower95": "lower", "Upper95": "upper",
})[["location", "metric", "year", "value", "lower", "upper"]].copy()
fc["type"] = "Forecast"
fc["location"] = fc["location"].astype(str).str.strip().replace(NAME_FIX)

n_2035 = int((fc["year"] == 2035).sum())
print(f"[forecast] 2035 年记录数 = {n_2035}（应为 624）")
if n_2035 != 624:
    print("[warn] 2035 年记录数不是 624，请检查 CSV 是否完整。")

_sa = fc[(fc.location == "South Africa") & (fc.metric == "ASIR") & (fc.year == 2035)]
if len(_sa):
    _v = float(_sa.value.iloc[0])
    if abs(_v - MANUSCRIPT_SA_IR_2035) > 0.05:
        print(f"[warn] South Africa IR 2035 = {_v:,.2f}，与手稿的 {MANUSCRIPT_SA_IR_2035:,.2f} 不一致。")
        print("       这可能是 22:55 那一版预测（不含手稿数字），重画出来的图会和正文不符。")
    else:
        print(f"[ok] 版本核对通过：South Africa IR 2035 = {_v:,.2f}")


# ============================================================
# 3. 读历史数据（1990–2023）
# ============================================================
frames, missing = [], []
for ind, fname in HIST_FILES.items():
    p = find_hist(fname)
    if p is None:
        missing.append(fname)
        continue
    d = pd.read_csv(p, encoding="utf-8-sig")
    need_h = {"location", "year", "val"}
    if not need_h.issubset(d.columns):
        missing.append(fname + "（列名不符）")
        continue
    d = d[["location", "year", "val", "upper", "lower"]].rename(columns={"val": "value"})
    d["metric"] = ind
    d["type"] = "Actual"
    d["location"] = d["location"].astype(str).str.strip().replace(NAME_FIX)
    frames.append(d)

if missing:
    print("[warn] 以下历史文件未找到，对应指标将只画预测线：")
    for m in missing:
        print("       -", m)
    print("       已查找目录：\n         " + "\n         ".join(HIST_DIRS))

if frames:
    hist = pd.concat(frames, ignore_index=True)
    hist = hist[hist.year.between(1990, 2023)].copy()
    for c in ("value", "lower", "upper"):
        hist[c] = pd.to_numeric(hist[c], errors="coerce")
    print(f"[hist] 历史记录 {len(hist)} 行，国家 {hist.location.nunique()} 个")
else:
    hist = pd.DataFrame(columns=["location", "metric", "year", "value", "lower", "upper", "type"])

all_df = pd.concat([hist, fc], ignore_index=True, sort=False)


# ============================================================
# 4. 各指标取 2035 年前 6 国
# ============================================================
y35 = fc[fc.year == 2035]
top6 = (y35.sort_values(["metric", "value"], ascending=[True, False])
           .groupby("metric", group_keys=False)
           .head(6))

print("\n[top6] 各指标 2035 年前六国：")
for m in METRICS:
    s = top6[top6.metric == m].sort_values("value", ascending=False)
    print(f"  {LABELS[m]:>3s}: " + ", ".join(f"{r.location} {r.value:,.2f}" for r in s.itertuples()))

top6_pairs = top6[["metric", "location"]].drop_duplicates()
plot_df = all_df.merge(top6_pairs, on=["metric", "location"], how="inner")


# ============================================================
# 5. 绘图
# ============================================================
def draw_panel(ax, sub, metric, tag):
    df = sub.copy()
    use_k = USE_K[metric]
    scale = 1000.0 if use_k else 1.0
    df["dv"] = df["value"] / scale
    df["dl"] = df["lower"] / scale
    df["du"] = df["upper"] / scale

    act = df[df.type == "Actual"]
    fct = df[df.type == "Forecast"]

    countries = sorted(df["location"].unique())
    cmap = matplotlib.colormaps["tab10"]
    colors = {c: cmap(i % 10) for i, c in enumerate(countries)}

    # 95% 预测区间
    for c in countries:
        s = fct[fct.location == c].sort_values("year")
        if s.empty:
            continue
        ax.fill_between(s.year, s.dl, s.du, color=colors[c], alpha=0.10, linewidth=0)

    # 历史实线
    for c in countries:
        s = act[act.location == c].sort_values("year")
        if not s.empty:
            ax.plot(s.year, s.dv, color=colors[c], linestyle="-", linewidth=1.5)

    # 预测虚线
    for c in countries:
        s = fct[fct.location == c].sort_values("year")
        if not s.empty:
            ax.plot(s.year, s.dv, color=colors[c], linestyle="--", linewidth=1.5)

    # 预测起点
    ax.axvline(2023.5, color="red", linestyle=":", linewidth=1.2)

    ax.set_xlim(1990, 2035)
    ax.set_xticks(np.arange(1990, 2036, 5))
    ax.tick_params(axis="x", labelrotation=45, labelsize=9)
    ax.tick_params(axis="y", labelsize=9)
    if use_k:
        ax.yaxis.set_major_formatter(
            mticker.FuncFormatter(lambda v, _: "0" if abs(v) < 1 else f"{v:g}K"))
    else:
        ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda v, _: f"{v:,.0f}"))
    ax.grid(True, color="grey", alpha=0.3, linewidth=0.5)
    ax.set_facecolor("white")
    for sp in ("top", "right"):
        ax.spines[sp].set_visible(False)

    ax.set_title(LABELS[metric], fontsize=13, fontweight="bold")
    ax.text(-0.10, 1.03, tag, transform=ax.transAxes,
            fontsize=15, fontweight="bold", va="bottom", ha="left")

    # —— 每个面板自己的图例 ——
    handles = [Line2D([0], [0], color=colors[c], lw=2, label=c) for c in countries]
    if SHOW_LINETYPE_IN_ALL_PANELS or tag == "A":
        handles += [
            Line2D([0], [0], color="black", lw=2, linestyle="-", label="Historical"),
            Line2D([0], [0], color="black", lw=2, linestyle="--", label="Predicted"),
        ]
    ax.legend(handles=handles, loc=LEGEND_LOC.get(metric, "upper left"), ncol=2, fontsize=6.5,
              frameon=True, framealpha=0.85, edgecolor="none",
              handlelength=1.2, columnspacing=0.8, labelspacing=0.3)


fig, axes = plt.subplots(2, 2, figsize=(16, 10.5))
axes = axes.flatten()

for ax, metric, tag in zip(axes, METRICS, ["A", "B", "C", "D"]):
    draw_panel(ax, plot_df[plot_df.metric == metric], metric, tag)

fig.suptitle("Top six BRI countries by projected 2035 rate (per indicator)",
             fontsize=17, fontweight="bold", y=0.98)

plt.tight_layout(rect=[0, 0, 1.0, 0.96])
plt.savefig(OUT_PNG, dpi=300, bbox_inches="tight")
plt.savefig(OUT_PDF, bbox_inches="tight")
print(f"\n[saved] {OUT_PNG}")
print(f"[saved] {OUT_PDF}")
plt.show()
