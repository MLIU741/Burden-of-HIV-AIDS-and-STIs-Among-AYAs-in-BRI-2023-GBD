# ============================================================
# 0. 依赖
# ============================================================
import os
import subprocess, sys, importlib


def install_if_missing(pkgs):
    for pkg in pkgs:
        try:
            importlib.import_module(pkg)
        except ImportError:
            print(f"正在安装：{pkg}")
            subprocess.check_call([sys.executable, "-m", "pip", "install", pkg])


install_if_missing(["pandas", "numpy", "matplotlib"])

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import matplotlib.patheffects as pe


# ============================================================
# 1. 读取预测结果 CSV
# ============================================================
# 兼容两种文件名：forcast_... / forecast_...
possible_names = [
    "forcast_2024_2035_arima.csv",
    "forecast_2024_2035_arima.csv",
]

input_path = None
for name in possible_names:
    if os.path.exists(name):
        input_path = name
        break

if input_path is None:
    raise FileNotFoundError(
        "未找到预测 CSV 文件。请确认文件名为 'forcast_2024_2035_all.csv' 或 'forecast_2024_2035_all.csv'，并放在当前目录。"
    )

forecast_all = pd.read_csv(input_path)

# 只保留 2035 年
# 这里使用的是读取后的数据框，而不是未定义的变量

df = forecast_all.copy()

# 只保留 2035 年
df = df[df["Year"] == 2035].copy()

# 把 "United Republic of Tanzania" 改为 "Tanzania"
df["Country"] = df["Country"].replace(
    {"United Republic of Tanzania": "Tanzania"}
)

# Indicator 固定顺序
metric_order = ["ASIR", "ASPR", "ASMR", "ASDR"]
df["Indicator"] = pd.Categorical(df["Indicator"],
                                 categories=metric_order, ordered=True)

# 统一列名，方便后面画图
df = df.rename(columns={
    "Country":   "location",
    "Indicator": "metric",
    "Forecast":  "value",
})[["location", "metric", "value"]]

# 每个 metric 内按 value 降序取 Top 10
top10 = (
    df.sort_values(["metric", "value"], ascending=[True, False])
      .groupby("metric", observed=True)
      .head(10)
      .reset_index(drop=True)
)

print(top10)

# 为画图准备顺序：每个 metric 内按 value 升序（barh 从下往上画，大的在上）
top10 = (
    top10.sort_values(["metric", "value"], ascending=[True, True])
         .reset_index(drop=True)
)


# ============================================================
# 2. 配色
# ============================================================
colors = {
    "ASIR": "#7294D4",
    "ASPR": "#00A087",
    "ASMR": "#E6A0C4",
    "ASDR": "#E64B35",
}


# ============================================================
# 3. 画图：2×2 分面
# ============================================================
fig, axes = plt.subplots(2, 2, figsize=(14, 10))
axes = axes.flatten()

for ax, metric in zip(axes, metric_order):
    sub = top10[top10["metric"] == metric].copy()
    if sub.empty:
        ax.set_visible(False)
        continue

    sub = sub.sort_values("value", ascending=True).reset_index(drop=True)
    y_pos = np.arange(len(sub))

    # 条形
    ax.barh(y_pos, sub["value"], color=colors[metric], alpha=0.8, height=0.7)

    # 数值标签：放条形右端
    for y, v in zip(y_pos, sub["value"]):
        ax.text(v, y, f"{round(v):.0f}",
                va="center", ha="left",
                fontsize=8, fontweight="bold", color="black")

    # 国家名：放条形左端，加白色描边
    for y, loc in zip(y_pos, sub["location"]):
        ax.text(0, y, loc,
                va="center", ha="left",
                fontsize=9, fontweight="bold", color="black",
                path_effects=[pe.withStroke(linewidth=2, foreground="white")])

    # 去掉 y 轴刻度
    ax.set_yticks([])
    ax.set_ylabel("")

    # 分面标题
    ax.set_title(metric, fontsize=13, fontweight="bold",
                 color="white", backgroundcolor="slategray", pad=6)

    # x 轴：ASMR 用原始数值；其余用 K 单位
    if metric == "ASMR":
        ax.xaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{x:.0f}"))
    else:
        ax.xaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{x/1000:g}K"))

    # 网格
    ax.grid(axis="x", color="grey", linestyle="-", linewidth=0.5, alpha=0.5)
    ax.set_axisbelow(True)

    # 去掉上/右边框
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    # 给数字留空间
    ax.set_xlim(0, sub["value"].max() * 1.15)

# 总标题
fig.suptitle("Top 10 Countries with the Highest Predicted ASRs in 2035",
             fontsize=18, fontweight="bold", y=0.98)

# x 轴标签
fig.text(0.5, 0.02, "ASR(/100K)", ha="center", fontsize=12)

plt.tight_layout(rect=[0, 0.03, 1, 0.95])
plt.savefig("top10_2035_ASR.png", dpi=300, bbox_inches="tight")
plt.show()