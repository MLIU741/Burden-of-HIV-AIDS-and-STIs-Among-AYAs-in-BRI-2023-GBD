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
from matplotlib.patches import Rectangle
from matplotlib import cm
from matplotlib.colors import Normalize
from matplotlib.ticker import FuncFormatter


# ============================================================
# 1. 读取预测文件
# ============================================================
# 兼容你给的文件名：forcast_2024_2035_all.csv
# 以及当前工作区里实际存在的：forecast_2024_2035_all.csv
candidate_files = [
    "forcast_2024_2035_arima.csv",
    "forecast_2024_2035_arima.csv",
]

input_path = next((f for f in candidate_files if os.path.exists(f)), None)
if input_path is None:
    raise FileNotFoundError(
        "未找到预测文件。请将 forcast_2024_2035_all.csv 或 forecast_2024_2035_all.csv 放到当前目录。"
    )

forecast_all = pd.read_csv(input_path)

# ============================================================
# 2. 规范列名
# ============================================================
# 你的真实 CSV 列通常是：Key / Year / Forecast / Lower95 / Upper95 / Model / AICc / Country / Indicator
# 如果列名不同，做兼容处理
rename_map = {
    "Country": "location",
    "country": "location",
    "Indicator": "metric",
    "indicator": "metric",
    "Forecast": "value",
    "forecast": "value",
    "Year": "year",
    "year": "year",
    "continent": "continent",
    "Continent": "continent",
}

df = forecast_all.rename(columns={k: v for k, v in rename_map.items() if k in forecast_all.columns})

# 统一必要列
if "location" not in df.columns and "Key" in df.columns:
    df["location"] = df["Key"].str.split(" | ").str[0]
if "metric" not in df.columns and "Key" in df.columns:
    df["metric"] = df["Key"].str.split(" | ").str[1]
if "value" not in df.columns and "Forecast" in df.columns:
    df["value"] = df["Forecast"]
if "year" not in df.columns and "Year" in df.columns:
    df["year"] = df["Year"]

# 尝试从外部文件加载 continent 映射（BRI-long-location.csv），优先使用映射值
mapping_file = "BRI-long-location.csv"
if os.path.exists(mapping_file):
    try:
        loc_map = pd.read_csv(mapping_file, encoding="utf-8")
        # 自动检测可能的列名
        loc_col = None
        cont_col = None
        for c in ["location", "Location", "country", "Country", "loc"]:
            if c in loc_map.columns:
                loc_col = c
                break
        for c in ["continent", "Continent", "region", "Region"]:
            if c in loc_map.columns:
                cont_col = c
                break
        if loc_col and cont_col:
            loc_map = loc_map[[loc_col, cont_col]].rename(columns={loc_col: "location", cont_col: "continent"})
            loc_map["location"] = loc_map["location"].astype(str).str.strip()
            # 确保 df 有 location 列
            if "location" in df.columns:
                df["location"] = df["location"].astype(str).str.strip()
                df = df.merge(loc_map, on="location", how="left", suffixes=("", "_mapped"))
                # 若原数据已有 continent 列，则优先保留；否则使用映射值
                if "continent" in df.columns:
                    df["continent"] = df["continent"].fillna(df.get("continent_mapped"))
                else:
                    df["continent"] = df.get("continent_mapped")
                if "continent_mapped" in df.columns:
                    df = df.drop(columns=["continent_mapped"])
                print(f"Loaded continent mapping from {mapping_file} and merged.")
        else:
            print(f"{mapping_file} found but required columns not detected; skipping merge.")
    except Exception as e:
        print(f"Failed to read/merge {mapping_file}: {e}")

# 没有 continent 列时，退化为单个分组
if "continent" not in df.columns:
    df["continent"] = "All countries"

# 只保留 2035 年
if "year" in df.columns:
    df = df[df["year"] == 2035].copy()
else:
    df = df.copy()

# 把 Tanzania 改名
if "location" in df.columns:
    df["location"] = df["location"].replace({"United Republic of Tanzania": "Tanzania"})

# 只保留需要的列
cols = [c for c in ["location", "metric", "value", "continent"] if c in df.columns]
df = df[cols].copy()

# ============================================================
# 3. 找出每个分组各指标最高的前 3 个国家
# ============================================================
metrics = ["ASIR", "ASPR", "ASMR", "ASDR"]

# 兼容没有某些指标时只画已有指标
metrics = [m for m in metrics if m in df["metric"].dropna().unique()]

# 按 continent × metric 分组，各取 Top 3
if "continent" in df.columns:
    top3_by_group = (
        df.sort_values(["continent", "metric", "value"], ascending=[True, True, False])
          .groupby(["continent", "metric"], observed=True)
          .head(3)
          .reset_index(drop=True)
    )
    top3_by_group["rank"] = (
        top3_by_group.groupby(["continent", "metric"], observed=True)["value"]
        .rank(method="first", ascending=False)
        .astype(int)
    )
else:
    top3_by_group = df.copy()
    top3_by_group["continent"] = "All countries"
    top3_by_group = top3_by_group.sort_values(["metric", "value"], ascending=[True, False]).head(3).reset_index(drop=True)
    top3_by_group["rank"] = np.arange(1, len(top3_by_group) + 1)

print("\n2035年每个分组各指标最高的前3个国家:\n")
for m in metrics:
    sub = top3_by_group[top3_by_group["metric"] == m]
    if len(sub) > 0:
        print(f"\n{m}:")
        print(sub[["continent", "location", "value", "rank"]].to_string(index=False))

# ============================================================
# 4. 准备热力图数据
# ============================================================
heatmap_data = (
    top3_by_group
    .sort_values(["continent", "metric", "rank"])
    [["continent", "metric", "location", "value", "rank"]]
    .reset_index(drop=True)
)

heatmap_data["metric"] = pd.Categorical(heatmap_data["metric"], categories=metrics, ordered=True)
heatmap_data = heatmap_data.sort_values(["continent", "metric", "rank"])

# ============================================================
# 5. 画图：按分组分面的热力图
# ============================================================
continents = sorted(heatmap_data["continent"].unique().tolist())
n_cont = len(continents)

fig, axes = plt.subplots(
    nrows=n_cont, ncols=1,
    figsize=(12, 1.2 * n_cont + 2),
    sharex=True,
)

if n_cont == 1:
    axes = [axes]

cmap = plt.get_cmap("RdBu_r")
vmin = heatmap_data["value"].min()
vmax = heatmap_data["value"].max()
norm = Normalize(vmin=vmin, vmax=vmax)

x_pos = {m: i for i, m in enumerate(metrics)}

for ax, cont in zip(axes, continents):
    sub = heatmap_data[heatmap_data["continent"] == cont]

    for _, row in sub.iterrows():
        xi = x_pos[row["metric"]]
        yi = 3 - row["rank"]
        color = cmap(norm(row["value"]))

        rect = Rectangle(
            (xi - 0.5, yi - 0.5), 1.0, 1.0,
            facecolor=color, edgecolor="white", linewidth=1.5, alpha=0.9
        )
        ax.add_patch(rect)

        ax.text(xi, yi + 0.18, row["location"], ha="center", va="center", fontsize=9, fontweight="bold", color="black")
        ax.text(xi, yi - 0.18, f"{row['value']:.2f}", ha="center", va="center", fontsize=9, fontweight="bold", color="darkorange")

    ax.set_xlim(-0.5, len(metrics) - 0.5)
    ax.set_ylim(-0.5, 2.5)
    ax.set_yticks([2, 1, 0])
    ax.set_yticklabels(["1st", "2nd", "3rd"], fontweight="bold")
    ax.set_xticks(range(len(metrics)))
    ax.set_ylabel(cont, fontsize=11, fontweight="bold", rotation=0, labelpad=60, va="center")
    for spine in ax.spines.values():
        spine.set_visible(False)
    ax.tick_params(left=False, bottom=False)
    ax.grid(False)

axes[-1].set_xticklabels(metrics, fontweight="bold", fontsize=11)

fig.suptitle(
    "Top 3 Countries with the Highest Predicted ASRs by Group in 2035",
    fontsize=16, fontweight="bold", y=0.98
)

sm = cm.ScalarMappable(cmap=cmap, norm=norm)
sm.set_array([])
# 在 figure 下方创建一个单独的 axes 用于放置水平 colorbar，避免遮挡子图内容
# [left, bottom, width, height] in figure coordinates
cax = fig.add_axes([0.12, 0.04, 0.76, 0.03])
cbar = fig.colorbar(sm, cax=cax, orientation="horizontal")
cbar.set_label("ASR(/100K)", fontsize=11, fontweight="bold")
cbar.ax.xaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{x:,.0f}"))

# 调整子图底部以腾出 colorbar 空间
plt.subplots_adjust(hspace=0.5, bottom=0.12)
plt.savefig("top3_by_group_2035.png", dpi=300, bbox_inches="tight")
plt.show()