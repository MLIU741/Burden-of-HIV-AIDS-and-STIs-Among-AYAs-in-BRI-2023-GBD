####13.ARIMA预测模型#####


#####13.1 读取并处理数据#####
bri_loc <- read.csv("BRI-long-location.csv")

df_ts_asir <- read.csv("全球204国家青少年性病总1990-2023发病率.csv")
df_ts_aspr <- read.csv("全球204国家青少年性病总1990-2023患病率.csv")
df_ts_asmr <- read.csv("全球204国家青少年性病总1990-2023死亡率.csv")
df_ts_asDr <- read.csv("全球204国家青少年性病总1990-2023DALY率.csv")


# 对每个数据集进行 inner_join 并仅保留指定列
df_ts_asir_clean <- df_ts_asir %>%
  inner_join(bri_loc, by = "location") %>%
  select(location, continent, measure, year, val, upper, lower, NO.)

df_ts_aspr_clean <- df_ts_aspr %>%
  inner_join(bri_loc, by = "location") %>%
  select(location, continent, measure, year, val, upper, lower, NO.)

df_ts_asmr_clean <- df_ts_asmr %>%
  inner_join(bri_loc, by = "location") %>%
  select(location, continent, measure, year, val, upper, lower, NO.)

df_ts_asDr_clean <- df_ts_asDr %>%
  inner_join(bri_loc, by = "location") %>%
  select(location, continent, measure, year, val, upper, lower, NO.)

# 把 4 张表合并成一张长表，方便批量处理 
all_metrics <- bind_rows(
  df_ts_asir_clean %>% mutate(metric = "ASIR"),
  df_ts_aspr_clean %>% mutate(metric = "ASPR"),
  df_ts_asmr_clean %>% mutate(metric = "ASMR"),
  df_ts_asDr_clean %>% mutate(metric = "ASDR")
) %>% arrange(location, metric, year)

# 确保指标名称正确
all_metrics <- all_metrics %>%
  mutate(metric = case_when(
    metric == "ASIR" ~ "ASIR",
    metric == "ASPR" ~ "ASPR", 
    metric == "ASMR" ~ "ASMR",
    metric == "ASDR" ~ "ASDR",
    TRUE ~ metric
  ))

#####13.2读取Python预测结果（替代 R 自身的 auto.arima）#####
# 数据源：桌面\新版PY图\forecast_2024_2035_arima.csv（纯 ARIMA，max_d = 1）
# 若该输出路径变化，只改下面这一行
py_fc_path <- "C:/Users/tianq/Desktop/新版PY图/forecast_2024_2035_arima.csv"

metrics <- c("ASIR", "ASPR", "ASMR", "ASDR")

py_fc <- read.csv(py_fc_path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

fc_part <- py_fc %>%
  transmute(location = Country,
            metric   = Indicator,
            year     = Year,
            value    = Forecast,
            lower    = Lower95,
            upper    = Upper95,
            type     = "Forecast") %>%
  left_join(bri_loc %>% select(location, continent), by = "location")

# 历史实测：下游 growth_rates / model_stats 需要 2023 基线
act_part <- all_metrics %>%
  filter(year <= 2023) %>%
  transmute(location, continent, metric, year,
            value = val, lower = lower, upper = upper, type = "Actual")

forecast_df <- bind_rows(act_part, fc_part)

cat("\n读取 Python 预测：", nrow(fc_part), " 行；历史实测：", nrow(act_part), " 行\n")
stopifnot(nrow(dplyr::filter(fc_part, year == 2035)) == 624)
cat("2035 年序列数校验通过：624\n")

#####13.3找出2035年各指标最高的前10个国家#####
top10_2035 <- forecast_df %>%
  filter(year == 2035, type == "Forecast") %>%
  group_by(metric) %>%
  arrange(metric, desc(value)) %>%
  slice_head(n = 10) %>%
  ungroup()

cat("\n2035年各指标最高的前10个国家:\n")
for(metric_type in metrics) {
  cat("\n", metric_type, ":\n")
  print(top10_2035 %>% filter(metric == metric_type) %>% select(location, value, continent))
}


# 准备数据
top10_data <- top10_2035 %>%
  mutate(
    # 将"United Republic of Tanzania"改为"Tanzania"
    location = ifelse(location == "United Republic of Tanzania", "Tanzania", location),
    metric = factor(metric, levels = c("ASIR", "ASPR", "ASMR", "ASDR"))
  ) %>%
  group_by(metric) %>%
  arrange(desc(value)) %>%
  mutate(
    rank = row_number(),
    # 在每个指标内重新排序，确保从上到下是由大到小
    location = reorder(location, value)
  ) %>%
  ungroup()
# 创建图表
ptop10 <- ggplot(top10_data, aes(x = value, y = reorder_within(location, value, metric))) +
  geom_col(aes(fill = metric), alpha = 0.8, width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", value)), hjust = -0.1,
            size = 2.5, fontface = "bold", color = "black") +
  geom_text(aes(label = location, x = 0), hjust = 0,
            size = 4, fontface = "bold", color = "black") +
  facet_wrap(~metric, ncol = 2, scales = "free",
             labeller = as_labeller(c(ASIR = "IR", ASPR = "PR",
                                      ASMR = "MR", ASDR = "DR"))) +
  labs(title = "Top 10 Countries with the Highest Predicted Rates in 2035",
       x = "Rate(/100K)", y = NULL) +
  # 默认刻度：K 单位
  scale_x_continuous(labels = function(x) paste0(x/1000, "K"),
                     expand = expansion(mult = c(0.02, 0.18))) +
  scale_y_discrete(labels = function(x) gsub("__.+$", "", x)) + # 移除reorder_within添加的后缀
  scale_fill_manual(values = c("#7294D4", "#00A087FF", "#E6A0C4", "#E64B35FF")) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.5),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "slategray4"),
    strip.text = element_text(color = "grey10", face = "bold"),
    legend.position = "none"
  ) +
  # 关键：仅 ASMR 面板用原始数值刻度
  facetted_pos_scales(
    x = list(
      metric == "ASMR" ~ scale_x_continuous(labels = scales::number_format(accuracy = 1))
    )
  )

print(ptop10)

#####13.3b导出CSV（保留2位小数）#####
# 1) 各指标 2035 年前 10 名
top10_out <- top10_2035 %>%
  group_by(metric) %>%
  arrange(desc(value), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  transmute(metric, rank, location, continent,
            value_2035 = sprintf("%.2f", value),
            lower95    = sprintf("%.2f", lower),
            upper95    = sprintf("%.2f", upper)) %>%
  arrange(metric, rank)

write.csv(top10_out,
          "C:/Users/tianq/Desktop/青少年性病/top10_2035_2dp.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# 2) 全部 156 国 x 4 指标 的 2035 年值（624 行）
fc2035_out <- fc_part %>%
  filter(year == 2035) %>%
  transmute(location, continent, metric,
            value_2035 = sprintf("%.2f", value),
            lower95    = sprintf("%.2f", lower),
            upper95    = sprintf("%.2f", upper)) %>%
  arrange(metric, desc(value_2035))

write.csv(fc2035_out,
          "C:/Users/tianq/Desktop/青少年性病/forecast_2035_all_2dp.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n已导出 top10_2035_2dp.csv（", nrow(top10_out),
    " 行）与 forecast_2035_all_2dp.csv（", nrow(fc2035_out), " 行）\n")



#####13.4找出2035年每个大洲各指标最高的前3个国家#####
top3_by_continent <- forecast_df %>%
  filter(year == 2035, type == "Forecast") %>%
  group_by(continent, metric) %>%
  arrange(continent, metric, desc(value)) %>%
  slice_head(n = 3) %>%
  ungroup()

cat("\n2035年每个大洲各指标最高的前3个国家:\n")
for(metric_type in metrics) {
  cat("\n", metric_type, ":\n")
  print(top3_by_continent %>% filter(metric == metric_type))
}

# 准备热力图数据 - 绘图用
heatmap_data_plot <- top3_by_continent %>%
  mutate(
    location = ifelse(location == "United Republic of Tanzania", "Tanzania", location)
  ) %>%
  group_by(continent, metric) %>%
  arrange(desc(value)) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  select(continent, metric, location, value, rank) %>%
  arrange(continent, metric, rank)

# 调整版本 - 更清晰的标签位置，数值保留两位小数
p2_adjusted <- ggplot(heatmap_data_plot, aes(x = metric, y = factor(rank, levels = 3:1), fill = value)) +
  # 添加热力瓷砖
  geom_tile(color = "white", size = 0.8, alpha = 0.9) +
  # 添加国家名称标签（在格子顶部）
  geom_text(
    aes(label = location),
    color = "black",
    size = 3,
    fontface = "bold",
    vjust = -1.5  # 进一步向上移动国家名称
  ) +
  # 添加数值标签（在格子底部）- 保留两位小数
  geom_text(
    aes(label = sprintf("%.2f", value)),  # 使用sprintf格式化保留两位小数
    color = "darkorange4",
    size = 3.5,
    fontface = "bold",
    vjust = 2  # 进一步向下移动数值
  ) +
  # 分面 - 按大洲分面
  facet_grid(continent ~ ., scales = "free_y", space = "free_y") +
  # 坐标轴和标签 - 去掉横轴和纵轴标题
  labs(
    title = "Top 3 Countries with the Highest Predicted ASRs by Continent in 2035",
    fill = "Rate(/100K)",
    x = NULL,
    y = NULL
  ) +
  # 颜色渐变
  scale_fill_gradientn(
    colors = c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fddbc7", "#f4a582", "#d6604d", "#b2182b"),
    name = "Rate(/100K)",
    labels = comma
  ) +
  # y轴刻度标签
  scale_y_discrete(labels = c("1st", "2nd", "3rd")) +
  # 主题设置
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_rect(fill = "slategray4", color = "grey20"),
    strip.text = element_text(color = "white", face = "bold", size = 12),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  )

print(p2_adjusted)


#####13.5 top6国家预测#####
#准备数据
top6_countries <- top10_2035 %>%
  group_by(metric) %>%
  slice_head(n = 6) %>%
  ungroup() %>%
  select(metric, location)

forecast_top6 <- forecast_df %>%
  inner_join(top6_countries, by = c("metric", "location")) %>%
  mutate(
    metric = factor(metric, levels = c("ASIR", "ASPR", "ASMR", "ASDR")),
    location = ifelse(location == "United Republic of Tanzania", "Tanzania", location)
  )

# 创建单个指标图表的函数 - 简化版本
create_single_metric_plot <- function(metric_data, metric_name, use_k_units = FALSE) {
  # 如果需要使用K单位，调整数值
  if(use_k_units) {
    metric_data <- metric_data %>%
      mutate(
        display_value = value / 1000,
        display_lower = lower / 1000,
        display_upper = upper / 1000
      )
  } else {
    metric_data <- metric_data %>%
      mutate(
        display_value = value,
        display_lower = lower,
        display_upper = upper
      )
  }
  
  p <- ggplot(metric_data, aes(x = year, y = display_value, color = location)) +
    # 预测区间
    geom_ribbon(
      data = metric_data %>% filter(type == "Forecast"),
      aes(ymin = display_lower, ymax = display_upper, fill = location),
      alpha = 0.1,
      color = NA
    ) +
    # 实际数据线 - 实线
    geom_line(
      data = metric_data %>% filter(type == "Actual"),
      aes(linetype = "Historical"),
      size = 0.8
    ) +
    # 预测数据线 - 虚线
    geom_line(
      data = metric_data %>% filter(type == "Forecast"),
      aes(linetype = "Predicted"),
      size = 0.8
    ) +
    # 垂直线标记预测开始年份
    geom_vline(xintercept = 2023.5, linetype = "dotted", color = "red", size = 0.7) +
    # 坐标轴和标签 - 去掉横轴和纵轴标题
    labs(
      title = metric_name,
      x = NULL,
      y = NULL,
      color = "Country",
      fill = "Country",
      linetype = "Data Type"
    ) +
    # 设置线型
    scale_linetype_manual(
      values = c("Historical" = "solid", "Predicted" = "dashed")
    ) +
    # 设置X轴刻度 - 从1990到2035，步长为5年
    scale_x_continuous(
      breaks = seq(1990, 2035, by = 5),
      limits = c(1990, 2035)
    ) +
    # 设置Y轴标签
    scale_y_continuous(
      labels = ifelse(use_k_units, 
                      function(x) paste0(x, "K"), 
                      comma)
    ) +
    # 主题设置
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", size = 0.3),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      legend.key.width = unit(1.2, "cm"),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10)
    )
  
  return(p)
}

# 为每个指标创建图表
asir_plot <- create_single_metric_plot(
  forecast_top6 %>% filter(metric == "ASIR"), 
  "ASIR", 
  use_k_units = TRUE
)

aspr_plot <- create_single_metric_plot(
  forecast_top6 %>% filter(metric == "ASPR"), 
  "ASPR", 
  use_k_units = TRUE
)

asmr_plot <- create_single_metric_plot(
  forecast_top6 %>% filter(metric == "ASMR"), 
  "ASMR", 
  use_k_units = FALSE
)

asdr_plot <- create_single_metric_plot(
  forecast_top6 %>% filter(metric == "ASDR"), 
  "ASDR", 
  use_k_units = TRUE
)

# 使用patchwork组合图表
combined_plot <- (asir_plot | aspr_plot) / (asmr_plot | asdr_plot) +
  plot_annotation(
    title = "Top 6 Countries Forecast by ASRs (1990-2035)",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))
  )

print(combined_plot)

# 使用patchwork组合图表并添加带边框的A、B、C、D标签
combined_plot <- (asir_plot | aspr_plot) / (asmr_plot | asdr_plot) +
  plot_annotation(
    title = "Top 6 Countries Forecast by ASRs (1990-2035)",
    tag_levels = 'A',
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
    )
  ) +
  # 自定义标签外观
  theme(
    plot.tag = element_text(size = 16, face = "bold", color = "black"),
    plot.tag.position = c(0, 1)  # 标签位置(左上角)
  )

print(combined_plot)

#####13.6 导出结果为表格#####

# 创建输出目录
if(!dir.exists("ARIMA_Results")) {
  dir.create("ARIMA_Results")
}

# 1. 导出2035年各指标前10国家表格
top10_2035_table <- top10_2035 %>%
  select(metric, location, continent, value) %>%
  arrange(metric, desc(value)) %>%
  rename(
    Metric = metric,
    Country = location,
    Continent = continent,
    `Predicted Value (2035)` = value
  )

write.csv(top10_2035_table, 
          "ARIMA_Results/top10_countries_2035.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# 2. 导出每个大洲前3国家表格 - 先添加排名列
top3_by_continent_table <- top3_by_continent %>%
  group_by(continent, metric) %>%
  arrange(desc(value)) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  select(continent, metric, location, value, rank) %>%
  arrange(continent, metric, rank) %>%
  rename(
    Continent = continent,
    Metric = metric,
    Country = location,
    `Predicted Value (2035)` = value,
    Rank = rank
  )

write.csv(top3_by_continent_table, 
          "ARIMA_Results/top3_by_continent_2035.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# 3. 导出前6国家完整预测数据表格
forecast_top6_table <- forecast_top6 %>%
  select(metric, location, continent, year, value, lower, upper, type) %>%
  arrange(metric, location, year) %>%
  rename(
    Metric = metric,
    Country = location,
    Continent = continent,
    Year = year,
    Value = value,
    `Lower CI` = lower,
    `Upper CI` = upper,
    `Data Type` = type
  )

write.csv(forecast_top6_table, 
          "ARIMA_Results/top6_countries_forecast.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# 4. 导出所有国家的预测汇总表格（2035年）
all_countries_2035 <- forecast_df %>%
  filter(year == 2035, type == "Forecast") %>%
  select(metric, location, continent, value, lower, upper) %>%
  arrange(metric, desc(value)) %>%
  rename(
    Metric = metric,
    Country = location,
    Continent = continent,
    `Predicted Value (2035)` = value,
    `Lower CI` = lower,
    `Upper CI` = upper
  )

write.csv(all_countries_2035, 
          "ARIMA_Results/all_countries_2035_predictions.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# 5. 修复：模型性能统计表格 - 使用显式的 dplyr::n()
model_stats <- forecast_df %>%
  filter(type == "Actual") %>%
  group_by(metric, location) %>%
  summarise(
    `Data Points` = dplyr::n(),  # 使用 dplyr::n() 避免冲突
    `Mean Value` = mean(value, na.rm = TRUE),
    `Min Value` = min(value, na.rm = TRUE),
    `Max Value` = max(value, na.rm = TRUE),
    `SD` = sd(value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(metric, location)

write.csv(model_stats, 
          "ARIMA_Results/model_statistics.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# 6. 导出预测增长率表格（2024-2035）
growth_rates <- forecast_df %>%
  filter(type == "Forecast", year %in% c(2024, 2035)) %>%
  group_by(metric, location, continent) %>%
  arrange(year) %>%
  summarise(
    `Value in 2024` = value[year == 2024],
    `Value in 2035` = value[year == 2035],
    `Absolute Change` = value[year == 2035] - value[year == 2024],
    `Percentage Change` = (value[year == 2035] - value[year == 2024]) / value[year == 2024] * 100,
    .groups = 'drop'
  ) %>%
  arrange(metric, desc(`Percentage Change`))

write.csv(growth_rates, 
          "ARIMA_Results/growth_rates_2024_2035.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# 7. 导出图表数据表格（用于在Excel中重新创建图表）

# 7.1 Top 10国家柱状图数据
top10_chart_data <- top10_data %>%
  select(metric, location, value, rank) %>%
  arrange(metric, rank) %>%
  rename(
    Metric = metric,
    Country = location,
    Value = value,
    Rank = rank
  )

write.csv(top10_chart_data, 
          "ARIMA_Results/top10_chart_data.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# 7.2 热力图数据 - 先添加排名列（使用不同的变量名避免冲突）
heatmap_data_export <- top3_by_continent %>%
  mutate(
    location = ifelse(location == "United Republic of Tanzania", "Tanzania", location)
  ) %>%
  group_by(continent, metric) %>%
  arrange(desc(value)) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  select(continent, metric, location, value, rank) %>%
  arrange(continent, metric, rank) %>%
  rename(
    Continent = continent,
    Metric = metric,
    Country = location,
    Value = value,
    Rank = rank
  )

write.csv(heatmap_data_export, 
          "ARIMA_Results/heatmap_data.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# 8. 创建汇总报告
summary_report <- list()

# 各指标的基本统计
metric_summary <- forecast_df %>%
  filter(year == 2035, type == "Forecast") %>%
  group_by(metric) %>%
  summarise(
    `Number of Countries` = n_distinct(location),
    `Mean Prediction` = mean(value, na.rm = TRUE),
    `Median Prediction` = median(value, na.rm = TRUE),
    `Min Prediction` = min(value, na.rm = TRUE),
    `Max Prediction` = max(value, na.rm = TRUE),
    `Standard Deviation` = sd(value, na.rm = TRUE),
    .groups = 'drop'
  )

summary_report$metric_summary <- metric_summary

# 大洲级别的统计
continent_summary <- forecast_df %>%
  filter(year == 2035, type == "Forecast") %>%
  group_by(continent, metric) %>%
  summarise(
    `Number of Countries` = n_distinct(location),
    `Mean Prediction` = mean(value, na.rm = TRUE),
    `Max Prediction` = max(value, na.rm = TRUE),
    .groups = 'drop'
  )

summary_report$continent_summary <- continent_summary

# 将汇总报告保存为多个表格
write.csv(metric_summary, 
          "ARIMA_Results/metric_summary_2035.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

write.csv(continent_summary, 
          "ARIMA_Results/continent_summary_2035.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# 9. 创建README文件说明各个表格的内容
readme_content <- c(
  "ARIMA预测模型结果表格说明",
  "======================================",
  "",
  "文件列表及说明：",
  "",
  "1. top10_countries_2035.csv - 2035年各指标预测值前10国家",
  "   - Metric: 健康指标 (ASIR, ASPR, ASMR, ASDR)",
  "   - Country: 国家名称",
  "   - Continent: 所属大洲",
  "   - Predicted Value (2035): 2035年预测值",
  "",
  "2. top3_by_continent_2035.csv - 每个大洲各指标前3国家",
  "   - Continent: 大洲",
  "   - Metric: 健康指标",
  "   - Country: 国家名称", 
  "   - Predicted Value (2035): 2035年预测值",
  "   - Rank: 排名 (1-3)",
  "",
  "3. top6_countries_forecast.csv - 前6国家完整预测数据",
  "   - Metric: 健康指标",
  "   - Country: 国家名称",
  "   - Continent: 所属大洲",
  "   - Year: 年份 (1990-2035)",
  "   - Value: 指标值",
  "   - Lower CI: 预测区间下限",
  "   - Upper CI: 预测区间上限",
  "   - Data Type: 数据类型 (Actual=历史数据, Forecast=预测数据)",
  "",
  "4. all_countries_2035_predictions.csv - 所有国家2035年预测值",
  "   - 包含所有国家在2035年的预测值及置信区间",
  "",
  "5. model_statistics.csv - 模型统计信息",
  "   - 每个国家每个指标的历史数据统计",
  "",
  "6. growth_rates_2024_2035.csv - 预测期增长率",
  "   - 显示2024-2035年的绝对变化和百分比变化",
  "",
  "7. top10_chart_data.csv - Top 10图表数据",
  "   - 用于重新创建Top 10国家柱状图的数据",
  "",
  "8. heatmap_data.csv - 热力图数据", 
  "   - 用于重新创建大洲热力图的数据",
  "",
  "9. metric_summary_2035.csv - 指标级别汇总统计",
  "   - 各指标在2035年的基本统计信息",
  "",
  "10. continent_summary_2035.csv - 大洲级别汇总统计",
  "    - 各指标在各大洲的基本统计信息",
  "",
  "生成时间:", as.character(Sys.time()),
  "数据来源: 全球疾病负担研究 (Global Burden of Disease Study)",
  "预测方法: ARIMA时间序列预测"
)

writeLines(readme_content, "ARIMA_Results/README.txt")










#####13.7 一带一路国家2035年各大洲平均预测指标图表#####

# 筛选BRI国家的预测数据
bri_forecast_2035 <- forecast_df %>%
  filter(year == 2035, 
         type == "Forecast",
         location %in% bri_countries)

# 检查是否有数据
if(nrow(bri_forecast_2035) == 0) {
  cat("警告: 没有找到BRI国家的预测数据，请检查BRI国家列表\n")
  cat("使用所有国家进行计算...\n")
  bri_forecast_2035 <- forecast_df %>%
    filter(year == 2035, type == "Forecast")
  bri_countries <- unique(bri_forecast_2035$location)
}

cat("用于计算的BRI国家预测数据量:", nrow(bri_forecast_2035), "\n")

# 计算每个大洲每个指标的平均值
bri_continent_avg <- bri_forecast_2035 %>%
  group_by(continent, metric) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    n_countries = n(),
    se_value = sd_value / sqrt(n_countries),
    ci_lower = mean_value - 1.96 * se_value,
    ci_upper = mean_value + 1.96 * se_value,
    .groups = 'drop'
  ) %>%
  mutate(
    metric = factor(metric, levels = c("ASIR", "ASPR", "ASMR", "ASDR")),
    # 创建显示文本
    hr_ci_text = sprintf("%.2f (%.2f-%.2f)", mean_value, ci_lower, ci_upper)
  ) %>%
  arrange(continent, metric)

# 为了绘图，我们需要创建一个唯一的ID用于排序
bri_continent_avg <- bri_continent_avg %>%
  arrange(continent, metric) %>%
  mutate(
    id = row_number(),
    Variable = factor(metric, levels = rev(c("ASIR", "ASPR", "ASMR", "ASDR")))
  )

# 定义颜色 - 每个指标一个颜色（与前面图表保持一致）
metric_colors <- c(
  "ASIR" = "#7294D4",
  "ASPR" = "#00A087FF", 
  "ASMR" = "#E6A0C4",
  "ASDR" = "#E64B35FF"
)

# 定义大洲颜色
n_continents <- length(unique(bri_continent_avg$continent))
continent_colors <- brewer.pal(max(6, n_continents), "Set3")[1:n_continents]
names(continent_colors) <- unique(bri_continent_avg$continent)

# 创建自定义的刻度标签函数
k_format <- function(x) {
  ifelse(x == 0, "0", 
         ifelse(x < 1000, as.character(x), 
                paste0(x/1000, "K")))
}

# 获取数据的最大值以确定坐标轴范围
max_value <- max(bri_continent_avg$ci_upper, na.rm = TRUE)

# 计算矩形的宽度和位置
rect_width <- max_value * 0.06  # 矩形的宽度
rect_x_left <- -max_value * 0.08 - rect_width/2  # 矩形的左边界
rect_x_right <- -max_value * 0.08 + rect_width/2  # 矩形的右边界

# 创建图表
p_bri_continent <- ggplot(bri_continent_avg, aes(y = Variable)) +
  # 添加交替的背景色
  geom_rect(data = bri_continent_avg %>% filter(id %% 2 == 0), 
            inherit.aes = FALSE,
            aes(ymin = as.numeric(Variable) - 0.5, 
                ymax = as.numeric(Variable) + 0.5), 
            xmin = -100,  # 使用固定值3000作为参考
            xmax = 0,    # 使用固定值3000作为参考，缩小右侧边界
            fill = "grey90", alpha = 0.5) +
  # 添加点估计和置信区间 - 使用指标对应的颜色
  scale_color_manual(values = metric_colors) +
  scale_fill_manual(values = metric_colors) +
  labs(title = "",
       x = NULL, y = NULL) + 
  # 设置x轴范围，X轴最大范围位于3000左右
  coord_cartesian(clip = "off", 
                  xlim = c(-100, 0)) +  # X轴最大范围设为3000
  # 左侧：添加扁平长方形表示指标
  geom_rect(data = bri_continent_avg,
            aes(xmin = -105 ,  # 使用3000重新计算
                xmax = -101,  # 使用3000重新计算
                ymin = as.numeric(Variable) - 0.4, 
                ymax = as.numeric(Variable) + 0.4,
                fill = metric),
            color = "white", size = 0.8) +
  # 在左侧长方形中添加指标缩写 - 使用白色文本
  geom_text(data = bri_continent_avg,
            aes(x = -103, y = Variable, label = metric),  # 使用3000计算位置
            size = 3.2, fontface = "bold", color = "white") +
  # 右侧：添加完整的置信区间文本（包含均值）- 使用指标对应的颜色
  geom_text(data = bri_continent_avg,
            aes(x = -99, y = Variable,  # 使用3000计算位置
                label = sprintf("%.2f (%.2f-%.2f)", mean_value, ci_lower, ci_upper),
                color = metric),
            size = 3.5, fontface = "bold", hjust = 0) +
  # 只在整个图表最上方添加一个列标题
  annotate("text", x = -1000 * 0.05,  # 使用3000计算位置
           y = max(as.numeric(bri_continent_avg$Variable)) + 0.8,
           label = "", size = 4.5, fontface = "bold", hjust = 0) +
  # 添加大洲分组 - 使用facet_wrap并将strip.position设为left
  facet_grid(continent ~ ., scales = "free_y", space = "free_y", switch = "y") +
  # 主题设置
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.y = element_blank(),  # 隐藏y轴标签
    axis.text.x = element_blank(),  # 隐藏x轴标签（去掉X轴刻度）
    axis.ticks.x = element_blank(), # 去掉X轴刻度线
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(0.8, "lines"),
    # 将分面标签移到左侧并调整样式 - 缩小与大洲分组标签和measure列之间的距离
    strip.background = element_rect(fill = "slategray4", color = "grey20"),
    strip.text.y = element_text(color = "white", face = "bold", size = 11, 
                                angle = 0, hjust = 0.5, 
                                margin = margin(r = 2)),  # 减小右边距，从10改为2
    # 使用switch = "y"后，需要调整strip的位置
    strip.placement = "outside",  # 将分面标签放在外部
    legend.position = "none",  # 移除图例，因为颜色已经通过标签明确
    plot.margin = margin(1, 1, 1, 0.5, unit = "cm")  # 缩小左侧边距，从2cm改为0.5cm
  )

p_bri_continent



p02 <- ggplot(bri_continent_avg, aes(y = Variable)) +
  # 添加交替的背景色
  geom_rect(data = bri_continent_avg %>% filter(id %% 2 == 0), 
            inherit.aes = FALSE,
            aes(ymin = as.numeric(Variable) - 0.5, 
                ymax = as.numeric(Variable) + 0.5), 
            xmin = -max_value * 0.15, 
            xmax = max_value * 1.15,
            fill = "grey90", alpha = 0.5) +
  # 添加点估计和置信区间 - 使用指标对应的颜色
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper, color = metric), 
                 height = 0.15, size = 0.8) +
  geom_point(aes(x = mean_value, y = Variable, color = metric, fill = metric), 
             shape = 21, size = 3.5, stroke = 1.2) +
  scale_color_manual(values = metric_colors) +
  scale_fill_manual(values = metric_colors) +
  labs(title = "",
       x = "Rate(/100K)", y = NULL) + 
  coord_cartesian(clip = "off") +
  # 设置X轴 - 使用K格式
  scale_x_continuous(
    labels = k_format,
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  # 添加大洲分组
  facet_grid(continent ~ ., scales = "free_y", space = "free_y") +
  # 主题设置
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black", face = "bold"),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(0.8, "lines"),
    # 隐藏右侧的大洲标签（strip.text.y和strip.background.y）
    strip.text.y = element_blank(),
    strip.background.y = element_blank(),
    strip.text.x = element_text(color = "white", face = "bold", size = 11),
    strip.background = element_rect(fill = "slategray4", color = "grey20"),
    legend.position = "none",  # 移除图例，因为颜色已经通过标签明确
    plot.margin = margin(1, 2, 1, 1, unit = "cm")
  )

p02


# 使用cowplot包的align_plots函数（如果安装）
if (!require("cowplot")) {
  install.packages("cowplot")
  library(cowplot)
}

# 对齐两个图的高度
aligned_plots <- align_plots(p_bri_continent_adj, p02_adj, align = "v")

# 然后使用plot_grid组合
combined_plot <- plot_grid(
  aligned_plots[[1]], 
  aligned_plots[[2]],
  nrow = 1,
  rel_widths = c(0.8, 1.2),
  align = "h"
)

combined_plot

