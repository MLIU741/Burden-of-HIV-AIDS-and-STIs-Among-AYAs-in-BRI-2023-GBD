####8.年龄差异####

#####8.1 数据处理#####
#
age_all <- read.csv("2023-204-teen-STD分年龄段ASIR,ASPR,ASMR,ASDR.csv")
bri_cnt <- read.csv("BRI-long-location.csv")
#
age_matched <- inner_join(
  age_all,
  distinct(bri_cnt, location, continent),
  by = "location"
)
#
age_matched <- age_matched %>% 
  mutate(measure = case_match(trimws(toupper(measure)),
                              "INCIDENCE" ~ "ASIR",
                              "PREVALENCE" ~ "ASPR",
                              "DEATHS" ~ "ASMR",
                              "DALYS (DISABILITY-ADJUSTED LIFE)" ~ "ASDR")) %>% 
  mutate(measure = as.character(measure))
head(age_matched)
## 1. 按 continent 拆列表
conti_list <- split(age_matched, age_matched$continent)



## 三级拆分：先大洲 → 再指标
for (conti in names(conti_list)) {
  safe_conti <- str_replace_all(conti, " ", "_")   # 大洲安全名
  df_conti   <- conti_list[[conti]]                # 取出大洲数据
  
  ## 按 measure 拆
  measure_sub <- split(df_conti, df_conti$measure)
  
  ## 落地到全局：{大洲}_{指标} 形式
  for (m in names(measure_sub)) {
    obj_name <- str_c(safe_conti, "_", m)          # 如 Europe_ASPR
    assign(obj_name, measure_sub[[m]], envir = .GlobalEnv)
  }
}

head(Africa_ASIR)
Africa_ASPR
Africa_ASMR
Africa_ASDR
Asia_ASIR
Asia_ASPR
Asia_ASMR
Asia_ASDR
Europe_ASIR
Europe_ASPR
Europe_ASMR
Europe_ASDR
North_America_ASIR
North_America_ASPR
North_America_ASMR
North_America_ASDR
Oceania_ASIR
Oceania_ASPR
Oceania_ASMR
Oceania_ASDR
South_America_ASIR
South_America_ASPR
South_America_ASMR
South_America_ASDR

#####8.2 年龄组差异分析 ####
#1. 数据处理 

age_all <- read.csv("2023-204-teen-STD分年龄段ASIR,ASPR,ASMR,ASDR.csv")
bri_cnt <- read.csv("BRI-long-location.csv")
#
age_matched <- inner_join(
  age_all,
  distinct(bri_cnt, location, continent),
  by = "location"
)
#
age_matched <- age_matched %>% 
  mutate(measure = case_match(trimws(toupper(measure)),
                              "INCIDENCE" ~ "ASIR",
                              "PREVALENCE" ~ "ASPR",
                              "DEATHS" ~ "ASMR",
                              "DALYS (DISABILITY-ADJUSTED LIFE)" ~ "ASDR")) %>% 
  mutate(measure = as.character(measure))
# 按 measure 拆分成 4 份
measure_list_age <- split(age_matched, age_matched$measure)

# 2. 不同年龄组val值分布的箱线图（最终优化版）#
# 最终优化绘图函数
plot_age_distribution_final <- function(dat, title) {
  # 根据不同的measure调整y轴范围和单位
  max_val <- max(dat$val, na.rm = TRUE)
  
  # 为ASMR特别处理纵轴范围
  if (title == "MR" && max_val < 10) {
    # 如果ASMR值很小，使用原始数值
    y_info <- list(limits = c(0, max_val * 1.1), label = "MR (/100K)")
    break_interval <- ifelse(max_val > 5, 2, 
                             ifelse(max_val > 2, 1, 0.5))
    use_k_format <- FALSE
  } else {
    # 其他情况使用K格式
    y_info <- list(limits = c(0, max_val * 1.1), 
                   label = paste0(title, " (/100K)"))
    break_interval <- ifelse(max_val > 100000, 20000,
                             ifelse(max_val > 50000, 10000,
                                    ifelse(max_val > 20000, 5000,
                                           ifelse(max_val > 10000, 2000, 1000))))
    use_k_format <- TRUE
  }
  
  ggplot(dat, aes(x = continent, y = val, fill = age)) +
    # 箱线图
    geom_boxplot(alpha = 0.8, outlier.shape = NA, linewidth = 0.5, width = 0.6) +
    # 散点图 - 透明度调整到0.6
    geom_point(aes(color = age), 
               position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
               alpha = 0.6, size = 1.0, shape = 16) +  # 透明度调整到0.6
    
    # 颜色设置
    scale_fill_manual(values = c("10-14" = "#66C2A5",
                                 "15-19" = "#FC8D62", 
                                 "20-24" = "#8DA0CB"),
                      name = "Age Group") +
    scale_color_manual(values = c("10-14" = "#66C2A5",
                                  "15-19" = "#FC8D62",
                                  "20-24" = "#8DA0CB"),
                       name = "Age Group") +
    
    # 坐标轴设置
    coord_cartesian(ylim = y_info$limits) +
    scale_y_continuous(
      labels = function(x) {
        if(use_k_format && max_val > 10000) {
          # 使用K为单位
          paste0(x/1000, "K")
        } else {
          # 对于较小数值保持原样
          format(x, big.mark = ",", scientific = FALSE)
        }
      },
      breaks = seq(0, y_info$limits[2], by = break_interval)
    ) +
    
    # 主题设置
    theme_classic() +
    theme(
      plot.title = element_blank(),  # 去掉表头
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
      axis.text.y = element_text(color = "black", size = 9),
      axis.title = element_text(size = 11),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.position = "right",
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10),
      panel.grid.major.y = element_line(color = "grey93", linewidth = 0.3),
      panel.grid.minor.y = element_blank(),
      plot.margin = margin(5, 10, 5, 5)
    ) +
    labs(x = "",
         y = y_info$label)
}

# 生成最终优化版的4个图
p_age1_final <- plot_age_distribution_final(measure_list_age[["ASIR"]], "IR")
p_age2_final <- plot_age_distribution_final(measure_list_age[["ASPR"]], "PR") 
p_age3_final <- plot_age_distribution_final(measure_list_age[["ASMR"]], "MR")
p_age4_final <- plot_age_distribution_final(measure_list_age[["ASDR"]], "DR")

# 显示单个图形检查效果，特别是ASMR
p_age3_final  # 检查ASMR的纵轴刻度

# 拼合成一排
p_age_dist_final <- ggarrange(p_age1_final, p_age2_final, p_age3_final, p_age4_final,
                              nrow = 1, ncol = 4,
                              common.legend = TRUE, legend = "bottom",
                              labels = c("A", "B", "C", "D"))

p_age_dist_final


#3. 不同年龄组val值分布的箱线图（简化稳定版）#
# 简化的绘图函数 - 不使用断裂坐标轴
plot_age_distribution_simple <- function(dat, title) {
  max_val <- max(dat$val, na.rm = TRUE)
  y_label <- paste0(title, " (/100K)")
  
  # 创建基础图形
  p <- ggplot(dat, aes(x = continent, y = val, fill = age)) +
    # 箱线图
    geom_boxplot(alpha = 0.8, outlier.shape = NA, linewidth = 0.5, width = 0.6) +
    # 散点图
    geom_point(aes(color = age), 
               position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
               alpha = 0.6, size = 1.0, shape = 16) +
    
    # 颜色设置
    scale_fill_manual(values = c("10-14" = "#66C2A5",
                                 "15-19" = "#FC8D62", 
                                 "20-24" = "#8DA0CB"),
                      name = "Age Group") +
    scale_color_manual(values = c("10-14" = "#66C2A5",
                                  "15-19" = "#FC8D62",
                                  "20-24" = "#8DA0CB"),
                       name = "Age Group") +
    
    # 主题设置
    theme_classic() +
    theme(
      plot.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
      axis.text.y = element_text(color = "black", size = 9),
      axis.title = element_text(size = 11),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.position = "right",
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10),
      panel.grid.major.y = element_line(color = "grey93", linewidth = 0.3),
      panel.grid.minor.y = element_blank(),
      plot.margin = margin(5, 10, 5, 5)
    ) +
    labs(x = "",
         y = y_label)
  
  # 根据数据类型设置y轴
  if (title %in% c("ASMR", "ASDR")) {
    # 对于ASMR和ASDR，使用更合适的y轴范围
    # 计算合适的y轴上限，排除极端异常值
    q95 <- quantile(dat$val, 0.95, na.rm = TRUE)
    y_upper <- min(max_val, q95 * 1.5)  # 使用95%分位数的1.5倍或最大值中的较小者
    
    p <- p + 
      coord_cartesian(ylim = c(0, y_upper)) +
      scale_y_continuous(
        labels = function(x) {
          if (max_val > 10000) {
            paste0(x/1000, "K")
          } else {
            format(round(x, 1), nsmall = 1)
          }
        }
      )
  } else {
    # 对于ASIR和ASPR，使用常规y轴
    p <- p +
      coord_cartesian(ylim = c(0, max_val * 1.1)) +
      scale_y_continuous(
        labels = function(x) {
          if (max_val > 10000) {
            paste0(x/1000, "K")
          } else {
            format(round(x, 1), nsmall = 1)
          }
        }
      )
  }
  
  return(p)
}

# 生成简化版的4个图
p_age1_simple <- plot_age_distribution_simple(measure_list_age[["ASIR"]], "IR")
p_age2_simple <- plot_age_distribution_simple(measure_list_age[["ASPR"]], "PR")
p_age3_simple <- plot_age_distribution_simple(measure_list_age[["ASMR"]], "MR")
p_age4_simple <- plot_age_distribution_simple(measure_list_age[["ASDR"]], "DR")

# 显示单个图形检查效果
p_age1_simple
p_age2_simple
p_age3_simple
p_age4_simple

# 拼合成一排
p_age_dist_simple <- ggarrange(p_age1_simple, p_age2_simple, p_age3_simple, p_age4_simple,
                               nrow = 2, ncol = 2,
                               common.legend = TRUE, legend = "bottom")

p_age_dist_simple



##### 3.5 年龄组比值数据准备 #####
# 说明：原先缺少这一步，导致第4步的 ASIR_ratio 等对象不存在而报错。
#       此处补齐后，第4步（比值箱线图）、第5步（分类占比堆积图）、第6步（t检验）共用。
age_ratio_wide <- age_matched %>%
  select(measure, continent, location, age, val) %>%
  pivot_wider(names_from = age, values_from = val) %>%
  mutate(ratio_15_19 = `15-19` / `10-14`,
         ratio_20_24 = `20-24` / `10-14`)

# 四张比值表：下游函数固定引用 continent / ratio_15_19 / ratio_20_24 这三列
for (m in c("ASIR", "ASPR", "ASMR", "ASDR")) {
  assign(paste0(m, "_ratio"), filter(age_ratio_wide, measure == m))
}

# 长表：供第6步 t 检验使用（measure 须为 IR/PR/MR/DR，age_comparison 标签须与图例一致）
age_ratio_all <- age_ratio_wide %>%
  pivot_longer(c(ratio_15_19, ratio_20_24),
               names_to = "ratio_type", values_to = "ratio") %>%
  mutate(age_comparison = recode(ratio_type,
                                 ratio_15_19 = "15-19 vs 10-14",
                                 ratio_20_24 = "20-24 vs 10-14"),
         measure = recode(measure, ASIR = "IR", ASPR = "PR",
                          ASMR = "MR", ASDR = "DR"))

# 分类占比：供第5步堆积图使用（阈值与 5.6.4 性别比一致，取 ±5%）
age_diff_analysis <- age_ratio_all %>%
  mutate(cat = case_when(ratio > 1.05 ~ "older_higher",
                         ratio < 0.95 ~ "younger_higher",
                         TRUE         ~ "similar")) %>%
  count(measure, continent, age_comparison, cat) %>%
  group_by(measure, continent, age_comparison) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(-n) %>%                                # 关键：n 若留着会进入 id，导致无法合并
  pivot_wider(names_from = cat, values_from = prop, values_fill = 0) %>%
  transmute(measure, continent, age_comparison,
            prop_older_higher   = older_higher,
            prop_similar        = similar,
            prop_younger_higher = younger_higher)

#4. 相对差异分析（年龄组比值）- 稳定版
# 稳定版的绘图函数，不使用断裂坐标轴
plot_age_ratio_stable <- function(dat, title) {
  # 转换为长格式
  dat_long <- dat %>%
    pivot_longer(cols = c(ratio_15_19, ratio_20_24),
                 names_to = "ratio_type", 
                 values_to = "ratio_value") %>%
    mutate(ratio_type = factor(ratio_type,
                               levels = c("ratio_15_19", "ratio_20_24"),
                               labels = c("15-19/10-14", "20-24/10-14")))
  
  # 计算合适的y轴范围，排除极端异常值
  q95 <- quantile(dat_long$ratio_value, 0.95, na.rm = TRUE)
  q99 <- quantile(dat_long$ratio_value, 0.99, na.rm = TRUE)
  max_ratio <- max(dat_long$ratio_value, na.rm = TRUE)
  
  # 设置y轴上限，避免极端值影响可视化
  y_upper <- ifelse(max_ratio > 3 * q95, min(max_ratio, q99 * 1.5), max_ratio * 1.1)
  
  ggplot(dat_long, aes(x = continent, y = ratio_value, 
                       fill = ratio_type, color = ratio_type)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA, linewidth = 0.6, width = 0.6) +
    geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
               alpha = 0.6, size = 1.2, shape = 16) +
    
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.4) +
    
    # 使用两种新颜色，修改图例标题
    scale_fill_manual(values = c("15-19/10-14" = "#1f78b4",  # 蓝色
                                 "20-24/10-14" = "#33a02c"), # 绿色
                      name = "Ratio between age groups") +
    scale_color_manual(values = c("15-19/10-14" = "#1f78b4",
                                  "20-24/10-14" = "#33a02c"),
                       name = "Ratio between age groups") +
    
    # 设置y轴范围
    coord_cartesian(ylim = c(0, y_upper)) +
    
    theme_classic() +
    theme(
      plot.title = element_blank(),  # 去掉表头
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
      axis.text.y = element_text(color = "black", size = 9),
      axis.title = element_text(size = 11),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.position = "right",
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10),
      panel.grid.major.y = element_line(color = "grey93", linewidth = 0.3),
      panel.grid.minor.y = element_blank(),
      plot.margin = margin(5, 10, 5, 5)
    ) +
    labs(x = "",
         y = paste0(title, "-Ratio(older/10-14)"))
}

# 生成稳定版的4个比值图
p_ratio1_stable <- plot_age_ratio_stable(ASIR_ratio, "IR")
p_ratio2_stable <- plot_age_ratio_stable(ASPR_ratio, "PR")
p_ratio3_stable <- plot_age_ratio_stable(ASMR_ratio, "MR") 
p_ratio4_stable <- plot_age_ratio_stable(ASDR_ratio, "DR")

# 显示单个图形检查效果
p_ratio1_stable
p_ratio2_stable
p_ratio3_stable
p_ratio4_stable

# 拼合成一排
p_age_ratio_stable <- ggarrange(p_ratio1_stable, p_ratio2_stable, p_ratio3_stable, p_ratio4_stable,
                                nrow = 2, ncol = 2,
                                common.legend = TRUE, legend = "bottom")

p_age_ratio_stable

Paangb<-ggarrange(p_age_dist_simple, p_age_ratio_stable,
                  nrow = 2, ncol = 1,
                  labels = c("A", "B"))
Paangb

#5. 大洲间年龄差异统计 - 优化版 
# 绘制堆积条图（去掉表头 + 新配色）
p_age_diff_type_opt <- age_diff_analysis %>%
  select(measure, continent, age_comparison, prop_older_higher, prop_similar, prop_younger_higher) %>%
  pivot_longer(cols = c(prop_older_higher, prop_similar, prop_younger_higher),
               names_to = "diff_type", values_to = "proportion") %>%
  mutate(
    diff_type = factor(diff_type,
                       levels = c("prop_older_higher", "prop_similar", "prop_younger_higher"),
                       labels = c("Older higher", "Similar", "Younger higher")),
    measure = factor(measure, levels = c("IR", "PR", "MR", "DR"))
  ) %>%
  ggplot(aes(x = age_comparison, y = proportion, fill = diff_type)) +
  geom_col(position = "stack", width = 0.7) +
  facet_grid(continent ~ measure) +
  scale_y_continuous(labels = scales::percent) +
  # 使用新的配色方案
  scale_fill_manual(values = c("Older higher" = "firebrick4",  
                               "Similar"       = "green4",  
                               "Younger higher" = "royalblue2")) + 
  labs(
    title = NULL,  # 去掉表头题目
    x = "",
    y = "Proportion of countries",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

p_age_diff_type_opt

#6. t检验差异统计 
# 配对t检验
age_ttest_results <- age_ratio_all %>%
  group_by(measure, continent, age_comparison) %>%
  summarise(
    n_countries = n(),
    t_test_result = list(tryCatch(
      t.test(ratio, mu = 1, alternative = "two.sided"),
      error = function(e) NULL
    )),
    .groups = "drop"
  ) %>%
  mutate(
    t_statistic = map_dbl(t_test_result, ~ if(!is.null(.x)) .x$statistic else NA),
    p_value = map_dbl(t_test_result, ~ if(!is.null(.x)) .x$p.value else NA),
    mean_ratio = map_dbl(t_test_result, ~ if(!is.null(.x)) .x$estimate else NA),
    ci_lower = map_dbl(t_test_result, ~ if(!is.null(.x)) .x$conf.int[1] else NA),
    ci_upper = map_dbl(t_test_result, ~ if(!is.null(.x)) .x$conf.int[2] else NA)
  ) %>%
  select(-t_test_result)

# 可视化t检验结果
p_age_ttest <- age_ttest_results %>%
  filter(!is.na(mean_ratio)) %>%
  mutate(measure = factor(measure, levels = c("IR", "PR", "MR", "DR"))) %>%
  ggplot(aes(x = continent, y = mean_ratio - 1, fill = age_comparison)) +
  geom_col(position = position_dodge(0.8), alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_lower - 1, ymax = ci_upper - 1),
                position = position_dodge(0.8), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~measure, nrow = 1) +
  scale_fill_manual(values = c("15-19 vs 10-14" = "sienna3",
                               "20-24 vs 10-14" = "mediumpurple3"),
                    name = "Age Comparison") +
  labs(
    title = "Deviation of age ratio from 1 by indicator and continent",
    subtitle = "Error bars = 95% CI; positive = older higher, negative = younger higher",
    x = "",
    y = "Mean ratio deviation (older/10-14 − 1)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor.y = element_blank()
  )

p_age_ttest

Pcangd<-ggarrange(p_age_diff_type_opt, p_age_ttest,
                  nrow = 2, ncol = 1,
                  labels = c("C", "D"))
Pcangd
#7. 最终图形组合
# 组合所有图形
final_age_plot <- ggarrange(
  Paangb,           # 年龄分布图
  Pcangd,          # 年龄比值图           # t检验结果图
  nrow = 1, ncol = 2)

final_age_plot


#####8.3具体国别图 ASIR-age六洲图#####

## 1. 数据预处理（沿用你已有的 Africa_ASIR）

## 0. 预先计算 val 范围（K 单位）
Africa_ASIR <- Africa_ASIR %>% 
  mutate(
    location = factor(location, levels = sort(unique(location))),
    age      = factor(age, levels = c("10-14", "15-19", "20-24")),
    val_k    = val / 1000                       # 转 K
  )


## 1. 绘图

Africa_ASIR_bar <- ggplot(
  Africa_ASIR,
  aes(x = val_k, y = location, fill = age, xmin = lower / 1000, xmax = upper / 1000)
) +
  geom_col(width = 0.6, colour = "black", size = 0.15) +
  geom_errorbar(width = 0.2, colour = "black", size = 0.15) +
  
  ## 关键：横轴 20K 步长
  scale_x_continuous(
    breaks = seq(0, ceiling(max(Africa_ASIR$val_k, na.rm = TRUE) / 20) * 20, by = 20),
    labels = ~ paste0(.x, "K")
  ) +
  
  scale_fill_manual(values = c("10-14" = "#3B6AA0","15-19" = "#F4A261","20-24" = "#6AB04C")) +
  labs(x = NULL, y = NULL, title = "IR") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.2, colour = "grey80"),
    axis.text.y = element_text(size = 8)
  )
Africa_ASIR_bar


Africa_ASPR <- Africa_ASPR %>% 
  mutate(location = factor(location, levels = sort(unique(location))),
         age      = factor(age, levels = c("10-14","15-19","20-24")),
         val_k    = val / 1000,
         lower_k  = lower / 1000,
         upper_k  = upper / 1000)

Africa_ASPR_bar <- ggplot(Africa_ASPR, aes(x = val_k, y = location, fill = age,
                                           xmin = lower_k, xmax = upper_k)) +
  geom_col(width = 0.6, colour = "black", size = 0.15) +
  geom_errorbar(width = 0.2, colour = "black", size = 0.15) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 20),   # 0-100K，20K 步长
    labels = ~ paste0(.x, "K")
  ) +
  scale_fill_manual(values = c("10-14" = "#3B6AA0","15-19" = "#F4A261","20-24" = "#6AB04C")) +
  labs(x = NULL, y = NULL, title = "PR", fill = "ASPR") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2, colour = "grey80"),
        axis.text.y = element_text(size = 8)
  )
Africa_ASPR_bar 

Africa_ASMR <- Africa_ASMR %>% 
  mutate(location = factor(location, levels = sort(unique(location))),
         age      = factor(age, levels = c("10-14","15-19","20-24")))

## 按实际上限取整到 20 倍数
x_max  <- ceiling(max(Africa_ASMR$val, na.rm = TRUE) / 20) * 20

Africa_ASMR_bar <- ggplot(Africa_ASMR, aes(x = val, y = location, fill = age,
                                           xmin = lower, xmax = upper)) +
  geom_col(width = 0.6, colour = "black", size = 0.15) +
  geom_errorbar(width = 0.2, colour = "black", size = 0.15) +
  scale_x_continuous(
    breaks = seq(0, 280, by = 40),
    labels = ~ .x
  ) +
  scale_fill_manual(values = c("10-14" = "#3B6AA0","15-19" = "#F4A261","20-24" = "#6AB04C")) +
  labs(x = NULL, y = NULL, title = "MR", fill = "ASMR") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2, colour = "grey80"),
        axis.text.y = element_text(size = 8))
Africa_ASMR_bar

Africa_ASDR <- Africa_ASDR %>% 
  mutate(location = factor(location, levels = sort(unique(location))),
         age      = factor(age, levels = c("10-14","15-19","20-24")),
         val_k    = val / 1000,
         lower_k  = lower / 1000,
         upper_k  = upper / 1000)

fix_k <- c(3, 6, 9, 12)

Africa_ASDR_bar <- ggplot(Africa_ASDR, aes(x = val_k, y = location, fill = age,
                                           xmin = lower_k, xmax = upper_k)) +
  geom_col(width = 0.6, colour = "black", size = 0.15) +
  geom_errorbar(width = 0.2, colour = "black", size = 0.15) +
  
  ## 统一刻度 3K/6K/9K/12K
  scale_x_continuous(
    #    breaks = fix_k,
    labels = ~ paste0(.x, "K")
  ) +
  
  scale_fill_manual(values = c("10-14" = "#3B6AA0","15-19" = "#F4A261","20-24" = "#6AB04C")) +
  labs(x = NULL, y = NULL, title = "DR", fill = "ASDR") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2, colour = "grey80"),
        axis.text.y = element_text(size = 8))
Africa_ASDR_bar