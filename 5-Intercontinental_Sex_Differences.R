#####5.6大洲性别差异#####
######5.6.1数据处理#####
# 1. 读数据 
IPMD9023coun <- read.csv("1990和2023全球青少年STD204国家ASIR,ASPR,ASMR,ASDR男女.csv")
bri_loc      <- read.csv("BRI-long-location.csv")

#2. 合并 + 筛选 2023 年 
matched_raw <- IPMD9023coun %>%
  inner_join(bri_loc, by = "location") %>%
  filter(year == 2023)

#3. 按真实 measure 名称拆成 4 份 
measure_list <- split(matched_raw, matched_raw$measure)

#4. 计算指标函数
calc_index <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df %>%
    group_by(location, continent) %>%
    summarise(
      female_val = val[sex == "Female"],
      male_val   = val[sex == "Male"],
      .groups    = "drop"
    ) %>%
    mutate(
      gender_ratio  = female_val / male_val,
      absolute_diff = female_val - male_val
    )
}

# 5. 生成 4 个独立表格
Incidence_tbl  <- calc_index(measure_list[["Incidence"]])
Prevalence_tbl <- calc_index(measure_list[["Prevalence"]])
Deaths_tbl     <- calc_index(measure_list[["Deaths"]])
DALYs_tbl      <- calc_index(measure_list[["DALYs (Disability-Adjusted Life Years)"]])

######5.6.2相对差异#####
# 1. 快速统一列名（保险） 
col_names <- c("location", "continent", "female_val", "male_val",
               "gender_ratio", "absolute_diff")
for(tbl in list(Incidence_tbl, Prevalence_tbl, Deaths_tbl, DALYs_tbl)){
  colnames(tbl) <- col_names
}

# 2. 绘图函数 —— 单张箱线图 + y=1 参考线 
plot_box <- function(dat, title){
  ggplot(dat, aes(x = continent, y = gender_ratio,
                  fill = continent, color = continent)) +
    
    # 1. 箱线
    geom_boxplot(alpha = 0.7, outlier.shape = NA, linewidth = 0.6, width = 0.5) +
    
    # 2. 散点
    geom_point(position = position_jitter(width = 0.25, height = 0),
               alpha = 0.5, size = 1.8, shape = 16) +
    
    # 3. 参考线
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.4) +
    
    # 4. 配色（图例标题置空）
    scale_fill_manual(values = c(
      "Africa" = "orangered4", "Asia" = "#377EB8", "Europe" = "#4DAF4A",
      "North America" = "thistle3", "South America" = "darkgoldenrod1", "Oceania" = "orchid4"),
      name = element_blank()) +   # ← 去掉图例标题
    scale_color_manual(values = c(
      "Africa" = "orangered4", "Asia" = "#377EB8", "Europe" = "#4DAF4A",
      "North America" = "thistle3", "South America" = "darkgoldenrod1", "Oceania" = "orchid4"),
      name = element_blank()) +   # ← 去掉图例标题
    
    # 5. 主题不变
    theme_classic() +
    theme(
      plot.title       = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.x      = element_text(angle = 45, hjust = 1, color = "black"),
      axis.text.y      = element_text(color = "black"),
      axis.title       = element_text(size = 11),
      legend.position  = "right",
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
      panel.grid.minor.y = element_blank()
    ) +
    labs(title = title,
         x = "",
         y = "Gender ratio (female / male)")
}
p601 <- plot_box(Incidence_tbl,  "IR")
p602 <- plot_box(Prevalence_tbl, "PR")
p603 <- plot_box(Deaths_tbl,     "MR")
p604 <- plot_box(DALYs_tbl,      "DR")
p601


p601234<-ggarrange(p601,p602,p603,p604,
                   nrow = 2,ncol=2,
                   common.legend = T,legend = 'bottom')
p601234


######5.6.3绝对差异#####

#1通用绘图函数
plot_abs_diff <- function(dat, title){
  ggplot(dat, aes(x = continent, y = absolute_diff, fill = continent, color = continent)) +
    geom_boxplot(outlier.shape = NA, linewidth = 0.3, alpha = 0.7, width = 0.6) +
    geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
               size = 1, alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue", linewidth = 0.8) +
    
    scale_fill_manual(values = c(
      "Africa" = "orangered4", "Asia" = "#377EB8", "Europe" = "#4DAF4A",
      "North America" = "thistle3", "South America" = "darkgoldenrod1", "Oceania" = "orchid4"),
      name = element_blank())+
    scale_color_manual(values = c(
      "Africa" = "orangered4", "Asia" = "#377EB8", "Europe" = "#4DAF4A",
      "North America" = "thistle3", "South America" = "darkgoldenrod1", "Oceania" = "orchid4"),
      name = element_blank()) +
    labs(title = title,
         subtitle = "Absolute difference = Female − Male (blue line = 0)",
         x = "",
         y = "Absolute difference",
         fill = "Continent",
         color = "Continent") +
    
    theme_classic() +
    theme(
      plot.title       = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle    = element_text(size = 10, hjust = 0.5),
      axis.text.x      = element_text(angle = 45, hjust = 1, color = "black"),
      axis.text.y      = element_text(color = "black"),
      axis.title       = element_text(size = 11),
      legend.position  = "right",
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
      panel.grid.minor.y = element_blank()
    )
}

#2出图（按新命名）
p701 <- plot_abs_diff(Incidence_tbl,  "IR")
p702 <- plot_abs_diff(Prevalence_tbl, "PR")
p703 <- plot_abs_diff(Deaths_tbl,     "MR")
p704 <- plot_abs_diff(DALYs_tbl,      "DR")
p701

p701234<-ggarrange(p701,p702,p703,p704,
                   nrow = 2,ncol=2,
                   common.legend = T,legend = 'bottom')
p701234



######5.6.4大洲差异约计#####

#0. 把 4 张表合并，并赋予标准缩写 
diff_type_raw <- bind_rows(
  Incidence_tbl  %>% mutate(measure = "IR"),
  Prevalence_tbl %>% mutate(measure = "PR"),
  Deaths_tbl     %>% mutate(measure = "MR"),
  DALYs_tbl      %>% mutate(measure = "DR")
)

#1. 统计差异类型
diff_type_analysis <- diff_type_raw %>%
  group_by(measure, continent) %>%
  summarise(
    n_female_higher = sum(gender_ratio > 1.05),
    n_similar       = sum(gender_ratio >= 0.95 & gender_ratio <= 1.05),
    n_male_higher   = sum(gender_ratio < 0.95),
    total_countries = n(),
    .groups = "drop"
  ) %>%
  mutate(
    prop_female_higher = n_female_higher / total_countries,
    prop_similar       = n_similar       / total_countries,
    prop_male_higher   = n_male_higher   / total_countries
  )

# 2. 长数据 + 因子顺序（保证 ASIR-ASPR-ASMR-ASDR）
p_diff_type <- diff_type_analysis %>%
  select(measure, continent, prop_female_higher, prop_similar, prop_male_higher) %>%
  pivot_longer(cols = c(prop_female_higher, prop_similar, prop_male_higher),
               names_to = "diff_type", values_to = "proportion") %>%
  mutate(
    diff_type = factor(diff_type,
                       levels = c("prop_female_higher", "prop_similar", "prop_male_higher"),
                       labels = c("Female higher", "Similar", "Male higher")),
    measure   = factor(measure, levels = c("IR", "PR", "MR", "DR"))
  ) %>%
  ggplot(aes(x = measure, y = proportion, fill = diff_type)) +
  geom_col(position = "stack", width = 0.7) +
  facet_wrap(~continent, nrow = 2) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Female higher" = "#E41A1C",
                               "Similar"       = "#377EB8",
                               "Male higher"   = "#4DAF4A")) +
  labs(
    title = "Proportion of countries by gender difference type across continents",
    x = "",
    y = "Proportion of countries",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, color = "black"),
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.text       = element_text(face = "bold"),
    legend.position  = "right"
  )

#3. 出图 
p_diff_type

######5.6.5 t检验差异约计#####

#0. 合并 4 表并赋予标准缩写
matched_data <- bind_rows(
  Incidence_tbl  %>% mutate(measure = "IR"),
  Prevalence_tbl %>% mutate(measure = "PR"),
  Deaths_tbl     %>% mutate(measure = "MR"),
  DALYs_tbl      %>% mutate(measure = "DR")
)

#1. 配对 t 检验（按 measure & continent 分组）
paired_ttest_results <- matched_data %>%
  group_by(measure, continent) %>%
  summarise(
    n_countries = n(),
    t_test_result = list(tryCatch(
      t.test(gender_ratio, mu = 1, alternative = "two.sided"),
      error = function(e) NULL
    )),
    .groups = "drop"
  ) %>%
  mutate(
    t_statistic = map_dbl(t_test_result, ~ if(!is.null(.x)) .x$statistic else NA),
    p_value     = map_dbl(t_test_result, ~ if(!is.null(.x)) .x$p.value   else NA),
    mean_ratio  = map_dbl(t_test_result, ~ if(!is.null(.x)) .x$estimate  else NA),
    ci_lower    = map_dbl(t_test_result, ~ if(!is.null(.x)) .x$conf.int[1] else NA),
    ci_upper    = map_dbl(t_test_result, ~ if(!is.null(.x)) .x$conf.int[2] else NA)
  ) %>%
  select(-t_test_result)

# 2. 可视化（英文版）
p_ttest <- paired_ttest_results %>%
  filter(!is.na(mean_ratio)) %>%
  mutate(measure = factor(measure, levels = c("IR", "PR", "MR", "DR"))) %>%  # 固定顺序
  ggplot(aes(x = measure, y = mean_ratio - 1, fill = continent)) +
  geom_col(position = position_dodge(0.8), alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_lower - 1, ymax = ci_upper - 1),
                position = position_dodge(0.8), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c(
    "Africa" = "#E41A1C", "Asia" = "#377EB8", "Europe" = "#4DAF4A",
    "North America" = "#984EA3", "South America" = "#FF7F00", "Oceania" = "#FFFF33"),
    name = element_blank()) +
  labs(
    title = "Deviation of gender ratio from 1 by indicator and continent",
    subtitle = "Error bars = 95% CI; positive = female higher, negative = male higher",
    x = "",
    y = "Mean ratio deviation (female/male − 1)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, color = "black"),
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 10),
    legend.title     = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor.y = element_blank()
  )

p_ttest



#1. 仅合并 A+B 图例 
pAB <- ggarrange(p601234, p701234,
                 nrow = 1, ncol = 2,
                 common.legend = TRUE, legend = "bottom",
                 labels = c("A", "B"))
pAB
pCD<-ggarrange(p_diff_type,  p_ttest,
               nrow = 1, ncol = 2,
               common.legend = F, 
               labels = c("C", "D"))
pCD
# 2. 组合 A/B/C/D 并加字母标签 
final_plot <- ggarrange(
  pAB, pCD,       
  nrow = 2,ncol = 1)

# 3. 查看 / 保存
final_plot
