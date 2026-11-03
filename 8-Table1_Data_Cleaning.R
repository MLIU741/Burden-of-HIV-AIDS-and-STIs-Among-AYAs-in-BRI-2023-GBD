####7.表1####
#1. 读取原始文件 
bri_loc <- read.csv("BRI-long-location.csv") %>% distinct(location, .keep_all = TRUE)

df_change_num  <- read.csv("1990-2023全球204国家发病数变化百分比.csv")
df_change_asir <- read.csv("1990-2023全球204国家ASIR变化百分比.csv")
df_ts_num      <- read.csv("全球204国家青少年性病总1990-2023发病数.csv")
df_ts_asir     <- read.csv("全球204国家青少年性病总1990-2023发病率.csv")

#2. 清洗：变化百分比
change_num <- df_change_num %>% 
  filter(year_start == 1990, year_end == 2023) %>% 
  transmute(location,
            change_num       = val,
            change_num_lower = lower,
            change_num_upper = upper)

change_asir <- df_change_asir %>% 
  filter(year_start == 1990, year_end == 2023) %>% 
  transmute(location,
            change_asir       = val,
            change_asir_lower = lower,
            change_asir_upper = upper)

#3. 清洗：1990 & 2023 时序 
extract_90_23 <- function(df){
  df %>% 
    filter(year %in% c(1990, 2023)) %>% 
    select(location, year, val, lower, upper) %>% 
    pivot_longer(cols = c(val, lower, upper),
                 names_to = "stat",
                 values_to = "v") %>% 
    unite(tmp, stat, year, sep = "_") %>% 
    pivot_wider(names_from = tmp, values_from = v)
}

num_90_23  <- extract_90_23(df_ts_num)  %>% rename_with(~paste0("num_", .), -location)
asir_90_23 <- extract_90_23(df_ts_asir) %>% rename_with(~paste0("asir_", .), -location)

#4. 合并
final_tbl <- bri_loc %>% 
  left_join(change_num,  by = "location") %>% 
  left_join(change_asir, by = "location") %>% 
  left_join(num_90_23,   by = "location") %>% 
  left_join(asir_90_23,  by = "location")

#5. 计算 EAPC 
eapc_asir <- df_ts_asir %>% 
  filter(measure == "Incidence",
         sex == "Both",
         age == "10-24 years",
         cause == "HIV/AIDS and sexually transmitted infections",
         metric == "Rate",
         year %in% 1990:2023,
         !is.na(val)) %>% 
  group_by(location) %>% 
  do({
    mod <- lm(log(val) ~ year, data = .)
    slope <- coef(mod)[2]
    ci  <- confint(mod, "year", level = 0.95)
    p   <- summary(mod)$coefficients["year", "Pr(>|t|)"]
    tibble(
      eapc          = round(100 * (exp(slope) - 1), 2),
      eapc_ci_lower = round(100 * (exp(ci[1]) - 1), 2),
      eapc_ci_upper = round(100 * (exp(ci[2]) - 1), 2),
      p_value       = ifelse(p < 0.001, "p<0.001", sprintf("%.3f", p))
    )
  }) %>% ungroup()

asir_std_90to23_new <- eapc_asir %>% 
  transmute(
    location,
    EAPC_95CI = paste(comma(eapc, accuracy = 0.01),
                      "\n(",
                      comma(eapc_ci_lower, accuracy = 0.01),
                      "to",
                      comma(eapc_ci_upper, accuracy = 0.01),
                      ")"),
    EAPC_P = p_value
  )

#6. 最终合并 
final_big <- final_tbl %>% left_join(asir_std_90to23_new, by = "location")

# 7. 构造 Table1（0 位整数；2 位小数）
STD_table1_asir <- data.frame(
  continent = final_big$continent,
  location  = final_big$location,
  # 7.1 发病数 1990 （0 位）
  inc_number1990 = paste(
    comma(round(final_big$num_val_1990, 0), accuracy = 1),
    "\n(",
    comma(round(final_big$num_lower_1990, 0), accuracy = 1),
    "to",
    comma(round(final_big$num_upper_1990, 0), accuracy = 1),
    ")"),
  # 7.2 发病数 2023 （0 位）
  inc_number2023 = paste(
    comma(round(final_big$num_val_2023, 0), accuracy = 1),
    "\n(",
    comma(round(final_big$num_lower_2023, 0), accuracy = 1),
    "to",
    comma(round(final_big$num_upper_2023, 0), accuracy = 1),
    ")"),
  # 7.3 发病数变化百分比 （2 位）
  inc_number_change = paste(
    comma(round(final_big$change_num * 100, 2), accuracy = 0.01),
    "\n(",
    comma(round(final_big$change_num_lower * 100, 2), accuracy = 0.01),
    "to",
    comma(round(final_big$change_num_upper * 100, 2), accuracy = 0.01),
    ")"),
  # 7.4 ASIR 1990 （2 位）
  inc_rate1990 = paste(
    comma(round(final_big$asir_val_1990, 2), accuracy = 0.01),
    "\n(",
    comma(round(final_big$asir_lower_1990, 2), accuracy = 0.01),
    "to",
    comma(round(final_big$asir_upper_1990, 2), accuracy = 0.01),
    ")"),
  # 7.5 ASIR 2023 （2 位）
  inc_rate2023 = paste(
    comma(round(final_big$asir_val_2023, 2), accuracy = 0.01),
    "\n(",
    comma(round(final_big$asir_lower_2023, 2), accuracy = 0.01),
    "to",
    comma(round(final_big$asir_upper_2023, 2), accuracy = 0.01),
    ")"),
  # 7.6 ASIR 变化百分比 （2 位）
  inc_rate_change = paste(
    comma(round(final_big$change_asir * 100, 2), accuracy = 0.01),
    "\n(",
    comma(round(final_big$change_asir_lower * 100, 2), accuracy = 0.01),
    "to",
    comma(round(final_big$change_asir_upper * 100, 2), accuracy = 0.01),
    ")"),
  EAPC_95CI = final_big$EAPC_95CI,
  `P-value` = final_big$EAPC_P
) %>% arrange(continent, location)


#8. 导出 
write.csv(STD_table1_with_gap,
          file = "ASIR-EAPC-STD-BRI.csv",
          row.names = FALSE)

####8.S Table 1 数据整理####

#1. 读5份原始文件
bri_loc   <- read.csv("BRI-long-location.csv") %>% distinct(location, .keep_all = TRUE)

df_change_num_p  <- read.csv("1990-2023全球204国家患病数变化百分比.csv")
df_change_aspr <- read.csv("1990-2023全球204国家ASPR变化百分比.csv")
df_ts_num_p      <- read.csv("全球204国家青少年性病总1990-2023患病数.csv")
df_ts_aspr     <- read.csv("全球204国家青少年性病总1990-2023患病率.csv")

#2. 清洗
#5.2.1 变化%（含upper/lower）
df_change_num_p <- df_change_num_p %>% 
  filter(year_start == 1990, year_end == 2023) %>% 
  transmute(location,
            change_num       = val,
            change_num_lower = lower,
            change_num_upper = upper)

df_change_aspr <- df_change_aspr %>% 
  filter(year_start == 1990, year_end == 2023) %>% 
  transmute(location,
            change_aspr       = val,
            change_aspr_lower = lower,
            change_aspr_upper = upper)

# 5.2.2 1990 & 2023 两年时序（含upper/lower）
extract_aspr_90_23 <- function(df) {
  df %>% 
    filter(year %in% c(1990, 2023)) %>% 
    select(location, year, val, lower, upper) %>% 
    pivot_longer(cols = c(val, lower, upper),
                 names_to = "stat",
                 values_to = "v") %>% 
    unite(tmp, stat, year, sep = "_") %>% 
    pivot_wider(names_from = tmp, values_from = v)
}

nump_90_23  <- extract_aspr_90_23(df_ts_num_p)  %>% 
  rename_with(~paste0("num_", .), -location)
aspr_90_23 <- extract_aspr_90_23(df_ts_aspr) %>%
  rename_with(~paste0("aspr_", .), -location)

#5.2.3. 合并
final_tbl_p <- bri_loc %>% 
  left_join(df_change_num_p,  by = "location") %>% 
  left_join(df_change_aspr, by = "location") %>% 
  left_join(nump_90_23,   by = "location") %>% 
  left_join(aspr_90_23,  by = "location")

#5.2.4计算EAPC


eapc_aspr <- df_ts_aspr %>% 
  filter(measure == "Prevalence",
         sex == "Both",
         age == "10-24 years",
         cause == "HIV/AIDS and sexually transmitted infections",
         metric == "Rate",
         year %in% 1990:2023,
         !is.na(val)) %>% 
  group_by(location) %>% 
  do({                       # 每个国家单独拟合
    mod <- lm(log(val) ~ year, data = .)
    slope <- coef(mod)[2]
    ci  <- confint(mod, "year", level = 0.95)
    p   <- summary(mod)$coefficients["year", "Pr(>|t|)"]
    
    tibble(
      eapc          = round(100 * (exp(slope) - 1), 2),
      eapc_ci_lower = round(100 * (exp(ci[1]) - 1), 2),
      eapc_ci_upper = round(100 * (exp(ci[2]) - 1), 2),
      p_value       = ifelse(p < 0.001, "p<0.001", sprintf("%.3f", p))
    )
  }) %>% ungroup()


aspr_std_90to23_new <- eapc_aspr %>% 
  transmute(
    location,
    EAPC_95CI = paste(comma(eapc, accuracy = 0.01),
                      "\n(",
                      comma(eapc_ci_lower, accuracy = 0.01),
                      "to",
                      comma(eapc_ci_upper, accuracy = 0.01),
                      ")"),
    EAPC_P = p_value
  )


# 5.2.6 按 location 右接（保留 BRI 顺序）
final_big_p <- final_tbl_p  %>% 
  left_join(aspr_std_90to23_new, by = "location")


STD_table1_aspr<-data.frame(
  continent = final_big_p$continent,
  location = final_big_p$location,
  pre_number1990 = paste(
    comma(round(final_big_p$num_val_1990,0),accuracy = 1),
    '\n(',
    comma(round(final_big_p$num_lower_1990,0),accuracy = 1),
    'to',
    comma(round(final_big_p$num_upper_1990,0),accuracy = 1),
    ')'),
  pre_number2023 = paste(
    comma(round(final_big_p$num_val_2023,0),accuracy = 1),
    '\n(',
    comma(round(final_big_p$num_lower_2023,0),accuracy = 1),
    'to',
    comma(round(final_big_p$num_upper_2023,0),accuracy = 1),
    ')'),
  pre_number_change =paste(
    comma(round(final_big_p$change_num*100,2),accuracy = 0.01),
    '\n(',
    comma(round(final_big_p$change_num_lower*100,2),accuracy = 0.01),
    'to',
    comma(round(final_big_p$change_num_upper*100,2),accuracy = 0.01),
    ')'),
  pre_rate1990=paste(
    comma(round(final_big_p$aspr_val_1990,2), accuracy = 0.01),
    '\n(',
    comma(round(final_big_p$aspr_lower_1990,2),accuracy = 0.01),
    'to',
    comma(round(final_big_p$aspr_upper_1990,2),accuracy = 0.01),
    ')'),
  pre_rate2023=paste(
    comma(round(final_big_p$aspr_val_2023,2),accuracy = 0.01),
    '\n(',
    comma(round(final_big_p$aspr_lower_2023,2),accuracy = 0.01),
    'to',
    comma(round(final_big_p$aspr_upper_2023,2),accuracy = 0.01),
    ')'),
  pre_rate_change =paste(
    comma(round(final_big_p$change_aspr*100,2),accuracy = 0.01),
    '\n(',
    comma(round(final_big_p$change_aspr_lower*100,2),accuracy = 0.01),
    'to',
    comma(round(final_big_p$change_aspr_upper*100,2),accuracy = 0.01),
    ')'),
  EAPC_95CI=final_big_p$EAPC_95CI,
  `P-value` = final_big_p$EAPC_P)%>% 
  arrange(continent, location)   # 一步到位
write.csv(STD_table1_aspr,'ASPR-EAPC-STD-BRI.csv')#全球各大区大表匹配

####9.SUP Table 2 数据整理####





















# 统一模板：强制保留 n 位小数，并加千位分隔符 ---------------------------
num_fmt <- function(x, digits = 0) {
  formatC(x,
          format   = "f",
          digits   = digits,
          big.mark = ",",
          big.interval = 3)
}

# 1 读 5 份原始文件 
bri_loc <- read.csv("BRI-long-location.csv") %>% 
  distinct(location, .keep_all = TRUE)

df_change_num_m  <- read.csv("1990-2023全球204国家死亡数变化百分比.csv")
df_change_asmr   <- read.csv("1990-2023全球204国家ASMR变化百分比.csv")
df_ts_num_m      <- read.csv("全球204国家青少年性病总1990-2023死亡数.csv")
df_ts_asmr       <- read.csv("全球204国家青少年性病总1990-2023死亡率.csv")

#  变化 %（含上下界）
df_change_num_m <- df_change_num_m %>% 
  filter(year_start == 1990, year_end == 2023) %>% 
  transmute(location,
            change_num       = val,
            change_num_lower = lower,
            change_num_upper = upper)

df_change_asmr <- df_change_asmr %>% 
  filter(year_start == 1990, year_end == 2023) %>% 
  transmute(location,
            change_asmr       = val,
            change_asmr_lower = lower,
            change_asmr_upper = upper)

# 1990 & 2023 两年时序（含上下界）
extract_asmr_90_23 <- function(df) {
  df %>% 
    filter(year %in% c(1990, 2023)) %>% 
    select(location, year, val, lower, upper) %>% 
    pivot_longer(cols = c(val, lower, upper),
                 names_to = "stat",
                 values_to = "v") %>% 
    unite(tmp, stat, year, sep = "_") %>% 
    pivot_wider(names_from = tmp, values_from = v)
}

numm_90_23  <- extract_asmr_90_23(df_ts_num_m) %>% 
  rename_with(~ paste0("num_", .), -location)
asmr_90_23  <- extract_asmr_90_23(df_ts_asmr) %>% 
  rename_with(~ paste0("asmr_", .), -location)

#合并
final_tbl_m <- bri_loc %>% 
  left_join(df_change_num_m, by = "location") %>% 
  left_join(df_change_asmr,  by = "location") %>% 
  left_join(numm_90_23,      by = "location") %>% 
  left_join(asmr_90_23,      by = "location")

#  按国家计算 EAPC（ASMR）
eapc_asmr <- df_ts_asmr %>% 
  filter(measure == "Deaths",
         sex == "Both",
         age == "10-24 years",
         cause == "HIV/AIDS and sexually transmitted infections",
         metric == "Rate",
         year %in% 1990:2023,
         !is.na(val)) %>% 
  group_by(location) %>% 
  filter(n_distinct(val) >= 2) %>% 
  do({
    mod <- lm(log(val) ~ year, data = .)
    slope <- coef(mod)[2]
    ci  <- confint(mod, "year", level = 0.95)
    p   <- summary(mod)$coefficients["year", "Pr(>|t|)"]
    
    tibble(
      eapc          = round(100 * (exp(slope) - 1), 2),
      eapc_ci_lower = round(100 * (exp(ci[1]) - 1), 2),
      eapc_ci_upper = round(100 * (exp(ci[2]) - 1), 2),
      p_value       = ifelse(p < 0.001, "p<0.001", sprintf("%.3f", p))
    )
  }) %>% 
  ungroup()

# 整理 EAPC 字符串（固定 2 位小数）
asmr_std_90to23_new <- eapc_asmr %>% 
  transmute(
    location,
    EAPC_95CI = paste0(
      num_fmt(eapc, 2), " (",
      num_fmt(eapc_ci_lower, 2), " to ",
      num_fmt(eapc_ci_upper, 2), ")"
    ),
    EAPC_P = p_value
  )

# 合并回主表 
final_big_m <- final_tbl_m %>% 
  left_join(asmr_std_90to23_new, by = "location")


# 最终表格：全部强制小数位 + 千位分隔符 + 换行 
STD_table2_asmr <- data.frame(
  new_ID        = final_big_m$NO.,
  continent     = final_big_m$continent,
  location      = final_big_m$location,
  
  # 死亡数：0 位小数，带换行
  mor_number1990 = paste0(
    num_fmt(round(final_big_m$num_val_1990, 2), 2), "\n(",
    num_fmt(round(final_big_m$num_lower_1990, 2), 2), " to ",
    num_fmt(round(final_big_m$num_upper_1990, 2), 2), ")"
  ),
  mor_number2023 = paste0(
    num_fmt(round(final_big_m$num_val_2023, 2), 2), "\n(",
    num_fmt(round(final_big_m$num_lower_2023, 2), 2), " to ",
    num_fmt(round(final_big_m$num_upper_2023, 2), 2), ")"
  ),
  
  # 死亡数变化：2 位小数，带换行
  mor_number_change = paste0(
    num_fmt(round(final_big_m$change_num * 100, 2), 2), "\n(",
    num_fmt(round(final_big_m$change_num_lower * 100, 2), 2), " to ",
    num_fmt(round(final_big_m$change_num_upper * 100, 2), 2), ")"
  ),
  
  # 死亡率：2 位小数，带换行
  mor_rate1990 = paste0(
    num_fmt(round(final_big_m$asmr_val_1990, 2), 2), "\n(",
    num_fmt(round(final_big_m$asmr_lower_1990, 2), 2), " to ",
    num_fmt(round(final_big_m$asmr_upper_1990, 2), 2), ")"
  ),
  mor_rate2023 = paste0(
    num_fmt(round(final_big_m$asmr_val_2023, 2), 2), "\n(",
    num_fmt(round(final_big_m$asmr_lower_2023, 2), 2), " to ",
    num_fmt(round(final_big_m$asmr_upper_2023, 2), 2), ")"
  ),
  
  # 死亡率变化：2 位小数，带换行
  mor_rate_change = paste0(
    num_fmt(round(final_big_m$change_asmr * 100, 2), 2), "\n(",
    num_fmt(round(final_big_m$change_asmr_lower * 100, 2), 2), " to ",
    num_fmt(round(final_big_m$change_asmr_upper * 100, 2), 2), ")"
  ),
  
  # EAPC 已经带换行，直接沿用
  EAPC_95CI = final_big_m$EAPC_95CI,
  `P-value` = final_big_m$EAPC_P
) %>% 
  arrange(continent, location)

# 写 CSV 
write.csv(STD_table2_asmr, "ASMR-EAPC-STD-BRI.csv", row.names = FALSE)



####10.SUP Table 3 数据整理####


# 统一模板：强制保留 n 位小数，并加千位分隔符 
num_fmt <- function(x, digits = 0) {
  formatC(x,
          format   = "f",
          digits   = digits,
          big.mark = ",",
          big.interval = 3)
}

# 7.1 读 5 份原始文件 
bri_loc <- read.csv("BRI-long-location.csv") %>% 
  distinct(location, .keep_all = TRUE)

df_change_num_D  <- read.csv("1990-2023全球204国家DALY数变化百分比.csv")
df_change_asDr   <- read.csv("1990-2023全球204国家DALY率变化百分比.csv")
df_ts_num_D      <- read.csv("全球204国家青少年性病总1990-2023DALY数.csv")
df_ts_asDr       <- read.csv("全球204国家青少年性病总1990-2023DALY率.csv")

# 5.2.1 变化 %（含上下界）
df_change_num_D <- df_change_num_D %>% 
  filter(year_start == 1990, year_end == 2023) %>% 
  transmute(location,
            change_num       = val,
            change_num_lower = lower,
            change_num_upper = upper)

df_change_asDr <- df_change_asDr %>% 
  filter(year_start == 1990, year_end == 2023) %>% 
  transmute(location,
            change_asDr       = val,
            change_asDr_lower = lower,
            change_asDr_upper = upper)

# 5.2.2 1990 & 2023 两年时序（含上下界）
extract_asDr_90_23 <- function(df) {
  df %>% 
    filter(year %in% c(1990, 2023)) %>% 
    select(location, year, val, lower, upper) %>% 
    pivot_longer(cols = c(val, lower, upper),
                 names_to = "stat",
                 values_to = "v") %>% 
    unite(tmp, stat, year, sep = "_") %>% 
    pivot_wider(names_from = tmp, values_from = v)
}

numD_90_23  <- extract_asDr_90_23(df_ts_num_D) %>% 
  rename_with(~ paste0("num_", .), -location)
asDr_90_23  <- extract_asDr_90_23(df_ts_asDr) %>% 
  rename_with(~ paste0("asDr_", .), -location)

# 5.2.3 合并
final_tbl_D <- bri_loc %>% 
  left_join(df_change_num_D, by = "location") %>% 
  left_join(df_change_asDr,  by = "location") %>% 
  left_join(numD_90_23,      by = "location") %>% 
  left_join(asDr_90_23,      by = "location")

# 5.2.4 按国家计算 EAPC（DALY 率）
eapc_asDr <- df_ts_asDr %>% 
  filter(measure == "DALYs",
         sex == "Both",
         age == "10-24 years",
         cause == "HIV/AIDS and sexually transmitted infections",
         metric == "Rate",
         year %in% 1990:2023,
         !is.na(val)) %>% 
  group_by(location) %>% 
  filter(n_distinct(val, na.rm = TRUE) >= 2,
         all(val > 0, na.rm = TRUE)) %>%   # 关键：必须 >0 且能变异
  do({
    mod <- lm(log(val) ~ year, data = .)
    slope <- coef(mod)[2]
    ci  <- confint(mod, "year", level = 0.95)
    p   <- summary(mod)$coefficients["year", "Pr(>|t|)"]
    
    tibble(
      eapc          = round(100 * (exp(slope) - 1), 2),
      eapc_ci_lower = round(100 * (exp(ci[1]) - 1), 2),
      eapc_ci_upper = round(100 * (exp(ci[2]) - 1), 2),
      p_value       = ifelse(p < 0.001, "p<0.001", sprintf("%.3f", p))
    )
  }) %>% 
  ungroup()

# 5.2.5 整理 EAPC 字符串（固定 2 位小数 + 换行）
asDr_std_90to23_new <- eapc_asDr %>% 
  transmute(
    location,
    EAPC_95CI = paste0(
      num_fmt(eapc, 2), "\n(",
      num_fmt(eapc_ci_lower, 2), " to ",
      num_fmt(eapc_ci_upper, 2), ")"
    ),
    EAPC_P = p_value
  )

# 5.2.6 合并回主表 
final_big_D <- final_tbl_D %>% 
  left_join(asDr_std_90to23_new, by = "location")

# 5.2.7 最终表格：千位分隔 + 固定小数位 + 换行
STD_table3_asDr <- data.frame(
  new_ID        = final_big_D$NO.,
  continent     = final_big_D$continent,
  location      = final_big_D$location,
  
  # DALY 数：0 位小数
  DALY_number1990 = paste0(
    num_fmt(round(final_big_D$num_val_1990, 0), 0), "\n(",
    num_fmt(round(final_big_D$num_lower_1990, 0), 0), " to ",
    num_fmt(round(final_big_D$num_upper_1990, 0), 0), ")"
  ),
  DALY_number2023 = paste0(
    num_fmt(round(final_big_D$num_val_2023, 0), 0), "\n(",
    num_fmt(round(final_big_D$num_lower_2023, 0), 0), " to ",
    num_fmt(round(final_big_D$num_upper_2023, 0), 0), ")"
  ),
  
  # DALY 数变化：2 位小数
  DALY_number_change = paste0(
    num_fmt(round(final_big_D$change_num * 100, 2), 2), "\n(",
    num_fmt(round(final_big_D$change_num_lower * 100, 2), 2), " to ",
    num_fmt(round(final_big_D$change_num_upper * 100, 2), 2), ")"
  ),
  
  # DALY 率：2 位小数
  DALY_rate1990 = paste0(
    num_fmt(round(final_big_D$asDr_val_1990, 2), 2), "\n(",
    num_fmt(round(final_big_D$asDr_lower_1990, 2), 2), " to ",
    num_fmt(round(final_big_D$asDr_upper_1990, 2), 2), ")"
  ),
  DALY_rate2023 = paste0(
    num_fmt(round(final_big_D$asDr_val_2023, 2), 2), "\n(",
    num_fmt(round(final_big_D$asDr_lower_2023, 2), 2), " to ",
    num_fmt(round(final_big_D$asDr_upper_2023, 2), 2), ")"
  ),
  
  # DALY 率变化：2 位小数
  DALY_rate_change = paste0(
    num_fmt(round(final_big_D$change_asDr * 100, 2), 2), "\n(",
    num_fmt(round(final_big_D$change_asDr_lower * 100, 2), 2), " to ",
    num_fmt(round(final_big_D$change_asDr_upper * 100, 2), 2), ")"
  ),
  
  # EAPC
  EAPC_95CI = final_big_D$EAPC_95CI,
  `P-value` = final_big_D$EAPC_P
) %>% 
  arrange(continent, location)

# 写 CSV 
write.csv(STD_table3_asDr, "ASDR-EAPC-STD-BRI.csv", row.names = FALSE)



#####补充表3
#######7.1. 读5份原始文件
bri_loc   <- read.csv("BRI-long-location.csv") %>% distinct(location, .keep_all = TRUE)

df_change_num_D  <- read.csv("1990-2023全球204国家DALY数变化百分比.csv")
df_change_asDr <- read.csv("1990-2023全球204国家DALY率变化百分比.csv")
df_ts_num_D      <- read.csv("全球204国家青少年性病总1990-2023DALY数.csv")
df_ts_asDr     <- read.csv("全球204国家青少年性病总1990-2023DALY率.csv")

#####5.2. 清洗
#5.2.1 变化%（含upper/lower）
df_change_num_D <- df_change_num_D %>% 
  filter(year_start == 1990, year_end == 2023) %>% 
  transmute(location,
            change_num       = val,
            change_num_lower = lower,
            change_num_upper = upper)

df_change_asDr <- df_change_asDr %>% 
  filter(year_start == 1990, year_end == 2023) %>% 
  transmute(location,
            change_asDr       = val,
            change_asDr_lower = lower,
            change_asDr_upper = upper)

# 5.2.2 1990 & 2023 两年时序（含upper/lower）
extract_asDr_90_23 <- function(df) {
  df %>% 
    filter(year %in% c(1990, 2023)) %>% 
    select(location, year, val, lower, upper) %>% 
    pivot_longer(cols = c(val, lower, upper),
                 names_to = "stat",
                 values_to = "v") %>% 
    unite(tmp, stat, year, sep = "_") %>% 
    pivot_wider(names_from = tmp, values_from = v)
}

numD_90_23  <- extract_asDr_90_23(df_ts_num_D)  %>% 
  rename_with(~paste0("num_", .), -location)
asDr_90_23 <- extract_asDr_90_23(df_ts_asDr) %>%
  rename_with(~paste0("asDr_", .), -location)

#5.2.3. 合并
final_tbl_D <- bri_loc %>% 
  left_join(df_change_num_D,  by = "location") %>% 
  left_join(df_change_asDr, by = "location") %>% 
  left_join(numD_90_23,   by = "location") %>% 
  left_join(asDr_90_23,  by = "location")

#5.2.4计算EAPC


eapc_asDr <- df_ts_asDr %>%
  group_by(location) %>%
  summarise(
    eapc = round(100 * (exp(coef(lm(log(val) ~ year)))[2] - 1),2),
    eapc_ci_lower = round(100 * (exp(confint(lm(log(val) ~ year), 
                                             level = 0.95)[2, 1]) - 1),2),
    eapc_ci_upper = round(100 * (exp(confint(lm(log(val) ~ year),
                                             level = 0.95)[2, 2]) - 1),2),
    p_value = ifelse(summary(lm(log(val) ~ year))$coefficients["year", "Pr(>|t|)"] < 0.001,
                     "p<0.001",
                     sprintf("%.3f", 
                             summary(lm(log(val) ~ year))$coefficients["year", "Pr(>|t|)"])))




asDr_std_90to23_new <- data.frame(
  location = eapc_asDr$location,
  EAPC = paste(eapc_asDr$eapc,'\n(',
               eapc_asDr$eapc_ci_lower,
               'to',eapc_asDr$eapc_ci_upper,')'),
  `P-value` = eapc_asDr$p_value)



# 5.2.6 按 location 右接（保留 BRI 顺序）
final_big_D <- final_tbl_D  %>% 
  left_join(asDr_std_90to23_new, by = "location")


# 0. 统一模板：强制 n 位小数 + 千位分隔符 ------------------------------
num_fmt <- function(x, digits = 0) {
  # 先定宽，再去加逗号
  vec <- formatC(x, format = "f", digits = digits, big.mark = ",", big.interval = 3)
  # 去掉前后空格（formatC 会给负号留空）
  trimws(vec)
}

# 5.2.7 最终表格：全部强制小数位 + 千位分隔符 + 换行 --------------------
STD_table3_asDr <- data.frame(
  continent = final_big_D$continent,
  location  = final_big_D$location,
  
  # DALY 数：0 位小数
  DALY_number1990 = paste0(
    num_fmt(round(final_big_D$num_val_1990, 0), 0), "\n(",
    num_fmt(round(final_big_D$num_lower_1990, 0), 0), " to ",
    num_fmt(round(final_big_D$num_upper_1990, 0), 0), ")"
  ),
  DALY_number2023 = paste0(
    num_fmt(round(final_big_D$num_val_2023, 0), 0), "\n(",
    num_fmt(round(final_big_D$num_lower_2023, 0), 0), " to ",
    num_fmt(round(final_big_D$num_upper_2023, 0), 0), ")"
  ),
  
  # DALY 数变化：2 位小数
  DALY_number_change = paste0(
    num_fmt(round(final_big_D$change_num * 100, 2), 2), "\n(",
    num_fmt(round(final_big_D$change_num_lower * 100, 2), 2), " to ",
    num_fmt(round(final_big_D$change_num_upper * 100, 2), 2), ")"
  ),
  
  # DALY 率：2 位小数
  DALY_rate1990 = paste0(
    num_fmt(round(final_big_D$asDr_val_1990, 2), 2), "\n(",
    num_fmt(round(final_big_D$asDr_lower_1990, 2), 2), " to ",
    num_fmt(round(final_big_D$asDr_upper_1990, 2), 2), ")"
  ),
  DALY_rate2023 = paste0(
    num_fmt(round(final_big_D$asDr_val_2023, 2), 2), "\n(",
    num_fmt(round(final_big_D$asDr_lower_2023, 2), 2), " to ",
    num_fmt(round(final_big_D$asDr_upper_2023, 2), 2), ")"
  ),
  
  # DALY 率变化：2 位小数
  DALY_rate_change = paste0(
    num_fmt(round(final_big_D$change_asDr * 100, 2), 2), "\n(",
    num_fmt(round(final_big_D$change_asDr_lower * 100, 2), 2), " to ",
    num_fmt(round(final_big_D$change_asDr_upper * 100, 2), 2), ")"
  ),
  
  # EAPC 已经带换行，只需统一 2 位小数
  EAPC_95CI=final_big_D$EAPC,
  `P-value` = final_big_D$P.value)
STD_table3_asDr <- STD_table3_asDr %>% 
  arrange(continent, location)

# 写 CSV ---------------------------------------------------------------
write.csv(STD_table3_asDr, "ASDR-EAPC-STD-BRI.csv", row.names = FALSE)

