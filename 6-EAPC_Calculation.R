####6.计算EAPC####

## —— 1 读数据 ——
loc <- read.csv("BRI-long-location.csv", stringsAsFactors = FALSE)
std <- read.csv("全球204国家青少年性病总1990-2023发病率.csv", stringsAsFactors = FALSE)

## —— 2 筛选 BRI 国家（全部洲）+ 10-24 岁 + 男女合计 ——
bri_std <- std %>% 
  filter(location %in% loc$location,
         age == "10-24 years",
         sex == "Both",
         cause == "HIV/AIDS and sexually transmitted infections") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(location) %>% 
  filter(n() >= 10)

## —— 3 EAPC 计算 ——
calc_eapc <- function(df){
  mod <- lm(log(val) ~ year, data = df)
  sl  <- tidy(mod)$estimate[2]
  pv  <- tidy(mod)$p.value[2]
  ci  <- confint(mod)[2, ]
  tibble(
    eapc       = round(100 * (exp(sl) - 1), 2),
    eapc_lower = round(100 * (exp(ci[1]) - 1), 2),
    eapc_upper = round(100 * (exp(ci[2]) - 1), 2),
    p_value    = if(pv < .001) "p<0.001" else sprintf("%.3f", pv)
  )
}

## —— 4 计算 EAPC 并连接 continent ——
bri_eapc <- bri_std %>% 
  group_by(location) %>% 
  do(calc_eapc(.)) %>% 
  ungroup() %>% 
  left_join(loc, by = "location")

## —— 5 提取 1990 与 2023 的 val/lower/upper ——
pt_1990 <- bri_std %>% filter(year == 1990) %>% 
  select(location, val_1990 = val, lower_1990 = lower, upper_1990 = upper)
pt_2023 <- bri_std %>% filter(year == 2023) %>% 
  select(location, val_2023 = val, lower_2023 = lower, upper_2023 = upper)

## —— 6 合并 + 洲内 A→Z 排序 ——
bri_final <- bri_eapc %>% 
  left_join(pt_1990, by = "location") %>% 
  left_join(pt_2023, by = "location") %>% 
  select(location, continent, starts_with("val_"), starts_with("lower_"), 
         starts_with("upper_"), eapc, eapc_lower, eapc_upper, p_value) %>% 
  arrange(continent, location)   # ← 先洲，再国家字母序

## 查看/导出
print(bri_final)

# write.csv(bri_final, "bri_eapc_full_AZ.csv", row.names = FALSE)

