####4.SDI与国家拟合曲线####


#####4.1单年份拟合#####
#1. 读数据
bri_cnt <- read.csv("BRI-long-location.csv") %>% pull(location)   # 需要的国家向量

# 4 个指标
asir_df <- read.csv("2023全球204国家STD-ASIR.csv") %>% filter(location %in% bri_cnt)
aspr_df <- read.csv("2023全球204国家STD-ASPR.csv") %>% filter(location %in% bri_cnt)
asmr_df <- read.csv("2023全球204国家STD-ASMR.csv") %>% filter(location %in% bri_cnt)
asdr_df <- read.csv("2023全球204国家STD-ASDR.csv") %>% filter(location %in% bri_cnt)

# SDI 数据（1990-2023）
sdi_df <- read.csv("1990-2023全球各国SDI值.csv")

# 2. 统一合并 SDI 时序
merge_sdi <- function(dat){
  dat %>% inner_join(sdi_df, by = c("location", "year"))
}
pso_asir <- merge_sdi(asir_df)
pso_aspr <- merge_sdi(aspr_df)
pso_asmr <- merge_sdi(asmr_df)
pso_asdr <- merge_sdi(asdr_df)


# 3. 统一给 4 份数据加 SDI_tier
## 3.1 读界值并转成长表
sdi_cut <- read.csv("SDI-界值.csv") %>% 
  select(SDI_tier = location_name, upper = upper_bound)

## 3.2 2023 年各国 SDI 值并划档（只保留 location + SDI + SDI_tier）
sdi_2023 <- sdi_df %>% 
  filter(year == 2023) %>% 
  select(location, SDI) %>% 
  mutate(
    idx      = findInterval(SDI, c(0, sdi_cut$upper)),
    SDI_tier = sdi_cut$SDI_tier[idx]
  )

## 3.3 一次性把 SDI_tier 并到 4 份指标
pso_asir <- pso_asir %>% inner_join(sdi_2023, by = c("location", "SDI"))
pso_aspr <- pso_aspr %>% inner_join(sdi_2023, by = c("location", "SDI"))
pso_asmr <- pso_asmr %>% inner_join(sdi_2023, by = c("location", "SDI"))
pso_asdr <- pso_asdr %>% inner_join(sdi_2023, by = c("location", "SDI"))

# 4. 通用绘图函数 
plot_vs_sdi <- function(dat, title_lab, y_lab){
  ggplot(dat, aes(SDI, val)) +
    geom_point(aes(colour = SDI_tier), size = 2, show.legend = TRUE) +
    geom_text_repel(aes(label = location), size = 3, show.legend = FALSE) +
    geom_smooth(formula = y ~ x, method = "lm",
                colour = "black", size = .8, se = TRUE) +
    stat_poly_eq(
      aes(label = paste(..eq.label.., ..rr.label..,
                        stat(p.value.label), sep = "~`, `~")),
      formula = y ~ x, parse = TRUE,
      label.x = 0.05, label.y = 0.95, size = 3.5
    ) +
    scale_y_continuous(labels = comma) +
    scale_colour_brewer(palette = "Set2", name = "SDI_tier") +
    labs(x = "SDI", y = y_lab, title = title_lab) +
    theme(
      legend.position = "right",
      legend.title = element_blank(),
      panel.background = element_rect(fill = "white", colour = "white"),
      axis.line = element_line(colour = "black")
    )
}

# 5. 一次出 4 图
p1401 <- plot_vs_sdi(pso_asir, "A", "IR (/100K)")
p1402 <- plot_vs_sdi(pso_aspr, "B", "PR (/100K)")
p1403 <- plot_vs_sdi(pso_asmr, "C", "MR (/100K)")
p1404 <- plot_vs_sdi(pso_asdr, "D", "DR (/100K)")

# 6. 查看 / 导出
p1401
p1402
p1403
p1404


#####4.2多年份拟合#####
stdasir <- read.csv("全球204国家青少年性病总1990-2023发病率.csv")
stdaspr <- read.csv("全球204国家青少年性病总1990-2023患病率.csv")
stdasmr <- read.csv("全球204国家青少年性病总1990-2023死亡率.csv")
stdasdr <- read.csv("全球204国家青少年性病总1990-2023DALY率.csv")

SDI_val90to21 <- read.csv('1990-2023全球各国SDI值.csv')
bri_cnt <- read.csv("BRI-long-location.csv") %>% pull(location)   # 需要的国家向量

# 1. 统一给任意指标数据加 SDI 并筛成 BRI 国家
mk_BRI_SDI <- function(df){
  df %>% 
    inner_join(SDI_val90to21, by = c("location", "year")) %>%   # 与SDI数据合并
    filter(location %in% bri_cnt)                               # 筛选BRI国家
}

# 2. 绘图模板 - 不同国家用不同颜色，Y轴以K为单位
plot_SDI_vs <- function(dat, title, y_lab){
  # 假设数据框中有val列，如果没有需要根据实际情况调整
  ggplot(dat, aes(SDI, val)) +
    geom_point(aes(color = location), alpha = 0.6, size = 1) +    # 按国家着色
    geom_smooth(formula = y ~ x, method = "lm",
                colour = "red", size = .8, se = TRUE) +
    stat_poly_eq(
      aes(label = paste(..eq.label..,..rr.label..,
                        stat(p.value.label), sep = "~`, `~")),
      formula = y ~ x, parse = TRUE,
      label.x = 0.05, label.y = 0.95, size = 3.5
    ) +
    scale_y_continuous(
      labels = label_number(scale = 1e-3, suffix = "K")  # 使用K表示千
    ) +
    labs(x = "SDI", y = y_lab, title = title) +
    theme_bw(base_size = 14) +
    theme(legend.position = "none", panel.grid = element_blank())  # 隐藏图例以避免过于拥挤
}

# 3. 一次性处理四张表 
pso_asir <- mk_BRI_SDI(stdasir)
pso_aspr <- mk_BRI_SDI(stdaspr)
pso_asmr <- mk_BRI_SDI(stdasmr)
pso_asdr <- mk_BRI_SDI(stdasdr)

# 4. 出图 
p9001 <- plot_SDI_vs(pso_asir, "A", "IR (/100K)")
p9002 <- plot_SDI_vs(pso_aspr, "B", "PR (/100K)")
p9003 <- plot_SDI_vs(pso_asmr, "C", "MR (/100K)")
p9004 <- plot_SDI_vs(pso_asdr, "D", "DR (/100K)")

p9001
p9002
p9003
p9004