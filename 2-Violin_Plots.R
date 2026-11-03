####3. 小提琴图####
#1. 读数据
asir <- read.csv("2023全球204国家STD-ASIR.csv")
aspr <- read.csv("2023全球204国家STD-ASPR.csv")
asmr <- read.csv("2023全球204国家STD-ASMR.csv")
asdr <- read.csv("2023全球204国家STD-ASDR.csv")

bri  <- read.csv("BRI-long-location.csv")   # 两列：NO.  region

#2. 只保留 BRI 国家
bri_cnt <- bri %>% pull(location)        # 需要的国家向量

filter_bri <- function(df){
  df %>% filter(location %in% bri_cnt)
}

asir_bri <- filter_bri(asir)
aspr_bri <- filter_bri(aspr)
asmr_bri <- filter_bri(asmr)
asdr_bri <- filter_bri(asdr)

#3. 合并成长数据
combined_bri <- bind_rows(
  asir_bri %>% mutate(measure = "IR"),
  aspr_bri %>% mutate(measure = "PR"),
  asmr_bri %>% mutate(measure = "MR"),
  asdr_bri %>% mutate(measure = "DR")
) %>% 
  mutate(measure = factor(measure, levels = c("IR","PR","MR","DR")))

vilin<-data.frame(combined_bri$measure,combined_bri$location,
                  combined_bri$val)

# 4. 画图
colors <- c("IR" = "rosybrown2",
            "PR" = "#31a354",
            "MR" = "#e6550d",
            "DR" = "#3182bd")

pvl<-ggplot(combined_bri, aes(x = factor(1), y = val, fill = measure)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, alpha = 0.8, fill = "white", colour = "black") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors) +
  facet_wrap(~ measure, scales = "free_y", nrow = 1) +
  labs(x = NULL,
       y = "Age-specific rate（/100K）",
       fill = "Measure") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text   = element_text(face = "bold"))
pvl

## 1. 四分位数
quartiles <- quantile(asdr_bri$val, probs = c(0.25, 0.5, 0.75))
quartiles
# 输出示例
#   25%   50%   75%
#  x.xx  x.xx  x.xx

## 2. 均数 ± 标准差
mean_val   <- mean(asir_bri$val)      # 如有缺失值：mean(asir_bri$val, na.rm = TRUE)
sd_val     <- sd(asir_bri$val)        # 如有缺失值：sd(asir_bri$val, na.rm = TRUE)
mean_sd    <- sprintf("%.2f ± %.2f", mean_val, sd_val)
mean_sd

d <- density(asdr_bri$val, na.rm = TRUE)   # 密度估计
peak_val <- d$x[which.max(d$y)]            # 密度最大处对应的 val 值

peak_val   #



## 2. 均数 ± 标准差
mean_val   <- mean(asir_bri$val)      # 如有缺失值：mean(asir_bri$val, na.rm = TRUE)
sd_val     <- sd(asir_bri$val)        # 如有缺失值：sd(asir_bri$val, na.rm = TRUE)
mean_sd    <- sprintf("%.2f ± %.2f", mean_val, sd_val)
mean_sd

d <- density(asir_bri$val, na.rm = TRUE)   # 密度估计
peak_val <- d$x[which.max(d$y)]            # 密度最大处对应的 val 值

peak_val   #
d <- density(asmr_bri$val, na.rm = TRUE)   # 密度估计
peak_val <- d$x[which.max(d$y)]            # 密度最大处对应的 val 值

peak_val   #
