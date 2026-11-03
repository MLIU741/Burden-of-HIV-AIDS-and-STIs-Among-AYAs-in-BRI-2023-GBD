####1.环境设置####
##### 1.1加载包#####

library(scales)
library(ggplot2)
library(purrr) 
library(gghighlight)
library(ggprism)
library(scales)
library(reshape2)
library(wesanderson)
library(ggbump)
library(scales)
library(ggalluvial)
library(RColorBrewer)
library(dplyr)
library(ggsci)
library(Cairo)
library(magrittr)
library(ggpubr)
library(maps)
library(sp)
library(broom)
library(mapproj)
library(ggmap)
library(dplyr)
library(ggalt)
library(data.table)
library(ggthemes)
library(patchwork)
library(ggpattern)  
library(MetBrewer) 
library(ggtext)  
library(ggalt)
library(data.table)
library(ggthemes)
library(stringr)
library(tidyr)
library(scales)
library(ggh4x)
library(ggsci)
library(ggrepel)
library(gridExtra)
library(tidytext)
library(patchwork)
library(janitor)
library(gganimate)
library(scales)
library(gifski)
library(snakecase)
library(ggpmisc)
library(forecast) 
options(scipen = 200)


##### 1.2设定存储路径#####


####2.图1地图####
##### 2.1ASIR地图#####
#1.参数
data.file   <- '2023全部STD全球204国家发病率地图数据.csv'
bri.file    <- '核对BRI-long.csv'
column.name <- 'val'

#2. 读数据
dat     <- fread(data.file)
vv      <- dat[[column.name]]
bri_cty <- fread(bri.file) %>% pull(region)

#3. 颜色索引
color_cut <- quantile(vv, probs = seq(0, 1, .2))
color_cut[length(color_cut)] <- 1.1 * tail(color_cut, 1)

dat <- dat %>% 
  mutate(color_index = cut(!!sym(column.name),
                           breaks = color_cut,
                           right  = FALSE,
                           labels = c('[0%,20%)', '[20%,40%)', '[40%,60%)',
                                      '[60%,80%)', '>80%')))

#4. 地图底图
# 4-1 全球轮廓（去南极洲）
world_all <- map_data("world") %>% 
  filter(region != "Antarctica")

# 4-2 与数据拼接
world <- left_join(world_all, dat, by = "region") %>% as.data.table()

# 4-3 仅 BRI 国家
world_bri <- world %>% 
  filter(str_to_title(region) %in% str_to_title(bri_cty))

#5. 颜色与图例
cc <- c('steelblue4', 'palegreen4', 'darkgoldenrod2',
        'chocolate2', 'darkred')
legend.title <- 'IR'

#6. 主题
theme_map <- theme_minimal() +
  theme(axis.line     = element_blank(),
        axis.text     = element_blank(),
        axis.ticks    = element_blank(),
        axis.title    = element_blank(),
        panel.grid    = element_blank(),
        panel.border  = element_blank(),
        plot.background = element_blank())

#7. 主图函数
getWorld <- function() {
  ggplot() +
    ## 7-1 全球灰色轮廓（去南极洲）
    geom_map(data = world_all, map = world_all,
             aes(x = long, y = lat, map_id = region),
             fill = NA, colour = "grey60", size = 0.05) +
    ## 7-2 BRI 国家填色
    geom_map(data = world_bri, map = world_bri,
             aes(x = long, y = lat, map_id = region, fill = color_index),
             colour = NA, size = 0.05, alpha = 0.8) +
    theme_map() +
    scale_fill_manual(values = cc, na.translate = FALSE, drop = FALSE) +
    labs(fill = legend.title) +
    ## 7-3 原文本标注
    annotate("text", x = 30,  y = -35,
             label = paste("Lesotho",
                           round(dat[region == "Lesotho"][[column.name]][1], 0),
                           sep = "\n"),
             hjust = 0, color = "red") +
    annotate("point", x = 27.5, y = -29.5) +
    annotate("text", x = 63, y = 23,
             label = paste("Pakistan",
                           round(dat[region == "Pakistan"][[column.name]][1], 0),
                           sep = "\n"),
             hjust = 0, color = "midnightblue") +
    annotate("point", x = 68, y = 29)
}

#8. 输出
whole_world <- getWorld()
p1101 <- whole_world
p1101

#####2.2 ASPR地图#####

#1. 参数
data.file   <- '2023全部STD全球204国家患病率地图数据.csv'   # ← 患病率文件
bri.file    <- '核对BRI-long.csv'
column.name <- 'val'                       # 要映射的指标列

#2. 读数据
dat     <- fread(data.file)
vv      <- dat[[column.name]]
bri_cty <- fread(bri.file) %>% pull(region)

# 3. 五分位颜色索引
color_cut <- quantile(vv, probs = seq(0, 1, .2))
color_cut[length(color_cut)] <- 1.1 * tail(color_cut, 1)

dat <- dat %>% 
  mutate(color_index = cut(!!sym(column.name),
                           breaks = color_cut,
                           right  = FALSE,
                           labels = c('[0%,20%)', '[20%,40%)', '[40%,60%)',
                                      '[60%,80%)', '>80%')))

#4. 地图底图 
# 4-1 全球轮廓（去南极洲）
world_all <- map_data("world") %>% 
  filter(region != "Antarctica")

# 4-2 与数据拼接
world <- left_join(world_all, dat, by = "region") %>% as.data.table()

# 4-3 仅保留 BRI 国家
world_bri <- world %>% 
  filter(str_to_title(region) %in% str_to_title(bri_cty))

#5. 颜色与图例  
cc <- c('steelblue4', 'palegreen4', 'darkgoldenrod2',
        'chocolate2', 'darkred')
legend.title <- 'PR'   # ← 图例标题改为患病率

#6. 主题
theme_map <- theme_minimal() +
  theme(axis.line     = element_blank(),
        axis.text     = element_blank(),
        axis.ticks    = element_blank(),
        axis.title    = element_blank(),
        panel.grid    = element_blank(),
        panel.border  = element_blank(),
        plot.background = element_blank())

#7. 主图函数
getWorld <- function() {
  ggplot() +
    ## 7-1 全球灰色轮廓
    geom_map(data = world_all, map = world_all,
             aes(x = long, y = lat, map_id = region),
             fill = NA, colour = "grey60", size = 0.05) +
    ## 7-2 BRI 国家填色
    geom_map(data = world_bri, map = world_bri,
             aes(x = long, y = lat, map_id = region, fill = color_index),
             colour = NA, size = 0.05, alpha = 0.8) +
    theme_map() +
    scale_fill_manual(values = cc, na.translate = FALSE, drop = FALSE) +
    labs(fill = legend.title) +
    ## 7-3 原文本标注（可自改）
    annotate("text", x = 30,  y = -35,
             label = paste("Lesotho",
                           round(dat[region == "Lesotho"][[column.name]][1], 0),
                           sep = "\n"),
             hjust = 0, color = "red") +
    annotate("point", x = 27.5, y = -29.5) +
    annotate("text", x = 63, y = 23,
             label = paste("Pakistan",
                           round(dat[region == "Pakistan"][[column.name]][1], 0),
                           sep = "\n"),
             hjust = 0, color = "midnightblue") +
    annotate("point", x = 68, y = 29)
}

# 8. 输出 
whole_world <- getWorld()
p1102 <- whole_world
p1102

#####2.3ASMR地图####
#1. 参数 
data.file <- '2023全部STD全球204国家死亡率地图数据.csv'
bri.file  <- '核对BRI-long.csv'
column.name <- 'val'

#2. 读数据
dat  <- fread(data.file)
vv   <- dat[[column.name]]
bri_cty <- fread(bri.file) %>% pull(country)

# 3. 统一国家名称格式（首字母大写）
dat <- dat %>% mutate(region = str_to_title(region))
bri_cty <- str_to_title(bri_cty)

#4. 五分位颜色索引 
color_cut <- quantile(vv, probs = seq(0, 1, .2))
color_cut[length(color_cut)] <- 1.1 * tail(color_cut, 1)

dat <- dat %>%
  mutate(color_index = cut(!!sym(column.name),
                           breaks = color_cut,
                           right  = FALSE,
                           labels = c('[0%,20%)', '[20%,40%)', '[40%,60%)',
                                      '[60%,80%)', '>80%')))

# 5. 地图底图 
# 5-1 全球轮廓（去南极洲）
world_all <- map_data("world") %>% filter(region != "Antarctica")

# 5-2 与数据拼接
world <- left_join(world_all, dat, by = "region") %>% as.data.table()

# 5-3 仅保留 BRI 国家
world_bri <- world %>% filter(region %in% bri_cty)

# 6. 颜色与图例  
cc <- c('steelblue4', 'palegreen4', 'darkgoldenrod2',
        'chocolate2', 'darkred')
legend.title <- 'MR'

#7. 主题  
theme_map <- theme_minimal() +
  theme(axis.line     = element_blank(),
        axis.text     = element_blank(),
        axis.ticks    = element_blank(),
        axis.title    = element_blank(),
        panel.grid    = element_blank(),
        panel.border  = element_blank(),
        plot.background = element_blank())

#8. 寻找 BRI 国家中 ASMR 最低的国家
bri_dat   <- dat %>% filter(region %in% bri_cty)
lowest_bri <- bri_dat %>% slice_min(!!sym(column.name), n = 1)
low_name <- pull(lowest_bri, region)
low_val  <- pull(lowest_bri, column.name)

#9. 获取 Andorra 的地图中心坐标（用于标注）
andorra_map <- world_all %>% filter(region == low_name)
andorra_long <- mean(range(andorra_map$long))
andorra_lat  <- mean(range(andorra_map$lat))

#10. 主图函数  
getWorld <- function() {
  ggplot() +
    ## 10-1 全球灰色轮廓
    geom_map(data = world_all, map = world_all,
             aes(x = long, y = lat, map_id = region),
             fill = NA, colour = "grey60", size = 0.05) +
    ## 10-2 BRI 国家填色
    geom_map(data = world_bri, map = world_bri,
             aes(x = long, y = lat, map_id = region, fill = color_index),
             colour = NA, size = 0.05, alpha = 0.8) +
    theme_map() +
    scale_fill_manual(values = cc, na.translate = FALSE, drop = FALSE) +
    labs(fill = legend.title) +
    ## 10-3 标注最高 & 最低（BRI 内）
    annotate("text", x = 30,  y = -35,
             label = paste("Lesotho", round(dat[region == "Lesotho"][[column.name]][1], 2), sep = "\n"),
             hjust = 0, color = "red") +
    annotate("point", x = 27.5, y = -29.5) +
    annotate("text", x = andorra_long + 23, 
             y = andorra_lat - 0.5,
             label = paste(low_name, round(low_val, 3), sep = "\n"),
             hjust = 0.5, color = "midnightblue") +
    annotate("point", x = andorra_long, y = andorra_lat)
}

#11. 输出  
whole_world <- getWorld()
p1103 <- whole_world
p1103

#####2.5 ASDR地图#####

#1. 参数
data.file <- '2023全部STD全球204国家DALY率地图数据.csv'
bri.file  <- '核对BRI-long.csv'
column.name <- 'val'

#2. 读数据
dat  <- fread(data.file)
vv   <- dat[[column.name]]
bri_cty <- fread(bri.file) %>% pull(country)

#3. 统一国家名称格式（首字母大写）
dat <- dat %>% mutate(region = str_to_title(region))
bri_cty <- str_to_title(bri_cty)

#4. 五分位颜色索引
color_cut <- quantile(vv, probs = seq(0, 1, .2))
color_cut[length(color_cut)] <- 1.1 * tail(color_cut, 1)

dat <- dat %>%
  mutate(color_index = cut(!!sym(column.name),
                           breaks = color_cut,
                           right  = FALSE,
                           labels = c('[0%,20%)', '[20%,40%)', '[40%,60%)',
                                      '[60%,80%)', '>80%')))

# 5. 地图底图
# 5-1 全球轮廓（去南极洲）
world_all <- map_data("world") %>% filter(region != "Antarctica")

# 5-2 与数据拼接
world <- left_join(world_all, dat, by = "region") %>% as.data.table()

# 5-3 仅保留 BRI 国家
world_bri <- world %>% filter(region %in% bri_cty)

# 6. 颜色与图例 
cc <- c('steelblue4', 'palegreen4', 'darkgoldenrod2',
        'chocolate2', 'darkred')
legend.title <- 'DR'

#7. 主题 
theme_map <- theme_minimal() +
  theme(axis.line     = element_blank(),
        axis.text     = element_blank(),
        axis.ticks    = element_blank(),
        axis.title    = element_blank(),
        panel.grid    = element_blank(),
        panel.border  = element_blank(),
        plot.background = element_blank())

#8. 寻找 BRI 国家中 ASDR 最低的国家 
bri_dat   <- dat %>% filter(region %in% bri_cty)
lowest_bri <- bri_dat %>% slice_min(!!sym(column.name), n = 1)
low_name <- pull(lowest_bri, region)
low_val  <- pull(lowest_bri, column.name)

#9. 获取最低国家的地图中心坐标（用于标注）
low_map <- world_all %>% filter(region == low_name)
low_long <- mean(range(low_map$long))
low_lat  <- mean(range(low_map$lat))

# 10. 主图函数  
getWorld <- function() {
  ggplot() +
    ## 10-1 全球灰色轮廓
    geom_map(data = world_all, map = world_all,
             aes(x = long, y = lat, map_id = region),
             fill = NA, colour = "grey60", size = 0.05) +
    ## 10-2 BRI 国家填色
    geom_map(data = world_bri, map = world_bri,
             aes(x = long, y = lat, map_id = region, fill = color_index),
             colour = NA, size = 0.05, alpha = 0.8) +
    theme_map() +
    scale_fill_manual(values = cc, na.translate = FALSE, drop = FALSE) +
    labs(fill = legend.title) +
    ## 10-3 标注最高 & 最低（BRI 内）
    annotate("text", x = 30,  y = -35,
             label = paste("Lesotho", round(dat[region == "Lesotho"][[column.name]][1], 0), sep = "\n"),
             hjust = 0, color = "red") +
    annotate("point", x = 27.5, y = -29.5) +
    annotate("text", x = low_long+20, y = low_lat - 1,
             label = paste(low_name, round(low_val, 1), sep = "\n"),
             hjust = 0.5, color = "midnightblue") +
    annotate("point", x = low_long, y = low_lat)
}

# 11. 输出 
whole_world <- getWorld()
p1104 <- whole_world
p1104



p1101234<-ggarrange(p1101,p1102,p1103,p1104,nrow = 2,ncol=2,
                    common.legend = T,legend = 'bottom',
                    
                    labels = c('a  IR','b  PR','c  MR','d  DR'))
p1101234
