####5.哑铃图+分性别柱状图####
#####5.1 哑铃图#####

# 定义处理函数
process_and_plot_continent_data <- function(file_path, measure_name) {
  # 读取数据
  data_raw <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # 数据清洗
  data_clean <- data_raw %>% filter(location %in% location_df$location,
                                    age == "10-24 years",
                                    sex == "Both",
                                    cause == "HIV/AIDS and sexually transmitted infections") %>%
    mutate(year = as.numeric(year)) %>%
    group_by(location) %>%
    filter(n() >= 10) %>%
    ungroup()
  
  # 提取1990和2023年的数据点
  point_data <- data_clean %>%
    filter(year %in% c(1990, 2023)) %>%
    select(location, year, val, lower, upper) %>%
    pivot_wider(names_from = year,
                values_from = c(val, lower, upper),
                names_sep = "_")
  
  # 合并大洲信息并创建方向变量
  dumbbell_data <- point_data %>%
    left_join(location_df, by = "location")
  
  # 绘图函数 - 增加边距确保纵轴标签显示
  plot_continent <- function(df, cont_name) {
    # 按2023年val值从小到大排序（由下到上）
    df <- df %>%
      arrange(val_2023) %>%  # 升序排列
      mutate(location = factor(location, levels = unique(location)))
    
    # 计算当前数据范围，用于动态调整x轴
    x_min <- min(c(df$val_1990, df$val_2023), na.rm = TRUE)
    x_max <- max(c(df$val_1990, df$val_2023), na.rm = TRUE)
    
    # 创建合适的breaks
    x_range <- x_max - x_min
    break_interval <- ifelse(x_range > 10000, 2000, 
                             ifelse(x_range > 5000, 1000, 500))
    x_breaks <- seq(floor(x_min/break_interval)*break_interval, 
                    ceiling(x_max/break_interval)*break_interval, 
                    by = break_interval)
    
    # 根据数据范围选择合适的标签格式
    if (x_max > 10000) {
      x_labels <- ~paste0(.x/1000, "K")
      x_title <- measure_name  # 只显示measure_name
    } else if (x_max > 1000) {
      x_labels <- ~paste0(.x/1000, "K")
      x_title <- measure_name  # 只显示measure_name
    } else {
      x_labels <- waiver()
      x_title <- measure_name  # 只显示measure_name
    }
    
    # 创建方向变量
    df <- df %>%
      mutate(direction = ifelse(val_2023 > val_1990, "Increase", "Decrease"))
    
    p <- ggplot(df, aes(x = val_1990, xend = val_2023, y = location)) +
      geom_segment(aes(colour = direction), size = 1.2) +
      scale_colour_manual(values = c(Decrease = "#2ca02c", Increase = "#d62728"), name = NULL) +
      
      geom_point(aes(x = val_1990, shape = "1990"), colour = "#1f77b4", size = 2) +
      geom_point(aes(x = val_2023, shape = "2023"), colour = "#ff7f0e", size = 2) +
      scale_shape_manual(values = c("1990" = 16, "2023" = 17), name = NULL) +
      
      scale_x_continuous(labels = x_labels,
                         breaks = x_breaks,
                         expand = expansion(mult = c(0.05, 0.1))) +
      
      labs(x = x_title, y = NULL, title = cont_name) +  # 标题只显示大洲名称
      theme_bw() +
      theme(legend.position = "top",
            # 增加边距确保纵轴国家名称显示
            axis.text.y = element_text(color = "black", size = 9),
            axis.ticks.y = element_line(),
            panel.background = element_blank(),
            plot.background = element_blank(),
            axis.text.x = element_text(color = "black", size = 9),
            axis.title.x = element_text(color = "black", size = 11),
            plot.margin = margin(c(0.5, 1, 0.5, 2), unit = "cm"),  # 增加左边距
            plot.title = element_text(hjust = 0, size = 12, face = "bold",  # 左对齐标题
                                      margin = margin(b = 10)))  # 添加底部边距
    
    return(p)
  }
  
  # 为每个大洲生成图
  continents <- c("Asia", "Europe", "Africa", "North America", "South America", "Oceania")
  plot_list <- list()
  
  for (cn in continents) {
    df_sub <- dumbbell_data %>% filter(continent == cn)
    if(nrow(df_sub) > 0) {
      plot_name <- paste0("p_", gsub(" ", "_", cn), "_", measure_name)
      p <- plot_continent(df_sub, cn)
      
      # 预览图片
      print(p)
      cat("预览: ", plot_name, "\n")
      readline(prompt = "按回车继续或Esc退出...")
      
      plot_list[[plot_name]] <- p
    } else {
      cat("跳过 ", cn, " - 无数据\n")
    }
  }
  
  # 创建组合图
  if (length(plot_list) > 0) {
    # 创建图例
    legend_plot <- ggplot(dumbbell_data %>% head(2), 
                          aes(x = val_1990, xend = val_2023, y = location)) +
      geom_segment(aes(colour = "Increase"), size = 1.2) +
      geom_point(aes(shape = "1990"), colour = "#1f77b4", size = 2) +
      geom_point(aes(shape = "2023"), colour = "#ff7f0e", size = 2) +
      scale_colour_manual(values = c(Increase = "#d62728"), name = NULL) +
      scale_shape_manual(values = c("1990" = 16, "2023" = 17), name = NULL) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.box = "horizontal",
        legend.text = element_text(size = 10)
      )
    
    legend <- get_legend(legend_plot)
    
    # 组合所有大洲的图 - 增加边距确保纵轴标签显示
    combined_plot <- wrap_plots(plot_list, ncol = 2, nrow = 3) +
      plot_layout(guides = "collect") &
      theme(legend.position = "top",
            plot.margin = margin(0.5, 0.5, 0.5, 1.5, "cm"))  # 增加左边距
    
    # 添加图例和标题
    final_combined_plot <- 
      (wrap_elements(panel = legend) / combined_plot) + 
      plot_layout(heights = c(0.05, 1)) +
      plot_annotation(
        title = paste0(measure_name, " Changes in BRI Countries by Continent (1990-2023)"),
        theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
      )
    
    plot_list[["combined"]] <- final_combined_plot
  }
  
  return(plot_list)
}

# 主程序
# 1. 读取基础数据
loc_file <- "BRI-long-location.csv"
location_df <- read.csv(loc_file, stringsAsFactors = FALSE)

# 2. 定义文件路径和指标名称
file_paths <- c(
  "全球204国家青少年性病总1990-2023发病率.csv",
  "全球204国家青少年性病总1990-2023患病率.csv",
  "全球204国家青少年性病总1990-2023死亡率.csv",
  "全球204国家青少年性病总1990-2023DALY率.csv"
)

measure_names <- c("IR", "PR", "MR", "DR")

# 3. 处理所有数据并生成图表
all_plots <- list()

for (i in 1:length(file_paths)) {
  cat("正在处理:", measure_names[i], "\n")
  plots <- process_and_plot_continent_data(file_paths[i], measure_names[i])
  all_plots <- c(all_plots, plots)
  
  # 预览组合图
  if ("combined" %in% names(plots)) {
    cat("预览组合图:", measure_names[i], "\n")
    print(plots[["combined"]])
    readline(prompt = "按回车继续或Esc退出...")
  }
}

# 4. 将所有图表赋值到全局环境
list2env(all_plots, envir = .GlobalEnv)

# 5. 现在您可以在全局环境中访问所有图表，例如：
# p_Asia_ASIR, p_Europe_ASIR, p_Africa_ASIR, ...
# p_Asia_ASPR, p_Europe_ASPR, p_Africa_ASPR, ...
# combined (每个指标的组合图)

p_Africa_ASPR



##### 5.2哑铃图 - 单独处理患病率数据 #####

# 定义处理函数
process_and_plot_aspr <- function() {
  
  # 1. 读取基础数据
  cat("正在读取BRI国家信息...\n")
  location_df <- read.csv("BRI-long-location.csv", stringsAsFactors = FALSE)
  
  # 2. 读取患病率数据
  cat("正在读取患病率数据...\n")
  data_raw <- read.csv("全球204国家青少年性病总1990-2023患病率.csv", stringsAsFactors = FALSE)
  
  # 3. 数据清洗
  cat("正在清洗数据...\n")
  data_clean <- data_raw %>% 
    filter(location %in% location_df$location,
           age == "10-24 years",
           sex == "Both",
           cause == "HIV/AIDS and sexually transmitted infections") %>%
    mutate(year = as.numeric(year)) %>%
    group_by(location) %>%
    filter(n() >= 10) %>%  # 确保有足够多年份数据
    ungroup()
  
  # 4. 提取1990和2023年的数据点
  cat("正在提取1990和2023年数据...\n")
  point_data <- data_clean %>%
    filter(year %in% c(1990, 2023)) %>%
    select(location, year, val) %>%
    pivot_wider(names_from = year, 
                values_from = val,
                names_prefix = "val_")
  
  # 5. 合并大洲信息
  dumbbell_data <- point_data %>%
    inner_join(location_df, by = "location") %>%
    filter(!is.na(val_1990) & !is.na(val_2023))  # 移除缺失值
  
  # 6. 创建方向变量
  dumbbell_data <- dumbbell_data %>%
    mutate(direction = ifelse(val_2023 > val_1990, "Increase", "Decrease"))
  
  # 7. 绘图函数
  plot_continent <- function(df, cont_name) {
    # 按2023年值排序
    df <- df %>%
      arrange(val_2023) %>%
      mutate(location = factor(location, levels = unique(location)))
    
    # 计算数据范围，用于设置横轴刻度
    x_min <- min(c(df$val_1990, df$val_2023), na.rm = TRUE)
    x_max <- max(c(df$val_1990, df$val_2023), na.rm = TRUE)
    
    # 创建合适的breaks和标签
    x_range <- x_max - x_min
    break_interval <- ifelse(x_range > 100000, 20000, 
                             ifelse(x_range > 50000, 10000, 
                                    ifelse(x_range > 10000, 5000, 1000)))
    
    x_breaks <- seq(floor(x_min/break_interval)*break_interval, 
                    ceiling(x_max/break_interval)*break_interval, 
                    by = break_interval)
    
    # 格式化标签为"80K"形式
    x_labels <- function(x) {
      ifelse(x >= 1000, paste0(round(x/1000), "K"), as.character(x))
    }
    
    # 创建绘图
    p <- ggplot(df, aes(x = val_1990, xend = val_2023, y = location)) +
      # 哑铃线段
      geom_segment(aes(color = direction), linewidth = 1.2, alpha = 0.7) +
      scale_color_manual(values = c(Decrease = "#2ca02c", Increase = "#d62728"), 
                         name = NULL) +  # 移除"Trend"字样
      
      # 1990年点
      geom_point(aes(x = val_1990, shape = "1990"), color = "#1f77b4", size = 2.5) +
      # 2023年点  
      geom_point(aes(x = val_2023, shape = "2023"), color = "#ff7f0e", size = 2.5) +
      scale_shape_manual(values = c("1990" = 16, "2023" = 17), name = NULL) +  # 移除"Year"字样
      
      # 横轴刻度标签
      scale_x_continuous(
        labels = x_labels,
        breaks = x_breaks,
        expand = expansion(mult = c(0.05, 0.1))
      ) +
      
      # 标签和主题
      labs(x = "ASPR", 
           y = NULL, 
           title = cont_name) +  # 只显示大洲名称
      theme_bw() +
      theme(
        legend.position = "top",
        axis.text.y = element_text(size = 8, color = "black"),
        axis.text.x = element_text(size = 9, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", hjust = 0.5),  # 横轴标题居中
        plot.title = element_text(size = 12, face = "bold", hjust = 0,  # 标题左对齐
                                  margin = margin(b = 10, t = 5)),  # 调整标题位置
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    
    return(p)
  }
  
  # 8. 为每个大洲生成图
  continents <- c("Asia", "Europe", "Africa", "North America", "South America", "Oceania")
  plot_list <- list()
  
  for (continent in continents) {
    cat("正在处理", continent, "...\n")
    df_continent <- dumbbell_data %>% filter(continent == !!continent)
    
    if (nrow(df_continent) > 0) {
      p <- plot_continent(df_continent, continent)
      plot_name <- paste0("p_", gsub(" ", "_", tolower(continent)), "_ASPR")
      plot_list[[plot_name]] <- p
      
      # 将图表直接赋值到全局环境
      assign(plot_name, p, envir = .GlobalEnv)
      
      # 显示图表
      print(p)
      cat("已生成:", plot_name, "\n")
    } else {
      cat(continent, "无数据，跳过...\n")
    }
  }
  
  # 9. 返回绘图列表
  return(plot_list)
}

# 执行函数
aspr_plots <- process_and_plot_aspr()

# 输出完成信息
cat("ASPR哑铃图生成完成！\n")
cat("生成的图表对象：\n")
print(names(aspr_plots))

# 现在您可以直接访问各个图表，例如：
p_africa_ASPR
p_asia_ASPR
p_europe_ASPR
p_north_america_ASPR
p_south_america_ASPR
p_oceania_ASPR


##### 5.3 哑铃图 - 单独处理死亡率数据 #####
# 定义处理函数
process_and_plot_asmr <- function() {
  
  # 1. 读取基础数据
  cat("正在读取BRI国家信息...\n")
  location_df <- read.csv("BRI-long-location.csv", stringsAsFactors = FALSE)
  
  # 2. 读取死亡率数据
  cat("正在读取死亡率数据...\n")
  data_raw <- read.csv("全球204国家青少年性病总1990-2023死亡率.csv", stringsAsFactors = FALSE)
  
  # 3. 数据清洗
  cat("正在清洗数据...\n")
  data_clean <- data_raw %>% 
    filter(location %in% location_df$location,
           age == "10-24 years",
           sex == "Both",
           cause == "HIV/AIDS and sexually transmitted infections") %>%
    mutate(year = as.numeric(year)) %>%
    group_by(location) %>%
    filter(n() >= 10) %>%  # 确保有足够多年份数据
    ungroup()
  
  # 4. 提取1990和2023年的数据点
  cat("正在提取1990和2023年数据...\n")
  point_data <- data_clean %>%
    filter(year %in% c(1990, 2023)) %>%
    select(location, year, val) %>%
    pivot_wider(names_from = year, 
                values_from = val,
                names_prefix = "val_")
  
  # 5. 合并大洲信息
  dumbbell_data <- point_data %>%
    inner_join(location_df, by = "location") %>%
    filter(!is.na(val_1990) & !is.na(val_2023))  # 移除缺失值
  
  # 6. 创建方向变量
  dumbbell_data <- dumbbell_data %>%
    mutate(direction = ifelse(val_2023 > val_1990, "Increase", "Decrease"))
  
  # 7. 绘图函数
  plot_continent <- function(df, cont_name) {
    # 按2023年值排序
    df <- df %>%
      arrange(val_2023) %>%
      mutate(location = factor(location, levels = unique(location)))
    
    # 计算数据范围，用于设置横轴刻度
    x_min <- min(c(df$val_1990, df$val_2023), na.rm = TRUE)
    x_max <- max(c(df$val_1990, df$val_2023), na.rm = TRUE)
    
    # 创建合适的breaks和标签
    x_range <- x_max - x_min
    break_interval <- ifelse(x_range > 100, 20, 
                             ifelse(x_range > 50, 10, 
                                    ifelse(x_range > 10, 5, 1)))
    
    x_breaks <- seq(floor(x_min/break_interval)*break_interval, 
                    ceiling(x_max/break_interval)*break_interval, 
                    by = break_interval)
    
    # 格式化标签 - 根据数据范围决定是否使用K单位
    x_labels <- function(x) {
      if(max(x, na.rm = TRUE) > 1000) {
        ifelse(x >= 1000, paste0(round(x/1000), "K"), as.character(x))
      } else {
        as.character(x)
      }
    }
    
    # 创建绘图
    p <- ggplot(df, aes(x = val_1990, xend = val_2023, y = location)) +
      # 哑铃线段
      geom_segment(aes(color = direction), linewidth = 1.2, alpha = 0.7) +
      scale_color_manual(values = c(Decrease = "#2ca02c", Increase = "#d62728"), 
                         name = NULL) +  # 移除"Trend"字样
      
      # 1990年点
      geom_point(aes(x = val_1990, shape = "1990"), color = "#1f77b4", size = 2.5) +
      # 2023年点  
      geom_point(aes(x = val_2023, shape = "2023"), color = "#ff7f0e", size = 2.5) +
      scale_shape_manual(values = c("1990" = 16, "2023" = 17), name = NULL) +  # 移除"Year"字样
      
      # 横轴刻度标签
      scale_x_continuous(
        labels = x_labels,
        breaks = x_breaks,
        expand = expansion(mult = c(0.05, 0.1))
      ) +
      
      # 标签和主题
      labs(x = "ASMR", 
           y = NULL, 
           title = cont_name) +  # 只显示大洲名称
      theme_bw() +
      theme(
        legend.position = "top",
        axis.text.y = element_text(size = 8, color = "black"),
        axis.text.x = element_text(size = 9, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", hjust = 0.5),  # 横轴标题居中
        plot.title = element_text(size = 12, face = "bold", hjust = 0,  # 标题左对齐
                                  margin = margin(b = 10, t = 5)),  # 调整标题位置
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    
    return(p)
  }
  
  # 8. 为每个大洲生成图
  continents <- c("Asia", "Europe", "Africa", "North America", "South America", "Oceania")
  plot_list <- list()
  
  for (continent in continents) {
    cat("正在处理", continent, "...\n")
    df_continent <- dumbbell_data %>% filter(continent == !!continent)
    
    if (nrow(df_continent) > 0) {
      p <- plot_continent(df_continent, continent)
      plot_name <- paste0("p_", gsub(" ", "_", tolower(continent)), "_ASMR")
      plot_list[[plot_name]] <- p
      
      # 将图表直接赋值到全局环境
      assign(plot_name, p, envir = .GlobalEnv)
      
      # 显示图表
      print(p)
      cat("已生成:", plot_name, "\n")
    } else {
      cat(continent, "无数据，跳过...\n")
    }
  }
  
  # 9. 返回绘图列表
  return(plot_list)
}

# 执行函数
asmr_plots <- process_and_plot_asmr()


# 现在您可以直接访问各个图表，例如：
p_africa_ASMR
p_asia_ASMR
p_europe_ASMR
p_north_america_ASMR
p_south_america_ASMR
p_oceania_ASMR


##### 5.4 哑铃图 - 单独处理DALY率数据 #####

# 定义处理函数
process_and_plot_asdr <- function() {
  
  # 1. 读取基础数据
  cat("正在读取BRI国家信息...\n")
  location_df <- read.csv("BRI-long-location.csv", stringsAsFactors = FALSE)
  
  # 2. 读取DALY率数据
  cat("正在读取DALY率数据...\n")
  data_raw <- read.csv("全球204国家青少年性病总1990-2023DALY率.csv", stringsAsFactors = FALSE)
  
  # 3. 数据清洗
  cat("正在清洗数据...\n")
  data_clean <- data_raw %>% 
    filter(location %in% location_df$location,
           age == "10-24 years",
           sex == "Both",
           cause == "HIV/AIDS and sexually transmitted infections") %>%
    mutate(year = as.numeric(year)) %>%
    group_by(location) %>%
    filter(n() >= 10) %>%  # 确保有足够多年份数据
    ungroup()
  
  # 4. 提取1990和2023年的数据点
  cat("正在提取1990和2023年数据...\n")
  point_data <- data_clean %>%
    filter(year %in% c(1990, 2023)) %>%
    select(location, year, val) %>%
    pivot_wider(names_from = year, 
                values_from = val,
                names_prefix = "val_")
  
  # 5. 合并大洲信息
  dumbbell_data <- point_data %>%
    inner_join(location_df, by = "location") %>%
    filter(!is.na(val_1990) & !is.na(val_2023))  # 移除缺失值
  
  # 6. 创建方向变量
  dumbbell_data <- dumbbell_data %>%
    mutate(direction = ifelse(val_2023 > val_1990, "Increase", "Decrease"))
  
  # 7. 绘图函数
  plot_continent <- function(df, cont_name) {
    # 按2023年值排序
    df <- df %>%
      arrange(val_2023) %>%
      mutate(location = factor(location, levels = unique(location)))
    
    # 计算数据范围，用于设置横轴刻度
    x_min <- min(c(df$val_1990, df$val_2023), na.rm = TRUE)
    x_max <- max(c(df$val_1990, df$val_2023), na.rm = TRUE)
    
    # 创建合适的breaks和标签
    x_range <- x_max - x_min
    break_interval <- ifelse(x_range > 10000, 2000, 
                             ifelse(x_range > 5000, 1000, 
                                    ifelse(x_range > 1000, 500, 100)))
    
    x_breaks <- seq(floor(x_min/break_interval)*break_interval, 
                    ceiling(x_max/break_interval)*break_interval, 
                    by = break_interval)
    
    # 格式化标签为"80K"形式
    x_labels <- function(x) {
      ifelse(x >= 1000, paste0(round(x/1000), "K"), as.character(x))
    }
    
    # 创建绘图
    p <- ggplot(df, aes(x = val_1990, xend = val_2023, y = location)) +
      # 哑铃线段
      geom_segment(aes(color = direction), linewidth = 1.2, alpha = 0.7) +
      scale_color_manual(values = c(Decrease = "#2ca02c", Increase = "#d62728"), 
                         name = NULL) +  # 移除"Trend"字样
      
      # 1990年点
      geom_point(aes(x = val_1990, shape = "1990"), color = "#1f77b4", size = 2.5) +
      # 2023年点  
      geom_point(aes(x = val_2023, shape = "2023"), color = "#ff7f0e", size = 2.5) +
      scale_shape_manual(values = c("1990" = 16, "2023" = 17), name = NULL) +  # 移除"Year"字样
      
      # 横轴刻度标签
      scale_x_continuous(
        labels = x_labels,
        breaks = x_breaks,
        expand = expansion(mult = c(0.05, 0.1))
      ) +
      
      # 标签和主题
      labs(x = "ASDR", 
           y = NULL, 
           title = cont_name) +  # 只显示大洲名称
      theme_bw() +
      theme(
        legend.position = "top",
        axis.text.y = element_text(size = 8, color = "black"),
        axis.text.x = element_text(size = 9, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", hjust = 0.5),  # 横轴标题居中
        plot.title = element_text(size = 12, face = "bold", hjust = 0,  # 标题左对齐
                                  margin = margin(b = 10, t = 5)),  # 调整标题位置
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    
    return(p)
  }
  
  # 8. 为每个大洲生成图
  continents <- c("Asia", "Europe", "Africa", "North America", "South America", "Oceania")
  plot_list <- list()
  
  for (continent in continents) {
    cat("正在处理", continent, "...\n")
    df_continent <- dumbbell_data %>% filter(continent == !!continent)
    
    if (nrow(df_continent) > 0) {
      p <- plot_continent(df_continent, continent)
      plot_name <- paste0("p_", gsub(" ", "_", tolower(continent)), "_ASDR")
      plot_list[[plot_name]] <- p
      
      # 将图表直接赋值到全局环境
      assign(plot_name, p, envir = .GlobalEnv)
      
      # 显示图表
      print(p)
      cat("已生成:", plot_name, "\n")
    } else {
      cat(continent, "无数据，跳过...\n")
    }
  }
  
  # 9. 返回绘图列表
  return(plot_list)
}

# 执行函数
asdr_plots <- process_and_plot_asdr()


# 现在您可以直接访问各个图表，例如：
p_africa_ASDR
p_asia_ASDR
p_europe_ASDR
p_north_america_ASDR
p_south_america_ASDR
p_oceania_ASDR



#####5.5分性别柱状图(桃心图)#####

######5.5.1分性别柱状图(桃心图)ASIR######

# 读取数据
bri_loc <- read.csv("BRI-long-location.csv")
changeasirmf <- read.csv("1990和2023全球青少年STD204国家ASIR男女.csv")

# 定义绘图函数
create_sex_barplot <- function(continent_name) {
  # 数据处理 - 首先计算性别差异
  gender_diff_data <- changeasirmf %>%
    inner_join(bri_loc, by = "location") %>%
    filter(continent == continent_name, year == 2023) %>%
    # 计算每个国家的性别差异
    group_by(location) %>%
    summarise(
      female_val = val[sex == "Female"],
      male_val = val[sex == "Male"],
      # 计算男女平均值用于排序
      mean_val = mean(c(female_val, male_val)),
      # 绝对差异：女性-男性
      absolute_diff = female_val - male_val,
      # 相对差异：女性/男性，保留2位小数
      relative_diff = round(female_val / male_val, 2),
      .groups = 'drop'
    ) %>%
    # 确定差异符号和形状
    mutate(
      diff_shape = ifelse(absolute_diff > 0, 24, 25),  # 24=上三角, 25=下三角
      diff_color = ifelse(absolute_diff > 0, "red", "darkgreen"),
      # 创建标注文本
      label_text = paste0(relative_diff)
    )
  
  # 主要数据处理
  processed_data <- changeasirmf %>%
    inner_join(bri_loc, by = "location") %>%
    filter(continent == continent_name, year == 2023) %>%
    mutate(
      val_adj = ifelse(sex == "Male", -val, val),
      upper_adj = ifelse(sex == "Male", -upper, upper),
      lower_adj = ifelse(sex == "Male", -lower, lower)
    ) %>%
    # 合并排序数据
    left_join(gender_diff_data %>% select(location, mean_val), by = "location") %>%
    # 🔥真正按 mean_val 升序排国家（从下到上）
    mutate(location = factor(location, levels = gender_diff_data %>% arrange(mean_val) %>% pull(location))) %>%
    # 合并性别差异数据
    left_join(gender_diff_data %>% select(-mean_val), by = "location")
  
  if (nrow(processed_data) == 0) {
    cat("跳过", continent_name, "- 无数据\n")
    return(NULL)
  }
  
  # 计算横轴范围，为右侧标注留出空间
  x_range <- max(abs(c(processed_data$lower_adj, processed_data$upper_adj)), na.rm = TRUE)
  x_breaks <- seq(-ceiling(x_range/10000)*10000, ceiling(x_range/10000)*10000, by = 10000)
  
  # 确定右侧标注的x位置（最大值的1.15倍）
  max_x_val <- max(abs(c(processed_data$val_adj, processed_data$upper_adj)), na.rm = TRUE)
  label_x_position <- max_x_val * 1.15
  
  # 为标注创建单独的数据框
  label_data <- processed_data %>%
    filter(sex == "Female") %>%
    distinct(location, .keep_all = TRUE) %>%
    select(location, label_text, diff_shape, diff_color, relative_diff)
  
  # 绘图
  p <- ggplot(processed_data, aes(x = val_adj, y = location)) +
    geom_bar(
      aes(fill = sex), 
      color = "black",  # 直接设置边框颜色为黑色
      stat = "identity", 
      linewidth = 0.3
    ) +
    geom_errorbar(
      aes(xmin = lower_adj, xmax = upper_adj),
      width = 0.5, 
      position = position_dodge(0.9),
      linewidth = 0.3,
      color = "black"  # 直接设置误差线颜色为黑色
    ) +
    # 添加性别差异标注 - 使用形状标记
    geom_point(
      data = label_data,
      aes(x = label_x_position, y = location, shape = factor(diff_shape), color = diff_color),
      size = 3,
      fill = label_data$diff_color  # 填充颜色
    ) +
    # 添加相对差异数值 - 根据相对差异值设置颜色
    geom_text(
      data = label_data,
      aes(x = label_x_position * 1.05, y = location, label = label_text, 
          color = ifelse(relative_diff > 1, "red", "darkgreen")),
      size = 2.5,
      hjust = 0,
      vjust = 0.5,
      fontface = "bold"
    ) +
    scale_x_continuous(
      labels = ~ paste0(abs(.x)/1000, "K"),
      breaks = x_breaks,
      expand = expansion(mult = c(0.05, 0.3))  # 增加右侧空间以容纳标注
    ) +
    scale_shape_manual(
      values = c("24" = 24, "25" = 25),
      guide = "none"  # 不显示形状图例
    ) +
    scale_color_identity(guide = "none") +  # 不显示颜色图例
    labs(
      x = 'IR (/100k)', 
      y = '',
      title = continent_name
    ) + 
    theme_minimal() +
    theme(
      legend.position = 'bottom',  # 将图例放在底部
      legend.title = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), 'lines'),
      plot.title = element_text(
        hjust = 0,
        size = 12,
        face = "bold",
        margin = margin(b = 10)
      ),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(size = 12), 
      axis.title.y = element_text(size = 12),
      axis.line = element_line(colour = 'black'),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.text = element_text(size = 10),
      legend.box = "horizontal",  # 水平排列图例
      legend.direction = "horizontal"  # 水平方向
    ) +
    scale_fill_manual(
      values = c('mistyrose3', 'skyblue3'),
      labels = c("Female", "Male")
    )
  
  return(p)
}

# 创建水平排列的图例图形
create_horizontal_legend_plot <- function() {
  # 创建简单的图例数据 - 增加红绿三角形之间的间距
  legend_data <- data.frame(
    type = c("fill", "fill", "shape", "shape"),
    label = c("Female", "Male", "Female>Male (Female/Male)", "Female<Male (Female/Male)"),
    color = c("mistyrose3", "skyblue3", "red", "darkgreen"),
    shape = c(15, 15, 24, 25),  # 15=正方形，24=上三角，25=下三角
    x = c(1, 2, 4, 5.5),  # 增加红绿三角形之间的x坐标距离
    y = c(1, 1, 1, 1)
  )
  
  # 创建水平图例图形
  legend_plot <- ggplot(legend_data, aes(x = x, y = y)) +
    # 添加填充图例项
    geom_point(
      data = legend_data[1:2, ],
      aes(shape = factor(shape), fill = color),
      size = 5,
      color = "black"
    ) +
    # 添加形状图例项
    geom_point(
      data = legend_data[3:4, ],
      aes(shape = factor(shape), fill = color),
      size = 5,
      color = "black"
    ) +
    # 添加标签 - 前两个标签为黑色，后两个标签使用相应颜色
    geom_text(
      data = legend_data[1:2, ],
      aes(label = label),
      hjust = 0,
      nudge_x = 0.3,
      size = 3.5,
      color = "black"
    ) +
    geom_text(
      data = legend_data[3, ],
      aes(label = label),
      hjust = 0,
      nudge_x = 0.3,
      size = 3.5,
      color = "red"  # 红色文字
    ) +
    geom_text(
      data = legend_data[4, ],
      aes(label = label),
      hjust = 0,
      nudge_x = 0.3,
      size = 3.5,
      color = "darkgreen"  # 绿色文字
    ) +
    scale_fill_identity() +
    scale_shape_manual(
      values = c("15" = 15, "24" = 24, "25" = 25),
      guide = "none"
    ) +
    scale_x_continuous(limits = c(0.5, 8.5)) +  # 调整x轴范围以适应更大的间距
    scale_y_continuous(limits = c(0.5, 1.5)) +
    theme_void() +
    theme(
      plot.margin = margin(5, 5, 5, 5)
    )
  
  return(legend_plot)
}

# 大洲列表
continents <- c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")

# 创建列表存储所有图形
all_plots <- list()

# 生成并显示每个大洲的图形
for (continent in continents) {
  plot_name <- paste0("p_", tolower(gsub(" ", "_", continent)), "_ASIR_sex")
  p <- create_sex_barplot(continent)
  
  if (!is.null(p)) {
    assign(plot_name, p, envir = .GlobalEnv)
    all_plots[[plot_name]] = p
    cat("已生成:", plot_name, "\n")
    
    if (exists(plot_name, envir = .GlobalEnv)) {
      cat("确认: 对象", plot_name, "已成功创建\n")
    } else {
      cat("警告: 对象", plot_name, "创建失败\n")
    }
  } else {
    cat("跳过", continent, "- 无数据\n")
  }
}

# 创建水平图例
horizontal_legend <- create_horizontal_legend_plot()

# 输出完成信息
cat("\n最终确认 - 全局环境中的ASIR相关对象:\n")
objects_in_env <- ls(envir = .GlobalEnv)
sex_plot_objects <- objects_in_env[grep("_ASIR_sex$", objects_in_env)]
print(sex_plot_objects)

cat("\n所有大洲的ASIR分性别柱状图生成完成！\n")
cat("可用的图表对象：\n")
for (obj_name in sex_plot_objects) {
  cat("-", obj_name, "\n")
}

# 单独显示每个大洲的图形和对应的水平图例
for (continent in continents) {
  plot_name <- paste0("p_", tolower(gsub(" ", "_", continent)), "_ASIR_sex")
  if (exists(plot_name, envir = .GlobalEnv)) {
    main_plot <- get(plot_name)
    
    # 组合主图和水平图例
    combined_plot <- ggarrange(
      main_plot, 
      horizontal_legend,
      nrow = 2, 
      heights = c(0.9, 0.1)
    )
    
    print(combined_plot)
    cat("显示:", plot_name, "与水平图例\n")
  }
}

# 组合所有大洲的图形（不带图例）
p_africa_ASIR_sex 
p_asia_ASIR_sex 
p_europe_ASIR_sex 
p_north_america_ASIR_sex 
p_oceania_ASIR_sex 
p_south_america_ASIR_sex 

psex6conti <- ggarrange(p_africa_ASIR_sex, p_asia_ASIR_sex,
                        p_europe_ASIR_sex, p_north_america_ASIR_sex,
                        p_oceania_ASIR_sex, p_south_america_ASIR_sex,
                        labels = c("A", "B", "C", "D","E","F"),
                        nrow = 2, ncol = 3,
                        common.legend = FALSE, legend = 'none')

# 显示组合图并在底部添加水平图例
final_plot <- ggarrange(
  psex6conti,
  horizontal_legend,
  nrow = 2,
  heights = c(0.95, 0.05)
)

print(final_plot)


######5.5.2分性别柱状图(桃心图)ASPR######

# 读取数据
bri_loc <- read.csv("BRI-long-location.csv")
changeasprmf <- read.csv("1990和2023全球青少年STD204国家ASPR男女.csv")

# 定义绘图函数
create_sex_barplot_aspr <- function(continent_name) {
  # 数据处理 - 首先计算性别差异
  gender_diff_data <- changeasprmf %>%
    inner_join(bri_loc, by = "location") %>%
    filter(continent == continent_name, year == 2023) %>%
    # 计算每个国家的性别差异
    group_by(location) %>%
    summarise(
      female_val = val[sex == "Female"],
      male_val = val[sex == "Male"],
      # 计算男女平均值用于排序
      mean_val = mean(c(female_val, male_val)),
      # 绝对差异：女性-男性
      absolute_diff = female_val - male_val,
      # 相对差异：女性/男性，保留2位小数
      relative_diff = round(female_val / male_val, 2),
      .groups = 'drop'
    ) %>%
    # 确定差异符号和形状
    mutate(
      diff_shape = ifelse(absolute_diff > 0, 24, 25),  # 24=上三角, 25=下三角
      diff_color = ifelse(absolute_diff > 0, "red", "darkgreen"),
      # 创建标注文本
      label_text = paste0(relative_diff)
    )
  
  # 主要数据处理
  processed_data <- changeasprmf %>%
    inner_join(bri_loc, by = "location") %>%
    filter(continent == continent_name, year == 2023) %>%
    mutate(
      val_adj = ifelse(sex == "Male", -val, val),
      upper_adj = ifelse(sex == "Male", -upper, upper),
      lower_adj = ifelse(sex == "Male", -lower, lower)
    ) %>%
    # 合并排序数据
    left_join(gender_diff_data %>% select(location, mean_val), by = "location") %>%
    # 按男女平均值排序location（从下到上由小到大）
    mutate(location = factor(location, levels = unique(location[order(mean_val)]))) %>%
    # 合并性别差异数据
    left_join(gender_diff_data %>% select(-mean_val), by = "location")
  
  if (nrow(processed_data) == 0) {
    cat("跳过", continent_name, "- 无数据\n")
    return(NULL)
  }
  
  # 计算横轴范围，为右侧标注留出空间
  x_range <- max(abs(c(processed_data$lower_adj, processed_data$upper_adj)), na.rm = TRUE)
  x_breaks <- seq(-ceiling(x_range/10000)*10000, ceiling(x_range/10000)*10000, by = 10000)
  
  # 确定右侧标注的x位置（最大值的1.15倍）
  max_x_val <- max(abs(c(processed_data$val_adj, processed_data$upper_adj)), na.rm = TRUE)
  label_x_position <- max_x_val * 1.15
  
  # 为标注创建单独的数据框
  label_data <- processed_data %>%
    filter(sex == "Female") %>%
    distinct(location, .keep_all = TRUE) %>%
    select(location, label_text, diff_shape, diff_color, relative_diff)
  
  # 绘图
  p <- ggplot(processed_data, aes(x = val_adj, y = location)) +
    geom_bar(
      aes(fill = sex), 
      color = "black",  # 直接设置边框颜色为黑色
      stat = "identity", 
      linewidth = 0.3
    ) +
    geom_errorbar(
      aes(xmin = lower_adj, xmax = upper_adj),
      width = 0.5, 
      position = position_dodge(0.9),
      linewidth = 0.3,
      color = "black"  # 直接设置误差线颜色为黑色
    ) +
    # 添加性别差异标注 - 使用形状标记
    geom_point(
      data = label_data,
      aes(x = label_x_position, y = location, shape = factor(diff_shape), color = diff_color),
      size = 3,
      fill = label_data$diff_color  # 填充颜色
    ) +
    # 添加相对差异数值 - 根据相对差异值设置颜色
    geom_text(
      data = label_data,
      aes(x = label_x_position * 1.05, y = location, label = label_text, 
          color = ifelse(relative_diff > 1, "red", "darkgreen")),
      size = 2.5,
      hjust = 0,
      vjust = 0.5,
      fontface = "bold"
    ) +
    scale_x_continuous(
      labels = ~ paste0(abs(.x)/1000, "K"),
      breaks = x_breaks,
      expand = expansion(mult = c(0.05, 0.3))  # 增加右侧空间以容纳标注
    ) +
    scale_shape_manual(
      values = c("24" = 24, "25" = 25),
      guide = "none"  # 不显示形状图例
    ) +
    scale_color_identity(guide = "none") +  # 不显示颜色图例
    labs(
      x = 'ASPR (/100k)', 
      y = '',
      title = continent_name
    ) + 
    theme_minimal() +
    theme(
      legend.position = 'bottom',  # 将图例放在底部
      legend.title = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), 'lines'),
      plot.title = element_text(
        hjust = 0,
        size = 12,
        face = "bold",
        margin = margin(b = 10)
      ),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(size = 12), 
      axis.title.y = element_text(size = 12),
      axis.line = element_line(colour = 'black'),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.text = element_text(size = 10),
      legend.box = "horizontal",  # 水平排列图例
      legend.direction = "horizontal"  # 水平方向
    ) +
    scale_fill_manual(
      values = c('mistyrose3', 'skyblue3'),
      labels = c("Female", "Male")
    )
  
  return(p)
}

# 大洲列表
continents <- c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")

# 创建列表存储所有图形
all_plots_aspr <- list()

# 生成并显示每个大洲的图形
for (continent in continents) {
  plot_name <- paste0("p_", tolower(gsub(" ", "_", continent)), "_ASPR_sex")
  p <- create_sex_barplot_aspr(continent)
  
  if (!is.null(p)) {
    assign(plot_name, p, envir = .GlobalEnv)
    all_plots_aspr[[plot_name]] = p
    cat("已生成:", plot_name, "\n")
    
    if (exists(plot_name, envir = .GlobalEnv)) {
      cat("确认: 对象", plot_name, "已成功创建\n")
    } else {
      cat("警告: 对象", plot_name, "创建失败\n")
    }
  } else {
    cat("跳过", continent, "- 无数据\n")
  }
}

# 输出完成信息
cat("\n最终确认 - 全局环境中的ASPR相关对象:\n")
objects_in_env <- ls(envir = .GlobalEnv)
aspr_plot_objects <- objects_in_env[grep("_ASPR_sex$", objects_in_env)]
print(aspr_plot_objects)

cat("\n所有大洲的ASPR分性别柱状图生成完成！\n")
cat("可用的图表对象：\n")
for (obj_name in aspr_plot_objects) {
  cat("-", obj_name, "\n")
}

# 单独显示每个大洲的图形和对应的水平图例
for (continent in continents) {
  plot_name <- paste0("p_", tolower(gsub(" ", "_", continent)), "_ASPR_sex")
  if (exists(plot_name, envir = .GlobalEnv)) {
    main_plot <- get(plot_name)
    
    # 组合主图和水平图例
    combined_plot <- ggarrange(
      main_plot, 
      horizontal_legend,
      nrow = 2, 
      heights = c(0.9, 0.1)
    )
    
    print(combined_plot)
    cat("显示:", plot_name, "与水平图例\n")
  }
}

# 组合所有大洲的图形（不带图例）
p_africa_ASPR_sex 
p_asia_ASPR_sex 
p_europe_ASPR_sex 
p_north_america_ASPR_sex 
p_oceania_ASPR_sex 
p_south_america_ASPR_sex 

psex6conti_aspr <- ggarrange(p_africa_ASPR_sex, p_asia_ASPR_sex,
                             p_europe_ASPR_sex, p_north_america_ASPR_sex,
                             p_oceania_ASPR_sex, p_south_america_ASPR_sex,
                             labels = c("A", "B", "C", "D","E","F"),
                             nrow = 2, ncol = 3,
                             common.legend = FALSE, legend = 'none')

# 显示组合图并在底部添加水平图例
final_plot_aspr <- ggarrange(
  psex6conti_aspr,
  horizontal_legend,
  nrow = 2,
  heights = c(0.95, 0.05)
)

print(final_plot_aspr)


######5.5.3分性别柱状图(桃心图)ASMR######

# 读取数据
bri_loc <- read.csv("BRI-long-location.csv")
changeasmrmf <- read.csv("1990和2023全球青少年STD204国家ASMR男女.csv")

# 定义绘图函数
create_sex_barplot_asmr <- function(continent_name) {
  # 数据处理 - 首先计算性别差异
  gender_diff_data <- changeasmrmf %>%
    inner_join(bri_loc, by = "location") %>%
    filter(continent == continent_name, year == 2023) %>%
    # 计算每个国家的性别差异
    group_by(location) %>%
    summarise(
      female_val = val[sex == "Female"],
      male_val = val[sex == "Male"],
      # 计算男女平均值用于排序
      mean_val = mean(c(female_val, male_val)),
      # 绝对差异：女性-男性
      absolute_diff = female_val - male_val,
      # 相对差异：女性/男性，保留2位小数
      relative_diff = round(female_val / male_val, 2),
      .groups = 'drop'
    ) %>%
    # 确定差异符号和形状
    mutate(
      diff_shape = ifelse(absolute_diff > 0, 24, 25),  # 24=上三角, 25=下三角
      diff_color = ifelse(absolute_diff > 0, "red", "darkgreen"),
      # 创建标注文本
      label_text = paste0(relative_diff)
    )
  
  # 主要数据处理
  processed_data <- changeasmrmf %>%
    inner_join(bri_loc, by = "location") %>%
    filter(continent == continent_name, year == 2023) %>%
    mutate(
      val_adj = ifelse(sex == "Male", -val, val),
      upper_adj = ifelse(sex == "Male", -upper, upper),
      lower_adj = ifelse(sex == "Male", -lower, lower)
    ) %>%
    # 合并排序数据
    left_join(gender_diff_data %>% select(location, mean_val), by = "location") %>%
    # 按男女平均值排序location（从下到上由小到大）
    mutate(location = factor(location, levels = unique(location[order(mean_val)]))) %>%
    # 合并性别差异数据
    left_join(gender_diff_data %>% select(-mean_val), by = "location")
  
  if (nrow(processed_data) == 0) {
    cat("跳过", continent_name, "- 无数据\n")
    return(NULL)
  }
  
  # 计算横轴范围，为右侧标注留出空间
  x_range <- max(abs(c(processed_data$lower_adj, processed_data$upper_adj)), na.rm = TRUE)
  
  # 根据数据范围调整刻度
  if (x_range > 1000) {
    x_breaks <- seq(-ceiling(x_range/1000)*1000, ceiling(x_range/1000)*1000, by = 1000)
  } else {
    x_breaks <- seq(-ceiling(x_range/100)*100, ceiling(x_range/100)*100, by = 100)
  }
  
  # 确定右侧标注的x位置（最大值的1.15倍）
  max_x_val <- max(abs(c(processed_data$val_adj, processed_data$upper_adj)), na.rm = TRUE)
  label_x_position <- max_x_val * 1.15
  
  # 为标注创建单独的数据框
  label_data <- processed_data %>%
    filter(sex == "Female") %>%
    distinct(location, .keep_all = TRUE) %>%
    select(location, label_text, diff_shape, diff_color, relative_diff)
  
  # 绘图
  p <- ggplot(processed_data, aes(x = val_adj, y = location)) +
    geom_bar(
      aes(fill = sex), 
      color = "black",  # 直接设置边框颜色为黑色
      stat = "identity", 
      linewidth = 0.3
    ) +
    geom_errorbar(
      aes(xmin = lower_adj, xmax = upper_adj),
      width = 0.5, 
      position = position_dodge(0.9),
      linewidth = 0.3,
      color = "black"  # 直接设置误差线颜色为黑色
    ) +
    # 添加性别差异标注 - 使用形状标记
    geom_point(
      data = label_data,
      aes(x = label_x_position, y = location, shape = factor(diff_shape), color = diff_color),
      size = 3,
      fill = label_data$diff_color  # 填充颜色
    ) +
    # 添加相对差异数值 - 根据相对差异值设置颜色
    geom_text(
      data = label_data,
      aes(x = label_x_position * 1.05, y = location, label = label_text, 
          color = ifelse(relative_diff > 1, "red", "darkgreen")),
      size = 2.5,
      hjust = 0,
      vjust = 0.5,
      fontface = "bold"
    ) +
    scale_x_continuous(
      labels = ~ ifelse(abs(.x) >= 1000, paste0(abs(.x)/1000, "K"), as.character(abs(.x))),
      breaks = x_breaks,
      expand = expansion(mult = c(0.05, 0.3))  # 增加右侧空间以容纳标注
    ) +
    scale_shape_manual(
      values = c("24" = 24, "25" = 25),
      guide = "none"  # 不显示形状图例
    ) +
    scale_color_identity(guide = "none") +  # 不显示颜色图例
    labs(
      x = 'ASMR (/100k)', 
      y = '',
      title = continent_name
    ) + 
    theme_minimal() +
    theme(
      legend.position = 'bottom',  # 将图例放在底部
      legend.title = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), 'lines'),
      plot.title = element_text(
        hjust = 0,
        size = 12,
        face = "bold",
        margin = margin(b = 10)
      ),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(size = 12), 
      axis.title.y = element_text(size = 12),
      axis.line = element_line(colour = 'black'),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.text = element_text(size = 10),
      legend.box = "horizontal",  # 水平排列图例
      legend.direction = "horizontal"  # 水平方向
    ) +
    scale_fill_manual(
      values = c('mistyrose3', 'skyblue3'),
      labels = c("Female", "Male")
    )
  
  return(p)
}

# 大洲列表
continents <- c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")

# 创建列表存储所有图形
all_plots_asmr <- list()

# 生成并显示每个大洲的图形
for (continent in continents) {
  plot_name <- paste0("p_", tolower(gsub(" ", "_", continent)), "_ASMR_sex")
  p <- create_sex_barplot_asmr(continent)
  
  if (!is.null(p)) {
    assign(plot_name, p, envir = .GlobalEnv)
    all_plots_asmr[[plot_name]] = p
    cat("已生成:", plot_name, "\n")
    
    if (exists(plot_name, envir = .GlobalEnv)) {
      cat("确认: 对象", plot_name, "已成功创建\n")
    } else {
      cat("警告: 对象", plot_name, "创建失败\n")
    }
  } else {
    cat("跳过", continent, "- 无数据\n")
  }
}

# 输出完成信息
cat("\n最终确认 - 全局环境中的ASMR相关对象:\n")
objects_in_env <- ls(envir = .GlobalEnv)
asmr_plot_objects <- objects_in_env[grep("_ASMR_sex$", objects_in_env)]
print(asmr_plot_objects)

cat("\n所有大洲的ASMR分性别柱状图生成完成！\n")
cat("可用的图表对象：\n")
for (obj_name in asmr_plot_objects) {
  cat("-", obj_name, "\n")
}

# 单独显示每个大洲的图形和对应的水平图例
for (continent in continents) {
  plot_name <- paste0("p_", tolower(gsub(" ", "_", continent)), "_ASMR_sex")
  if (exists(plot_name, envir = .GlobalEnv)) {
    main_plot <- get(plot_name)
    
    # 组合主图和水平图例
    combined_plot <- ggarrange(
      main_plot, 
      horizontal_legend,
      nrow = 2, 
      heights = c(0.9, 0.1)
    )
    
    print(combined_plot)
    cat("显示:", plot_name, "与水平图例\n")
  }
}

# 组合所有大洲的图形（不带图例）
p_africa_ASMR_sex 
p_asia_ASMR_sex 
p_europe_ASMR_sex 
p_north_america_ASMR_sex 
p_oceania_ASMR_sex 
p_south_america_ASMR_sex 

psex6conti_asmr <- ggarrange(p_africa_ASMR_sex, p_asia_ASMR_sex,
                             p_europe_ASMR_sex, p_north_america_ASMR_sex,
                             p_oceania_ASMR_sex, p_south_america_ASMR_sex,
                             labels = c("A", "B", "C", "D","E","F"),
                             nrow = 2, ncol = 3,
                             common.legend = FALSE, legend = 'none')

# 显示组合图并在底部添加水平图例
final_plot_asmr <- ggarrange(
  psex6conti_asmr,
  horizontal_legend,
  nrow = 2,
  heights = c(0.95, 0.05)
)

print(final_plot_asmr)


######5.5.4分性别柱状图(桃心图)ASDR######

# 读取数据
bri_loc <- read.csv("BRI-long-location.csv")
changeasdrmf <- read.csv("1990和2023全球青少年STD204国家ASDR男女.csv")

# 定义绘图函数
create_sex_barplot_asdr <- function(continent_name) {
  # 数据处理 - 首先计算性别差异
  gender_diff_data <- changeasdrmf %>%
    inner_join(bri_loc, by = "location") %>%
    filter(continent == continent_name, year == 2023) %>%
    # 计算每个国家的性别差异
    group_by(location) %>%
    summarise(
      female_val = val[sex == "Female"],
      male_val = val[sex == "Male"],
      # 计算男女平均值用于排序
      mean_val = mean(c(female_val, male_val)),
      # 绝对差异：女性-男性
      absolute_diff = female_val - male_val,
      # 相对差异：女性/男性，保留2位小数
      relative_diff = round(female_val / male_val, 2),
      .groups = 'drop'
    ) %>%
    # 确定差异符号和形状
    mutate(
      diff_shape = ifelse(absolute_diff > 0, 24, 25),  # 24=上三角, 25=下三角
      diff_color = ifelse(absolute_diff > 0, "red", "darkgreen"),
      # 创建标注文本
      label_text = paste0(relative_diff)
    )
  
  # 主要数据处理
  processed_data <- changeasdrmf %>%
    inner_join(bri_loc, by = "location") %>%
    filter(continent == continent_name, year == 2023) %>%
    mutate(
      val_adj = ifelse(sex == "Male", -val, val),
      upper_adj = ifelse(sex == "Male", -upper, upper),
      lower_adj = ifelse(sex == "Male", -lower, lower)
    ) %>%
    # 合并排序数据
    left_join(gender_diff_data %>% select(location, mean_val), by = "location") %>%
    # 按男女平均值排序location（从下到上由小到大）
    mutate(location = factor(location, levels = unique(location[order(mean_val)]))) %>%
    # 合并性别差异数据
    left_join(gender_diff_data %>% select(-mean_val), by = "location")
  
  if (nrow(processed_data) == 0) {
    cat("跳过", continent_name, "- 无数据\n")
    return(NULL)
  }
  
  # 计算横轴范围，为右侧标注留出空间
  x_range <- max(abs(c(processed_data$lower_adj, processed_data$upper_adj)), na.rm = TRUE)
  x_breaks <- seq(-ceiling(x_range/10000)*10000, ceiling(x_range/10000)*10000, by = 10000)
  
  # 确定右侧标注的x位置（最大值的1.15倍）
  max_x_val <- max(abs(c(processed_data$val_adj, processed_data$upper_adj)), na.rm = TRUE)
  label_x_position <- max_x_val * 1.15
  
  # 为标注创建单独的数据框
  label_data <- processed_data %>%
    filter(sex == "Female") %>%
    distinct(location, .keep_all = TRUE) %>%
    select(location, label_text, diff_shape, diff_color, relative_diff)
  
  # 绘图
  p <- ggplot(processed_data, aes(x = val_adj, y = location)) +
    geom_bar(
      aes(fill = sex), 
      color = "black",  # 直接设置边框颜色为黑色
      stat = "identity", 
      linewidth = 0.3
    ) +
    geom_errorbar(
      aes(xmin = lower_adj, xmax = upper_adj),
      width = 0.5, 
      position = position_dodge(0.9),
      linewidth = 0.3,
      color = "black"  # 直接设置误差线颜色为黑色
    ) +
    # 添加性别差异标注 - 使用形状标记
    geom_point(
      data = label_data,
      aes(x = label_x_position, y = location, shape = factor(diff_shape), color = diff_color),
      size = 3,
      fill = label_data$diff_color  # 填充颜色
    ) +
    # 添加相对差异数值 - 根据相对差异值设置颜色
    geom_text(
      data = label_data,
      aes(x = label_x_position * 1.05, y = location, label = label_text, 
          color = ifelse(relative_diff > 1, "red", "darkgreen")),
      size = 2.5,
      hjust = 0,
      vjust = 0.5,
      fontface = "bold"
    ) +
    scale_x_continuous(
      labels = ~ paste0(abs(.x)/1000, "K"),
      breaks = x_breaks,
      expand = expansion(mult = c(0.05, 0.3))  # 增加右侧空间以容纳标注
    ) +
    scale_shape_manual(
      values = c("24" = 24, "25" = 25),
      guide = "none"  # 不显示形状图例
    ) +
    scale_color_identity(guide = "none") +  # 不显示颜色图例
    labs(
      x = 'ASDR (/100k)', 
      y = '',
      title = continent_name
    ) + 
    theme_minimal() +
    theme(
      legend.position = 'bottom',  # 将图例放在底部
      legend.title = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), 'lines'),
      plot.title = element_text(
        hjust = 0,
        size = 12,
        face = "bold",
        margin = margin(b = 10)
      ),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(size = 12), 
      axis.title.y = element_text(size = 12),
      axis.line = element_line(colour = 'black'),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.text = element_text(size = 10),
      legend.box = "horizontal",  # 水平排列图例
      legend.direction = "horizontal"  # 水平方向
    ) +
    scale_fill_manual(
      values = c('mistyrose3', 'skyblue3'),
      labels = c("Female", "Male")
    )
  
  return(p)
}

# 大洲列表
continents <- c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")

# 创建列表存储所有图形
all_plots_asdr <- list()

# 生成并显示每个大洲的图形
for (continent in continents) {
  plot_name <- paste0("p_", tolower(gsub(" ", "_", continent)), "_ASDR_sex")
  p <- create_sex_barplot_asdr(continent)
  
  if (!is.null(p)) {
    assign(plot_name, p, envir = .GlobalEnv)
    all_plots_asdr[[plot_name]] = p
    cat("已生成:", plot_name, "\n")
    
    if (exists(plot_name, envir = .GlobalEnv)) {
      cat("确认: 对象", plot_name, "已成功创建\n")
    } else {
      cat("警告: 对象", plot_name, "创建失败\n")
    }
  } else {
    cat("跳过", continent, "- 无数据\n")
  }
}

# 输出完成信息
cat("\n最终确认 - 全局环境中的ASDR相关对象:\n")
objects_in_env <- ls(envir = .GlobalEnv)
asdr_plot_objects <- objects_in_env[grep("_ASDR_sex$", objects_in_env)]
print(asdr_plot_objects)

cat("\n所有大洲的ASDR分性别柱状图生成完成！\n")
cat("可用的图表对象：\n")
for (obj_name in asdr_plot_objects) {
  cat("-", obj_name, "\n")
}

# 单独显示每个大洲的图形和对应的水平图例
for (continent in continents) {
  plot_name <- paste0("p_", tolower(gsub(" ", "_", continent)), "_ASDR_sex")
  if (exists(plot_name, envir = .GlobalEnv)) {
    main_plot <- get(plot_name)
    
    # 组合主图和水平图例
    combined_plot <- ggarrange(
      main_plot, 
      horizontal_legend,
      nrow = 2, 
      heights = c(0.9, 0.1)
    )
    
    print(combined_plot)
    cat("显示:", plot_name, "与水平图例\n")
  }
}

# 组合所有大洲的图形（不带图例）
p_africa_ASDR_sex 
p_asia_ASDR_sex 
p_europe_ASDR_sex 
p_north_america_ASDR_sex 
p_oceania_ASDR_sex 
p_south_america_ASDR_sex 

psex6conti_asdr <- ggarrange(p_africa_ASDR_sex, p_asia_ASDR_sex,
                             p_europe_ASDR_sex, p_north_america_ASDR_sex,
                             p_oceania_ASDR_sex, p_south_america_ASDR_sex,
                             labels = c("A", "B", "C", "D","E","F"),
                             nrow = 2, ncol = 3,
                             common.legend = FALSE, legend = 'none')

# 显示组合图并在底部添加水平图例
final_plot_asdr <- ggarrange(
  psex6conti_asdr,
  horizontal_legend,
  nrow = 2,
  heights = c(0.95, 0.05)
)

print(final_plot_asdr)
