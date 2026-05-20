# 清除环境
rm(list = ls())


# 加载必要的包
library(ggplot2)
library(dplyr)
library(maps)
library(countrycode)
library(viridis)
library(patchwork)
library(RColorBrewer)

# 创建输出目录
if(!dir.exists("disaster_maps")) {
  dir.create("disaster_maps")
}

# 定义灾害类型和协调的配色方案
disaster_types <- c("Flood","Storm","Drought","Extreme temperature")

# 为每个灾害类型定义协调的颜色方案
# 方案1
# disaster_palettes <- list(
#   "Storm" = colorRampPalette(c("#E3F2FD", "#1976D2", "#0D47A1"))(100),
#   "Extreme temperature" = colorRampPalette(c("#FFEBEE", "#F44336", "#B71C1C"))(100),
#   "Drought" = colorRampPalette(c("#FFF3E0", "#FF9800", "#E65100"))(100),
#   "Flood" = colorRampPalette(c("#E8F5E8", "#4CAF50", "#1B5E20"))(100)
# )
# #方案2
# disaster_palettes <- list(
#   "Storm" = colorRampPalette(c("#F0F8FF", "#4682B4", "#2F4F4F"))(100),        # 钢蓝色系
#   "Extreme temperature" = colorRampPalette(c("#FFF5EE", "#CD5C5C", "#8B0000"))(100),  # 深红色系
#   "Drought" = colorRampPalette(c("#FDF5E6", "#DAA520", "#8B4513"))(100),      # 金色/棕色系
#   "Flood" = colorRampPalette(c("#F0FFF0", "#20B2AA", "#006400"))(100)         # 海绿色系
# )
# 方案3
# disaster_palettes <- list(
#   "Storm" = colorRampPalette(c("#F8F9FA", "#6C757D", "#343A40"))(100),      # 灰色系
#   "Extreme temperature" = colorRampPalette(c("#FFF5F5", "#E74C3C", "#C0392B"))(100), # 红色系
#   "Drought" = colorRampPalette(c("#FEF9E7", "#F39C12", "#E67E22"))(100),    # 橙色系
#   "Flood" = colorRampPalette(c("#E8F6F3", "#3498DB", "#2980B9"))(100)       # 蓝色系
# )
# 方案4
# disaster_palettes <- list(
#   "Storm" = colorRampPalette(c("#E8EAF6", "#3F51B5", "#283593"))(100),      # 靛蓝色
#   "Extreme temperature" = colorRampPalette(c("#FFEBEE", "#E53935", "#B71C1C"))(100), # 深红色
#   "Drought" = colorRampPalette(c("#FFF3E0", "#FF9800", "#EF6C00"))(100),    # 琥珀色
#   "Flood" = colorRampPalette(c("#E0F2F1", "#009688", "#00695C"))(100)       # 青绿色
# )


# 方案5
disaster_palettes <- list(
  "Storm" = colorRampPalette(c("#E8EAF6", "#3F51B5", "#283593"))(100),      # 靛蓝色
  "Extreme temperature" = colorRampPalette(c("#FFF5EE", "#CD5C5C", "#8B0000"))(100),  # 深红色系
  "Drought" = colorRampPalette(c("#FDF5E6", "#DAA520", "#8B4513"))(100),      # 金色/棕色系
  "Flood" = colorRampPalette(c("#E0F2F1", "#009688", "#00695C"))(100)       # 青绿色
)





# 获取世界地图数据
world_map <- map_data("world")

# 函数：处理国家名称以匹配地图数据
clean_country_names <- function(country_names) {
  cleaned <- countrycode(country_names, "country.name", "country.name")
  # 手动处理一些常见的不匹配情况
  cleaned[country_names == "United States of America"] <- "USA"
  cleaned[country_names == "United Kingdom of Great Britain and Northern Ireland"] <- "UK"
  cleaned[country_names == "Russian Federation"] <- "Russia"
  cleaned[country_names == "Democratic Republic of the Congo"] <- "Democratic Republic of Congo"
  cleaned[country_names == "Republic of Congo"] <- "Republic of Congo"
  cleaned[country_names == "Czechia"] <- "Czech Republic"
  cleaned[country_names == "Cabo Verde"] <- "Cape Verde"
  cleaned[country_names == "C?te dˇŻIvoire"] <- "Ivory Coast"
  cleaned[country_names == "T¨ąrkiye"] <- "Turkey"
  cleaned[country_names == "R¨¦union"] <- "Reunion"
  cleaned[country_names == "Viet Nam"] <- "Vietnam"
  cleaned[country_names == "Lao People's Democratic Republic"] <- "Laos"
  cleaned[country_names == "Iran (Islamic Republic of)"] <- "Iran"
  cleaned[country_names == "Syrian Arab Republic"] <- "Syria"
  return(cleaned)
}

# 函数：为单个灾害类型创建地图
create_disaster_maps <- function(disaster_type, file_path) {
  # 读取数据
  disaster_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # 清理国家名称
  disaster_data$Country <- clean_country_names(disaster_data$Country)
  
  # 转换为长格式
  disaster_long <- disaster_data %>%
    pivot_longer(cols = -Country, 
                 names_to = "Period", 
                 values_to = "Frequency") %>%
    mutate(Period = factor(Period, 
                           levels = c("X1900.1949", "X1950.1979", 
                                      "X1980.1999", "X2000.2025"),
                           labels = c("1900-1949", "1950-1979", 
                                      "1980-1999", "2000-2025")))
  
  # 获取该灾害类型的频率范围
  freq_range <- range(disaster_long$Frequency, na.rm = TRUE)
  
  # 合并地图数据
  map_data <- world_map %>%
    left_join(disaster_long, by = c("region" = "Country"))
  
  # 创建四个时期的地图
  plots <- list()
  
  for(period in levels(disaster_long$Period)) {
    period_data <- map_data %>% filter(Period == period)
    
    p <- ggplot() +
      geom_polygon(data = world_map, 
                   aes(x = long, y = lat, group = group),
                   fill = "gray95", color = "white", size = 0.1) +
      geom_polygon(data = period_data,
                   aes(x = long, y = lat, group = group, fill = Frequency),
                   color = "white", size = 0.1) +
      scale_fill_gradientn(
        name = "Frequency",
        colours = disaster_palettes[[disaster_type]],
        limits = freq_range,
        na.value = "gray95",
        guide = guide_colorbar(
          direction = "horizontal",
          barheight = unit(2, "mm"),
          barwidth = unit(50, "mm"),
          title.position = "top"
        )
      ) +
      labs(title = paste(disaster_type, "-", period)) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "bottom",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      ) +
      coord_fixed(1.3)
    
    plots[[period]] <- p
  }
  
  # 组合四个地图
  combined_plot <- wrap_plots(plots, ncol = 2) + 
    plot_annotation(
      title = paste("Global", disaster_type, "Disaster Frequency by Period"),
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
    )
  
  # 保存组合地图
  ggsave(paste0("disaster_maps/", disaster_type, "_combined_maps.png"), 
         combined_plot, width = 16, height = 12, dpi = 300)
  
  # 也保存单个地图
  for(period in levels(disaster_long$Period)) {
    ggsave(paste0("disaster_maps/", disaster_type, "_", gsub("-", "_", period), ".png"), 
           plots[[period]], width = 10, height = 6, dpi = 300)
  }
  
  cat("Created maps for", disaster_type, "\n")
  return(plots)
}

# 为每个灾害类型创建地图
all_plots <- list()

for(disaster in disaster_types) {
  file_name <- paste0(disaster, ".csv")
  if(file.exists(file_name)) {
    all_plots[[disaster]] <- create_disaster_maps(disaster, file_name)
  } else {
    warning("File not found: ", file_name)
  }
}

# 创建所有16张地图的组合图
cat("Creating combined visualization of all 16 maps...\n")

# 重新组织所有地图到一个列表中
all_maps_list <- list()
for(disaster in disaster_types) {
  for(period in c("1900-1949", "1950-1979", "1980-1999", "2000-2025")) {
    all_maps_list[[paste(disaster, period)]] <- all_plots[[disaster]][[period]]
  }
}

# 创建4x4的组合图
full_combined <- wrap_plots(all_maps_list, ncol = 4, nrow = 4) + 
  plot_annotation(
    title = "Global Disaster Frequency Analysis (1900-2025)",
    subtitle = "Four Disaster Types Across Four Time Periods",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
      plot.subtitle = element_text(hjust = 0.5, size = 14)
    )
  )

# 保存完整组合图
ggsave("disaster_maps/all_16_maps_combined.pdf", 
       full_combined, width = 20, height = 16, dpi = 300)

# 创建按灾害类型分组的组合图（4行4列）
for(disaster in disaster_types) {
  type_combined <- wrap_plots(all_plots[[disaster]], ncol = 2) + 
    plot_annotation(
      title = paste(disaster, "Disaster Frequency Across Time Periods"),
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
    )
  ggsave(paste0("disaster_maps/", disaster, "_4_periods.pdf"), 
         type_combined, width = 12, height = 10, dpi = 300)
}



