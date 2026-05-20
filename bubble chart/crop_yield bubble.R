# 加载必要的包
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(forcats)

# 创建气泡图 - 适应新的数据格式
create_bubble_chart_wide_format <- function(data) {
  
  # 将数据从宽格式转换为长格式
  plot_data <- data %>%
    pivot_longer(
      cols = -Region, # 除了Region列之外的所有列
      names_to = "Crop",
      values_to = "Change"
    ) %>%
    filter(!is.na(Change)) %>% # 移除NA值
    mutate(
      # 固定区域顺序为数据中出现的顺序，但使用rev()反转顺序
      Region = factor(Region, levels = rev(unique(Region))),
      # 创建变化方向分类
      Direction = ifelse(Change > 0, "Increase", 
                         ifelse(Change < 0, "Decrease", "Stable")),
      Direction = factor(Direction, levels = c("Increase", "Stable", "Decrease")),
      # 变化绝对值（用于气泡大小）
      AbsChange = abs(Change)
    )
  
  # 创建图表
  p <- ggplot(plot_data, aes(x = Region, y = Crop)) +
    # 气泡点
    geom_point(aes(size = AbsChange, fill = Change), 
               shape = 21,color = "white", stroke = 0.5) + # 边框改为灰色
    
    # 专业颜色梯度
    scale_fill_gradient2(
      low = "#5B8FA8",    # 蓝色表示减少
      mid = "#f7f7f7",    # 白色表示稳定
      high = "#D67236",   # 红色表示增加
      midpoint = 0,
      name = "Yield Change (%)",
      limits = c(-50, 30),
      breaks = seq(-50, 30, by = 10)
    ) +
    
    # # 专业颜色梯度
    # scale_fill_gradient2(
    #   low = "#4A6FA5",    # 蓝色表示减少
    #   mid = "#f7f7f7",    # 白色表示稳定
    #   high = "#B74B4B",   # 红色表示增加
    #   midpoint = 0,
    #   name = "Yield Change (%)",
    #   limits = c(-50, 30),
    #   breaks = seq(-50, 30, by = 10)
    # ) +
    
    # 气泡大小 - 修复图例显示，使用白色填充和灰色边框
    scale_size_continuous(
      range = c(2, 10),
      name = "Change Magnitude (%)",
      breaks = c(10, 20, 30, 40, 50),
      labels = c("10", "20", "30", "40", "50"),
      guide = guide_legend(
        override.aes = list(
          fill = "white",  # 设置图例气泡的填充色为白色
          color = "grey40"   # 设置图例气泡的边框色为灰色
        )
      )
    ) +
    
    # 在气泡内部显示数值
    geom_text(aes(label = Change), 
              color = "black",  # 改为黑色，在白色背景上更清晰
              size = 2.5, 
              fontface = "bold") +
    
    # 坐标轴翻转，便于阅读
    coord_flip() +
    
    # 调整横坐标（作物类型）的间距
    scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) + # 减少横坐标扩展
    scale_y_discrete(expand = expansion(mult = c(0.1, 0.1))) +   # 减少纵坐标扩展
    
    # 专业主题 - 使用theme_classic作为基础，它包含坐标轴线和刻度线
    theme_classic(base_size = 11) +
    theme(
      # 面板和网格
      panel.background = element_rect(fill = "white", color = "grey70"),
      panel.grid.major = element_line(color = "grey92", size = 0.3),
      
      # 坐标轴线 - 确保显示
      axis.line = element_line(color = "black", size = 0.6),
      axis.ticks = element_line(color = "black", size = 0.6),
      
      # 文本样式 - 调整横坐标标签角度和位置
      axis.text.x = element_text(angle = 0, hjust = 0.5, color = "grey30", 
                                 margin = margin(t = 2, b = 2)),
      axis.text.y = element_text(color = "grey30", 
                                 margin = margin(r = 2, l = 2)),
      
      # 添加坐标轴标题
      axis.title.x = element_text(
        size = 11,
        face = "bold",
        color = "grey30",
        margin = margin(t = 8)
      ),
      axis.title.y = element_text(
        size = 11,
        face = "bold", 
        color = "grey30",
        margin = margin(r = 8)
      ),
      
      # 图例
      legend.position = "right",
      legend.box = "vertical",
      legend.key = element_rect(fill = "white"),
      legend.title = element_text(face = "bold", size = 9, color = "grey30"),
      legend.text = element_text(size = 8, color = "grey30"),
      
      # 边距和标题
      plot.margin = margin(10, 10, 10, 10),
      plot.title = element_text(
        face = "bold", 
        size = 14, 
        hjust = 0.5,
        color = "#2c3e50",
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        size = 11, 
        hjust = 0.5,
        color = "#7f8c8d",
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        size = 9,
        color = "grey50",
        hjust = 0.5,
        margin = margin(t = 10)
    )
    
  )
  
  return(p)
}

# 读取数据
crop_data <- read.csv("crop_yield_projection.csv")

# 生成气泡图 - 适应新格式
bubble_plot_wide <- create_bubble_chart_wide_format(crop_data)
print(bubble_plot_wide)

# 保存
ggsave("crop_yield_bubble.pdf", 
       plot = bubble_plot_wide,
       width = 10,
       height = 8,
       device = cairo_pdf)
