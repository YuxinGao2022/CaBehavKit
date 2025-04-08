#' 绘制时间轴上的均值趋势图（透明背景）
#'
#' @param file_path CSV 文件路径，包含 ON、OFF、Other 神经元均值
#' @return 透明背景的 ggplot2 图形对象
#' @export
plot_mean_values <- function(file_path) {
  # 加载必要的包
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("请先安装 ggplot2 包：install.packages('ggplot2')")
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("请先安装 readxl 包：install.packages('readxl')")
  }

  library(ggplot2)
  library(readxl)

  # 读取数据
  average_data <- read.csv(file_path, header = TRUE)

  # 计算缩放因子
  scale_factor <- max(max(average_data$ON_mean), max(average_data$OFF_mean), max(average_data$Other_mean))
  average_data$resultTable <- average_data$resultTable * scale_factor

  # 生成绘图
  plot <- ggplot(average_data, aes(x = 1:nrow(average_data))) +
    geom_col(aes(y = resultTable), fill = "grey", alpha = 0.5, position = "dodge") +
    geom_line(aes(y = ON_mean, group = 1), color = "red", size = 0.5, alpha = 0.7) +
    geom_line(aes(y = OFF_mean, group = 1), color = "blue", size = 0.5, alpha = 0.7) +
    geom_line(aes(y = Other_mean, group = 1), color = "grey", size = 0.5, alpha = 0.7) +

    labs(title = "Mean Values Over Time", x = "Time", y = "Values") +

    theme_void() +  # 无坐标轴
    theme(
      panel.background = element_rect(fill = "transparent", color = NA), # 透明背景
      plot.background = element_rect(fill = "transparent", color = NA)
    ) +
    coord_fixed(ratio = 1500)  # 调整横纵比例

  return(plot)
}
