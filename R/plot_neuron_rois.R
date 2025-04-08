#' 绘制分类神经元的 ROI 位置
#'
#' @param classification_file CSV 文件路径，包含神经元索引和分类标签
#' @param roi_folder_path ROI 文件所在的文件夹路径
#' @param fill 是否填充 ROI，默认为 FALSE（空心），如果为 TRUE，则绘制实心
#' @return 绘制神经元 ROI 图
#' @export
plot_neuron_rois <- function(classification_file, roi_folder_path, fill = FALSE) {
  # 加载所需库
  if (!requireNamespace("RImageJROI", quietly = TRUE)) {
    stop("请先安装 RImageJROI 包：install.packages('RImageJROI')")
  }
  library(RImageJROI)

  # 读取分类结果
  classification_data <- read.csv(classification_file, header = FALSE, stringsAsFactors = FALSE)
  neuron_indices <- classification_data[, 2]  # 神经元索引
  classification_labels <- classification_data[, 1]  # 分类标签

  # 颜色映射
  color_map <- c("OFF neuron" = "blue", "ON neuron" = "red", "Other neuron" = "grey")

  # 创建空白绘图窗口
  plot(NULL, xlim = c(0, 400), ylim = c(0, 400), xlab = 'X坐标', ylab = 'Y坐标', main = '分类后的神经元细胞 ROI')

  # 读取 ROI 文件夹
  roi_files <- list.files(roi_folder_path, pattern = "\\.roi$", full.names = TRUE)

  for (file_name in roi_files) {
    roi_index_str <- tools::file_path_sans_ext(basename(file_name))
    roi_index <- as.numeric(roi_index_str)

    # 匹配神经元分类
    label_idx <- which(neuron_indices == roi_index)
    if (length(label_idx) == 0) next
    classification_label <- classification_labels[label_idx]

    # 读取 ROI
    roi_data <- tryCatch({
      read.ijroi(file_name)
    }, error = function(e) {
      message(paste("读取文件失败:", file_name, "-", e$message))
      return(NULL)
    })

    if (!is.null(roi_data)) {
      color <- color_map[classification_label]
      if (fill) {
        polygon(roi_data$coords[,1], roi_data$coords[,2], col = color, border = NA)  # 填充多边形
      } else {
        lines(roi_data$coords[,1], roi_data$coords[,2], col = color)  # 仅绘制轮廓
      }
    }
  }
}
