#' 计算两个向量之间的相似性
#'
#' @param Cn 一个神经元的钙成像数据向量
#' @param B 一个与行为相关的数据向量
#' @return 返回一个表示相似性的数值
#' @export

calculate_similarity <- function(Cn, B) {
  # 计算相似性
  return((2 * sum(B * Cn)) / (sum(B^2) + sum(Cn^2)))
}
