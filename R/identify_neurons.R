#' 神经元类型标识
#'
#' 根据实际相似性与随机相似性的比较，标识神经元类型为 "ON神经元"、"OFF神经元" 或 "其他神经元"。
#'
#' @param Cn 一个神经元的钙成像数据向量
#' @param B 一个与行为相关的数据向量
#' @return 返回一个神经元类型的字符串（"ON神经元"、"OFF神经元" 或 "其他神经元"）
#' @export
# identify_neurons.R
identify_neurons <- function(Cn, B) {
  # 神经元类型标识
  actual_similarity <- calculate_similarity(Cn, B)
  random_similarity <- permutation_test(Cn, B)

  quantiles <- quantile(random_similarity, c(0.0083, 0.9917))

  if (actual_similarity > quantiles[2]) {
    return("ON神经元")
  } else if (actual_similarity < quantiles[1]) {
    return("OFF神经元")
  } else {
    return("其他神经元")
  }
}
