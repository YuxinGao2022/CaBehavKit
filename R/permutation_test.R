#' 随机重排置换测试
#'
#' 进行随机重排的置换测试，用于计算相似性分布。
#'
#' @param Cn 一个神经元的钙成像数据向量
#' @param B 一个与行为相关的数据向量
#' @param iterations 置换测试的迭代次数，默认为 10000
#' @return 返回一个包含随机相似性值的向量
#' @export
permutation_test <- function(Cn, B, iterations = 10000) {
  random_similarity <- numeric(iterations)

  for (i in 1:iterations) {
    shuffled_B <- sample(B)
    random_similarity[i] <- calculate_similarity(Cn, shuffled_B)
  }

  return(random_similarity)
}
