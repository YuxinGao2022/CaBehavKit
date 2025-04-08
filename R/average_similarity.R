#' 计算每个神经元类型的平均相似性
#'
#' 该函数根据每个神经元的相似性计算每个类型（ON、OFF、其他）的平均相似性。
#'
#' @param neuron_data 数据框，包含神经元的类型、索引和相似性。
#'
#' @return 返回一个数据框，包含每个神经元类型及其平均相似性。
#' @export
average_similarity <- function(neuron_data) {
  average_similarity <- neuron_data %>%
    mutate(Similarity = abs(Similarity)) %>%  # 取绝对值
    group_by(Type) %>%
    summarize(AverageSimilarity = mean(Similarity))

  return(average_similarity)
}
