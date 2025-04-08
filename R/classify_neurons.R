#' 生成神经元类型索引与相似性数据
#'
#' 根据神经元的钙成像数据和行为数据，生成神经元类型索引及其相似性。
#'
#' @param trace_mat 神经元的钙成像数据矩阵
#' @param result_table 行为数据的表格
#' @return 返回包含神经元类型、索引和相似性的数据框
#' @export
# classify_neurons.R
classify_neurons <- function(trace_mat, result_table) {
  # 神经元分类
  neuron_data <- data.frame(Type = character(), Index = numeric(), Similarity = numeric())

  for (i in 1:nrow(trace_mat)) {
    neuron_type <- identify_neurons(trace_mat[i,], result_table$resultTable)
    actual_similarity <- calculate_similarity(trace_mat[i,], result_table$resultTable)
    neuron_data <- rbind(neuron_data, data.frame(Type = neuron_type, Index = i, Similarity = actual_similarity))
  }

  # 返回神经元分类数据
  return(neuron_data)
}
