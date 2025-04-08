#' 计算神经元相似性
#'
#' 该函数计算两个神经元活动的相似性，基于余弦相似度的公式。
#'
#' @param Cn 数值向量，表示一个神经元的活动。
#' @param B 数值向量，表示另一个神经元的活动。
#'
#' @return 返回一个数值，表示两个神经元活动的相似性。
#' @export
calculate_similarity <- function(Cn, B) {
  return((2 * sum(B * Cn)) / (sum(B^2) + sum(Cn^2)))
}


#' 随机重排置换测试
#'
#' 该函数执行随机重排的置换测试，评估两个神经元活动的相似性是否显著。
#'
#' @param Cn 数值向量，表示一个神经元的活动。
#' @param B 数值向量，表示另一个神经元的活动。
#' @param iterations 整数，表示置换测试的重复次数，默认值为 10000 次。
#'
#' @return 返回一个数值向量，表示每次置换后的相似性。
#' @export
permutation_test <- function(Cn, B, iterations = 10000) {
  random_similarity <- numeric(iterations)

  for (i in 1:iterations) {
    shuffled_B <- sample(B)
    random_similarity[i] <- calculate_similarity(Cn, shuffled_B)
  }

  return(random_similarity)
}


#' 神经元类型识别
#'
#' 该函数根据神经元活动与随机重排的置换测试结果，识别神经元类型（ON、OFF 或 其他）。
#'
#' @param Cn 数值向量，表示一个神经元的活动。
#' @param B 数值向量，表示另一个神经元的活动。
#'
#' @return 返回一个字符型变量，表示神经元的类型（"ON神经元"、"OFF神经元" 或 "其他神经元"）。
#' @export
identify_neurons <- function(Cn, B) {
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


#' 神经元分类与相似性计算
#'
#' 该函数通过对每个神经元的活动数据进行分析，自动分类神经元类型，并计算它们的相似性。分类结果及相似性被存储在数据框中。
#'
#' @param trace_mat 数值矩阵，每行表示一个神经元的活动。
#' @param result_table 数据框，包含其他与神经元相关的表格数据（例如行为数据）。
#'
#' @return 返回一个包含神经元类型、索引和相似性的完整数据框。
#' @export
neuron_classification <- function(trace_mat, result_table) {
  neuron_data <- data.frame(Type = character(), Index = numeric(), Similarity = numeric())

  # 遍历每个神经元进行分类
  for (i in 1:nrow(trace_mat)) {
    neuron_type <- identify_neurons(trace_mat[i,], result_table$resultTable)
    actual_similarity <- calculate_similarity(trace_mat[i,], result_table$resultTable)
    neuron_data <- rbind(neuron_data, data.frame(Type = neuron_type, Index = i, Similarity = actual_similarity))
  }

  # 返回数据框
  return(neuron_data)
}


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


#' 按照相似性排序神经元
#'
#' 该函数根据神经元的相似性对不同类型的神经元进行排序。
#'
#' @param neuron_data 数据框，包含神经元的类型、索引和相似性。
#'
#' @return 返回一个包含按相似性排序后的神经元数据框。
#' @export
sort_neurons_by_similarity <- function(neuron_data) {
  ON_neurons <- neuron_data[neuron_data$Type == "ON神经元", ]
  OFF_neurons <- neuron_data[neuron_data$Type == "OFF神经元", ]
  Other_neurons <- neuron_data[neuron_data$Type == "其他神经元", ]

  # 按照相似性大小排序
  ON_neurons <- ON_neurons[order(-ON_neurons$Similarity), ]
  OFF_neurons <- OFF_neurons[order(-OFF_neurons$Similarity), ]
  Other_neurons <- Other_neurons[order(-Other_neurons$Similarity), ]

  # 返回排序后的数据框
  return(list(ON_neurons = ON_neurons, OFF_neurons = OFF_neurons, Other_neurons = Other_neurons))
}
