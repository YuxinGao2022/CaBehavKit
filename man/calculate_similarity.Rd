% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_similarity.R, R/function.R
\name{calculate_similarity}
\alias{calculate_similarity}
\title{计算两个向量之间的相似性}
\usage{
calculate_similarity(Cn, B)

calculate_similarity(Cn, B)
}
\arguments{
\item{Cn}{数值向量，表示一个神经元的活动。}

\item{B}{数值向量，表示另一个神经元的活动。}
}
\value{
返回一个表示相似性的数值

返回一个数值，表示两个神经元活动的相似性。
}
\description{
该函数计算两个神经元活动的相似性，基于余弦相似度的公式。
}
