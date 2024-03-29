% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MatrixLazyEval_core.R
\name{EvaluateLazyMatrix}
\alias{EvaluateLazyMatrix}
\title{Compute a matrix product.}
\usage{
EvaluateLazyMatrix(
  M,
  LEFT = Matrix::Diagonal(1, n = nrow(M)),
  RIGHT = Matrix::Diagonal(1, n = ncol(M))
)
}
\arguments{
\item{M}{object of class LazyMatrix.}

\item{LEFT}{x in xMy.

This method is EAGER -- it triggers calculations immediately.}

\item{RIGHT}{y in xMy.}
}
\description{
Compute a matrix product.
}
