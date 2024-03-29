% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MatrixLazyEval_convenience.R
\name{CenterLazily}
\alias{CenterLazily}
\title{Center rows or columns.}
\usage{
CenterLazily(M, do_rows = T)
}
\arguments{
\item{M}{Any matrix with a multiplication operator and nrow/ncol methods defined. Usually from the Matrix package.}

\item{do_rows}{If true, the rows are centered so every row has mean 0.
If false, the columns are centered so every column has mean 0.

This method is LAZY -- calculations are put off until later, not triggered by calling this function.}
}
\description{
Center rows or columns.
}
