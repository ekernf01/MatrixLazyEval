% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MatrixLazyEval_convenience.R
\name{RandomSVDLazyMatrix}
\alias{RandomSVDLazyMatrix}
\title{Compute randomized SVD.}
\usage{
RandomSVDLazyMatrix(
  M,
  ncomp = 5,
  n_iter_spectrum_flatten = 2,
  nproj_left = min(8 * ncomp, dim(M)[1]),
  nproj_right = min(8 * ncomp, dim(M)[2])
)
}
\arguments{
\item{M}{object of class LazyMatrix.}

\item{ncomp}{number of components to compute}

\item{n_iter_spectrum_flatten}{How many times to multiply by \eqn{M^TM} to emphasize the larger
singular values when forming the projectors. If you don't know what this is, don't change it.}

\item{nproj_left}{dimension of random column space to project into}

\item{nproj_right}{dimension of random row    space to project into}
}
\description{
Compute randomized SVD.
}
