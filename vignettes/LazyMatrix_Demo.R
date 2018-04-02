## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(Matrix)
data(CAex)
M = CAex
dim(M)
class(M)
sum( M > 0 )
library(MatrixLazyEval)

