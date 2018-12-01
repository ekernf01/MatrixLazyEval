context("demo")
rm(list = ls())

library(MatrixLazyEval)
library(Matrix)
data(CAex)
M = rbind(CAex, CAex)
cs = colSums( M )
w = as.matrix( data.frame( intercept = 1, slope = 1:ncol(M) ) )



testthat::test_that("scaling example works", {
  K = 10000
  C = Matrix::Diagonal( x = K / colSums( M ) )
  M_scaled = M %*% C
  M_scaled_lazy = NewLazyMatrix( components = list("M" = M, "C" = C ),
                                 dim = dim(M),
                                 eval_rule  = " M %*% C " )
  expect_equal( dim(M_scaled),
                dim(M_scaled_lazy) )
  expect_equal( M_scaled      %*% w,
                M_scaled_lazy %*% w )
})


testthat::test_that("Shifting example works", {
  R    = matrix( nrow = nrow(M), ncol = 1, data = rowMeans( M ) )
  ONES = matrix( ncol = ncol(M), nrow = 1, data = 1 )
  M_shifted = M - R %*% ONES
  M_shifted_lazy = NewLazyMatrix( components = list("M" = M, "R" = R, "ONES" = ONES ),
                                  dim = dim(M),
                                  eval_rule  = "(LEFT %*% M %*% RIGHT) - (LEFT %*% R) %*% (ONES %*% RIGHT)" )
  expect_equal( dim(M_shifted),
                dim(M_shifted_lazy) )
  expect_equal( M_shifted      %*% w,
                M_shifted_lazy %*% w )
})

testthat::test_that("Regression example works", {
  X = as.matrix( data.frame( intercept = 1, slope = 1:nrow(M) ) )
  XtXinv = solve(t(X) %*% X) # this is just 2 by 2
  M_regressed = M - X %*% XtXinv %*% t(X) %*% M
  M_regressed_lazy = NewLazyMatrix( components = list("M" = M, "X" = X, "XtXinv" = XtXinv ),
                                    dim = dim(M),
                                    eval_rule  = "( LEFT %*% M %*% RIGHT ) - ( LEFT %*% X %*% XtXinv) %*% ( t(X) %*% M %*% RIGHT ) " )

  expect_equal( dim(M_regressed),
                dim(M_regressed_lazy) )
  expect_equal( M_regressed      %*% w,
                M_regressed_lazy %*% w )
})
