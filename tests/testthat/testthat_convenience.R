rm(list = ls())

context("convenience")

library(Matrix)
library(MatrixLazyEval)

data(CAex)
M = rbind(CAex, CAex)
M = matrix(stats::rnorm(prod(dim(M))), nrow = nrow(M))

M_lazy = NewLazyMatrix( components = list("M" = M ),
                        dim = dim(M),
                        eval_rule  = " M ", test = F )

testthat::test_that("RankOneUpdateLazily", {
  L = as.matrix( data.frame( intercept = 1, slope = 1:nrow(M) ) )
  R = as.matrix( data.frame( intercept = 1, slope = 1:ncol(M) ) ) %>% t
  M_updated = M -  L %*% R
  M_updated_lazy = RankOneUpdateLazily( M, L, R )
  x = stats::rnorm( ncol( M ) )
  expect_lt(abs(max(M_updated %*% x - M_updated_lazy %*% x)), 1e-10)

  L = 1:nrow(M)
  R = 1:ncol(M)
  M_updated = M -  L %o% R
  M_updated_lazy = RankOneUpdateLazily( M, L, R )
  x = stats::rnorm( ncol( M ) )
  expect_lt(abs(max(M_updated %*% x - M_updated_lazy %*% x)), 1e-10)
} )


testthat::test_that("RegressOutLazily", {
  X = as.matrix( data.frame( intercept = 1, slope = 1:nrow(M) ) )
  XtXinv = solve(t(X) %*% X) # this is just 2 by 2
  M_regressed = M - X %*% XtXinv %*% t(X) %*% M
  M_regressed_lazy = RegressOutLazily( M, X )
  x = stats::rnorm( ncol( M ) )
  expect_lt(abs(max(M_regressed %*% x - M_regressed_lazy %*% x)), 1e-10)
} )


testthat::test_that("CenterLazily", {
  M_regressed = sweep( M , MARGIN = 1, rowMeans(M), "-" )
  M_regressed_lazy = CenterLazily( M )
  x = stats::rnorm( ncol( M ) )
  expect_lt(abs(max(M_regressed %*% x - M_regressed_lazy %*% x)), 1e-10)
} )


testthat::test_that("ScaleLazily", {
  M_regressed = sweep( M , MARGIN = 1, rowSums(M), "/" )
  M_regressed_lazy = ScaleLazily( M )
  x = stats::rnorm( ncol( M ) )
  expect_lt(abs(max(M_regressed %*% x - M_regressed_lazy %*% x)), 1e-10)
} )


testthat::test_that("SVDLazyMatrix", {
  svd_regular = irlba::irlba(M)
  svd_lazy = RandomSVDLazyMatrix(M_lazy)
  for( ii in 1:5){
    cu = cor(svd_regular$u[,ii],
             svd_lazy$u[,ii])
    cv = cor(svd_regular$v[,ii],
             svd_lazy$v[,ii])
    expect_gt( abs(cu), 0.9)
    expect_gt( abs(cv), 0.9)
  }
} )

