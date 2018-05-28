rm(list = ls())

context("pca")

library(Matrix)
data(CAex)
M = rbind(CAex, CAex)

M_lazy = NewLazyMatrix( components = list("M" = M ),
                        dim = dim(M),
                        eval_rule  = " M ", test = F )

test_that("TestLazyMatrix works", {
  expect_silent(TestLazyMatrix(M_lazy) )
})


