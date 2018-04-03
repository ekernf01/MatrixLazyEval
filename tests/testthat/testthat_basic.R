library(testthat)
library(MatrixLazyEval)

context("basics")

library(Matrix)
data(CAex)
M = rbind(CAex, CAex)

M_lazy = NewLazyMatrix( components = list("M" = M ),
                        dim = dim(M),
                        eval_rule  = " M ", test = F )

test_that("TestLazyMatrix works", {
  expect_silent(TestLazyMatrix(M_lazy) )
})

test_that("summary_works", {
  expect_equal(summary(M_lazy), summary(M_lazy))
})

test_that("subsetting_works", {
  expect_equal(M_lazy[1:2, 1:2], M[1:2, 1:2])
  expect_equal(M_lazy[1:2,    ], M[1:2,    ])
  expect_equal(M_lazy[   , 1:2], M[   , 1:2])
  expect_equal(M_lazy[   , ], M[   , ])
})


test_that("transpose_works", {
  expect_equal(t(M_lazy), t(M_lazy))
})

test_that("dim, nrow, ncol works", {
  expect_equal(ncol(M_lazy), ncol(M))
  expect_equal(nrow(M_lazy), nrow(M))
  expect_equal(dim(M_lazy), dim(M))
})
