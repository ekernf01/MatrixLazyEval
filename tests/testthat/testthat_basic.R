
context("basics")

library(Matrix)
data(CAex)
M = rbind(CAex, CAex)

M_lazy = NewLazyMatrix( components = list("M" = M ),
                        dim = dim(M),
                        eval_rule  = "M", test = F )


testthat::test_that("AsLazyMatrix works", {
  expect_silent( AsLazyMatrix(M) )
})

M_lazy_layered = NewLazyMatrix( components = list("M" = M_lazy ),
                                dim = dim(M),
                                eval_rule  = "M", test = F )


test_that("TestLazyMatrix works", {
  expect_silent(TestLazyMatrix(M_lazy) )
})

test_that("summary_works", {
  expect_equal(summary(M_lazy), summary(M_lazy))
})

test_that("subsetting_works", {
  expect_equal(M_lazy[1:2, 1:2], M[1:2, 1:2] )
  expect_equal(M_lazy[1:2,    ], M[1:2,    ] )
  expect_equal(M_lazy[   , 1:2], M[   , 1:2] )
  expect_equal(M_lazy[   , ],    M[   ,    ] )
  expect_equal(M_lazy[1:5, 1:5], M[1:5, 1:5] )
})

test_that("dim, nrow, ncol works", {
  expect_equal(ncol(M_lazy), ncol(M))
  expect_equal(nrow(M_lazy), nrow(M))
  expect_equal(dim(M_lazy), dim(M))
})


test_that("evaluate_works", {
  for(ii in 1:2){
    x = rnorm(dim(M_lazy)[1]*ii) %>% matrix(ncol = ii) %>% t
    y = rnorm(dim(M_lazy)[2]*ii) %>% matrix(ncol = ii)
    expected = x %*% M %*% y
    expect_equal( EvaluateLazyMatrix(M_lazy, x, y), expected )
    expect_equal( (x %*% M_lazy) %*% y, expected )
    expect_equal( x %*% (M_lazy %*% y), expected )
  }
})


test_that("evaluate_works_when_layered", {
  for(ii in 1:2){
    x = rnorm(dim(M_lazy)[1]*ii) %>% matrix(ncol = ii) %>% t
    y = rnorm(dim(M_lazy)[2]*ii) %>% matrix(ncol = ii)
    expected = x %*% M %*% y
    expect_equal( EvaluateLazyMatrix(M_lazy_layered, x, y), expected )
    expect_equal( (x %*% M_lazy_layered) %*% y, expected )
    expect_equal( x %*% (M_lazy_layered %*% y), expected )
  }
})


testthat::test_that("transpose method exists", {
  transpose_methods =
    capture_output(showMethods("t")) %>%
    strsplit("\nx=") %>%
    unlist %>%
    gsub("[^[:alnum:]=\\.]", "", .)
  expect_true( "LazyMatrix" %in% transpose_methods )
} )


test_that("transpose_runs", {
  expect_equal(t(M_lazy), t(M_lazy))
})

test_that("transpose_works", {
  expect_equal( t(M_lazy)[1:5, 1:5], t(M)[1:5, 1:5] )
  for(ii in 1:2){
    x = rnorm(dim(M_lazy)[1]*ii) %>% matrix(ncol = ii) %>% t
    y = rnorm(dim(M_lazy)[2]*ii) %>% matrix(ncol = ii)
    expected = x %*% M %*% y
    expect_equal(   x  %*%   M_lazy  %*% y,    expected )
    expect_equal( t(y) %*% t(M_lazy) %*% t(x), t( expected ) )
  }
})

test_that("tcrossprod_works", {
  mtm       = t(M_lazy) %*% M_lazy
  mtm_other = tcrossprod(M_lazy)
  for(ii in 1:2){
    x = rnorm(dim(M_lazy)[2]*ii) %>% matrix(ncol = ii) %>% t
    y = rnorm(dim(M_lazy)[2]*ii) %>% matrix(ncol = ii)
    baseline = x %*% t(M) %*% M %*% y
    expect_equal( baseline, x %*% mtm       %*% y )
    expect_equal( baseline, x %*% mtm_other %*% y )
  }
})


test_that("extend_works", {
  for(ii in 1:2){
    w = rnorm(dim(M_lazy)[1]*ii) %>% matrix(ncol = ii) %>% t
    z = rnorm(dim(M_lazy)[2]*ii) %>% matrix(ncol = ii)
    w2 = rnorm(dim(M_lazy)[1]*ii) %>% matrix(ncol = ii) %>% t
    z2 = rnorm(dim(M_lazy)[2]*ii) %>% matrix(ncol = ii)
    wMz = ExtendLazyMatrix( M_lazy, LEFT = w, RIGHT = z )
    simple_prod = t(w2) %*% w %*% M_lazy %*% z %*% t(z2)
    extend_prod = t(w2) %*%       wMz          %*% t(z2)
    expect_equal( simple_prod, extend_prod )
  }
})

