#' MatrixLazyEval class
#'
#' @slot components Named list containing matrices. Anything with a matrix multiplication operator ought to work.
#' Notably, you can put another instance of LazyMatrixEval.
#' @slot dim Dimensions of the matrix represented here.
#' @slot eval_rule Formula describing how to compute Mx or yM for this matrix (M) given x or y.
#'
#' @details eval_rule may only contain `LEFT`, `RIGHT`, names in `components`, and simple operations (`()`, `-*+`, `%*%`).
#'  `LEFT` and `RIGHT` specify the interface with the outside world: if your object is M and you perform M %*% x,
#'  then the value of x is substituted into RIGHT and LEFT is ignored.
#'  Like with R syntax, asterisk (`*`) means componentwise multiplication, and wrapped in percents
#'   (`%*%`) it means matrix multiplication.
#'



#' Does it conform to the rules?
#'
#' @param M object of class MatrixLazyEval.
IsValidLazyMatrix = function(M){
  rule_string = paste(M@eval_rule, collapse=" ")
  assertthat::are_equal(1, length(rule_string))
  assertthat::is.string(rule_string)
  operators = "-|\\(|\\)|\\+|\\*|%\\*%|LEFT|RIGHT" #escape everything but the minus and percents
  valid_names = paste0( names(components), collapse = "|")
  validity_re = paste0("[", operators, "|", valid_names, "|\\s]*" )
  is_alright = (gsub(validity_re, rule_string) == "")
  return( is_alright)
}


#' Compute a matrix product.
#'
#' @param M object of class MatrixLazyEval.
#' @param RIGHT y in xMy.
#' @param LEFT x in xMy.
#'
EvaluateLazyMatrix = function( M,
                     RIGHT = diag(1, nrow = ncol(M), ncol = ncol(M)),
                     LEFT  = diag(1, nrow = nrow(M), ncol = nrow(M))){
  assertthat::assert_that(isValid(M))
  M@components$LEFT = LEFT
  M@components$RIGHT = RIGHT
  eval(parse(text=M@eval_rule), envir = M@components)
}

#' Check matrix by computing xM, My, and xMy with random dense x, y.
#'
TestLazyMatrix = function( M, seed = 0 ){
  set.seed(seed)
  y = rnorm(nrow(M))
  x = rnorm(ncol(M))
  M %*% y
  x %*% M
  EvaluateLazyMatrix( M, x, y )
  return()
}


