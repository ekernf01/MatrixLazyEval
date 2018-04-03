#' Return an empty LazyMatrix.
#'
#' @param components Named list containing matrices. Anything with a matrix multiplication operator ought to work.
#' Notably, you can put another instance of LazyMatrixEval.
#' Names should come out unscathed when you do make.names to them, and they may not include `t` (reserved for the transpose).
#' @param dim Length-2 integer vector giving dimensions of final object.
#' @param eval_rule Length-1 character describing how to compute Mx or yM for this matrix (M) given x or y.
#' @param test Logical. If TRUE (default), object is tested immediately upon initialization.
#'
#' @details eval_rule may only contain `LEFT`, `RIGHT`, names in `components`, and simple arithmetic
#' operations (`()`, `t`, `-*+`, `%*%`).
#'  `LEFT` and `RIGHT` specify the interface with the outside world: if your object is M and you perform M %*% x,
#'  then the value of x is substituted into RIGHT (and an identity matrix for LEFT).
#'  Like with typical R syntax, asterisk (`*`) means componentwise multiplication, and wrapped in percents
#'   (`%*%`) it means matrix multiplication.
#'
#' @export
#'
NewLazyMatrix = function( components, dim, eval_rule, test = T ){

  # Allow the user to omit LEFT and RIGHT from the evaluation rule.
  if( !grepl("RIGHT", eval_rule)){
    eval_rule = paste0( "(", eval_rule,  ") %*% RIGHT" )
  }

  if( !grepl("LEFT", eval_rule)){
    eval_rule = paste0( "LEFT %*% (", eval_rule, ")" )
  }

  # Make, test, return
  M = new("LazyMatrix")
  M@components = components
  M@dim = dim
  M@eval_rule = eval_rule

  if( test ){
    IsValidRuleLazyMatrix(M)
    TestLazyMatrix(M)
  }
  return( M )
}


#' Compute a matrix product.
#'
#' @param M object of class LazyMatrix.
#' @param RIGHT y in xMy.
#' @param LEFT x in xMy.
#'
#' @export
#'
EvaluateLazyMatrix = function( M,
                               LEFT  = Matrix::Diagonal(1, n = nrow(M) ),
                               RIGHT  = Matrix::Diagonal(1, n = ncol(M) ) ){
  M@components$LEFT = LEFT
  M@components$RIGHT = RIGHT
  eval(parse(text=M@eval_rule), envir = M@components)
}


#' Does it conform to the rules?
#'
#' @param M object of class LazyMatrix.
#'
#' @export
#'
IsValidRuleLazyMatrix = function(M){
  rule_string = paste(M@eval_rule, collapse=" ")
  assertthat::are_equal(1, length(rule_string))
  assertthat::are_equal( names(M@components),
                         make.names( names( M@components ) ) )
  assertthat::is.string(rule_string)
  operators = "t|-|\\(|\\)|\\+|\\*|%\\*%|LEFT|RIGHT" #escape everything but the transpose, minus, and percents
  component_names = paste0( names(M@components), collapse = "|")
  validity_re = paste0("[", operators, "|", component_names, "|\\s]*" )
  is_alright = (gsub(validity_re, "", rule_string) == "")
  return( is_alright)
}


#' Check matrix by computing xM, My, and xMy with random dense x, y.
#'
#' @param M Lazymatrix
#' @param seed Seed for RNG.
#'
#' @export
#'
TestLazyMatrix = function( M, seed = 0 ){
  set.seed(seed)
  y = rnorm(ncol(M))
  x = rnorm(nrow(M))
  testthat::expect_equal( dim(M %*% y)[1], dim(M)[1] )
  testthat::expect_equal( dim(x %*% M)[2], dim(M)[2] )
  testthat::expect_equal( length(c(EvaluateLazyMatrix( M, LEFT = x, RIGHT = y ))), 1 )
  return()
}

#' Transpose lazily.
#'
#' @param M LazyMatrix
#'
TransposeLazyMatrix = function( M ){
  assertthat::assert_that( !any( grepl( "TRANSPOSE_TEMP",       M@eval_rule   )))
  assertthat::assert_that( !any( grepl( "TRANSPOSE_TEMP", names(M@components) )))
  M@eval_rule = gsub( "LEFT", "TRANSPOSE_TEMP",  M@eval_rule )
  M@eval_rule = gsub( "RIGHT", "LEFT",           M@eval_rule )
  M@eval_rule = gsub( "TRANSPOSE_TEMP", "RIGHT", M@eval_rule )
  M@eval_rule = paste0( "t(", M@eval_rule , ")")
  return( M )
}

#' Extend a LazyMatrix matrix M symbolically, without performing any calculations.
#'
#' @param M LazyMatrix to be extended.
#' @param LEFT @param RIGHT Lists of matrices to pre- and post-multiply.
#' Everything is done left to right, so if these contain X, Y (LEFT) and Z, W (RIGHT), the result will represent
#'  XYMZW .
#' @export
#'
ExtendLazyMatrix = function( M, LEFT = NULL, RIGHT = NULL ){
  assertthat::assert_that( !is.null(LEFT)  | !is.null(RIGHT) )

  # Check for name collisions between M, LEFT, and RIGHT.
  name_conflict_LR = intersect(names(LEFT), names(RIGHT)) %>% paste0(collapse = "   ")
  name_conflict_LM = intersect(names(M@components), names(LEFT)) %>% paste0(collapse = "   ")
  name_conflict_MR = intersect(names(M@components), names(RIGHT)) %>% paste0(collapse = "   ")
  if( length(name_conflict_LR) > 0 ){
    stop(paste0( "Name collisions between LEFT and RIGHT: ", name_conflict_LR ) )
  }
  if( length(name_conflict_LM) > 0 ){
    stop(paste0( "Name collisions between LEFT and M: ", name_conflict_LM ) )
  }
  if( length(name_conflict_MR) > 0 ){
    stop(paste0( "Name collisions between M and RIGHT: ", name_conflict_MR ) )
  }

  # Add new matrices to the @components
  M@components = c(LEFT, M@components, RIGHT)

  # Add new components to the @eval_rule.
  # Note that this code must work when LEFT or RIGHT is null.
  make_rule_extension = function( matrix_list, L_or_R ){
    assertthat::assert_that( (L_or_R == "LEFT") || (L_or_R == "RIGHT") )
    if( is.null( matrix_list ) ){
      eval_rule  = L_or_R
    } else {
      eval_rule  = paste0( names(matrix_list), collapse = " %*% " )
    }
    if( L_or_R == "RIGHT" ){
      eval_rule  = paste0(              eval_rule, " %*% RIGHT" )
    } else if ( L_or_R == "LEFT") {
      eval_rule  = paste0( "LEFT %*% ", eval_rule )
    } else {
      stop("Uh-oh! Kaboom! Shouldn't reach here! It's matrices, not tensors! We only have left and right! \n")
    }
    return( eval_rule )
  }

  rule_R = make_rule_extension(RIGHT, type = "RIGHT")
  rule_L = make_rule_extension(LEFT,  type = "LEFT")

  M@eval_rule = gsub( "RIGHT", rule_R, M@eval_rule )
  M@eval_rule = gsub( "LEFT",  rule_L, M@eval_rule )

  return(M)
}



ExtractElementsLazyMatrix = function( M, i = 1:nrow(M), j = 1:ncol(M), drop = T ){
  result = EvaluateLazyMatrix( M,
                               LEFT  = Matrix::Diagonal(1, n = nrow(M))[i, ],
                               RIGHT = Matrix::Diagonal(1, n = ncol(M))[, j] )
  if(drop & min(dim(result)) == 1){
    return(c(result))
  } else {
    return( result )
  }
}
