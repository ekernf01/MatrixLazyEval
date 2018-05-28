requireNamespace("Matrix")

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
  tt_taken =
    any( grepl( "TEMP",       eval_rule   ) ) ||
    any( grepl( "TEMP", names(components) ) )
  if( tt_taken ){
    stop("Names containing TEMP are reserved for internal use.\n")
  }
  assertthat::assert_that( !any( grepl( "TEMP", eval_rule )))
  assertthat::assert_that( !any( grepl( "TEMP", names( components ))))

  if( !identical(
    names(components),
    make.names( names( components ) )
  ) ){
    stop("names(components) must be invariant to make.names.\n")
  }

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
    HasValidRuleLazyMatrix(M)
    TestLazyMatrix(M)
  }
  return( M )
}

IsLazyMatrix = function(x) {
  (typeof(x) == "S4") &&
    (class(x) == "LazyMatrix")
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
HasValidRuleLazyMatrix = function(M){
  rule_string = paste(M@eval_rule, collapse=" ")
  assertthat::are_equal(1, length(rule_string))
  assertthat::are_equal( names(M@components),
                         make.names( names( M@components ) ) )
  assertthat::is.string(rule_string)
  operators = "t|-|\\(|\\)|\\+|\\*|%\\*%|LEFT|RIGHT" #escape everything but the transpose, minus, and percents
  # In this regex, will scan for longer names first, in case they contain any shorter names as substrings.
  o = names(M@components) %>% nchar %>% order(decreasing = T)
  component_names = paste0( names(M@components)[o], collapse = "|")
  validity_re = paste0("[", operators, "|", component_names, "|\\s]*" )
  is_alright = (trimws(gsub(validity_re, "", rule_string)) == "")
  return( is_alright)
}


#' Check matrix by computing xM, My, and xMy with random dense x, y.
#'
#' @param M Lazymatrix
#' @param seed Seed for RNG.
#'
#' @export
#'
#' @details
#' This does no more than make sure a quadratic form can be computed and that it yields a scalar as expected.
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


TransposeRule = function( eval_rule ){
  eval_rule = gsub( "LEFT", "TRANSPOSE_TEMP",     eval_rule )
  eval_rule = gsub( "RIGHT", "t(LEFT)",           eval_rule )
  eval_rule = gsub( "TRANSPOSE_TEMP", "t(RIGHT)", eval_rule )
  eval_rule = paste0( "t(",                       eval_rule , ")" )
  return(eval_rule)
}

#' Transpose lazily.
#'
#' @param M LazyMatrix
#'
#' Since \math{L M^T \times R = (R^TML^T)^T, can replace every occurence of LEFT
#'  with t(RIGHT) and vice versa, then transpose at the end.}
#' @export
#'
TransposeLazyMatrix = function( M ){
  M@dim = rev(M@dim)
  M@eval_rule = TransposeRule(M@eval_rule)
  assertthat::assert_that( !any( grepl( "TRANSPOSE_TEMP",       M@eval_rule   )))
  assertthat::assert_that( !any( grepl( "TRANSPOSE_TEMP", names(M@components) )))
  return( M )
}


#' Extend a LazyMatrix matrix M symbolically, without performing any calculations.
#'
#' @param M LazyMatrix to be extended.
#' @param LEFT @param RIGHT Lists of matrices to pre- and post-multiply.
#' Everything is done left to right, so if these contain X, Y (LEFT) and Z, W (RIGHT), the result will represent
#'  XYMZW . If these are not lists, they will be wrapped in lists and named using deparse(substitute()).
#' @export
#'
ExtendLazyMatrix = function( M, LEFT = NULL, RIGHT = NULL ){
  assertthat::assert_that( !is.null(LEFT)  | !is.null(RIGHT) )
  nm_L = deparse(substitute( LEFT  ))
  nm_R = deparse(substitute( RIGHT ))
  if(!is.list(LEFT  )){ LEFT   = list( LEFT  ); names( LEFT  ) = nm_L }
  if(!is.list(RIGHT )){ RIGHT  = list( RIGHT ); names( RIGHT ) = nm_R }

  # Check for name collisions between M, LEFT, and RIGHT.
  name_conflict_LR = intersect(names(RIGHT),        names(LEFT )) %>% paste0(collapse = " ;  ")
  name_conflict_LM = intersect(names(M@components), names(LEFT )) %>% paste0(collapse = " ;  ")
  name_conflict_MR = intersect(names(M@components), names(RIGHT)) %>% paste0(collapse = " ;  ")
  if( name_conflict_LR != "" ){
    stop(paste0( "Name collisions between LEFT and RIGHT: ", name_conflict_LR ) )
  }
  if( name_conflict_LM != "" ){
    stop(paste0( "Name collisions between LEFT and M: ", name_conflict_LM ) )
  }
  if( name_conflict_MR != "" ){
    stop(paste0( "Name collisions between M and RIGHT: ", name_conflict_MR ) )
  }

  # Add new matrices to the @components
  M@components = c(LEFT, M@components, RIGHT)

  # Format new components to replace LEFT or RIGHT in the current @eval_rule.
  # This code must work when certain inputs to the enclosing function are null.
  make_rule_extension = function( matrix_list, is_right ){
    assertthat::assert_that( is.logical( is_right ) )
    if( is.null( matrix_list ) ){
      rule_extension  = ifelse(is_right, "RIGHT", "LEFT")
    } else {
      rule_extension  = paste0( names(matrix_list), collapse = " %*% " )
    }
    if( is_right ){
      rule_extension  = paste0(              rule_extension, " %*% RIGHT" )
    } else {
      rule_extension  = paste0( "LEFT %*% ", rule_extension )
    }
    return( rule_extension )
  }

  rule_R = make_rule_extension(RIGHT, is_right = TRUE )
  rule_L = make_rule_extension(LEFT, is_right = FALSE )

  M@eval_rule = gsub( "RIGHT", rule_R, M@eval_rule )
  M@eval_rule = gsub( "LEFT",  rule_L, M@eval_rule )

  # Update dimensions to reflect new multiplicands
  if(!is.null(LEFT)){
    M@dim[1] = nrow(LEFT[[1]])
  }
  if(!is.null(RIGHT)){
    M@dim[2] = ncol(RIGHT[[length(RIGHT)]])
  }

  return(M)
}


#' Evaluate elements of a LazyMatrix.
#'
#' @param M LazyMatrix
#' @param i @param j Integer vectors.
#'
#' @export
#'
ExtractElementsLazyMatrix = function( M, i = 1:nrow(M), j = 1:ncol(M), drop = T ){
  result = EvaluateLazyMatrix( M,
                               LEFT  = Matrix::Diagonal(1, n = nrow(M))[i, ],
                               RIGHT = Matrix::Diagonal(1, n = ncol(M))[, j] )
  if(drop & min(dim(result)) == 1){
    return(as.vector(result))
  } else {
    return( result )
  }
}
