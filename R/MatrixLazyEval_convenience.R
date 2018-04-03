
#' LazyMatrix class.
#'
#' @slot components Named list containing matrices. Anything with a matrix multiplication operator ought to work.
#' Notably, you can put another instance of LazyMatrixEval.
#' @slot dim Dimensions of the matrix represented here.
#' @slot eval_rule Length-1 character describing how to compute Mx or yM for this matrix (M) given x or y.
#'
#' @details eval_rule may only contain `LEFT`, `RIGHT`, names in `components`, and simple arithmetic
#' operations (`()`, `-*+`, `%*%`).
#'  `LEFT` and `RIGHT` specify the interface with the outside world: if your object is M and you perform M %*% x,
#'  then the value of x is substituted into RIGHT (and an identity matrix for LEFT).
#'  Like with typical R syntax, asterisk (`*`) means componentwise multiplication, and wrapped in percents
#'   (`%*%`) it means matrix multiplication.
#'
setClass("LazyMatrix", representation(components    = "list",
                                      dim           = "integer",
                                      eval_rule     = "character" ))





# Code template
# setMethod( "func_name", signature(x = "LazyMatrix"), function(M) coolStuff(M) )


setMethod( "dim",  signature(x = "LazyMatrix"), function(x) x@dim )
setMethod( "nrow", signature(x = "LazyMatrix"), function(x) dim(x)[1] )
setMethod( "ncol", signature(x = "LazyMatrix"), function(x) dim(x)[2] )
dim_or_zero = function(X) {
  if(is.null(dim(X))){return(c(0,0))}
  return(dim(X))
}
setMethod( "summary", signature(object = "LazyMatrix"), function(object) t(sapply(object@components, dim_or_zero)))

setMethod( "rowSums", signature(x = "LazyMatrix"), function( x ) x %*% rep( 1, ncol( x ) )       )
setMethod( "colSums", signature(x = "LazyMatrix"), function( x )       rep( 1, nrow( x ) ) %*% x )

setMethod( "rowMeans", signature(x = "LazyMatrix"), function( x ) rowSums( x ) / ncol( x ) )
setMethod( "colMeans", signature(x = "LazyMatrix"), function( x ) colSums( x ) / nrow( x ) )

setMethod("%*%", signature(x = "LazyMatrix", y = "ANY"       ), function(x, y) EvaluateLazyMatrix( x, RIGHT = y ) )
setMethod("%*%", signature(x = "ANY",        y = "LazyMatrix"), function(x, y) EvaluateLazyMatrix( y, LEFT = x ) )
setMethod("%*%", signature(x = "LazyMatrix", y = "LazyMatrix"), function(x, y) EvaluateLazyMatrix( x, RIGHT = EvaluateLazyMatrix( y ) ) )

setMethod("t",   signature(x = "LazyMatrix"), function( x ) TransposeLazyMatrix( x ) )


idx_types = c("integer", "logical")
for( i_type in idx_types){
  for( j_type in idx_types){
    ## select both
    setMethod("[", signature(x = "LazyMatrix",
                             i = i_type, j = j_type, drop = "ANY"),
              function (x, i = 1:nrow(x), j = 1:ncol(x), ..., drop) {
                ExtractElementsLazyMatrix( x, i, j, ...,  drop )
              })
    ## select rows
    setMethod("[", signature(x = "LazyMatrix", i = i_type, j = "missing",
                             drop = "ANY"),
              function(x,i,j, ..., drop=TRUE) {
                ExtractElementsLazyMatrix( x, i = i,  drop = drop )
              })
    ## select rows
    setMethod("[", signature(x = "LazyMatrix", i = "missing", j = j_type,
                             drop = "ANY"),
              function(x,i,j, ..., drop=TRUE) {
                ExtractElementsLazyMatrix( x, j = j,  drop = drop )
              })
    ## select neither
    setMethod("[", signature(x = "LazyMatrix", i = "missing", j = "missing",
                             drop = "ANY"),
              function(x,i,j, ..., drop=TRUE) {
                ExtractElementsLazyMatrix( x,  drop = drop )
              })

  }
}

## Send message if any of (i,j,drop) is garbage
setMethod("[", signature(x = "LazyMatrix", i = "ANY", j = "ANY", drop = "ANY"),
          function(x,i,j, ..., drop)
            stop("invalid or not-yet-implemented 'LazyMatrix' subsetting"))

