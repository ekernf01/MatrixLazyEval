
#' LazyMatrix class.
#'
#' @slot components Named list containing matrices. Anything with a matrix multiplication operator ought to work.
#' Notably, you can put another instance of LazyMatrixEval.
#' @slot dim Dimensions of the matrix represented here.
#' @slot eval_rule Length-1 character describing how to compute Mx or yM for this matrix (M) given x or y.
#'
#' @details eval_rule may only contain LEFT, RIGHT, names in \code{components}, and simple arithmetic
#' operations \code{(), -*+, \%*\% }.
#'  LEFT and RIGHT specify the interface with the outside world: if your object is M and you perform \code{M \%*\% x},
#'  then the value of x is substituted into RIGHT (and an identity matrix for LEFT).
#'  Like with typical R syntax, asterisk (*) means componentwise multiplication, and wrapped in percents
#'   (\code{\%*\%}) it means matrix multiplication.
#'
#'
setClass("LazyMatrix", representation(components    = "list",
                                      dim           = "integer",
                                      eval_rule     = "character" ))





# Code template
# setMethod( "func_name", signature(x = "LazyMatrix"), function(M) coolStuff(M) )

#' What size is my matrix?
#'
setMethod( "dim",  signature(x = "LazyMatrix"), function(x) x@dim )

#' How tall is my matrix?
#'
setMethod( "nrow", signature(x = "LazyMatrix"), function(x) dim(x)[1] )

#' How wide is my matrix?
#'
setMethod( "ncol", signature(x = "LazyMatrix"), function(x) dim(x)[2] )

#' Show a few details on a LazyMatrix.
#'
setMethod( "summary", signature(object = "LazyMatrix"), function(object) {
  cat("\nComponent dimensions:\n")
  do_one = function(X) {tryCatch(dim(X), error = function(e) {c(0,0)})}
  print(funprog::Reduce(rbind, lapply(object@components, do_one)))
  cat("\nEvaluation rule:\n", object@eval_rule, "\n")
})

#' Get sums of rows for LazyMatrix.
#'
setMethod( "rowSums", signature(x = "LazyMatrix"), function( x ) x %*% rep( 1, ncol( x ) )       )

#' Get sums of columns for LazyMatrix.
#'
setMethod( "colSums", signature(x = "LazyMatrix"), function( x )       rep( 1, nrow( x ) ) %*% x )


#' Get means of rows for LazyMatrix.
#'
setMethod( "rowMeans", signature(x = "LazyMatrix"), function( x ) rowSums( x ) / ncol( x ) )


#' Get means of columns for LazyMatrix.
#'
setMethod( "colMeans", signature(x = "LazyMatrix"), function( x ) colSums( x ) / nrow( x ) )


#' Multiply a LazyMatrix by a regular hard-working matrix.
#'
setMethod("%*%", signature(x = "LazyMatrix", y = "ANY"       ), function(x, y) EvaluateLazyMatrix( x, RIGHT = y ) )
setMethod("%*%", signature(x = "ANY",        y = "LazyMatrix"), function(x, y) EvaluateLazyMatrix( y, LEFT = x ) )


#' Combine two LazyMatrices into another LazyMatrix.
#'
setMethod("%*%", signature(x = "LazyMatrix", y = "LazyMatrix"),
          function(x, y)  {
            NewLazyMatrix( components = list( x=x, y=y ),
                           dim = c( nrow(x),
                                    ncol(y) ),
                           # Avoid ( x %*% y ), as it would just call this function again.
                           eval_rule = " ( LEFT %*% x ) %*% ( y %*% RIGHT ) " ,
                           test = F )
          }
)

#' Transpose a LazyMatrix, lazily.
#'
setMethod("t",   signature(x = "LazyMatrix"), function( x ) TransposeLazyMatrix( x ) )

#' tcrossprod a LazyMatrix, lazily.
#'
setMethod("tcrossprod",   signature(x = "LazyMatrix", y = "missing"),
          function( x ) {
            NewLazyMatrix( components = list("X" = x),
                           eval_rule = " LEFT %*% t(X) %*% X %*% RIGHT ",
                           dim = rep(ncol(x), 2), test = F )
          }
)


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
    ## select columns
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
#' Abdicate responsibility for weird subsetting requests.
#'
setMethod("[", signature(x = "LazyMatrix", i = "ANY", j = "ANY", drop = "ANY"),
          function(x,i,j, ..., drop)
            stop("invalid or not-yet-implemented 'LazyMatrix' subsetting"))

