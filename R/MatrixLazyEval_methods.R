#' @importFrom Matrix t tcrossprod colSums rowMeans
NULL

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
#' @param x LazyMatrix
#'
setMethod( "dim",  signature(x = "LazyMatrix"), function(x) x@dim )

#' How tall is my matrix?
#'
#' @param x LazyMatrix
#'
setMethod( "nrow", signature(x = "LazyMatrix"), function(x) dim(x)[1] )

#' How wide is my matrix?
#'
#' @param x LazyMatrix
#'
setMethod( "ncol", signature(x = "LazyMatrix"), function(x) dim(x)[2] )

#' Show a few details on a LazyMatrix.
#'
#' @param object LazyMatrix
#'
setMethod( "summary", signature(object = "LazyMatrix"), function(object) {
  cat("\nComponent dimensions:\n")
  do_one = function(X) {tryCatch(dim(X), error = function(e) {c(0,0)})}
  print(base::Reduce(rbind, lapply(object@components, do_one)))
  cat("\nEvaluation rule:\n", object@eval_rule, "\n")
})

#' Get sums of rows for LazyMatrix.
#'
#' @param x LazyMatrix
#' 
#' This function is EAGER: it triggers immediate calculations.
#'
setMethod( "rowSums", signature(x = "LazyMatrix"), function( x ) x %*% rep( 1, ncol( x ) )       )

#' Get sums of columns for LazyMatrix.
#'
#' @param x LazyMatrix
#'
#' This function is EAGER: it triggers immediate calculations.
#'
setMethod( "colSums", signature(x = "LazyMatrix"), function( x )       rep( 1, nrow( x ) ) %*% x )


#' Get means of rows for LazyMatrix.
#'
#' @param x LazyMatrix
#'
#' This function is EAGER: it triggers immediate calculations.
#' 
setMethod( "rowMeans", signature(x = "LazyMatrix"), function( x ) rowSums( x ) / ncol( x ) )


#' Get means of columns for LazyMatrix.
#'
#' @param x LazyMatrix
#'
#' This function is EAGER: it triggers immediate calculations.
#' 
setMethod( "colMeans", signature(x = "LazyMatrix"), function( x ) colSums( x ) / nrow( x ) )


#' Multiply a LazyMatrix by another matrix-like object that supports \code{%*%}.
#'
#' @param x LazyMatrix
#' @param y This object must be able to interact with objects in the components slot of x via the usual matrix multiplication operator.
#' 
#' This function is EAGER: it triggers immediate calculations.
#' 
setMethod("%*%", signature(x = "LazyMatrix", y = "ANY"       ), function(x, y) EvaluateLazyMatrix( x, RIGHT = y ) )

#' Multiply a LazyMatrix by a regular hard-working matrix.
#'
#' @param y LazyMatrix
#' 
#' This function is EAGER: it triggers immediate calculations.
#' 
#' @param x Anything with a matrix multiplication method that accepts objects in the components slot of y.
setMethod("%*%", signature(x = "ANY",        y = "LazyMatrix"), function(x, y) EvaluateLazyMatrix( y, LEFT = x ) )


#' Combine two LazyMatrices into another LazyMatrix.
#'
#' @param x,y LazyMatrix
#' 
#' This function is LAZY: it defers calculations to later.
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
#' @param x LazyMatrix
#'
#' This function is LAZY: it defers calculations to later.
#' 
setMethod("t",   signature(x = "LazyMatrix"), function( x ) TransposeLazyMatrix( x ) )

#' tcrossprod a LazyMatrix, lazily.
#'
#' @param x LazyMatrix
#'
#' This function is LAZY: it defers calculations to later.
#' 
setMethod("tcrossprod",   signature(x = "LazyMatrix", y = "missing"),
          function( x ) {
            NewLazyMatrix( components = list("X" = x),
                           eval_rule = " LEFT %*% t(X) %*% X %*% RIGHT ",
                           dim = rep(ncol(x), 2), test = F )
          }
)


idx_types = c("integer", "logical", "missing")
for( i_type in idx_types){
  for( j_type in idx_types){
    ######### This is just how I make the documentation.
    # library(magrittr)
    # template =     "
    # \\docType{methods}
    # \\name{[,LazyMatrix,i_type,j_type,ANY-method}
    # \\alias{[,LazyMatrix,i_type,j_type,ANY-method}
    # \\title{Subset LazyMatrix}
    # \\usage{
    # \\S4method{[}{LazyMatrix,i_type,j_type,ANY}(x, i, j, drop = TRUE)
    # }
    # \\arguments{
    #   \\item{x}{LazyMatrix}
    #   \\item{i, j, drop}{These args may be integer or logical (integer disallowed for drop).}
    # }
    # \\description{
    # \"Subset as if from Matrix package.\"
    # }
    # " %>%
    #   sub("i_type,j_type", paste0(i_type, ",", j_type), . ) %>%
    #   sub("i_type,j_type", paste0(i_type, ",", j_type), . ) %>%
    #   sub("i_type,j_type", paste0(i_type, ",", j_type), . )
    # fname = "sub-LazyMatrix-i_type-j_type-ANY-method.Rd" %>% sub("i_type-j_type", paste0(i_type, "-", j_type), . )
    # cat( template, file = paste0("~/Desktop/software_projects/MatrixLazyEval/man/", fname))

    setMethod("[", signature(x = "LazyMatrix",
                             i = i_type,
                             j = j_type,
                             drop = "ANY"),
              function (x, i = 1:nrow(x), j = 1:ncol(x), drop) {
                ExtractElementsLazyMatrix( x, i, j, drop )
              })
  }
}

## Send message if any of (i,j,drop) is invalid.
#' Abdicate responsibility for weird subsetting requests.
#'
#' @param x LazyMatrix 
#' @param i,j,drop This failnice method gets dispatched if the methods above don't apply. 
#' It happens if the args are not integer or logical (also, integer disallowed for drop).
#'
setMethod("[", signature(x = "LazyMatrix", i = "ANY", j = "ANY", drop = "ANY"),
          function(x,i,j, drop)
            stop("invalid or not-yet-implemented 'LazyMatrix' subsetting"))

