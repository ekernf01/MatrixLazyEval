## ------------------------------------------------------------------------
library(Matrix)
data(CAex)
#M = CAex
M = t(MAST::maits$expressionmat) 
M = M * matrix(rbinom(prod(dim(M)), p = 0.01, 1), nrow(M), ncol(M))
M = Matrix::Matrix(M, sparse = T )
M = cbind(M, M, M, M, M, M, M, M, M, M)
dim(M)
class(M)
cat("NNZ: ", sum( M > 0 ), "\n")
cat("Proportion NZ: ", sum( M > 0 ) / prod( dim(M) ), "\n" )
library(MatrixLazyEval)

## ------------------------------------------------------------------------
R    = matrix( nrow = nrow(M), ncol = 1, data = rowMeans( M ) )
ONES = matrix( ncol = ncol(M), nrow = 1, data = 1 )
M_shifted = M - R %*% ONES
M_shifted_lazy = NewLazyMatrix( components = list("M" = M, "R" = R, "ONES" = ONES ), 
                                dim = dim(M),
                                eval_rule  = "(LEFT %*% M %*% RIGHT) - (LEFT %*% R) %*% (ONES %*% RIGHT)" )
object.size(M_shifted)
object.size(M_shifted_lazy)
x = rnorm( ncol( M ) )
microbenchmark::microbenchmark({M_shifted      %*% x})
microbenchmark::microbenchmark({M_shifted_lazy %*% x})

## ------------------------------------------------------------------------

X = as.matrix( data.frame( intercept = 1, slope = 1:nrow(M) ) )
XtXinv = solve(t(X) %*% X) # this is just 2 by 2
M_regressed = M - X %*% XtXinv %*% t(X) %*% M
M_regressed_lazy = NewLazyMatrix( components = list("M" = M, "X" = X, "XtXinv" = XtXinv ), 
                                  dim = dim(M),
                                  eval_rule  = "( LEFT %*% M %*% RIGHT ) - ( LEFT %*% X %*% XtXinv) %*% ( t(X) %*% M %*% RIGHT ) " )

object.size(M_regressed)
object.size(M_regressed_lazy)
x = rnorm( ncol( M ) )
microbenchmark::microbenchmark({M_regressed      %*% x})
microbenchmark::microbenchmark({M_regressed_lazy %*% x})

