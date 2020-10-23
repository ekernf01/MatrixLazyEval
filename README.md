### MatrixLazyEval

An R package for "lazy evaluation" of matrices, which can help save time and memory by being smarter about common tasks. 

##### Why? 

Suppose you have a 60,000 by 30,000 sparse matrix X with 1% of the entries nonzero (haphazardly distributed, not in orderly patterns). You want to center each column of X to form Z, then compute Zv for some vector v. If you center the columns naively, almost none of the zeroes will be preserved. Your matrix will be dense, and it will occupy **>14GB of memory** (1.8e9 doubles; 8 bytes per double). To avoid this, you can compute `(Xv) - 1*(m*v)`, where m is a row vector containing the column means of X. This consumes little memory beyond what is already used to store the data.

##### Applications 

Similar tricks can often help economize when you:

- scale rows/colums
- shift rows/columns
- extract residuals after linear regression
- multiply matrices
- compose multiple such operations
- perform approximate PCA and SVD's

##### Examples

This `R` package defines convenient interfaces to these operations via a "LazyMatrix" class. Please check out the vignette for examples. In brief: 

```    R
# Install
install.packages(c("remotes", "testthat", "Matrix", "irlba"))
remotes::install_github("ekernf01/MatrixLazyEval")

# Set up data
library(Matrix)
library(MatrixLazyEval)
data(CAex)
M_lazy = NewLazyMatrix(list(X = CAex), eval_rule = "X", dim = dim(CAex))
summary(M_lazy)

# Note that t() and %*% work as expected
summary(M_lazy %*% t(M_lazy))

# Try lazy scaling, regression, and SVD
M_lazy_scaled = ScaleLazily(M_lazy, do_rows = F, desired_sum = 1e4)
M_lazy_centered = RegressOutLazily(M_lazy_scaled, rep(1, ncol(M_lazy_scaled)))
summary(M_lazy_centered)
my_rsvd = RandomSVDLazyMatrix(M_lazy_centered)
```



​    

