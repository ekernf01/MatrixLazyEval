This is an R package for lazy evaluation of matrices. Please check out the vignette for examples. In brief: 

```    R
library(Matrix)
library(MatrixLazyEval)
data(CAex)
M_lazy = NewLazyMatrix(list(X = CAex), eval_rule = "X", dim = dim(CAex))
summary(M_lazy)
M_lazy_scaled = ScaleLazily(M_lazy, do_rows = F, desired_sum = 1e4)
M_lazy_centered = RegressOutLazily(M_lazy_scaled, rep(1, ncol(M_lazy_scaled)))
summary(M_lazy_centered)
my_rsvd = RandomSVDLazyMatrix(M_lazy_centered)
```



​    

