


#' Do a low-rank update to a matrix.
#'
#' @param M Any matrix with an `%*%` operator defined. Usually from the Matrix package.
#' @param L @param R  Matrices with dimensions such that M - LR works.
#'
#' @export
#'
#' @details This function returns a LazyMatrix representing M - LR.
#' The update doesn't actually have to be low-rank, but if L and R are too big
#' you may not gain any efficiency.
#'
#' @md
#'
RankOneUpdateLazily = function( M, L, R ){
  if(is.vector(L)){ L = matrix(L, nrow = length(L)) }
  if(is.vector(R)){ R = matrix(R, ncol = length(R)) }
  assertthat::are_equal(nrow(M), nrow(L))
  assertthat::are_equal(ncol(M), ncol(R))
  NewLazyMatrix( components = list("M" = M, "R" = R, "L" = L ),
                 dim = dim(M),
                 eval_rule  = "( LEFT %*% M %*% RIGHT ) - ( LEFT %*% L ) %*% ( R %*% RIGHT ) " )
}



#' Center rows or columns.
#'
#' @param M Any matrix with an `%*%` operator and nrow/ncol methods defined. Usually from the Matrix package.
#' @param do_rows If true, the rows are centered so every row has mean 0.
#' If false, the columns are centered so every column has mean 0.
#'
#' @export
#'
#' @md
#'
CenterLazily = function( M, do_rows = T ){
  if( do_rows ){
    M_ones  = rep( 1, ncol( M ) )
    M_means = M %*% ( M_ones / ncol(M) )
    return( RankOneUpdateLazily( M, L = M_means, R = M_ones ) )
  } else {
    M_ones  = rep( 1, nrow( M ) )
    M_means = ( M_ones / nrow(M) ) %*% M
    return( RankOneUpdateLazily( M, L = M_ones, R = M_means ) )
  }
}


#' Replace a matrix with residuals from an OLS regression.
#'
#' @param M Any matrix with an `%*%` operator defined. Usually from the Matrix package.
#' @param X This must be some sort of typical matrix such that `solve(t(X) %*% X)` works.
#'
#' @export
#'
#' @details This function returns a LazyMatrix representing the residuals after regressing M on X.
#' If X is too big, though, you may not gain any efficiency.
#'
#' @md
#'
RegressOutLazily = function( M, X ){
  XtXinv = solve(t(X) %*% X)
  L = X %*% XtXinv
  R = t(X) %*% M
  RankOneUpdateLazily( M, L, R )
}


#' Scale rows or columns to a desired total.
#'
#' @param M Any matrix with an `%*%` operator and nrow/ncol methods defined. Usually from the Matrix package.
#' @param desired_sum Desired sum for each row or column.
#' @param do_rows If true, the rows are scaled so every row has the desired sum.
#' If false, the columns are centered so every column has the desired sum.
#'
#' @export
#'
#' @md
#'
ScaleLazily = function( M, do_rows = T, desired_sum = 1 ){
  if( do_rows ){
    M_ones  = rep( 1, ncol( M ) )
    M_sums = M %*% M_ones
    rescaler = Matrix::Diagonal( x = desired_sum / M_sums )
    return( NewLazyMatrix( components = list(M = M, rescaler = rescaler ),
                           eval_rule = "rescaler %*% M",
                           dim = dim( M ) ) )
  } else {
    M_ones  = rep( 1, nrow( M ) )
    M_sums = M_ones %*% M
    rescaler = Matrix::Diagonal( x = desired_sum / M_sums )
    return( NewLazyMatrix( components = list(M = M, rescaler = rescaler ),
                           eval_rule = "M %*% rescaler",
                           dim = dim( M ) ) )
  }
}


#' Compute randomized SVD.
#'
#' @param M object of class LazyMatrix.
#' @param ncomp number of components to compute
#' @param nproj_left  dimension of random column space to project into
#' @param nproj_right dimension of random row    space to project into
#' @param n_iter_spectrum_flatten How many times to multiply by M^TM to emphasize the larger
#' singular values when forming the projectors. If you don't know what this is, don't change it.
#'
#' @export
#'
RandomSVDLazyMatrix = function( M, ncomp = 5,
                                n_iter_spectrum_flatten = 2,
                                nproj_left  = min( 8*ncomp, dim(M)[1] ),
                                nproj_right = min( 8*ncomp, dim(M)[2] ) ){
  if( !class( M ) == "LazyMatrix" ){
    stop( "This function accepts only LazyMatrix input. Hard-working matrices need not apply.\n" )
  }
  seed_for_left  = matrix( stats::rnorm( nproj_left  * dim( M )[2] ), ncol = nproj_left  )
  seed_for_right = matrix( stats::rnorm( nproj_right * dim( M )[1] ), ncol = nproj_right )
  seed_for_left  = qr.Q(qr(seed_for_left))
  seed_for_right = qr.Q(qr(seed_for_right))

  for( ii in 1:n_iter_spectrum_flatten){
    seed_for_left  = t(M) %*% (M %*% seed_for_left)
    seed_for_right = M %*% (t(M) %*% seed_for_right)
  }
  projector_left  = qr.Q( qr(    M   %*% seed_for_left  ) )
  projector_right = qr.Q( qr( t( M ) %*% seed_for_right ) )

  Z = EvaluateLazyMatrix( M,
                          LEFT  = t(projector_left),
                          RIGHT  = projector_right )

  svd = irlba::irlba( Z, nv = ncomp )
  svd$u = projector_left %*% svd$u
  svd$v = projector_right %*% svd$v
  return( svd )
}

