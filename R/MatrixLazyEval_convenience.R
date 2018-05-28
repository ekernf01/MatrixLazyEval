
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
  if(is.vector(L)){ L = matrix(L, ncol = length(L)) }
  if(is.vector(R)){ R = matrix(R, nrow = length(R)) }
  assertthat::are_equal(nrow(M), ncol(L))
  assertthat::are_equal(ncol(M), nrow(R))
  NewLazyMatrix( components = list("M" = M, "R" = R, "L" = L ),
                 dim = dim(M),
                 eval_rule  = "( LEFT %*% M %*% RIGHT ) - ( LEFT %*% L ) %*% ( R %*% RIGHT ) " )
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
  seed_for_left  = matrix( rnorm( nproj_left  * dim( M )[2] ), ncol = nproj_left  ) %>% qr %>% qr.Q
  seed_for_right = matrix( rnorm( nproj_right * dim( M )[1] ), ncol = nproj_right ) %>% qr %>% qr.Q
  mtm = tcrossprod(M) # Calls an efficient LazyMatrix method
  mmt = tcrossprod(t(M)) # Calls yet more efficient LazyMatrix methods
  for( ii in 1:n_iter_spectrum_flatten){
    seed_for_left  = mtm %*% seed_for_left
    seed_for_right = mmt %*% seed_for_right
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

