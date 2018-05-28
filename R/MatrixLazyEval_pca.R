

#' Compute randomized PCA.
#'
#' @param M object of class LazyMatrix.
#' @param ncomp number of components to compute
# @param center @param scale Whether to center and scale the data.
# This is done using built-in scaling and centering methods.
#' @param nproj_left  dimension of random column space to project into
#' @param nproj_right dimension of random row    space to project into
#'
#' @export
#'
PCALazyMatrix = function( M, ncomp,
                          #scale = F,
                          #center = F,
                          nproj_left  = min( 4*ncomp, dim(M)[1] ),
                          nproj_right = min( 4*ncomp, dim(M)[2] ) ){
  seed_for_left  = matrix( rnorm( nproj_left  * dim( M )[2] ), ncol = nproj_left  ) %>% qr %>% qr.Q
  seed_for_right = matrix( rnorm( nproj_right * dim( M )[1] ), ncol = nproj_right ) %>% qr %>% qr.Q
  # TODO: multiply by M^TM to drive down the tail of the spectrum.
  # TODO: write unit tests for this function.
  mtm = t(M) %*% M
  projector_left  = qr.Q( qr(    M   %*% seed_for_left  ) )
  projector_right = qr.Q( qr( t( M ) %*% seed_for_right ) )

  Z = EvaluateLazyMatrix( M,
                          LEFT  = t(projector_left),
                          RIGHT  = projector_right )


  svd = irlba::irlba( nv = ncomp )
  svd$u = projector_left %*% svd$u
  svd$v = projector_right %*% svd$v
  return( svd )
}


