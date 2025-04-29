#######################################################################
#######################################################################

#' Title
#'
#' @param data
#' @param radius Float; radius to search
#' @param points
#' @param getid.nearestneighbor Boolean; affected to nothing ? (default=FALSE)
#' @param verbose Boolean (default=TRUE)
#'
#' @return
#' @export
#'
#' @examples
evalfspherical <- function(data, radius, points, getid.nearestneighbor=FALSE, verbose=TRUE){

  result = rep(NA, nrow(points))

  points_numeric = t(as.matrix(points))
  nr = nrow(points)
  nc = ncol(points)

  tree <- kdtree_build(data,verbose=verbose)

  result <- kdtree_ball_query_multiple(tree, points_numeric, nr, nc, radius, verbose)


  rm(tree)


  return(result)
}


############################################################################
