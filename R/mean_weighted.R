#' Weighted mean
#'
#' Calcualtes weighted mean
#'
#' @param x a numerical vector.
#' @param w vector of weights. If NULL, non-weighted mean is calculated.
#'
#' @rdname mean_weighted
#' @keywords internal
#' @export



mean_weighted <- function(x, w = NULL) {
  if (is.null(w)) {
    return(mean(x))
  } else{
    return(sum(x*w)/sum(w))
  }
}
