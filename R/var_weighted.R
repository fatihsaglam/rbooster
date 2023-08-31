#' Weighted variance
#'
#' @description
#' Calculates weighted variance
#'
#' @param x a numerical vector.
#' @param w vector of weights. If NULL, non-weighted variance is calculated.
#' @param mean a pre-calculated mean. If NULL, mean is calculated.
#'
#' @rdname var_weighted
#' @keywords internal
#' @export


var_weighted <- function(x, w = NULL, mean = NULL) {
  if (is.null(w)) {
    if (is.null(mean)) {
      return(var(x))
    } else {
      return(sum((x - mean)^2)/(n - 1))
    }
  } else {
    if (is.null(mean)) {
      n_w <- sum(w)
      mn <- sum(x*w)/sum(w)
      return(sum(w*(x - mn)^2)/(n_w - 1))
    } else{
      n_w <- sum(w)
      return(sum(w*(x - mean)^2)/(n_w - 1))
    }
  }
}


