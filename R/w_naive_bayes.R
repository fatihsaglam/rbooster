#' Naive Bayes algorithm with case weights
#'
#' @description
#' Function for Naive Bayes algorithm classification with case weights.
#'
#' @param x_train explanatory variables.
#' @param y_train a factor class variable.
#' @param w a vector of case weights.
#' @param discretize If \code{TRUE} numerical variables are discretized and discrete naive bayes is applied,
#' @param breaks number of break points for discretization. Ignored if \code{discretize = TRUE}.
#'
#' @details
#' \code{w_naive_bayes} calls \code{w_gaussian_naive_bayes} or \code{w_discrete_naive_bayes}.
#'
#' if \code{discrete = FALSE}, \code{w_gaussian_naive_bayes} is called. It uses Gaussian densities with case weights and allows
#' multiclass classification.
#'
#' if \code{discrete = TRUE}, \code{w_discrete_naive_bayes} is called. It uses conditional probabilities for each category with
#' laplace smoothing and allows multiclass classification.
#'
#' @return a \code{w_naive_bayes} object with below components.
#'  \item{n_train}{Number of cases in the input dataset.}
#'  \item{p}{Number of explanatory variables.}
#'  \item{x_classes}{A list of datasets, which are \code{x_train} separated
#'  for each class.}
#'  \item{n_classes}{Number of cases for each class in input dataset.}
#'  \item{k_classes}{Number of classes in class variable.}
#'  \item{priors}{Prior probabilities.}
#'  \item{class_names}{Names of classes in class variable.}
#'  \item{means}{Weighted mean estimations for each variable.}
#'  \item{stds}{Weighted standart deviation estimations for each variable.}
#'  \item{categories}{Labels for discretized variables.}
#'  \item{boundaries}{Upper and lower boundaries for discretization.}
#'  \item{ps}{probabilities for each variable categories.}
#'
#' @examples
#' asd <- rnorm(100)
#'
#' @rdname w_naive_bayes
#' @export

w_naive_bayes <- function(x_train, y_train, w = NULL, discretize = TRUE, breaks = 3){
  if (discretize) {
    model <- w_discrete_naive_bayes(x_train = x_train, y_train = y_train, breaks = breaks, w = w)
    class(model) <- c("w_naive_bayes", "w_discrete_naive_bayes")
  } else {
    model <- w_gaussian_naive_bayes(x_train = x_train, y_train = y_train, w = w)
    class(model) <- c("w_naive_bayes", "w_gaussian_naive_bayes")
  }
  model$discretize <- discretize
  return(model)
}

#' @rdname w_naive_bayes
#' @export

w_gaussian_naive_bayes <- function(x_train, y_train, w = NULL){
  n_train <- nrow(x_train)
  p <- ncol(x_train)

  for (i in 1:p) {
    if (is.factor(x_train[,i])) {
      x_train[,i] <- as.numeric(x_train[,i])
    }
  }

  if (is.null(w)) {
    w <- rep(1, n_train)
  }
  w <- w*n_train/sum(w)

  class_names <- unique(y_train)
  k_classes <- length(class_names)

  n_train <- nrow(x_train)
  n_classes <- sapply(class_names, function(m) sum(y_train == m))

  priors <- sapply(class_names, function(m) sum(w[y_train == m])/n_train)

  x_classes <- lapply(class_names, function(m) x_train[y_train == m, ,drop = FALSE])
  w_classes <- lapply(class_names, function(m) w[y_train == m])

  means <- lapply(1:k_classes, function(m2) sapply(1:p, function(m) {
    ww <- w_classes[[m2]]/sum(w_classes[[m2]])*n_classes[m2]
    ms <- mean_weighted(x = x_classes[[m2]][,m], w = ww)
    return(ms)
  }))

  stds <- lapply(1:k_classes, function(m2) sapply(1:p, function(m) {
    ww <- w_classes[[m2]]/sum(w_classes[[m2]])*n_classes[m2]
    vars <- var_weighted(x = x_classes[[m2]][,m], w = ww, mean = means[[m2]][m])
    return(sqrt(vars))
  }))

  model <- structure(list(n_train = n_train,
                          p = p,
                          x_classes = x_classes,
                          n_classes = n_classes,
                          k_classes = k_classes,
                          priors = priors,
                          class_names = class_names,
                          means = means,
                          stds = stds),
                     class = "w_gaussian_naive_bayes")
  return(model)
}

#' @rdname w_naive_bayes
#' @export

w_discrete_naive_bayes <- function(x_train,
                                   y_train,
                                   breaks = 3,
                                   w = NULL) {

  n_train <- nrow(x_train)
  p <- ncol(x_train)

  any_numeric <- 0
  for (i in 1:p) {
    if (is.numeric(x_train[,1])) {
      any_numeric <- any_numeric + 1
    }
  }

  if (is.null(w)) {
    w <- rep(1/n_train, n_train)
  }

  w <- w/sum(w)*n_train
  discretization <- FALSE

  if (any_numeric == p) {
    discretization = TRUE
  }

  if (any_numeric < p & any_numeric != 0) {
    stop("All variables must be discrete or continuous. Use 'discretize' to discretize required variables.")
  }

  if (any_numeric == 0) {
    discretization == FALSE
  }

  class_names <- levels(y_train)
  k_classes <- length(class_names)
  n_classes <- sapply(class_names, function(m) sum(y_train == m))
  n_classes_weighted <- sapply(class_names, function(m) sum(w[y_train == m]))
  w_classes <- sapply(class_names, function(m) w[y_train == m])

  if (discretization) {
    m_discretize <- discretize(xx = x_train, breaks = breaks)

    x_train <- m_discretize$x_discrete
    boundaries <- m_discretize$boundaries
    categories <- m_discretize$categories
  } else {
    categories <- list()
    for (i in 1:p) {
      categories[[i]] <- levels(x_train[,i])
    }
  }

  x_classes <- lapply(class_names, function(m) x_train[y_train == m,,drop = FALSE])
  priors <- (n_classes_weighted + 1/k_classes)/(n_train + 1)

  ps <- lapply(1:p, function(m) {
    cat_temp <- categories[[m]]
    k_temp <- length(cat_temp)
    sapply(1:k_classes, function(m2) {
      ff <- c()
      for (ii in 1:k_temp) {
        ff[ii] <- sum((x_classes[[m2]][,m] == cat_temp[ii])*w_classes[[m2]])
      }
      (ff + 1/k_temp)/(sum(ff) + 1)
    })
  })

  results <- structure(list(categories = categories,
                            p = p,
                            boundaries = boundaries,
                            x_classes = x_classes,
                            n_classes = n_classes,
                            k_classes = k_classes,
                            class_names = class_names,
                            priors = priors,
                            ps = ps,
                            breaks = breaks
  ), class = "w_discrete_naive_bayes")
  return(results)
}
