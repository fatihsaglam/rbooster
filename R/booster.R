#' @title AdaBoost Framework for Any Classifier
#'
#' @description This function allows you to use any classifier to be used in
#' Discrete or Real AdaBoost framework.
#'
#' @param x_train feature matrix.
#' @param y_train a factor class variable. Boosting algorithm allows for
#' k >= 2. However, not all classifiers are capable of multiclass
#' classification.
#' @param classifier pre-ready or a custom classifier function. Pre-ready
#' classifiers are "rpart", "glm", "gnb", "dnb", "earth".
#' @param predictor prediction function for classifier. It's output must be a
#' factor variable with the same levels of y_train
#' @param method "discrete" or "real" for Discrete or Real Adaboost.
#' @param x_test optional test feature matrix. Can be used instead of predict
#' function. print_detail and print_plot gives information about test.
#' @param y_test optional a factor test class variable with the same levels as
#' y_train. Can be used instead of predict function. print_detail and print_plot
#' gives information about test.
#' @param weighted_bootstrap If classifier does not support case weights,
#' weighted_bootstrap must be TRUE used for weighting. If classifier supports
#' weights, it must be FALSE. default is FALSE.
#' @param max_iter maximum number of iterations. Default to 30. Probably should
#' be higher for classifiers other than decision tree.
#' @param lambda a parameter for model weights. Default to 1. Higher values
#' leads to unstable weak classifiers, which is good sometimes. Lower values
#' leads to slower fitting.
#' @param print_detail a logical for printing errors for each iteration.
#' Default to TRUE
#' @param print_plot a logical for plotting errors. Default to FALSE.
#' @param bag_frac a value between 0 and 1. It represents the proportion of
#' cases to be used in each iteration. Smaller datasets may be better to create
#' weaker classifiers. 1 means all cases. Default to 0.5. Ignored if
#' \code{weighted_bootstrap == TRUE}.
#' @param p_weak number of variables to use in weak classifiers. It is the
#' number of columns in \code{x_train} by default. Lower values lead to weaker
#' classifiers.
#' @param ... additional arguments for classifier and predictor functions.
#' weak classifiers.
#'
#' @details
#' \code{method} can be "discrete" and "real" at the moment and indicates Discrete
#' AdaBoost and Real AdaBoost. For multiclass classification, "discrete" means SAMME,
#' "real" means SAMME.R algorithm.
#'
#' Pre-ready classifiers are "rpart", "glm", "dnb", "gnb", "earth", which means
#' CART, logistic regression, Gaussian naive bayes, discrete naive bayes and MARS
#' classifier respectively.
#'
#' \code{predictor} is valid only if a custom \code{classifier} function is
#' given. A custom classifier funtion should be as \code{function(x_train, y_train,
#' weights, ...)} and its output is a model object which can be placed in
#' \code{predictor}. \code{predictor} function is \code{function(model, x_new, type
#' ...)} and its output must be a vector of class predictions. type must be "pred"
#' or "prob", which gives a vector of classes or a matrix of probabilities, which
#' each column represents each class. See \code{vignette("booster", package = "booster")}
#' for examples.
#'
#' \code{lambda} is a multiplier of model weights.
#'
#' \code{weighted_bootstrap} is for bootstrap sampling in each step. If the
#' classifier accepts case weights then it is better to turn it off. If classifier
#' does not accept case weights, then weighted bootstrap will make it into
#' weighted classifier using bootstrap. Learning may be slower this way.
#'
#' \code{bag_frac} helps a classifier to be "weaker" by reducing sample
#' size. Stronger classifiers may require lower proportions of \code{bag_frac}.
#'  \code{p_weak} does the same by reducing numbeer of variables.
#'
#' @return a booster object with below components.
#'  \item{n_train}{Number of cases in the input dataset.}
#'  \item{w}{Case weights for the final boost.}
#'  \item{p}{Number of features.}
#'  \item{weighted_bootstrap}{TRUE if weighted bootstrap applied. Otherwise FALSE.}
#'  \item{max_iter}{Maximum number of boosting steps.}
#'  \item{lambda}{The multiplier of model weights.}
#'  \item{predictor}{Function for prediction}
#'  \item{alpha}{Model weights.}
#'  \item{err_train}{A vector of train errors in each step of boosting.}
#'  \item{err_test}{A vector of test errors in each step of boosting. If there are
#'  no test data, it returns NULL}
#'  \item{models}{Models obtained in each boosting step}
#'  \item{x_classes}{A list of datasets, which are \code{x_train} separated for
#'  each class.}
#'  \item{n_classes}{Number of cases for each class in input dataset.}
#'  \item{k_classes}{Number of classes in class variable.}
#'  \item{bag_frac}{Proportion of input dataset used in each boosting step.}
#'  \item{class_names}{Names of classes in class variable.}
#'
#' @author Fatih Saglam, fatih.saglam@omu.edu.tr
#'
#' @importFrom stats predict
#'
#' @seealso \code{predict.booster}
#'
#' @examples
#' asd <- rnorm(100)
#'
#' @references
#' Freund, Y., & Schapire, R. E. (1997). A decision-theoretic generalization of
#' on-line learning and an application to boosting. Journal of computer and
#' system sciences, 55(1), 119-139.
#'
#' Hastie, T., Rosset, S., Zhu, J., & Zou, H. (2009). Multi-class AdaBoost.
#' Statistics and its Interface, 2(3), 349-360.
#'
#' @rdname booster
#' @export

booster <- function(x_train,
                    y_train,
                    classifier = "rpart",
                    predictor = NULL,
                    method = "discrete",
                    x_test = NULL,
                    y_test = NULL,
                    weighted_bootstrap = FALSE,
                    max_iter = 50,
                    lambda = 1,
                    print_detail = TRUE,
                    print_plot = FALSE,
                    bag_frac = 0.5,
                    p_weak = NULL,
                    ...) {

  bb <- get(paste0(method, "_adaboost"))
  mm <- bb(x_train = x_train,
           y_train = y_train,
           classifier = classifier,
           predictor = predictor,
           x_test = x_test,
           y_test = y_test,
           weighted_bootstrap = weighted_bootstrap,
           max_iter = max_iter,
           lambda = lambda,
           print_detail = print_detail,
           print_plot = print_plot,
           bag_frac = bag_frac,
           p_weak = p_weak,
           ... = )
  results <- mm
  results$method <- method
  class(results) <- "booster"

  return(results)
}

