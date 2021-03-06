#' root mean square error of approximation
#'
#' @param observed_y A numeric vector of observed ys.
#' @param predicted_y A numeric vector of predictions of y.
#'
#' @return A double vector of length 1
#' @export
#'
#' @examples
#' set.seed(1)
#' obs_y <- rnorm(4)
#' pred_y <- rnorm(4)
#' rmse(observed_y = obs_y, predicted_y = pred_y)

rmse <- function(observed_y, predicted_y){
  sqrt(sum((observed_y - predicted_y)^2)/length(observed_y))
  }

#' mean absolute error
#'
#' @param observed_y A numeric vector of observed ys.
#' @param predicted_y A numeric vector of predictions of y.
#'
#' @return A double vector of length 1
#' @export
#'
#' @examples
#' set.seed(1)
#' obs_y <- rnorm(4)
#' pred_y <- rnorm(4)
#' mae(observed_y = obs_y, predicted_y = pred_y)

mae <- function(observed_y, predicted_y){
  mean(abs(observed_y - predicted_y))/length(observed_y)
  }
