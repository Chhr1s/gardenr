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


#' classification accuracy
#'
#' @param observed_y A numeric vector of observed ys.
#' @param predicted_y A numeric vector of predictions of y.
#'
#' @return A double vector of length 1
#' @export
#'
#' @examples
#' set.seed(1)
#' set.seed(1)
#' obs_y <- rbinom(n = 4, size = 1, prob = 0.5)
#' pred_y <- rbinom(n = 4, size = 1, prob = 0.5)
#' class_acc(observed_y = obs_y, predicted_y = pred_y)

class_acc <-
  function(observed_y, predicted_y){
    acc <- sum(observed_y == predicted_y)/length(observed_y)
    return(acc)
  }

#' internal function which takes the temporary tuning grid (used within cross validate)
#' and swaps the "null" as a character to it as NULL
replace_prune_when_null <- function(tuning_grid_temp) {
  remove_i <- which(vapply(tuning_grid_temp, FUN = function(x)(x == 'null'), TRUE))

  tuning_grid_temp_new <- tuning_grid_temp[-remove_i]

  out <- append(tuning_grid_temp_new, list('prune_par' = c()))

  return(out)

}

#' internal function which takes the temporary tuning grid (used within cross validate)
#' returns what glmertree expects
correct_ranefstart <- function(tuning_grid_temp) {

  ranefstart_par_correction <- tuning_grid_temp[['ranefstart_par']]

  remove_i <-
    which(
      vapply(names(tuning_grid_temp), FUN = function(x)(x == 'ranefstart_par'), TRUE)
    )

  tuning_grid_temp_new <- tuning_grid_temp[-remove_i]


  if (ranefstart_par_correction == 'tree') {
    out <- append(tuning_grid_temp_new, list('ranefstart_par' = c()))
  }

  if (ranefstart_par_correction == 'ranef') {
    out <- append(tuning_grid_temp_new, list('ranefstart_par' = TRUE))
  }

  return(out)

}




