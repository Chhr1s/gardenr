#' fit `glmertree()` to a cross-validated data set
#'
#' @param cv_obj vfold_cv—a v-fold cross-validated dataset from `rsample::vfold_cv()`
#' @param mod_formula Formula—made from `as.Formula()`. uppercase required `:)`
#' @param seed integer—starting seed
#' @param tuning_grid — either tuning grid e.g., from `dials::grid_max_entropy()` or default grid (when `tuning_grid = NULL`).
#' @param ... additional arguments to be passed to `glmertree()`
#' @return tibble—fit statistics (rmse, mae) for object
#'
#' @export
#'
#' @examples
#' dat <- sim_multilevel()
#' example_split <- rsample::initial_split(dat)
#' example_train <- rsample::training(example_split)
#' example_test  <-  rsample::testing(example_split)
#' cv <- rsample::vfold_cv(data = example_train, v = 10)
#'
#' ex_formula <-
#'    Formula::as.Formula(
#'       'outcome ~ small_1 |
#'       (1 | id_vector) |
#'       small_c_1 + small_c_2 + nuisance_1a + nuisance_c_1a'
#'       )
#'
#' tuning_grid <-
#'   dials::grid_max_entropy(
#'     maxdepth_par(maxdepth_min = 0L, maxdepth_max = 20L),
#'     alpha_par(alpha_min = 0.10, alpha_max = 0.001),
#'     trim_par(trim_min = 0.01, trim_max = 0.5),
#'     size = 10
#'   )
#'
#' fitted <-
#'    cross_validate_it(
#'       cv_obj = cv,
#'       seed = 713,
#'       tuning_grid = tuning_grid,
#'       mod_formula = ex_formula
#'       )

cross_validate_it <-
  function(
    cv_obj,
    seed = 713,
    mod_formula,
    tuning_grid = NULL,
    ...
  ){
    if (is.null(tuning_grid)){
      tuning_grid <-
        grid_max_entropy(
          maxdepth_par(),
          alpha_par(),
          trim_par(),
          size = 25
        )
      message('meaningful defaults have not been implemented, please specify a tuning grid for better results')
    }
    set.seed(seed)
    number_cv_sets <- length(cv_obj$splits)
    results <- tibble()
    for (j in 1:nrow(tuning_grid)) {

      max_depth_temp <- tuning_grid$maxdepth_par[[j]]
      alpha_temp <- tuning_grid$alpha_par[[j]]
      trim_temp <- tuning_grid$trim_par[[j]]

      rmse_temp <- vector(mode = 'numeric', length = length(number_cv_sets))
      mae_temp <- vector(mode = 'numeric', length = length(number_cv_sets))

      for (i in 1:number_cv_sets) {

        temp_analysis <- analysis(cv_obj$splits[[i]])

        fitted_result <-
          lmertree(
            data = temp_analysis,
            formula = mod_formula,
            maxdepth = max_depth_temp,
            alpha = alpha_temp,
            trim = trim_temp,
            #...
          )

      temp_assessment <- assessment(cv_obj$splits[[i]])

      temp_predictions <-
        glmertree:::predict.lmertree(
          fitted_result,
          newdata = temp_assessment,
          allow.new.levels = TRUE
          )

      temp_new_Y <- temp_assessment[[attr(mod_formula, "lhs")[[1]]]]

      rmse_temp[i] <- rmse(observed_y = temp_new_Y, predicted_y = temp_predictions)
      mae_temp[i] <- mae(observed_y = temp_new_Y, predicted_y = temp_predictions)
      message(paste0("cv index ", i, " complete"))
      }

      mean_rmse <- mean(rmse_temp)
      mean_mae <- mean(mae_temp)

      se_rmse <- sd(rmse_temp)/sqrt(length(rmse_temp))
      se_mae <- sd(mae_temp)/sqrt(length(mae_temp))

      temp_results <-
        tuning_grid[j,] %>%
        mutate(
          grid_index = j,
          mean_rmse = mean_rmse,
          se_rmse = se_rmse,
          mean_mae = mean_mae,
          se_mae = se_mae,
          # build in option to extract each
          # fit = list(fitted_result),
        ) %>%
        select('grid_index', everything())

      results <-
        bind_rows(results, temp_results)

      message(paste0("hyperparameter index ", j, " complete"))
    }
    results
  }
