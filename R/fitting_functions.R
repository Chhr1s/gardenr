#' fit `lmertree()` to a cross-validated data set. For continuous outcomes
#'
#' @importFrom dplyr bind_rows everything select mutate `%>%` pull tibble
#' @importFrom dials grid_max_entropy
#' @importFrom Formula as.Formula
#' @importFrom glmertree glmertree lmertree
#' @importFrom rsample initial_split testing training analysis assessment vfold_cv
#' @importFrom stats sd
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
    tuning_grid = NULL
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
    for (j in 1:nrow(tuning_grid)){

      tuning_grid_temp <-
        as.list(
          tuning_grid[j,] %>%
            setNames(sub('_par', '', names(.)))
        )

      rmse_temp <- vector(mode = 'numeric', length = length(number_cv_sets))
      mae_temp <- vector(mode = 'numeric', length = length(number_cv_sets))

      aic_temp <- vector(mode = 'numeric', length = length(number_cv_sets))
      bic_temp <- vector(mode = 'numeric', length = length(number_cv_sets))

      for (i in 1:number_cv_sets){

        temp_analysis <- analysis(cv_obj$splits[[i]])

        fitted_result <-
          do.call(
            lmertree,
            c(list(
              data = temp_analysis,
              formula = mod_formula
            ),
            tuning_grid_temp
            )
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
      aic_temp[i] <- AIC(fitted_result)
      bic_temp[i] <- BIC(fitted_result)
      message(paste0("cv index ", i, " complete"))
      }

      mean_rmse <- mean(rmse_temp)
      mean_mae <- mean(mae_temp)

      mean_aic <- mean(aic_temp)
      mean_bic <- mean(bic_temp)

      se_rmse <- sd(rmse_temp)/sqrt(length(rmse_temp))
      se_mae <- sd(mae_temp)/sqrt(length(mae_temp))

      se_aic <- sd(aic_temp)/sqrt(length(aic_temp))
      se_bic <- sd(bic_temp)/sqrt(length(bic_temp))

      temp_results <-
        tuning_grid[j,] %>%
        mutate(
          grid_index = j,
          mean_rmse = mean_rmse,
          se_rmse = se_rmse,
          mean_mae = mean_mae,
          se_mae = se_mae,
          mean_aic = mean_aic,
          se_aic = se_aic,
          mean_bic = mean_bic,
          se_bic = se_bic,
          # build in option to extract each
          # fit = list(fitted_result),
        ) %>%
        select('grid_index', everything())

      results <-
        bind_rows(results, temp_results)

      message(paste0("hyperparameter index ", j, " complete"))
    }
    return(results)
  }


#' fit `glmertree()` to a cross-validated data set. For dichotomous outcomes.
#'
#' @importFrom dplyr bind_rows everything select mutate `%>%` pull tibble
#' @importFrom dials grid_max_entropy
#' @importFrom Formula as.Formula
#' @importFrom glmertree glmertree lmertree
#' @importFrom rsample initial_split testing training analysis assessment vfold_cv
#' @importFrom stats sd
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
#'    cross_validate_it_dichot(
#'       cv_obj = cv,
#'       seed = 713,
#'       tuning_grid = tuning_grid,
#'       mod_formula = ex_formula
#'       )

cross_validate_it_dichot <-
  function(
    cv_obj,
    seed = 713,
    mod_formula,
    tuning_grid = NULL,
    dichotomous = FALSE
  ){
    if (is.null(tuning_grid)){
      tuning_grid <-
        grid_max_entropy(
          maxdepth_par(),
          alpha_par(),
          size = 5
        )
      message('meaningful defaults have not been implemented, please specify a tuning grid for better results')
    }

    set.seed(seed)
    number_cv_sets <- length(cv_obj$splits)
    results <- tibble()



    for (j in 1:nrow(tuning_grid)){

      tuning_grid_temp <-
        as.list(tuning_grid[j,])

      if ('prune_par' %in% names(tuning_grid_temp)){
        tuning_grid_temp <-
          replace_prune_when_null(tuning_grid_temp)
      }

      if ('ranefstart_par' %in% names(tuning_grid_temp)){
        tuning_grid_temp <-
          correct_ranefstart(tuning_grid_temp)
      }

      tuning_grid_temp <-
        tuning_grid_temp %>%
        setNames(sub('_par', '', names(.)))

      class_acc_temp <- vector(mode = 'numeric', length = length(number_cv_sets))
      aic_temp <- vector(mode = 'numeric', length = length(number_cv_sets))
      bic_temp <- vector(mode = 'numeric', length = length(number_cv_sets))
      number_terminal_nodes_temp <- vector(mode = 'numeric', length = length(number_cv_sets))


      for (i in 1:number_cv_sets){

        temp_analysis <- analysis(cv_obj$splits[[i]])

        ## this can be modified to get the lmer/glmer in one function

        fitted_result <-
          do.call(
            glmertree,
            c(list(
              data = temp_analysis,
              formula = mod_formula,
              family = binomial(),
              glmer.control = glmerControl(optim = 'bobyqa'),
              nAGQ = 0
            ),
            tuning_grid_temp
            )
          )


        temp_assessment <- assessment(cv_obj$splits[[i]])

        temp_predictions <-
          glmertree:::predict.glmertree(
            fitted_result,
            newdata = temp_assessment,
            allow.new.levels = TRUE,
            type = 'response'
          )

        temp_new_Y <- temp_assessment[[attr(mod_formula, "lhs")[[1]]]]

        class_acc_temp[i] <- class_acc(observed_y = temp_new_Y, predicted_y = round(temp_predictions))
        aic_temp[i] <- AIC(fitted_result)
        bic_temp[i] <- BIC(fitted_result)
        number_terminal_nodes_temp[i] <- length(partykit:::.list.rules.party(fitted_result$tree))

        message(paste0("cv index ", i, " complete"))
      }


      mean_num_t_nodes <- mean(number_terminal_nodes_temp)
      se_num_t_nodes <- sd(number_terminal_nodes_temp)/sqrt(length(number_terminal_nodes_temp))


      mean_class_acc <- mean(class_acc_temp)

      mean_aic <- mean(aic_temp)
      mean_bic <- mean(bic_temp)

      se_class_acc <- sd(class_acc_temp)/sqrt(length(class_acc_temp))

      se_aic <- sd(aic_temp)/sqrt(length(aic_temp))
      se_bic <- sd(bic_temp)/sqrt(length(bic_temp))

      temp_results <-
        tuning_grid[j,] %>%
        mutate(
          grid_index = j,
          mean_class_acc = mean_class_acc,
          se_class_acc = se_class_acc,
          mean_aic = mean_aic,
          se_aic = se_aic,
          mean_bic = mean_bic,
          se_bic = se_bic,
          mean_num_t_nodes = mean_num_t_nodes,
          se_num_t_nodes = se_num_t_nodes
          # build in option to extract each
          # fit = list(fitted_result),
        ) %>%
        select('grid_index', everything())

      results <-
        bind_rows(results, temp_results)

      message(paste0("hyperparameter index ", j, " complete"))
    }
    return(results)
  }
