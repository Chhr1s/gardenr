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
      top_split_temp <- vector(mode = 'numeric', length = length(number_cv_sets))

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
      top_split_temp[i] <- extract_top_split(fitted_result$tree)
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
          top_split_list = list(top_split_temp)
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
    tuning_grid = NULL
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
      missing_fit <- vector(mode = 'numeric', length = length(number_cv_sets))
      aic_temp <- vector(mode = 'numeric', length = length(number_cv_sets))
      bic_temp <- vector(mode = 'numeric', length = length(number_cv_sets))
      loglik_temp <- vector(mode = 'numeric', length = length(number_cv_sets))
      number_terminal_nodes_temp <- vector(mode = 'numeric', length = length(number_cv_sets))
      top_split_temp <- vector(mode = 'numeric', length = length(number_cv_sets))


      for (i in 1:number_cv_sets){

        temp_analysis <- analysis(cv_obj$splits[[i]])

        other_args <-
          list(
            data = temp_analysis,
            formula = mod_formula,
            family = binomial(),
            glmer.control = glmerControl(optim = 'bobyqa'),
            nAGQ = 0
          )

        args_all <- append(tuning_grid_temp, other_args)
        temp_assessment <- assessment(cv_obj$splits[[i]])



        ## this can be modified to get the lmer/glmer in one function
        safe_gt <- safely(function(arguments){do.call(glmertree, arguments)})

        # Apply the safe_gt() function
        result_safely <- safe_gt(args_all)


        number_terminal_nodes_temp[i] <- NA_real_
        aic_temp[i] <- NA_real_
        bic_temp[i] <- NA_real_
        loglik_temp[i] <- NA_real_
        class_acc_temp[i] <- 0
        top_split_temp[i] <- NA_character_


        # Check the results
        if (!is.null(result_safely$error)) {
          ## do not update model fit if convergence issues
          message('model fitting error (or warning if `options(warn = 2)`\nFit set to 0')
          missing_fit[i] <- TRUE

        } else {
          fitted_result <- result_safely$result
          #cat("No warning or error occurred.\n")
          temp_predictions <-
            glmertree:::predict.glmertree(
              fitted_result,
              newdata = temp_assessment,
              allow.new.levels = TRUE,
              type = 'response'
            )

          top_split_temp[i] <- extract_top_split(fitted_result$tree)
          aic_temp[i] <- AIC(fitted_result)
          bic_temp[i] <- BIC(fitted_result)
          loglik_temp[i] <- result_safely$result$loglik
          missing_fit[i] <- FALSE

          number_terminal_nodes_temp[i] <-
            length(
              partykit:::.list.rules.party(fitted_result$tree)
            )

          temp_new_Y <- temp_assessment[[attr(mod_formula, "lhs")[[1]]]]

          class_acc_temp[i] <- class_acc(observed_y = temp_new_Y, predicted_y = round(temp_predictions))

        }


        message(paste0("cv index ", i, " complete"))
      }

      total_problematic <- sum(missing_fit)
      mean_num_t_nodes <- mean(number_terminal_nodes_temp, na.rm = TRUE)
      se_num_t_nodes <- sd(number_terminal_nodes_temp, na.rm = TRUE)/sqrt(length(number_terminal_nodes_temp))

      mean_loglik <- mean(loglik_temp, na.rm = TRUE)
      se_loglik <- sd(loglik_temp, na.rm = TRUE)/sqrt(length(loglik_temp))


      mean_class_acc <- mean(class_acc_temp, na.rm = TRUE)

      mean_aic <- mean(aic_temp, na.rm = TRUE)
      mean_bic <- mean(bic_temp, na.rm = TRUE)

      se_class_acc <- sd(class_acc_temp, na.rm = TRUE)/sqrt(length(class_acc_temp))

      se_aic <- sd(aic_temp, na.rm = TRUE)/sqrt(length(aic_temp))
      se_bic <- sd(bic_temp, na.rm = TRUE)/sqrt(length(bic_temp))

      temp_results <-
        tuning_grid[j,] %>%
        mutate(
          grid_index = j,
          total_problematic = total_problematic,
          mean_class_acc = mean_class_acc,
          se_class_acc = se_class_acc,
          mean_aic = mean_aic,
          se_aic = se_aic,
          mean_bic = mean_bic,
          se_bic = se_bic,
          mean_num_t_nodes = mean_num_t_nodes,
          se_num_t_nodes = se_num_t_nodes,
          mean_loglik = mean_loglik,
          se_loglik = se_loglik,
          top_split_list = list(top_split_temp)
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
