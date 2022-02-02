#' fit `glmertree()` to a cross-validated data set
#'
#' @importFrom dplyr bind_rows everything select mutate `%>%` pull tibble
#' @importFrom dials grid_max_entropy
#' @importFrom Formula as.Formula
#' @importFrom glmertree glmertree lmertree
#' @importFrom rsample initial_split testing training analysis assessment vfold_cv
#'
#' @param cv_obj vfold_cv—a v-fold cross-validated dataset from `rsample::vfold_cv()`
#' @param mod_formula Formula—made from `as.Formula()`. uppercase required `:)`
#' @param seed integer—starting seed
#' @param make_my_tuning_grid logical—should default tuning grid be used? (temporary)
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
#' fitted <-
#'    cross_validate_it(
#'       cv_obj = cv,
#'       seed = 713,
#'       mod_formula = ex_formula,
#'       make_my_tuning_grid = TRUE
#'       )


cross_validate_it <-
  function(
    cv_obj,
    seed = 713,
    # include_cluster = F,
    mod_formula,
    make_my_tuning_grid = FALSE,
    ...
  ){
    set.seed(seed)

    number_cv_sets <- length(cv_obj$splits)

    results <- tibble()


    for (i in 1:number_cv_sets){

      temp_analysis <- analysis(cv_obj$splits[[i]])

      # if (include_cluster == T){
      #   fitted_result <-
      #     lmertree(
      #       data = temp_analysis,
      #       formula = mod_formula,
      #       cores = 7,
      #       cluster = id_vector,
      #       ...
      #     )
      # }

      #else {
        fitted_result <-
          lmertree(
            data = temp_analysis,
            formula = mod_formula,
            #cores = 7,
            ...
          )
      #}
      temp_assessment <- assessment(cv_obj$splits[[i]])

      temp_predictions <-
        glmertree:::predict.lmertree(
          fitted_result,
          newdata = temp_assessment,
          allow.new.levels = TRUE
          )

      temp_new_Y <- temp_assessment$outcome

      rmse <- rmse(observed_y = temp_new_Y, predicted_y = temp_predictions)

      mae <- mae(observed_y = temp_new_Y, predicted_y = temp_predictions)

      if (make_my_tuning_grid == TRUE){
        tuning_grid <-
          grid_max_entropy(
            maxdepth_par(),
            #minsize_par,
            alpha_par(),
            trim_par(),
            #catsplit_par,
            size = 25
          )
      }

      temp_results <-
        tuning_grid[i,] %>%
        mutate(
          cv_index = i,
          rmse = rmse,
          mae = mae,
          #fit = list(fitted_result),
        ) %>%
        select(cv_index, everything())

      results <-
        bind_rows(results, temp_results)

      message(paste0('iteration ', i, ' complete'))
    }
    return(results)
  }

#####
### probably better to have a function, but not sure how to ###
### allow n number through pmap ###
# icc_33_tuning_res_cluster <-
#   cv %>%
#   pmap_dfr(
#     .l =
#       as.list(tuning_grid),
#     .f =
#       ~fit_glmertree_to_CV(
#         mod_formula = small_formula,
#         seed = 713,
#         cv_obj = cv,
#         maxdepth = ..1,
#         minsize = ..2,
#         alpha = ..3,
#         trim = ..4,
#         # catsplit = ..5,
#         include_cluster = TRUE
#       )
#   )
### ^^ probably better to have a function, but not sure how to  ^^ ###
### allow varied # of variables through pmap ###
