library(gardenr)

# simulate data

full_sim_dat <-
  sim_multilevel(
    j = 100,
    intercept_1 = 4,
    residual_var_sd_1 = 0.6,
    random_int_mean_2 = 2,
    residual_var_sd_2 = 0.8,
    start_seed = 111,
    num_in_groups = rpois(n = 100, lambda = 10)
  )

# is it a data.frame?
expect_s3_class(full_sim_dat, 'data.frame')

# write a formula
formula1 <-
  Formula::as.Formula(
    paste0(
      'outcome ~ smallest_1 | (1 | id_vector) | small_1 + small_c_1 '
      )
    )

# tuning parameters
maxdepth_tuning <-
  maxdepth_par(maxdepth_min = 0L, maxdepth_max = 20L)

alpha_tuning <-
  alpha_par(alpha_min = 0.10, alpha_max = 0.001)

trim_tuning <-
  trim_par(trim_min = 0.01, trim_max = 0.5)

# testing tuning parameters
expect_s3_class(maxdepth_tuning, c('quant_param', 'param'))
expect_s3_class(alpha_tuning, c('quant_param', 'param'))
expect_s3_class(trim_tuning, c('quant_param', 'param'))

# make a tuning grid
# NOT TESTING BECAUSE NOT MY FUNCTION
tuning_grid <-
  dials::grid_max_entropy(
    maxdepth_tuning,
    alpha_tuning,
    trim_tuning,
    size = 25
  )

# split data
# NOT TESTING BECAUSE NOT MY FUNCTION
sim_split <- rsample::initial_split(full_sim_dat)
training <- rsample::training(sim_split)
comparison_testing <- rsample::testing(sim_split)

# make vfold_cv obj
# NOT TESTING BECAUSE NOT MY FUNCTION
cv <-
  rsample::vfold_cv(data = training, v = 10)

# cross validate
fitted <-
  gardenr::cross_validate_it(
    cv_obj = cv,
    seed = 111,
    mod_formula = formula1,
    tuning_grid = tuning_grid,
    cluster = id_vector
  )

# testing returned table
expect_s3_class(fitted, c('tbl_df', 'tbl', 'data.frame'))

# cross validate
fitted_default_tuning <-
  gardenr::cross_validate_it(
    cv_obj = cv,
    seed = 111,
    mod_formula = formula1,
    tuning_grid = NULL,
    cluster = id_vector
  )

expect_s3_class(fitted_default_tuning, c('tbl_df', 'tbl', 'data.frame'))

# NOT TESTING BECAUSE NOT MY FUNCTION
fitted <- dplyr::arrange(fitted, rmse)

# NOT TESTING BECAUSE NOT MY FUNCTION
cv_trained <-
  glmertree:::lmertree(
    data = training,
    formula =
      formula1,
    maxdepth = fitted$maxdepth_par[1],
    alpha = fitted$alpha_par[1],
    trim = fitted$trim_par[1],
    cluster = id_vector,
    verbose = TRUE
  )

# testing rmse and mae
RMSE1 <-
  rmse(
    observed_y = comparison_testing$outcome,
    predicted_y = glmertree:::predict.lmertree(cv_trained, newdata = comparison_testing)
    )

MAE1 <-
  mae(
    observed_y = comparison_testing$outcome,
    predicted_y = glmertree:::predict.lmertree(cv_trained, newdata = comparison_testing)
  )

expect_type(RMSE1, 'double')
expect_type(MAE1, 'double')
