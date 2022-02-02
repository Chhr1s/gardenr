##### simulate multiple small effects #####

#' simulate tibble of two-level data with outcome,
#' id_vector, and categorical/continuous covariates with varied magnitudes of effect.
#' Nuisance predictors (orthogonal to outcome) also included.
#'
#' @importFrom stats rpois rnorm rbinom
#'
#' @param j integer—number level 2 units
#' @param intercept_1 double—intercept at level 1
#' @param residual_var_sd_1 double—standard deviation of residuals at level 1
#' @param random_int_mean_2 double—mean of random intercept at level 2
#' @param residual_var_sd_2 double—standard deviation of random intercept at level 2,
#' @param start_seed integer—starting seed,
#' @param num_in_groups numeric vector—number of observations per group length of argument j
#'
#' @return tibble
#'
#' @export
#'
#' @examples
#'    J <- 116
#'    dat <-
#'       sim_multilevel(
#'          j = J,
#'          intercept_1 = 0,
#'          residual_var_sd_1 = 0.7,
#'          random_int_mean_2 = 0,
#'          residual_var_sd_2 = 0.3,
#'          start_seed = 713,
#'          num_in_groups = rpois(n = J, lambda = 15)
#'         )

sim_multilevel <-
  function(
    j = 116, ## number level 2 units
    intercept_1 = 0, ## intercept at level 1
    residual_var_sd_1 = 0.4, ## standard deviation of residuals at level 1
    random_int_mean_2 = 0, ## mean of random intercept at level 2
    residual_var_sd_2 = 0.1, ## standard deviation of random intercept at level 2,
    start_seed = 713,
    num_in_groups = rpois(n = j, lambda = 15)
  ){
    ## magnitude of main effects
    ## so just specify once, since same for all

    small_effect <- 0.20
    smaller_effect <- 0.15
    smallest_effect <- 0.10

    set.seed(start_seed)
    number_in_groups <- num_in_groups

    ## placeholder for level 2 outcomes
    outcomes_j <- vector("list", j)

    # level 1
    ## continuous
    smallest_1 <- vector("list", j)
    smaller_1 <- vector("list", j)
    small_1 <- vector("list", j)

    ## categorical
    smallest_c_1 <- vector("list", j)
    smaller_c_1 <- vector("list", j)
    small_c_1 <- vector("list", j)

    ## nuisance
    nuisance_1a <- vector("list", j)
    nuisance_1b <- vector("list", j)
    nuisance_1c <- vector("list", j)

    nuisance_c_1a <- vector("list", j)
    nuisance_c_1b <- vector("list", j)
    nuisance_c_1c <- vector("list", j)

    # level 2

    ## continuous
    smallest_2 <- vector("list", j)
    smaller_2 <- vector("list", j)
    small_2 <- vector("list", j)

    ## categorical
    smallest_c_2 <- vector("list", j)
    smaller_c_2 <- vector("list", j)
    small_c_2 <- vector("list", j)

    ## nuisance
    nuisance_2a <- vector("list", j)
    nuisance_2b <- vector("list", j)
    nuisance_2c <- vector("list", j)

    nuisance_c_2a <- vector("list", j)
    nuisance_c_2b <- vector("list", j)
    nuisance_c_2c <- vector("list", j)

    id_list <- vector("list", j)

    # level 2 intercept
    a_j <- rnorm(j, random_int_mean_2, residual_var_sd_2)

    for(i in 1:j) {

      ## make level 1 predictor variables:
      ## all of these predictors are as if standardized (mean = 0, sd = 1)
      smallest_1[[i]] <- rnorm(number_in_groups[i])
      smaller_1[[i]] <- rnorm(number_in_groups[i])
      small_1[[i]] <- rnorm(number_in_groups[i])

      smallest_c_1[[i]] <- rbinom(number_in_groups[i], 1, prob = 0.5)
      smaller_c_1[[i]] <- rbinom(number_in_groups[i], 1, prob = 0.5)
      small_c_1[[i]] <- rbinom(number_in_groups[i], 1, prob = 0.5)

      ## level 2 predictor variables:
      ## all of these predictors are as if standardized (mean = 0, sd = 1)

      smallest_2[[i]] <- rep(rnorm(1), number_in_groups[i])
      smaller_2[[i]] <- rep(rnorm(1), number_in_groups[i])
      small_2[[i]] <- rep(rnorm(1), number_in_groups[i])

      smallest_c_2[[i]] <- rep(rbinom(1, 1, prob = 0.5), number_in_groups[i])
      smaller_c_2[[i]] <- rep(rbinom(1, 1, prob = 0.5), number_in_groups[i])
      small_c_2[[i]] <- rep(rbinom(1, 1, prob = 0.5), number_in_groups[i])

      nuisance_1a[[i]] <- rnorm(number_in_groups[i])
      nuisance_1b[[i]] <- rnorm(number_in_groups[i])
      nuisance_1c[[i]] <- rnorm(number_in_groups[i])

      nuisance_c_1a[[i]] <- rbinom(number_in_groups[i], 1, prob = 0.5)
      nuisance_c_1b[[i]] <- rbinom(number_in_groups[i], 1, prob = 0.5)
      nuisance_c_1c[[i]] <- rbinom(number_in_groups[i], 1, prob = 0.5)

      nuisance_2a[[i]] <- rep(rnorm(1), number_in_groups[i])
      nuisance_2b[[i]] <- rep(rnorm(1), number_in_groups[i])
      nuisance_2c[[i]] <- rep(rnorm(1), number_in_groups[i])

      nuisance_c_2a[[i]] <- rep(rbinom(1, 1, prob = 0.5), number_in_groups[i])
      nuisance_c_2b[[i]] <- rep(rbinom(1, 1, prob = 0.5), number_in_groups[i])
      nuisance_c_2c[[i]] <- rep(rbinom(1, 1, prob = 0.5), number_in_groups[i])

      outcomes_j[[i]] <-
        rnorm(
          number_in_groups[i],
          intercept_1 +
            # level 1 effects
            ## continuous
            smallest_effect*smallest_1[[i]] +
            smaller_effect*smaller_1[[i]] +
            small_effect*small_1[[i]] +
            ## categorical
            smallest_effect*smallest_c_1[[i]] +
            smaller_effect*smaller_c_1[[i]] +
            small_effect*small_c_1[[i]] +
            # level 2 effects
            ## continuous
            smallest_effect*smallest_2[[i]] +
            smaller_effect*smaller_2[[i]] +
            small_effect*small_2[[i]] +
            ## categorical
            smallest_effect*smallest_c_2[[i]] +
            smaller_effect*smaller_c_2[[i]] +
            small_effect*small_c_2[[i]] +
            ## random intercept
            a_j[i],
          ## unexplained variance
          residual_var_sd_1
        )
      #id_list[[i]] <- rep_len(paste0('group_', i), length.out = number_in_groups[i])
      id_list[[i]] <- rep_len(i, length.out = number_in_groups[i])

    }

    outcomes_df <-
      data.frame(
        id_vector = unlist(id_list),

        smallest_1 = unlist(smallest_1),
        smaller_1 = unlist(smaller_1),
        small_1 = unlist(small_1),

        smallest_c_1 = unlist(smallest_c_1),
        smaller_c_1 = unlist(smaller_c_1),
        small_c_1 = unlist(small_c_1),


        smallest_2 = unlist(smallest_2),
        smaller_2 = unlist(smaller_2),
        small_2 = unlist(small_2),

        smallest_c_2 = unlist(smallest_c_2),
        smaller_c_2 = unlist(smaller_c_2),
        small_c_2 = unlist(small_c_2),

        nuisance_1a = unlist(nuisance_1a),
        nuisance_1b = unlist(nuisance_1b),
        nuisance_1c = unlist(nuisance_1c),

        nuisance_c_1a = unlist(nuisance_c_1a),
        nuisance_c_1b = unlist(nuisance_c_1b),
        nuisance_c_1c = unlist(nuisance_c_1c),

        nuisance_2a = unlist(nuisance_2a),
        nuisance_2b = unlist(nuisance_2b),
        nuisance_2c = unlist(nuisance_2c),

        nuisance_c_2a = unlist(nuisance_c_2a),
        nuisance_c_2b = unlist(nuisance_c_2b),
        nuisance_c_2c = unlist(nuisance_c_2c),


        outcome = unlist(outcomes_j)
      )

    return(outcomes_df)

  }



##### simulate small medium and large effects #####
# simulate_two_level_data <-
#   function(
#     j = 116, ## number level 2 units
#     intercept_1 = 0, ## intercept at level 1
#     residual_var_sd_1 = 2, ## standard deviation of residuals at level 1
#     random_int_mean_2 = 0, ## mean of random intercept at level 2
#     residual_var_sd_2 = 4, ## standard deviation of random intercept at level 2,
#     start_seed = 713,
#     num_in_groups
#   ){
#     ## magnitude of main effects
#     ## so just specify once, since same for all
#
#     large_effect <- 0.8
#     medium_effect <- 0.5
#     small_effect <- 0.2
#
#     set.seed(start_seed)
#     #rpois(j = 81, lambda = 15)
#     number_in_groups <- num_in_groups
#
#     ## placeholder for level 2 outcomes
#     outcomes_j <- vector("list", j)
#
#     ## for each variable, make list
#     ## as long as school
#     ## fill each element with a list of level 1 outcome
#
#
#     # level 1
#
#     ## continuous
#     large_1 <- vector("list", j)
#     medium_1 <- vector("list", j)
#     small_1 <- vector("list", j)
#
#     ## categorical
#     large_c_1 <- vector("list", j)
#     medium_c_1 <- vector("list", j)
#     small_c_1 <- vector("list", j)
#
#     ## nuisance
#     nuisance_1a <- vector("list", j)
#     nuisance_1b <- vector("list", j)
#     nuisance_1c <- vector("list", j)
#
#     nuisance_c_1a <- vector("list", j)
#     nuisance_c_1b <- vector("list", j)
#     nuisance_c_1c <- vector("list", j)
#
#     # level 2
#
#     ## continuous
#     large_2 <- vector("list", j)
#     medium_2 <- vector("list", j)
#     small_2 <- vector("list", j)
#
#     ## categorical
#     large_c_2 <- vector("list", j)
#     medium_c_2 <- vector("list", j)
#     small_c_2 <- vector("list", j)
#
#     ## nuisance
#     nuisance_2a <- vector("list", j)
#     nuisance_2b <- vector("list", j)
#     nuisance_2c <- vector("list", j)
#
#     nuisance_c_2a <- vector("list", j)
#     nuisance_c_2b <- vector("list", j)
#     nuisance_c_2c <- vector("list", j)
#
#     id_list <- vector("list", j)
#
#     # level 2 intercept
#     a_j <- rnorm(j, random_int_mean_2, residual_var_sd_2)
#
#     for(i in 1:j) {
#
#       ## make level 1 predictor variables:
#       ## all of these predictors are as if standardized (mean = 0, sd = 1)
#       large_1[[i]] <- rnorm(number_in_groups[i])
#       medium_1[[i]] <- rnorm(number_in_groups[i])
#       small_1[[i]] <- rnorm(number_in_groups[i])
#
#       large_c_1[[i]] <- rbinom(number_in_groups[i], 1, prob = 0.5)
#       medium_c_1[[i]] <- rbinom(number_in_groups[i], 1, prob = 0.5)
#       small_c_1[[i]] <- rbinom(number_in_groups[i], 1, prob = 0.5)
#
#       ## level 2 predictor variables:
#       ## all of these predictors are as if standardized (mean = 0, sd = 1)
#
#       large_2[[i]] <- rep(rnorm(1), number_in_groups[i])
#       medium_2[[i]] <- rep(rnorm(1), number_in_groups[i])
#       small_2[[i]] <- rep(rnorm(1), number_in_groups[i])
#
#       large_c_2[[i]] <- rep(rbinom(1, 1, prob = 0.5), number_in_groups[i])
#       medium_c_2[[i]] <- rep(rbinom(1, 1, prob = 0.5), number_in_groups[i])
#       small_c_2[[i]] <- rep(rbinom(1, 1, prob = 0.5), number_in_groups[i])
#
#       nuisance_1a[[i]] <- rnorm(number_in_groups[i])
#       nuisance_1b[[i]] <- rnorm(number_in_groups[i])
#       nuisance_1c[[i]] <- rnorm(number_in_groups[i])
#
#       nuisance_c_1a[[i]] <- rbinom(number_in_groups[i], 1, prob = 0.5)
#       nuisance_c_1b[[i]] <- rbinom(number_in_groups[i], 1, prob = 0.5)
#       nuisance_c_1c[[i]] <- rbinom(number_in_groups[i], 1, prob = 0.5)
#
#       nuisance_2a[[i]] <- rep(rnorm(1), number_in_groups[i])
#       nuisance_2b[[i]] <- rep(rnorm(1), number_in_groups[i])
#       nuisance_2c[[i]] <- rep(rnorm(1), number_in_groups[i])
#
#       nuisance_c_2a[[i]] <- rep(rbinom(1, 1, prob = 0.5), number_in_groups[i])
#       nuisance_c_2b[[i]] <- rep(rbinom(1, 1, prob = 0.5), number_in_groups[i])
#       nuisance_c_2c[[i]] <- rep(rbinom(1, 1, prob = 0.5), number_in_groups[i])
#
#       outcomes_j[[i]] <-
#         rnorm(
#           number_in_groups[i],
#           intercept_1 +
#             # level 1 effects
#             ## continuous
#             large_effect*large_1[[i]] +
#             medium_effect*medium_1[[i]] +
#             small_effect*small_1[[i]] +
#             ## categorical
#             large_effect*large_c_1[[i]] +
#             medium_effect*medium_c_1[[i]] +
#             small_effect*small_c_1[[i]] +
#             # level 2 effects
#             ## continuous
#             large_effect*large_2[[i]] +
#             medium_effect*medium_2[[i]] +
#             small_effect*small_2[[i]] +
#             ## categorical
#             large_effect*large_c_2[[i]] +
#             medium_effect*medium_c_2[[i]] +
#             small_effect*small_c_2[[i]] +
#             ## random intercept
#             a_j[i],
#           ## unexplained variance
#           residual_var_sd_1
#         )
#       #id_list[[i]] <- rep_len(paste0('group_', i), length.out = number_in_groups[i])
#       id_list[[i]] <- rep_len(i, length.out = number_in_groups[i])
#
#     }
#
#     outcomes_df <-
#       data.frame(
#         id_vector = unlist(id_list),
#
#         large_1 = unlist(large_1),
#         medium_1 = unlist(medium_1),
#         small_1 = unlist(small_1),
#
#         large_c_1 = unlist(large_c_1),
#         medium_c_1 = unlist(medium_c_1),
#         small_c_1 = unlist(small_c_1),
#
#
#         large_2 = unlist(large_2),
#         medium_2 = unlist(medium_2),
#         small_2 = unlist(small_2),
#
#         large_c_2 = unlist(large_c_2),
#         medium_c_2 = unlist(medium_c_2),
#         small_c_2 = unlist(small_c_2),
#
#         nuisance_1a = unlist(nuisance_1a),
#         nuisance_1b = unlist(nuisance_1b),
#         nuisance_1c = unlist(nuisance_1c),
#
#         nuisance_c_1a = unlist(nuisance_c_1a),
#         nuisance_c_1b = unlist(nuisance_c_1b),
#         nuisance_c_1c = unlist(nuisance_c_1c),
#
#         nuisance_2a = unlist(nuisance_2a),
#         nuisance_2b = unlist(nuisance_2b),
#         nuisance_2c = unlist(nuisance_2c),
#
#         nuisance_c_2a = unlist(nuisance_c_2a),
#         nuisance_c_2b = unlist(nuisance_c_2b),
#         nuisance_c_2c = unlist(nuisance_c_2c),
#
#
#         outcome = unlist(outcomes_j)
#       )
#
#     return(outcomes_df)
#
#   }



