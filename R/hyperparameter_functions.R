#' make parameter for tuning maximum depth of tree
#'
#' @importFrom dials new_quant_param new_qual_param grid_max_entropy
#'
#' @param maxdepth_min An integer specifying the minimum value in the parameter grid
#' @param maxdepth_max An integer specifying the maximum value in the parameter grid
#'
#' @return A quantitative parameter, from `{dials}` for maxdepth
#' @export
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(maxdepth_min = 2, maxdepth_max = 5),
#'    alpha_par(alpha_min = 0.10, alpha_max = 0.001),
#'    trim_par(trim_min = 0.1, trim_max = 0.3),
#'    size = 10
#'    )

maxdepth_par <-
  function(maxdepth_min = 2, maxdepth_max = 12){
    new_quant_param(
      type = "integer",
      range = c(maxdepth_min, maxdepth_max),
      inclusive = c(TRUE, TRUE),
      trans = NULL,
      label = c(maxdepth_par = "max depth of tree"),
      finalize = NULL
      )
  }

### figure out how to work this one in later
# minsize_par <-
#   dials::new_quant_param(
#     type = "integer",
#     range = c(round(nrow(real_dat)*0.05), round(nrow(real_dat)*0.20)),
#     inclusive = c(TRUE, TRUE),
#     trans = NULL,
#     label = c(minsize_par = "minimum node size"),
#     finalize = NULL
#   )

#' make parameter for tuning level of significance in instability tests
#'
#' @importFrom dials new_quant_param new_qual_param grid_max_entropy
#'
#' @param alpha_min An integer specifying the minimum value in the parameter grid
#' @param alpha_max An integer specifying the maximum value in the parameter grid
#'
#' @return A quantitative parameter, from `{dials}` for maxdepth
#' @export
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(maxdepth_min = 2, maxdepth_max = 5),
#'    alpha_par(alpha_min = 0.10, alpha_max = 0.001),
#'    trim_par(trim_min = 0.1, trim_max = 0.3),
#'    size = 10
#'    )

alpha_par <-
  function(alpha_min = 0.001, alpha_max = 0.05){
    new_quant_param(
      type = "double",
      range = c(alpha_min, alpha_max),
      inclusive = c(TRUE, TRUE),
      trans = NULL,
      label = c(alpha_par = "significance level of split"),
      finalize = NULL
    )
  }

#' make parameter for tuning level of amount of outlier trimmed in instability tests
#'
#' @importFrom dials new_quant_param new_qual_param grid_max_entropy
#'
#' @param trim_min An integer specifying the minimum value in the parameter grid
#' @param trim_max An integer specifying the maximum value in the parameter grid
#'
#' @return A quantitative parameter, from `{dials}` for maxdepth
#' @export
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(maxdepth_min = 2, maxdepth_max = 5),
#'    alpha_par(alpha_min = 0.10, alpha_max = 0.001),
#'    trim_par(trim_min = 0.1, trim_max = 0.3),
#'    size = 10
#'    )


trim_par <-
  function(trim_min = 0.10, trim_max = 0.50){
    new_quant_param(
      type = "double",
      range = c(trim_min, trim_max),
      inclusive = c(TRUE, TRUE),
      trans = NULL,
      label = c(trim_par = "portion of node used in significance tests"),
      finalize = NULL
    )
  }

# #' make parameter for tuning level of amount of outlier trimmed in instability tests
# #'
# #' @importFrom dials new_quant_param new_qual_param grid_max_entropy
# #'
# #' @param trim_min An integer specifying the minimum value in the parameter grid
# #' @param trim_max An integer specifying the maximum value in the parameter grid
# #'
# #' @return A quantitative parameter, from `{dials}` for maxdepth
# #' @export
# #'
# #' @examples
# #' dials::grid_max_entropy(
# #'    maxdepth_par(maxdepth_min = 2, maxdepth_max = 5),
# #'    alpha_par(alpha_min = 0.10, alpha_max = 0.001),
# #'    trim_par(trim_min = 0.1, trim_max = 0.3),
# #'    size = 10
# #'    )
#
#  catsplit_par <-
#      new_qual_param(
#     type = "character",
#     values = c('binary', 'multiway'),
#     label = c(catsplit_par = "splitting method for (unordered) factors"),
#     finalize = NULL
#   )

## I don't know if I even need to make this because we can just use dials
# #' make tuning grid
# #'
# #' @importFrom dials new_quant_param new_qual_param grid_max_entropy
# #'
# #' @param maxdepth_min An integer specifying the minimum value in the parameter grid
# #' @param maxdepth_max An integer specifying the maximum value in the parameter grid
# #'
# #' @return A quantitative parameter, from `{dials}` for maxdepth
# #' @export
# #'
# #' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(maxdepth_min = 2, maxdepth_max = 5),
#'    alpha_par(alpha_min = 0.10, alpha_max = 0.001),
#'    trim_par(trim_min = 0.1, trim_max = 0.3),
#'    size = 10
#'    )
#
#
# make_my_tuning_grid <-
#   function(
#
#   ){
#
#   tuning_grid <-
#     grid_max_entropy(
#       maxdepth_par(),
#       #minsize_par,
#       alpha_par(),
#       trim_par(),
#       #catsplit_par,
#       size = 25
#     )
#   }
