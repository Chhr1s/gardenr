#' make parameter for tuning maximum depth of tree
#'
#' @importFrom dials new_quant_param new_qual_param grid_max_entropy
#'
#' @param min An integer specifying the minimum value in the parameter grid
#' @param max An integer specifying the maximum value in the parameter grid
#'
#' @return A quantitative parameter, from `{dials}` for maxdepth
#' @export
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(min = 2, max = 5),
#'    alpha_par(min = 0.10, max = 0.001),
#'    trim_par(min = 0.1, max = 0.3),
#'    size = 10
#'    )

maxdepth_par <-
  function(min = 2, max = 12){
    new_quant_param(
      type = "integer",
      range = c(min, max),
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
#' @param min An integer specifying the minimum value in the parameter grid
#' @param max An integer specifying the maximum value in the parameter grid
#'
#' @return A quantitative parameter, from `{dials}` for maxdepth
#' @export
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(min = 2, max = 5),
#'    alpha_par(min = 0.10, max = 0.001),
#'    trim_par(min = 0.1, max = 0.3),
#'    size = 10
#'    )

alpha_par <-
  function(min = 0.05, max = 0.001){
    new_quant_param(
      type = "double",
      range = c(min, max),
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
#' @param min An integer specifying the minimum value in the parameter grid
#' @param max An integer specifying the maximum value in the parameter grid
#'
#' @return A quantitative parameter, from `{dials}` for maxdepth
#' @export
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(min = 2, max = 5),
#'    alpha_par(min = 0.10, max = 0.001),
#'    trim_par(min = 0.1, max = 0.3),
#'    size = 10
#'    )

trim_par <-
  function(min = 0.10, max = 0.50){
    new_quant_param(
      type = "double",
      range = c(min, max),
      inclusive = c(TRUE, TRUE),
      trans = NULL,
      label = c(trim_par = "portion of node used in significance tests"),
      finalize = NULL
    )
  }

# catsplit_par <-
#     new_qual_param(
#       type = "character",
#       values = c('binary', 'multiway'),
#       label = c(catsplit_par = "splitting method for (unordered) factors"),
#       finalize = NULL
#     )

