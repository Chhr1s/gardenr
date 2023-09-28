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
#'    alpha_par(alpha_min = 0.001, alpha_max = 0.1),
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

#' make parameter for tuning minimum (terminal) node size
#'
#' @importFrom dials new_quant_param new_qual_param grid_max_entropy
#'
#' @param maxdepth_min An integer specifying the minimum value in the parameter grid
#' @param maxdepth_max An integer specifying the maximum value in the parameter grid
#'
#' @return A quantitative parameter, from `{dials}` for minsize
#' @export
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(maxdepth_min = 2, maxdepth_max = 5),
#'    alpha_par(alpha_min = 0.001, alpha_max = 0.1),
#'    minsize_par(minsize_min = 200, minsize_max = 500),
#'    size = 10
#'    )

minsize_par <- function(minsize_min, minsize_max){

  dials::new_quant_param(
    type = "integer",
    range = c(minsize_min, minsize_max), # perhaps unnecessary with integer
    inclusive = c(TRUE, TRUE),
    trans = NULL,
    label = c(minsize_par = "minimum node size"),
    finalize = NULL
  )
}

#' make parameter for mtry
#'
#' @importFrom dials new_quant_param new_qual_param grid_max_entropy
#'
#' @param mtry_min An integer specifying the minimum value in the parameter grid
#' @param mtry_max An integer specifying the maximum value in the parameter grid
#'
#' @return A quantitative parameter, from `{dials}` for mtry
#' @export
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(maxdepth_min = 2, maxdepth_max = 5),
#'    mtry_par(mtry_min = 1, mtry_max = 20),
#'    minsize_par(minsize_min = 200, minsize_max = 500),
#'    size = 10
#'    )

mtry_par <- function(mtry_min, mtry_max){

  dials::new_quant_param(
    type = "integer",
    range = c(mtry_min, mtry_max),
    inclusive = c(TRUE, TRUE),
    trans = NULL,
    label = c(mtry_par = "subset of variables to test per split"),
    finalize = NULL
  )
}

#' make parameter for method of splitting factors (unordered categorical variables)
#'
#' @importFrom dials new_quant_param new_qual_param grid_max_entropy
#'
#' @param trim_min An integer specifying the minimum value in the parameter grid
#' @param trim_max An integer specifying the maximum value in the parameter grid
#'
#' @return A quantitative parameter, from `{dials}` for catsplit
#' @export
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(maxdepth_min = 2, maxdepth_max = 5),
#'    alpha_par(alpha_min = 0.001, alpha_max = 0.1),
#'    prune_par(),
#'    size = 10
#'    )

prune_par <-
  function(values = c('aic', 'bic', 'null')){

    new_qual_param(
      type = "character",
      values = values,
      label = c(prune_par = "splitting method for (unordered) factors"),
      finalize = NULL
    )

  }


#' make parameter for implementing bonferroni corrections to mutliple testing when splitting.
#'
#' @importFrom dials new_quant_param new_qual_param grid_max_entropy
#'
#' @return A quantitative parameter, from `{dials}` for catsplit
#' @export
#'
#' @param values a vector of possible values; only TRUE/FALSE possible (the default)
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(maxdepth_min = 2, maxdepth_max = 5),
#'    alpha_par(alpha_min = 0.001, alpha_max = 0.1),
#'    bonferroni_par(),
#'    size = 10
#'    )

bonferroni_par <-
  function(values = c(TRUE, FALSE)){

    new_qual_param(
      type = "logical",
      values = values,
      label = c(bonferroni_par = "implement bonferroni corrections to mutliple testing when splitting"),
      finalize = NULL
    )

  }


#' make parameter for determining restart parameter. TRUE = re-estimate downstream estimates of continuous
#' splitting variables when same variable is re-selected. FALSE = start at prior split value
#'
#' @importFrom dials new_quant_param new_qual_param grid_max_entropy
#'
#' @return A quantitative parameter, from `{dials}` for catsplit
#' @export
#'
#' @param values a vector of possible values; only TRUE/FALSE possible (the default)
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(maxdepth_min = 2, maxdepth_max = 5),
#'    alpha_par(alpha_min = 0.001, alpha_max = 0.1),
#'    bonferroni_par(),
#'    size = 10
#'    )

restart_par <-
  function(values = c(TRUE, FALSE)){

    new_qual_param(
      type = "logical",
      values = values,
      label = c(restart_par = "Restart estimation for downstream splits?"),
      finalize = NULL
    )

  }


#' make parameter for determining how to handle ordered categorical splits
#'
#' @importFrom dials new_quant_param new_qual_param grid_max_entropy
#'
#' @return A quantitative parameter, from `{dials}` for catsplit
#' @export
#'
#' @param values a vector of possible values c("chisq", "max", "L2"), see ?mob_control()
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(maxdepth_min = 2, maxdepth_max = 5),
#'    alpha_par(alpha_min = 0.001, alpha_max = 0.1),
#'    bonferroni_par(),
#'    size = 10
#'    )

ordinal_par <-
  function(values = c("chisq", "max", "L2")){

    new_qual_param(
      type = "character",
      values = values,
      label = c(ordinal_par = "how to handle splitting of ordinal variables"),
      finalize = NULL
    )

  }



#' make parameter for `nrep`, number of replications in simulation of p-values with L2 statistic
#'
#' @importFrom dials new_quant_param new_qual_param grid_max_entropy
#'
#' @return A quantitative parameter, from `{dials}` for catsplit
#' @export
#'
#' @param nrep_min minimum of nrep range
#' @param nrep_max maximum of nrep range
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(maxdepth_min = 2, maxdepth_max = 5),
#'    alpha_par(alpha_min = 0.001, alpha_max = 0.1),
#'    nrep_par(1000, 10000),
#'    size = 10
#'    )

nrep_par <-
  function(nrep_min, nrep_max){

    new_quant_param(
      range =  c(nrep_min, nrep_max),
      label = c(nrep_par = "number of reps to estimate L2 statistic p-values"),
      inclusive = c(TRUE, TRUE),
      trans = NULL,
      finalize = NULL
    )

  }


#' make logical parameter which determines if ties should be broken randomly
#' with parameter stability tests of continuous splitting variables
#'
#'
#' @importFrom dials new_quant_param new_qual_param grid_max_entropy
#'
#' @param values a vector of possible values; only TRUE/FALSE possible (the default)
#'
#' @return A quantitative parameter, from `{dials}` for catsplit
#' @export
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(maxdepth_min = 2, maxdepth_max = 5),
#'    alpha_par(alpha_min = 0.001, alpha_max = 0.1),
#'    bonferroni_par(),
#'    size = 10
#'    )

breakties_par <-
  function(values = c(TRUE, FALSE)){

    new_qual_param(
      type = "logical",
      values = values,
      label = c(breakties_par = "implement bonferroni corrections to mutliple testing when splitting"),
      finalize = NULL
    )

  }


#ranefstart <- as.numeric(int_to_character(x[13], 'ranefstart'))

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
#'    alpha_par(alpha_min = 0.001, alpha_max = 0.1),
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
#'    alpha_par(alpha_min = 0.001, alpha_max = 0.1),
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

#' make parameter for method of splitting factors (unordered categorical variables)
#'
#' @importFrom dials new_quant_param new_qual_param grid_max_entropy
#'
#' @param values a vector of catsplit_par values (only 2 options currently available)
#'
#' @return A quantitative parameter, from `{dials}` for catsplit
#' @export
#'
#' @examples
#' dials::grid_max_entropy(
#'    maxdepth_par(maxdepth_min = 2, maxdepth_max = 5),
#'    alpha_par(alpha_min = 0.001, alpha_max = 0.1),
#'    catsplit_par(),
#'    size = 10
#'    )

catsplit_par <-
  function(values = c('binary', 'multiway')){
    new_qual_param(
      type = "character",
      values = values,
      label = c(catsplit_par = "splitting method for (unordered) factors"),
      finalize = NULL
    )
  }

