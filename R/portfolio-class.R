#' @include internal.R

#' @export
if (!methods::isClass("Portfolio")) methods::setOldClass("Portfolio")
NULL

#' Portfolio class
#'
#' This class encodes for the solutions obtained when solving
#' multiple instances. This includes several methods
#' to obtain information about both the optimization process and the solution associated with
#' the planning units and conservation actions. It is created using the *eval* functions
#' (e.g. `evalTarget()` or `evalBudget()`).
#'
#' @section Fields:
#' \describe{
#' \item{$data}{
#' `list`. Object containing data on the results of the optimization process.}
#' }
#'
#' @section Methods:
#' \describe{
#' \item{getNames()}{
#' `character`. Label indicating the name of solutions.}
#'
#' \item{print()}{
#' Print basic information of the model solution.}
#'
#' \item{show()}{
#' Call print method.}
#' }
#'
#' @return No return value.
#' @examples
#' \donttest{
#' # set seed for reproducibility
#' set.seed(14)
#'
#' ## Create model and solve
#' port <- evalBlm(pu = sim_pu_data, features = sim_features_data,
#'                 dist_features = sim_dist_features_data,
#'                 threats = sim_threats_data,
#'                 dist_threats = sim_dist_threats_data,
#'                 sensitivity = sim_sensitivity_data,
#'                 boundary = sim_boundary_data,
#'                 values = c(0.0, 0.01, 0.02, 0.03),
#'                 model_type = "minimizeCosts",
#'                 time_limit = 50,
#'                 output_file = FALSE, cores = 2)
#'
#' ## Use class methods
#' port$getNames()
#'
#' port$print()
#' }
#' @name portfolio-class
#'
#' @aliases Portfolio
NULL

#' @export
Portfolio <- pproto(
  "Portfolio",
  data = list(),
  print = function(self) {
    message(
      "Portfolio overview",
      "\n  solutions: ", length(self$data)
    )
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    "Portfolio object"
  },
  getNames = function(self) {
    return_list <- NULL

    for(it in seq_len(length(self$data))){

      out <- self$data[[it]]$name

      return_list <- c(return_list, out)
    }

    return(return_list)
  },
  getBlms = function(self) {
    return_list <- NULL

    for(it in seq_len(length(self$data))){

      out <- self$data[[it]]$OptimizationClass$data$args$blm

      return_list <- c(return_list, out)
    }

    return(return_list)
  }
)
