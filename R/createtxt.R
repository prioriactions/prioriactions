#' @title createtxt
#'
#' @description Put description here!
#'
#' @param x Put description of a parameter here!
#'
#' @name createtxt
#'
#' @return Put the "return" object here!
#'
#' @details Put details here!
#'
#' @seealso Put "seealso" comments!
#'
#' @examples
#' ## Put examples here!
#' @references
#' ## Put references here!
#' @noRd
createtxt <- function(x, ...) UseMethod("createtxt", x)

#' @rdname createtxt
#' @method createtxt Solution
#' @noRd
createtxt.Solution <- function(x, name = "output_prioriactions", ...) {
  assertthat::assert_that(inherits(x, "Solution"))

  name_output <- paste0(name,".txt")
  output_file <- base::file(name_output)

  msj_output <- paste0(
    "=============================================",
    "\n",
    "======== prioriactions output file ==========",
    "\n",
    "=============================================",
    "\n",
    "\n",
    "1) Parameters",
    "\n",
    "blm:  ", x$OptimizationClass$data$arg$beta1,
    "\n",
    "blm_actions:  ", x$OptimizationClass$data$arg$beta2,
    "\n",
    "exponent of benefit expression  (curve parameter):  ", x$OptimizationClass$data$arg$exponent,
    "\n",
    "number of linearization segments of benefit expression (segments parameter):  ", x$OptimizationClass$data$arg$segments,
    "\n",
    "solver:  ", x$data$arg$solver,
    "\n",
    "gap:  ", x$data$arg$gap,
    "\n",
    "time limit:  ", x$data$arg$timelimit,
    "\n",
    "computer cores:  ", x$data$arg$cores,
    "\n",
    "verbose?:  ", x$data$arg$verbose,
    "\n",
    "solution limit? (stop whth the first solution found):  ", x$data$arg$solution_limit,
    "\n",
    "output file?:  ", x$data$arg$output_file,
    "\n",
    "name of output prioriactions:  ", paste0(x$data$arg$name_output_file,".txt"),
    "\n",
    "log file? (only gurobi):  ", x$data$arg$log_file,
    "\n",
    "name of log solver (only gurobi):  ", paste0(x$data$arg$name_log,".txt"),
    "\n",
    "\n",
    "2) Instance information",
    "\n",
    "Number of planning units:  ", x$OptimizationClass$ConservationClass$getPlanningUnitsAmount(),
    "\n",
    "Number of features:  ", x$OptimizationClass$ConservationClass$getFeatureAmount(),
    "\n",
    "Number of threats:  ", x$OptimizationClass$ConservationClass$getThreatsAmount(),
    "\n",
    "\n",
    "3) Mathematical model",
    "\n",
    "Type of model:  ", x$OptimizationClass$getModelSense(),
    "\n",
    "Number of variables:  ", x$OptimizationClass$getNcol(),
    "\n",
    "Time to create model [sec]:  ", x$OptimizationClass$getTimeBuildingModel(),
    "\n",
    "\n",
    "4) Solution",
    "\n",
    "Objective value:  ", x$getObjetiveValue(),
    "\n",
    "Gap:  ", x$getGap(),
    "\n",
    "Status:  ", x$getStatus(),
    "\n",
    "Runtime [sec]:  ", x$getTimeSolvingModel(),
    "\n",
    "Total cost:  ", x$getTotalCost(),
    "\n",
    "  Monitoring cost:  ", x$getMonitoringCost(),
    "\n",
    "  Action cost:  ", x$getActionsCost(),
    "\n",
    "Total connectivity:  ", x$getTotalConnectivity(),
    "\n",
    "  Unit connectivity:  ", x$getUnitConnectivity(),
    "\n",
    "  Action connectivity:  ", x$getActionConnectivity(),
    "\n"
  )

  base::writeLines(msj_output, output_file)

  base::on.exit(base::close(output_file))

  output_file
}
