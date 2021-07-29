#' @title writeOutputs
#'
#' @description Put description here!
#'
#' @param x Put description of a parameter here!
#'
#' @name writeOutputs
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
writeOutputs <- function(x, ...) UseMethod("writeOutputs", x)

#' @rdname writeOutputs
#' @method writeOutputs Solution
#' @noRd
writeOutputs.Solution <- function(x, name = "output_prioriactions", ...) {
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
    "blm:  ", x$OptimizationClass$data$settings$blm,
    "\n",
    "blm_actions:  ", paste0(x$OptimizationClass$ConservationClass$data$threats$blm_actions, collapse=" "),
    "\n",
    "exponent of benefit expression  (curve parameter):  ", x$OptimizationClass$data$settings$curve,
    "\n",
    "number of linearization segments of benefit expression (segments parameter):  ", x$OptimizationClass$data$settings$segments,
    "\n",
    "solver:  ", x$data$arg$solver,
    "\n",
    "gap:  ", paste0(x$data$arg$gap, "%"),
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
    "Type of model:  ", getModelSense(x),
    "\n",
    "Number of variables:  ", getNvariables(x),
    "\n",
    "Number of constraints:  ", getNconstraints(x),
    "\n",
    "\n",
    "4) Solution",
    "\n",
    "Objective value:  ", getObjectiveValue(x),
    "\n",
    "Gap:  ", getGap(x),
    "\n",
    "Status:  ", getStatus(x),
    "\n",
    "Runtime [sec]:  ", getTimeSolving(x),
    "\n",
    "Total cost:  ", getTotalCost(x),
    "\n",
    "  Monitoring cost:  ", getPlanningUnitsCost(x),
    "\n",
    "  Action cost:  ", getActionsCost(x),
    "\n",
    "Total connectivity:  ", getTotalConnectivity(x),
    "\n",
    "  Unit connectivity:  ", getPlanningUnitsConnectivity(x),
    "\n",
    "  Action connectivity:  ", getActionsConnectivity(x),
    "\n"
  )

  base::writeLines(msj_output, output_file)

  base::on.exit(base::close(output_file))
}
