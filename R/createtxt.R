#' @title createtxt
#'
#' @description Put description here!
#'
#' @param x Put description of a parameter here!
#'
#' @name presolve
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
    "1) Data instance",
    "\n",
    "Number of planning units:  ", x$OptimizationClass$ConservationClass$getPlanningUnitsAmount(),
    "\n",
    "Number of features:  ", x$OptimizationClass$ConservationClass$getFeatureAmount(),
    "\n",
    "Number of threats:  ", x$OptimizationClass$ConservationClass$getThreatsAmount(),
    "\n",
    "\n",
    "2) Mathematical model",
    "\n",
    "Type of model:  ", x$OptimizationClass$getModelSense(),
    "\n",
    "Number of variables:  ", x$OptimizationClass$getNcol(),
    "\n",
    "Time to create model [sec]:  ", x$OptimizationClass$getTimeBuildingModel(),
    "\n",
    "blm:  ", x$OptimizationClass$data$arg$beta1,
    "\n",
    "blm actions:  ", x$OptimizationClass$data$arg$beta2,
    "\n",
    "Exponent of benefit expression:  ", x$OptimizationClass$data$arg$exponent,
    "\n",
    "Number of linearization segments of benefit expression:  ", x$OptimizationClass$data$arg$segments,
    "\n",
    "\n",
    "3) Solution",
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
    "\n",
    "\n",
    "4) Statistics",
    "\n"
  )

  base::writeLines(msj_output, output_file)

  base::on.exit()

  output_file
}
