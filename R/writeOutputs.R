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
writeOutputs.Solution <- function(x, name = "output", ...) {
  assertthat::assert_that(inherits(x, "Solution"))

  #parameters file---------------------------------------------

  name_output_params <- paste0(name,"_params.txt")
  output_file <- base::file(name_output_params)

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
    "blm:  ", x$OptimizationClass$data$args$blm,
    "\n",
    "budget:  ", x$OptimizationClass$data$args$budget,
    "\n",
    "curve:  ", x$OptimizationClass$data$args$curve,
    "\n",
    "segments: ", x$OptimizationClass$data$args$segments,
    "\n",
    "solver:  ", x$data$arg$solver,
    "\n",
    "gap_limit:  ", paste0(x$data$arg$gap, "%"),
    "\n",
    "time limit:  ", x$data$arg$timelimit,
    "\n",
    "solution limit:  ", x$data$arg$solution_limit,
    "\n",
    "cores:  ", x$data$arg$cores,
    "\n",
    "verbose:  ", x$data$arg$verbose,
    "\n",
    "name_output_file: ", x$data$arg$name_output_file,
    "\n",
    "output_file:  ", x$data$arg$output_file,
    "\n",
    "\n",
    "2) Instance information",
    "\n",
    "number of planning units:  ", x$OptimizationClass$ConservationClass$getPlanningUnitsAmount(),
    "\n",
    "number of features:  ", x$OptimizationClass$ConservationClass$getFeatureAmount(),
    "\n",
    "number of threats:  ", x$OptimizationClass$ConservationClass$getThreatsAmount(),
    "\n",
    "number of actions:  ", x$OptimizationClass$ConservationClass$getActionsAmount(),
    "\n",
    "\n",
    "3) Mathematical model",
    "\n",
    "type of model:  ", x$OptimizationClass$data$args$name_model,
    "\n",
    "number of variables:  ", base::ncol(x$OptimizationClass$data$A),
    "\n",
    "number of constraints:  ", base::nrow(x$OptimizationClass$data$A),
    "\n",
    "\n",
    "4) Solution",
    "\n",
    "objective value:  ", x$data$objval,
    "\n",
    "gap achieved:  ", x$data$gap,
    "\n",
    "runtime:  ", x$data$runtime,
    "\n",
    "total cost:  ", sum(as.numeric(prioriactions::getCost(x)[-1])),
    "\n",
    "cost of units:  ", sum(as.numeric(prioriactions::getCost(x)[2])),
    "\n",
    "cost of actions:  ", sum(as.numeric(prioriactions::getCost(x)[-c(1, 2)]))
  )

  base::writeLines(msj_output, output_file)

  base::on.exit(base::close(output_file))


  #writing benefits
  name_output_benefits <- paste0(name,"_benefits.txt")
  utils::write.csv(getSolutionBenefit(x, type = "local"), file = name_output_benefits, row.names = FALSE)

  #writing actions
  name_output_actions <- paste0(name,"_actions.txt")
  utils::write.csv(getActions(x, format = "large"), file = name_output_actions, row.names = FALSE)
}
