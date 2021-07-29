#' @include internal.R

#' @export
if (!methods::isClass("Solution")) methods::setOldClass("Solution")
NULL

#' Solution class
#'
#' This class is used to represent the solution of the MIP (Mixed-Integer Programming) model
#' related to the multi-action conservation planning problem. This includes several methods
#' to obtain information about both the optimization process and the solution associated with
#' the planning units and conservation actions. It is created using the \code{\link{solve}}
#' function.
#'
#' @section Fields:
#' \describe{
#' \item{$data}{\code{list} object containing data on the results of the optimization process.}
#' }
#'
#' @section Methods:
#' \describe{
#' \item{$getGap( )}{returns a \code{string} label indicating the optimality gap achieved for the MIP model.}
#'

#' \item{$getObjectiveValue( )}{returns a \code{numeric} number indicating the value of the objective function at the optimum.}
#'
#' \item{$getSolutionActions( )}{
#' returns a \code{data.frame} object interpreting the optimal solution of the MIP model
#' that relates to the conservation actions. It contains information on the conservation actions
#' that are suggested (value 1) and those that are not suggested (value 0) within the conservation plan.}
#'
#' \item{$getSolutionUnits( )}{
#' returns a \code{data.frame} object interpreting the optimal solution of the MIP model
#' that relates to the planning units. It contains information on the planning units that are
#' suggested to be included (value 1) and not included (value 0) within the conservation plan.}
#'
#' \item{$print( )}{print basic information of the model solution.}
#'
#' \item{$show( )}{call print method.}
#'
#' }
#'
#' @examples
#' ## Examples of how to use the methods of a Solution class object.
#'
#' ## Load data
#' data(example_pu_data, example_features_data, example_rij_data, example_threats_data, example_sensitivity_data, example_bound_data)
#'
#' ## Create data instance
#' problem_data <- problem(
#'   pu = example_pu_data, features = example_features_data, rij = example_rij_data,
#'   threats = example_threats_data, sensitivity = example_sensibility_data,
#'   bound = example_bound_data
#' )
#'
#' ## Create optimization model
#' problem_model <- min_costs(problem_data, blm = 1, blm_actions = 1)
#'
#' ## Solve the optimization model using a default solver
#' model_solution <- solve(a = problem_model, verbose = FALSE)
#'
#' ## Use class methods
#' model_solution$getGap()
#'
#' model_solution$getObjectiveValue()
#'
#' head(model_solution$getSolutionActions())
#'
#' head(model_solution$getSolutionUnits())
#'
#' model_solution$print()
#' @name Solution-class
#'
#' @aliases Solution
NULL

#' @export
Solution <- pproto(
  "Solution",
  data = list(),
  OptimizationClass = NULL,
  print = function(self) {
    message(
      "Solution overview",
      "\n  objective value: ", self$getObjectiveValue(),
      "\n  gap:  ", self$getGap(),
      "\n  status:  ", self$getStatus(),
      "\n  runtime: ", paste0(self$getTimeSolvingModel(), " sec")
    )
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    "Solution object"
  },
  createtxtFile = function(self) {
    invisible(createtxt(self))
  },
  getGap = function(self) {
    if (self$data$gap == "No reported" || is.null(self$data$gap)) {
      return("No reported")
    }
    else {
      return(paste0(base::round(self$data$gap * 100, 4), "%"))
    }
  },
  getObjectiveValue = function(self) {
    return(self$data$objval)
  },
  getSolutionUnits = function(self) {
    return(self$data$sol_pu)
  },
  getSolutionActions = function(self, format = "extended") {
    if (format == "extended") {
      return(self$data$sol_actions_extended)
    }
    else if (format == "reduced") {
      return(self$data$sol_actions_reduced)
    }
    else {
      paste0("The type should be 'extended' or 'reduced'")
    }
  },
  getBenefits = function(self) {

    objective <- self$getObjectiveValue()

    if(!is.null(objective)){
      solution_units <- self$getSolutionUnits()
      solution_actions <- self$getSolutionActions(format = "reduced")
    }
    else{
      solution_units <- c()
      solution_actions <- c()

      solution_units$solution <- 0
      solution_actions$solution < 0
    }

    a <- rcpp_stats_benefit(self$OptimizationClass$ConservationClass$data$pu,
                            self$OptimizationClass$ConservationClass$data$features,
                            self$OptimizationClass$ConservationClass$data$dist_features,
                            self$OptimizationClass$ConservationClass$data$threats,
                            self$OptimizationClass$ConservationClass$data$dist_threats,
                            self$OptimizationClass$ConservationClass$data$sensitivity,
                            c(solution_units$solution, solution_actions$solution))
    return(a)
  },
  getLocalBenefits = function(self) {
    return(self$data$local_benefits)
  },
  getStatusCode = function(self) {
    return(self$data$status)
  },
  getStatus = function(self) {
    # status_solution <- dplyr::case_when(
    #   status_mip == "OPTIMAL" ~ paste0("Optimal solution (according to gap tolerance: ", gap_limit * 100, "%)"),
    #   (status_mip == "INF_OR_UNBD" || status_mip == "INFEASIBLE" || status_mip == "UNBOUNDED") ~ "No solution (model was proven to be infeasible or unbounded)",
    #   (status_mip == "TIME_LIMIT" && !is.null(solution$objval)) ~ paste0("Feasible solution (according to time limit: ", time_limit, "[s])"),
    #   (status_mip == "TIME_LIMIT" && is.null(solution$objval)) ~ paste0("No solution (according to time limit: ", time_limit, "[s])"),
    #   (status_mip == "SOLUTION_LIMIT") ~ "First feasible solution",
    #   TRUE ~ "No solution information is available"
    # )
    #
    # solution$status_mip <- dplyr::case_when(
    #   (status_mip == 0L || status_mip == 231L) ~ paste0("Optimal solution (according to gap tolerance: ", gap_limit * 100, "%)"),
    #   (status_mip == 226L || status_mip == 237L) ~ "No solution (model was proven to be infeasible or unbounded)",
    #   (status_mip == 228L) ~ paste0("Feasible or unfeasible solution (according to time limit: ", time_limit, "[s])"),
    #   (status_mip == 235L) ~ paste0("Feasible solution (according to time limit: ", time_limit, "[s])"),
    #   (status_mip == 232L) ~ "First feasible solution",
    #   TRUE ~ "No solution information is available"
    # )
    statusCode <- self$getStatusCode()
    gap <- self$getGap()
    time_limit <- self$data$arg$timelimit
    if(statusCode == 0L){
      return(paste0("Optimal solution (according to gap tolerance: ", gap,")"))
    }
    else if(statusCode == 1L){
      return("No solution (model was proven to be infeasible or unbounded)")
    }
    else if(statusCode == 2L){
      return(paste0("Feasible solution (according to time limit: ", time_limit, " sec)"))
    }
    else if(statusCode == 3L){
      return(paste0("No solution (according to time limit: ", time_limit, " sec)"))
    }
    else if(statusCode == 4L){
      return("First feasible solution")
    }
    else{
      return("No solution information is available")
    }
  },
  getMonitoringCost = function(self) {

    solution_units <- self$getSolutionUnits()

    a <- rcpp_stats_costs_units(self$OptimizationClass$ConservationClass$data$pu,
                                c(solution_units$solution))
    return(sum(a))
  },
  getActionsCost = function(self) {
    solution_actions <- self$getSolutionActions(format = "reduced")

    a <- rcpp_stats_costs_actions(self$OptimizationClass$ConservationClass$data$dist_threats,
                                c(solution_actions$solution))
    return(sum(a))
  },
  getTotalCost = function(self) {
    return(self$getActionsCost() + self$getMonitoringCost())
  },
  getUnitConnectivity = function(self) {

    boundary_data <- self$OptimizationClass$ConservationClass$data$boundary

    if(is.null(boundary_data)){
      return("No boundary data available")
    }
    else{
      solution_units <- self$getSolutionUnits()
      connectivity <- rcpp_stats_connectivity_units(self$OptimizationClass$ConservationClass$data$pu,
                                                    boundary_data,
                                                    c(solution_units$solution))
      return(sum(connectivity))
    }
  },
  getActionConnectivity = function(self) {
    boundary_data <- self$OptimizationClass$ConservationClass$data$boundary

    if(is.null(boundary_data)){
      return("No boundary data available")
    }
    else{
      solution_actions <- self$getSolutionActions(format = "reduced")
      connectivity <- rcpp_stats_connectivity_actions(self$OptimizationClass$ConservationClass$data$pu,
                                                      self$OptimizationClass$ConservationClass$data$threats,
                                                      self$OptimizationClass$ConservationClass$data$dist_threats,
                                                      self$OptimizationClass$ConservationClass$data$boundary,
                                                      c(solution_actions$solution))
      return(sum(connectivity))
    }
  },
  getTotalConnectivity = function(self) {
    boundary_data <- self$OptimizationClass$ConservationClass$data$boundary

    if(is.null(boundary_data)){
      return("No boundary data available")
    }
    else{
      return(self$getActionConnectivity() + self$getUnitConnectivity())
    }
  },
  getTimeSolvingModel = function(self) {
    return(base::round(self$data$runtime, 2))
  },
)
