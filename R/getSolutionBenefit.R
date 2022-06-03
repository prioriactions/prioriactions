#' @title Extract benefit values
#'
#' @description Returns the total benefit induced by the corresponding solution. The
#' total benefit is computed as the sum of the benefits obtained, for all features, across
#' all the units in the planning area.
#'
#' @param x `Solution-class` or `Portfolio-class`.
#'
#' @param type `character`. Output format of the benefits matrix; `total` shows
#' the total benefit by feature, while `local` format shows the benefit achieved per feature and planning unit.
#'
#' @details For a given feature \eqn{s}, let \eqn{I_s} be the set of planning units associated with \eqn{s},
#' let \eqn{r_{is}} is the amount of feature \eqn{s} in planning unit \eqn{i}, let \eqn{K_{s}} be the
#' set of threats associated with \eqn{s}, and let \eqn{K_{i}} be the set of threats associated with \eqn{i}.
#' The local benefit associated with \eqn{s} in a unit \eqn{i} is given by:
#'
#' \deqn{
#' b_{is} = p_{is} r_{is} \\
#' b_{is} = \frac{ \sum_{k \in K_i \cap K_s}{x_{ik}}}{|K_i \cap K_s|} r_{is}
#' }
#'
#' Where \eqn{x_{ik}} is a decision variable such that \eqn{x_{ik} = 1} if an
#' action againts threat \eqn{k} is applied in unit \eqn{i}, and \eqn{x_{ik} = 0}, otherwise.
#' This expression for the probability of persistence of the feature (\eqn{p_{is}})
#' is defined only for the cases where we work with values of binary intensities
#' (presence or absence of threats). See the [sensitivities](https://prioriactions.github.io/prioriactions/articles/sensitivities.html)
#' vignette to know the work with continuous intensities.
#'
#' While the total benefit is calculated as the sum of the local benefits per feature:
#'
#' \deqn{
#' b_{s} = \sum_{i \in I_{s}}\frac{ \sum_{k \in K_i \cap K_s}{x_{ik}}}{|K_i \cap K_s|} r_{is}
#' }
#'
#' @return [data.frame].
#'
#' @examples
#' \donttest{
#' # set seed for reproducibility
#' set.seed(14)
#'
#' ## Load data
#' data(sim_pu_data, sim_features_data, sim_dist_features_data,
#' sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
#' sim_boundary_data)
#'
#' ## Create data instance
#' problem_data <- inputData(
#'   pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
#' )
#'
#' ## Get maximum benefits to obtain
#' getPotentialBenefit(problem_data)
#'
#' ## Create optimization model
#' problem_model <- problem(x = problem_data)
#'
#' ## Solve the optimization model
#' s <- solve(a = problem_model, time_limit = 2, output_file = FALSE, cores = 2)
#'
#' # get local benefits of solution
#' local_benefit <- getSolutionBenefit(s, type = "local")
#' head(local_benefit)
#'
#' # get total benefits of solution
#' total_benefit <- getSolutionBenefit(s, type = "total")
#' head(total_benefit)
#' }
#'
#' @name getSolutionBenefit
NULL

#' @rdname getSolutionBenefit
#' @export
getSolutionBenefit <- function(x, type = "total") {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution", "Portfolio")),
                          type %in% c("total", "local"))

  if(inherits(x, "Solution")){
    statusCode <- x$data$status

    if(!(statusCode %in% c(1,3))){
      if(type == "total"){
        solution_units <- x$data$sol_monitoring
        solution_actions <- x$data$sol_actions

        benefits <- rcpp_stats_benefit(x$OptimizationClass$ConservationClass$data$pu,
                                       x$OptimizationClass$ConservationClass$data$features,
                                       x$OptimizationClass$ConservationClass$data$dist_features,
                                       x$OptimizationClass$ConservationClass$data$threats,
                                       x$OptimizationClass$ConservationClass$data$dist_threats,
                                       x$OptimizationClass$ConservationClass$data$sensitivity,
                                       c(solution_units, solution_actions))

        benefits$solution_name <- x$name
      }
      else{
        #variables
        benefits <- x$OptimizationClass$ConservationClass$getData("dist_features")
        benefits <- benefits[!names(benefits) %in% c("amount","internal_feature","internal_pu")]
        benefits <- benefits[order(benefits$feature, benefits$pu), ]

        #getting information of solutions
        recovery_ben <- x$data$sol_recovery
        conservation_ben <- x$data$sol_conservation
        benefits$benefit.conservation <- conservation_ben
        benefits$benefit.recovery <- recovery_ben
        benefits$benefit.total <- recovery_ben + conservation_ben

        #Exporting local benefits
        benefits <- benefits[order(benefits$pu), ]
        benefits <- cbind(solution_name = x$name, benefits)
      }
      return(benefits)
    }
    else{
      return(NA)
    }
  }
  else if(inherits(x, "Portfolio")){
    benefits = c()

    for(it in seq_len(length(x$data))){

      benefit_solution <- getSolutionBenefit(x$data[[it]], type = type)
      benefits <- rbind(benefits, benefit_solution)
    }
    return(benefits)
  }
}
