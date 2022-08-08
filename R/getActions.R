#' @title Extract action information
#'
#' @description Returns the spatial deployment of the actions for each planning unit of
#' the corresponding solution.
#'
#' @param x [solution-class] or [portfolio-class] object.
#'
#' @param format `character`. Output format of the action matrix; `wide` format
#' shows one column per action, while `large` format shows four columns: solution_name, pu,
#' action and solution.
#'
#' @details `getActions()` function assumes that actions can be of three types:
#' 1) **to abate specific threats**: these actions have the *id* corresponding
#' to the threat to be abate.
#' 2) **to conservation**: that indicates if the planning unit is selected to
#' conservative any feature that is not threatened.
#' 3) **to connectivity**: that indicates if the planning
#' unit is selected only by connectivity (i.e. without performing conservation actions
#' or actions against a threat in said unit).
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
#' ## Create instance
#' problem_data <- inputData(
#'   pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
#' )
#'
#' ## Create optimization model
#' problem_model <- problem(x = problem_data)
#'
#' ## Solve the optimization model
#' s <- solve(a = problem_model, time_limit = 2, output_file = FALSE, cores = 2)
#'
#' # get actions information in large format
#' actions <- getActions(s, format = "large")
#' head(actions)
#'
#' # get actions information in wide format
#' actions <- getActions(s, format = "wide")
#' head(actions)
#' }
#'
#' @name getActions
#'
NULL

#' @rdname getActions
#' @importFrom rlang .data
#' @export
getActions <- function(x, format = "wide") {
  # assert argument is valid
  assertthat::assert_that(
    inherits(x, c("Solution", "Portfolio")),
    format %in% c("large", "wide"))

  if(inherits(x, "Solution")){

    statusCode <- x$data$status

    if(!(statusCode %in% c(1,3))){

      #variables
      # Getting monitoring solution
      monitoring <- x$OptimizationClass$ConservationClass$getData("pu")
      monitoring <- monitoring[!names(monitoring) %in% c("monitoring_cost", "status", "internal_id")]

      # Getting actions solution
      actions <- x$OptimizationClass$ConservationClass$getData("dist_threats")
      actions <- actions[names(actions) %in% c("pu","threat")]
      actions <- actions %>% dplyr::rename("action" = "threat")

      # Getting local benefits
      benefits <- x$OptimizationClass$ConservationClass$getData("dist_features")
      benefits <- benefits[!names(benefits) %in% c("amount","internal_feature","internal_pu")]
      benefits <- benefits[order(benefits$feature), ]

      #assign solutions to data.frame
      monitoring$solution <- x$data$sol_monitoring
      actions$solution <- x$data$sol_actions

      #Preparing action file: conservation
      benefits <- benefits[order(benefits$feature),]
      benefits$conservation <- x$data$sol_conservation
      benefits <- benefits[order(benefits$pu), ]

      complete_dist_features <- merge(x = monitoring, y = benefits, by.x = "id", by.y = "pu", all = TRUE)
      complete_dist_features <- complete_dist_features %>% tidyr::spread(key = .data$feature, value = conservation)

      complete_dist_features$`0` <- 0
      sum_rows <- rowSums(complete_dist_features[,!names(complete_dist_features) %in% c("id", "solution","<NA>")], na.rm = TRUE)
      conservation <- ifelse(sum_rows > 0, 1, 0)

      #Preparing action file: actions in sites
      complete_dist_threats <- merge(x = monitoring, y = actions, by.x = "id", by.y = "pu", all = TRUE)
      threats_data_extended <- complete_dist_threats %>% tidyr::spread(key = .data$action, value = .data$solution.y)

      threats_data_extended$`0` <- 0
      sum_rows_threats <- rowSums(threats_data_extended[,!names(threats_data_extended) %in% c("id", "solution.x", "<NA>")], na.rm = TRUE)
      threats_units <- ifelse(sum_rows_threats > 0, 1, 0)

      conservation_and_actions <- ifelse(conservation + threats_units >= 1, 1, 0)
      connectivity <- monitoring$solution - conservation_and_actions

      #Creating columns of new action file
      monitoring$solution <- conservation
      colnames(monitoring) <- c("pu", "solution")
      monitoring[,"action"] <- "conservation"
      actions <- rbind(actions, monitoring)

      monitoring$solution <- connectivity
      monitoring[,"action"] <- "connectivity"
      actions <- rbind(actions, monitoring)


      if(format == "large"){
        actions <- cbind(solution_name = x$name, actions)
        return(actions)
      }
      else{
        actions_wide <- actions[names(actions) %in% c("solution_name","pu","action","solution")]
        actions_wide <- actions_wide %>% tidyr::spread(key = .data$action, value = .data$solution, fill = 0)
        actions_wide <- base::round(actions_wide, digits = 1)
        actions_wide <- actions_wide[,c(1:(ncol(actions_wide) - 2), ncol(actions_wide), ncol(actions_wide) - 1)]
        actions_wide <- cbind(solution_name = x$name, actions_wide)

        return(actions_wide)
      }
    }
    else{
      return(NA)
    }
  }
  else if(inherits(x, "Portfolio")){

    actions = c()

    for(it in seq_len(length(x$data))){

      actions_solution <- getActions(x$data[[it]], format = format)
      actions <- rbind(actions, actions_solution)
    }
    return(actions)
  }
}
