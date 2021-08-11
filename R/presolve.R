#' @title presolve
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
presolve <- function(x, ...) UseMethod("presolve", x)

#' @rdname presolve
#' @method presolve ConservationProblem
#' @noRd
presolve.ConservationProblem <- function(x, name_model = "minimizeCosts", recovery = TRUE, budget = 0) {

  assertthat::assert_that(inherits(x, "ConservationProblem"))

  if (name_model == "minimizeCosts") {

    pu <- x$getData("pu")
    locked_out_units <- pu$id[c(which(pu$status == 3))]
    pu$solution <- 1
    pu$solution[locked_out_units] <- 0


    dist_threats <- x$getData("dist_threats")
    locked_out_actions <- pu$id[c(which(dist_threats$status == 3))]
    dist_threats$solution <- 1
    dist_threats$solution[locked_out_actions] <- 0

    targets <- x$data$features$target

    a <- rcpp_stats_benefit(x$data$pu,
                                      x$data$features,
                                      x$data$dist_features,
                                      x$data$threats,
                                      x$data$dist_threats,
                                      x$data$sensitivity,
                                      c(pu$solution, dist_threats$solution),
                                      recovery)


    if(recovery){

      diff_features <- which(targets > a$benefit.recovery)

      if(length(diff_features) > 0){
        warning(paste0("Infeasible model. There is not enough representativeness to achieve the targets required of following features: ", paste(a$feature[diff_features], collapse = " ")),
                call.=FALSE, immediate. = TRUE)

        x$data$features$target[diff_features] <- a$benefit.recovery[diff_features] - 10**(-4)
        warning("The targets for these features will be set to the maximum benefit values", call.=FALSE, immediate. = TRUE)
      }
    }
    else{
      diff_features <- which(targets > a$benefit.total)

      if(length(diff_features) > 0){
        warning(paste0("Infeasible model. There is not enough representativeness to achieve the targets required of following features: ", paste(a$feature[diff_features], collapse = " ")),
                call.=FALSE, immediate. = TRUE)

        x$data$features$target[diff_features] <- a$benefit.total[diff_features]- 10**(-4)

        warning("The targets for these features will be set to the maximum benefit values", call.=FALSE, immediate. = TRUE)
      }
    }
  }
  else if(name_model == "maximizeBenefits"){
    pu <- x$getData("pu")
    locked_in_units <- pu$id[c(which(pu$status == 2))]
    pu$solution <- 0
    pu$solution[locked_in_units] <- 1


    dist_threats <- x$getData("dist_threats")
    locked_in_actions <- pu$id[c(which(dist_threats$status == 2))]
    dist_threats$solution <- 0
    dist_threats$solution[locked_in_actions] <- 1


    costs_units <- rcpp_stats_costs_units(pu, pu$solution)
    costs_actions <- rcpp_stats_costs_actions(dist_threats, dist_threats$solution)

    if(budget < sum(costs_actions) + sum(costs_units)){
      warning("Infeasible model. There is not enough budget to achieve the actions required (lock-in)",
              call.=FALSE, immediate. = TRUE)

      warning(paste0("The budget will be set as the sum of the minimum costs to achieve the required actions: ", sum(costs_actions) + sum(costs_units)), call.=FALSE, immediate. = TRUE)

      return(sum(costs_actions) + sum(costs_units))
    }
    else{
      return(budget)
    }
  }
}
