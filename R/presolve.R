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
#' @method presolve Data
#' @noRd
presolve.Data <- function(x, ...) {

  assertthat::assert_that(inherits(x, "Data"))

  params = list(...)


  if (params$model_type == "minimizeCosts") {

    pu <- x$getData("pu")
    dist_threats <- x$getData("dist_threats")
    locked_out_units <- c(which(pu$status == 3))
    pu$solution <- 1
    pu$solution[locked_out_units] <- 0
    dist_threats$solution <- 1

    locked_out_actions <- any(dist_threats$internal_pu %in% pu$internal_id[locked_out_units])
    if(isTRUE(locked_out_actions)){
      dist_threats[dist_threats$internal_pu %in% pu$internal_id[locked_out_units], ]$solution <- 0
    }

    locked_out_actions <- c(which(dist_threats$status == 3))
    dist_threats$solution[locked_out_actions] <- 0

    a <- getPotentialBenefit(x)


    diff_features <- which(params$features$target_recovery > a$maximum.recovery.benefit)

    if(length(diff_features) > 0){
      warning(paste0("Infeasible model. There is not enough representativeness to achieve the targets of recovery required of following features: ",
                     paste(a$feature[diff_features], collapse = " "), ". \n For more information on the maximum benefits use the getPotentialBenefit() function."),
              call.=FALSE, immediate. = TRUE)

      params$features$target_recovery[diff_features] <- a$maximum.recovery.benefit[diff_features] - 10**(-1)
      warning("The targets of recovery for these features will be set to the maximum recovery benefit values", call.=FALSE, immediate. = TRUE)
    }

    diff_features <- which(params$features$target_conservation > a$maximum.conservation.benefit)

    if(length(diff_features) > 0){
      warning(paste0("Infeasible model. There is not enough representativeness to achieve the targets of conservation required of following features: ",
                     paste(a$feature[diff_features], collapse = " "), ". \n For more information on the maximum benefits use the getPotentialBenefit() function."),
              call.=FALSE, immediate. = TRUE)

      params$features$target_conservation[diff_features] <- a$maximum.conservation.benefit[diff_features] - 10**(-1)
      warning("The targets of conservation for these features will be set to the maximum convervation benefit values", call.=FALSE, immediate. = TRUE)
    }

    return(params$features)
  }
  else if(params$model_type == "maximizeBenefits"){

    pu <- x$getData("pu")
    locked_in_units <- pu$id[c(which(pu$status == 2))]
    pu$solution <- 0
    pu$solution[locked_in_units] <- 1

    threats <- x$getData("threats")
    dist_threats <- x$getData("dist_threats")
    locked_in_actions <- which(dist_threats$status == 2)
    dist_threats$solution <- 0
    dist_threats$solution[locked_in_actions] <- 1
    pu$solution[dist_threats$pu[locked_in_actions]] <- 1

    costs_units <- rcpp_stats_costs_units(pu, pu$solution)
    costs_actions <- rcpp_stats_costs_actions(pu, threats, dist_threats, dist_threats$solution)

    if(params$budget < sum(costs_actions) + costs_units){
      warning("Infeasible model. There is not enough budget to achieve the actions required (lock-in)",
              call.=FALSE, immediate. = TRUE)

      warning(paste0("The budget will be set as the sum of the minimum costs to achieve the required actions: ", sum(costs_actions) + costs_units), call.=FALSE, immediate. = TRUE)

      return(sum(costs_actions) + costs_units + 10**(-1))
    }
    else{
      return(params$budget)
    }
  }
}
