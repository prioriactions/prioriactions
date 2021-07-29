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
presolve.ConservationProblem <- function(x, objective = "min costs", recovery = FALSE, ...) {
  assertthat::assert_that(inherits(x, "ConservationProblem"))

  if (objective == "min costs") {

    pu <- x$getData("pu")
    locked_out_units <- pu$id[c(which(pu$status == 3))]
    pu$solution <- 1
    pu$solution[locked_out_units] <- 0


    dist_threats <- x$getData("dist_threats")
    locked_out_actions <- pu$id[c(which(dist_threats$status == 3))]
    dist_threats$solution <- 1
    dist_threats$solution[locked_out_actions] <- 0

    a <- rcpp_stats_benefit(x$data$pu,
                                      x$data$features,
                                      x$data$dist_features,
                                      x$data$threats,
                                      x$data$dist_threats,
                                      x$data$sensitivity,
                                      c(pu$solution, dist_threats$solution))


    if(isTRUE(recovery)){

      diff_features <- which(a$target > round(a$sol_benefit_recovery))

      if(length(diff_features) > 0){
        warning(paste0("Infeasible model. There is not enough representativeness to achieve the targets required of following features: ", paste(diff_features, collapse = " ")),call.=FALSE)

        x$data$features$target[diff_features] <- round(a$sol_benefit_recovery[diff_features])
        warning("The targets for these features will be set to the maximum benefit values", call.=FALSE)
      }
    }
    else{
      diff_features <- which(a$target > round(a$sol_benefit_recovery + a$sol_benefit_nothing))

      if(length(diff_features) > 0){
        warning(paste0("Infeasible model. There is not enough representativeness to achieve the targets required of following features: ", paste(diff_features, collapse = " ")),call.=FALSE)

        x$data$features$target[diff_features] <- round(a$sol_benefit_recovery[diff_features] + a$sol_benefit_nothing[diff_features])

        warning("The targets for these features will be set with the maximum benefit values", call.=FALSE)
      }
    }
  }
}
