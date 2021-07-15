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
presolve.ConservationProblem <- function(x, objective = "min costs", curve = 3, ...) {
  assertthat::assert_that(inherits(x, "ConservationProblem"))

  if (objective == "min costs") {

    ## Evaluating the feasibility (locked out in pu's)

    features <- x$getData("features")
    dist_features <- x$getData("dist_features")
    pu <- x$getData("pu")

    locked_out_units <- pu$id[c(which(pu$status == 3))]
    dist_features_free <- subset(dist_features, !pu %in% c(locked_out_units))
    summary_species <- stats::aggregate(. ~ species, dist_features_free, FUN = sum)

    if (!all(summary_species$amount >= features$target)) {
      features_below_target <- which(summary_species$amount < features$target)
      warning(paste0("Infeasible model \n There is not enough representativeness to achieve the targets required of following features: ", paste(features_below_target, collapse = " ")),call.=FALSE)

    }
    else {
      ## Evaluating the feasibility (locked out in threats)
      number_species <- x$getFeatureAmount()
      sensitivity <- x$getData("sensitivity")
      threats <- x$getData("dist_threats")

      for (i in 1:number_species) {
        counter_specie <- 0
        pu_per_specie <- rij_free$pu[which(rij_free$species == features$id[i])]
        threats_per_specie <- sensitivity$threats[which(sensitivity$species == features$id[i])]

        for (j in pu_per_specie) {
          threats_per_unit <- threats$threats[which(threats$pu == j)]
          threats_against_specie_in_unit <- intersect(threats_per_specie, threats_per_unit)

          if (length(threats_against_specie_in_unit) != 0) {
            threats_filtered <- subset(subset(threats, (pu %in% c(j))), (threats %in% c(threats_against_specie_in_unit)))
            lenght_threats <- nrow(threats_filtered)
            lenght_threats_blocked <- length(which(threats_filtered$status == 3))
            representation <- ((lenght_threats - lenght_threats_blocked) / (lenght_threats))^(curve)
            counter_specie <- counter_specie + representation
          }
          else {
            counter_specie <- counter_specie + 1
          }
        }
        if (counter_specie < features$target[i]) {
          stop("Infeasible model \n There is not enough representativeness to achieve the targets required \n Possible cause: Too many actions locked",call.=FALSE)
        }
      }
    }
  }
}
