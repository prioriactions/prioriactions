#' @include internal.R

#' @export
if (!methods::isClass("ConservationProblem")) methods::setOldClass("ConservationProblem")
NULL

#' Conservation problem class
#'
#' This class is used to represent a data instance for the multi-action
#' conservation planning problem. This includes several methods for obtaining
#' instance information. It is created using the \code{\link{problem}} function.
#'
#'
#' @section Fields: \describe{
#'
#'   \item{$data}{\code{list} object containing data.}
#'
#'   }
#'
#' @section Methods: \describe{ \item{$getActionsAmount( )}{return
#'   \code{integer} number of possible actions.}
#'
#'   \item{$getData(\code{character} name)}{return an \code{\link{data.frame}}
#'   object stored in the \code{data} field with the corresponding \code{name}.
#'   The argument \code{name} indicates the name of arguments of the
#'   \code{problem} function ("pu", "features", "rij", "threats", "sensitivity"
#'   or "boundary").}
#'
#'   \item{$getFeatureAmount( )}{return \code{integer} number of features.}
#'
#'   \item{$getFeatureNames( )}{return \code{character} names of features.}
#'
#'   \item{$getPlanningUnitCosts( )}{return the \code{numeric}
#'   \code{\link{vector}} cost of allocating each planning unit.}
#'
#'   \item{$getPlanningUnitsAmount( )}{return \code{integer} number of planning
#'   units.}
#'
#'   \item{$getThreatCosts( )}{return the \code{numeric} \code{\link{vector}}
#'   cost of actions each planning unit and threat.}
#'
#'   \item{$getThreatNames( )}{return \code{character} names of threats.}
#'
#'   \item{$getThreatsAmount( )}{return \code{integer} number of threats.}
#'
#'   \item{$print( )}{print basic information of the data instance.}
#'
#'   \item{$show( )}{call print method.}
#'
#'   }
#'
#' @examples
#' ## Examples of how to use the methods of a ConservationProblem class object.
#'
#' ## Load data
#' data(example_pu_data, example_features_data, example_rij_data, example_threats_data, example_sensitivity_data, example_boundary_data)
#'
#' ## Create data instance
#' problem_data <- problem(
#'   pu = example_pu_data, features = example_features_data, dist_features = example_dist_features__data,
#'   threats = example_threats_data, dist_therats = example_dist_threats_data, sensitivity = example_sensibility_data,
#'   boundary = example_boundary_data
#' )
#'
#' ## Use class methods
#' problem_data$getData("features")
#'
#' problem_data$getFeatureAmount()
#'
#' problem_data$getFeatureNames()
#'
#' problem_data$getPlanningUnitCosts()
#'
#' problem_data$getPlanningUnitsAmount()
#'
#' problem_data$getThreatCosts()
#'
#' problem_data$getThreatNames()
#'
#' problem_data$getThreatsAmount()
#'
#' problem_data$print()
#' @name ConservationProblem-class
#'
#' @aliases ConservationProblem
NULL

#' @export
ConservationProblem <- pproto(
  "ConservationProblem",
  data = list(),
  print = function(self) {
    unit_cs <- round(range(self$getPlanningUnitCosts(), na.rm = TRUE), 5)
    threat_cs <- round(range(self$getThreatCosts(), na.rm = TRUE), 5)
    message(paste0(
      "Conservation Problem",
      "\n  planning units: ", class(self$data$pu)[1], " (",
      self$getPlanningUnitsAmount(), " units)",
      "\n  unit costs:     min: ", unit_cs[1], ", max: ", unit_cs[2],
      "\n  features:       ", repr_atomic(self$getFeatureNames(), "features"),
      "\n  threats:        ", repr_atomic(self$getThreatNames(), "threats"),
      "\n  threat costs:   min: ", threat_cs[1], ", max: ", threat_cs[2]
    ))
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    "ConservationProblem object"
  },
  getData = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x))
    if (!x %in% names(self$data)) {
      return(paste0("x object do not found"))
    }
    return(self$data[[x]])
  },
  # set_data = function(self, x, value) {
  #   assertthat::assert_that(assertthat::is.string(x))
  #   self$data[[x]] <- value
  #   invisible()
  # },
  getPlanningUnitsAmount = function(self) {
    if (inherits(self$data$pu, "data.frame")) {
      return(sum(rowSums(!is.na(as.matrix(
        as.data.frame(self$data$pu$cost)
      ))) > 0))
    } else if (is.matrix(self$data$pu)) {
      return(sum(rowSums(!is.na(self$data$pu)) > 0))
    } else {
      stop("cost is of unknown class")
    }
  },
  getPlanningUnitCosts = function(self) {
    if (inherits(self$data$pu, "data.frame")) {
      m <- as.vector(self$data$pu$cost)
    } else {
      stop("cost is of unknown class")
    }
    return(m)
  },
  getFeatureAmount = function(self) {
    if (inherits(self$data$features, "data.frame")) {
      return(nrow(self$data$features))
    } else {
      stop("feature data is of an unrecognized class")
    }
  },
  getFeatureNames = function(self) {
    if (inherits(self$data$features, "data.frame")) {
      return(as.character(self$data$features$name))
    } else {
      stop("feature data is of an unrecognized class")
    }
  },
  getThreatNames = function(self) {
    if (inherits(self$data$threats, "data.frame")) {
      return(as.character(self$data$threats$name))
    } else {
      stop("threats data is of an unrecognized class")
    }
  },
  getThreatsAmount = function(self) {
    if (inherits(self$data$threats, "data.frame")) {
      return(nrow(self$data$threats))
    } else {
      stop("threats data is of an unrecognized class")
    }
  },
  getThreatCosts = function(self) {
    if (inherits(self$data$dist_threats, "data.frame")) {
      m <- as.vector(self$data$dist_threats$cost)
    } else {
      stop("cost is of unknown class")
    }
    return(m)
  },
  getActionsAmount = function(self) {
    if (inherits(self$data$dist_threats, "data.frame")) {
      return(nrow(self$data$dist_threats))
    } else {
      stop("threats data is of an unrecognized class")
    }
  },
  getBenefitInformation = function(self) {
    a <- rcpp_stats_calculate_benefit(self$data$pu, self$data$features,
                                         self$data$dist_features, self$data$threats,
                                         self$data$dist_threats, self$data$sensitivity)
    return(a)
  }
)
