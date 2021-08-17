#' @include internal.R

#' @export
if (!methods::isClass("ConservationProblem")) methods::setOldClass("ConservationProblem")
NULL

#' Conservation problem class
#'
#' This class is used to represent a data instance for the multi-action
#' conservation planning problem. This includes several methods for obtaining
#' instance information. It is created using the [problem()] function.
#'
#'
#' @section Fields: \describe{
#'
#'   \item{data}{`list` object containing data.}
#'
#'   }
#'
#' @section Methods: \describe{
#'   \item{getActionsAmount()}{
#'   `integer`. Number of possible actions.}
#'
#'   \item{getData(`character` name)}{
#'   [data.frame()]. Object stored in the `data` field with the corresponding `name`.
#'   The argument `name` indicates the name of arguments of the
#'   `problem` function ("pu", "features", "dist_features", "threats",
#'   "dist_threats", "sensitivity"
#'   or "boundary").}
#'
#'   \item{getFeatureAmount()}{
#'   `integer`. Number of features.}
#'
#'   \item{getFeatureNames()}{
#'   `character`. Names of features.}
#'
#'   \item{getPlanningUnitCosts()}{
#'   `numeric` [vector()]. Cost of allocating each planning unit.}
#'
#'   \item{getPlanningUnitsAmount()}{
#'   `integer`. Number of planning units.}
#'
#'   \item{getThreatCosts()}{
#'   `numeric` [vector()]. Cost of actions each planning unit and threat.}
#'
#'   \item{getThreatNames()}{
#'   `character`. Names of threats.}
#'
#'   \item{getThreatsAmount()}{
#'   `integer`. Number of threats.}
#'
#'   \item{print()}{
#'   Print basic information of the data instance.}
#'
#'   \item{show()}{
#'   Call print method.}
#'   }
#'
#' @examples
#' ## set seed for reproducibility
#' set.seed(14)
#'
#' ## Set prioriactions path
#' prioriactions_path <- system.file("extdata/input/", package = "prioriactions")
#'
#' ## Load in planning unit data
#' pu_data <- data.table::fread(paste0(prioriactions_path,"/pu.dat"), data.table = FALSE)
#' head(pu_data)
#'
#' ## Load in feature data
#' features_data <- data.table::fread(paste0(prioriactions_path,"/features.dat"), data.table = FALSE)
#' head(features_data)
#'
#' ## Load in planning unit vs feature data
#' dist_features_data <- data.table::fread(paste0(prioriactions_path,"/dist_features.dat"), data.table = FALSE)
#' head(dist_features_data)
#'
#' ## Load in the threats data
#' threats_data <- data.table::fread(paste0(prioriactions_path,"/threats.dat"), data.table = FALSE)
#' head(threats_data)
#'
#' ## Load in the threats distribution data
#' dist_threats_data <- data.table::fread(paste0(prioriactions_path,"/dist_threats.dat"), data.table = FALSE)
#' head(dist_threats_data)
#'
#' ## Load in the sensitivity data
#' sensitivity_data <- data.table::fread(paste0(prioriactions_path,"/sensitivity.dat"), data.table = FALSE)
#' head(sensitivity_data)
#'
#' ## Load in the boundary data
#' boundary_data <- data.table::fread(paste0(prioriactions_path,"/boundary.dat"), data.table = FALSE)
#' head(boundary_data)
#'
#' ## Create data instance
#' problem_data <- problem(
#'   pu = pu_data, features = features_data, dist_features = dist_features_data,
#'   dist_threats = dist_threats_data, threats = threats_data, sensitivity = sensitivity_data,
#'   boundary = boundary_data
#' )
#'
#' ## Summary
#' print(problem_data)
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
#' @name conservationProblem-class
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
  }
)
