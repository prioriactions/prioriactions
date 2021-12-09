#' @include internal.R

#' @export
if (!methods::isClass("Data")) methods::setOldClass("Data")
NULL

#' Data class
#'
#' This class is used to represent data of the instances of the corresponding
#' multi-action planning problem. It includes several methods for retrieving the information
#' of the instance (such as the spatial allocation of threats and species, the cost
#' of management actions or the structure of the spatial connectivity across
#' the area where the planning is carried out. This class is created using the
#' [inputData()] function.
#'
#'
#' @section Fields: \describe{
#'
#'   \item{data}{`list` object containing data.}
#'
#'   }
#'
#' @section Methods: \describe{
#'   \item{getActionsAmount():}{
#'   `integer`. Number of possible actions.}
#'
#'   \item{getData(`character` name):}{
#'   [data.frame()]. Object stored in the `data` field with the corresponding `name`.
#'   The argument `name` indicates the name of arguments of the
#'   `problem` function ("pu", "features", "dist_features", "threats",
#'   "dist_threats", "sensitivity"
#'   or "boundary").}
#'
#'   \item{getFeatureAmount():}{
#'   `integer`. Number of features.}
#'
#'   \item{getFeatureNames():}{
#'   `character`. Names of features.}
#'
#'   \item{getMonitoringCosts():}{
#'   `numeric` [vector()]. Cost of monitoring each planning unit.}
#'
#'   \item{getPlanningUnitsAmount():}{
#'   `integer`. Number of planning units.}
#'
#'   \item{getActionCosts():}{
#'   `numeric` [vector()]. Cost of actions each planning unit and threat.}
#'
#'   \item{getThreatNames():}{
#'   `character`. Names of threats.}
#'
#'   \item{getThreatsAmount():}{
#'   `integer`. Number of threats.}
#'
#'   \item{print():}{
#'   Print basic information of the data instance.}
#'
#'   \item{show():}{
#'   Call print method.}
#'   }
#'
#' @return No return value.
#'
#' @examples
#' ## set seed for reproducibility
#' set.seed(14)
#'
#' ## Set prioriactions path
#' prioriactions_path <- system.file("extdata/example_input/", package = "prioriactions")
#'
#' ## Load in planning unit data
#' pu_data <- data.table::fread(paste0(prioriactions_path,"/pu.dat"),
#'                              data.table = FALSE)
#' head(pu_data)
#'
#' ## Load in feature data
#' features_data <- data.table::fread(paste0(prioriactions_path,"/features.dat"),
#'                                    data.table = FALSE)
#' head(features_data)
#'
#' ## Load in planning unit vs feature data
#' dist_features_data <- data.table::fread(paste0(prioriactions_path,"/dist_features.dat"),
#'                                         data.table = FALSE)
#' head(dist_features_data)
#'
#' ## Load in the threats data
#' threats_data <- data.table::fread(paste0(prioriactions_path,"/threats.dat"),
#'                                   data.table = FALSE)
#' head(threats_data)
#'
#' ## Load in the threats distribution data
#' dist_threats_data <- data.table::fread(paste0(prioriactions_path,"/dist_threats.dat"),
#'                                        data.table = FALSE)
#' head(dist_threats_data)
#'
#' ## Load in the sensitivity data
#' sensitivity_data <- data.table::fread(paste0(prioriactions_path,"/sensitivity.dat"),
#'                                       data.table = FALSE)
#' head(sensitivity_data)
#'
#' ## Load in the boundary data
#' boundary_data <- data.table::fread(paste0(prioriactions_path,"/boundary.dat"),
#'                                    data.table = FALSE)
#' head(boundary_data)
#'
#' ## Create instance
#' problem_data <- inputData(
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
#' problem_data$getMonitoringCosts()
#'
#' problem_data$getPlanningUnitsAmount()
#'
#' problem_data$getActionCosts()
#'
#' problem_data$getThreatNames()
#'
#' problem_data$getThreatsAmount()
#'
#' problem_data$print()
#' @name data-class
#'
#' @aliases Data
NULL

#' @export
Data <- pproto(
  "Data",
  data = list(),
  print = function(self) {
    unit_cs <- round(range(self$getMonitoringCosts(), na.rm = TRUE), 5)
    threat_cs <- round(range(self$getActionCosts(), na.rm = TRUE), 5)
    message(paste0(
      "Data",
      "\n  planning units: ", class(self$data$pu)[1], " (",
      self$getPlanningUnitsAmount(), " units)",
      "\n  monitoring costs:     min: ", unit_cs[1], ", max: ", unit_cs[2],
      "\n  features:       ", repr_atomic(self$getFeatureNames(), "features"),
      "\n  threats:        ", repr_atomic(self$getThreatNames(), "threats"),
      "\n  action costs:   min: ", threat_cs[1], ", max: ", threat_cs[2]
    ))
  },
  show = function(self) {
    self$print()
  },
  repr = function(self) {
    "Data object"
  },
  getData = function(self, x) {
    assertthat::assert_that(assertthat::is.string(x))
    if (!x %in% names(self$data)) {
      return(paste0("x object do not found"))
    }
    return(self$data[[x]])
  },
  getPlanningUnitsAmount = function(self) {
    return(sum(!is.na(self$data$pu$monitoring_cost)))
  },
  getMonitoringCosts = function(self) {
    m <- as.vector(self$data$pu$monitoring_cost)
    return(m)
  },
  getFeatureAmount = function(self) {
    return(nrow(self$data$features))
  },
  getFeatureNames = function(self) {
    return(as.character(self$data$features$name))
  },
  getThreatNames = function(self) {
    return(as.character(self$data$threats$name))
  },
  getThreatsAmount = function(self) {
    return(nrow(self$data$threats))
  },
  getActionCosts = function(self) {
    m <- as.vector(self$data$dist_threats$action_cost)
    return(m)
  },
  getActionsAmount = function(self) {
    return(nrow(self$data$dist_threats))
  }
)
