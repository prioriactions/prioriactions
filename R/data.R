#' @include internal.R
NULL

#' @title Simulated multi-action planning data
#'
#' @description Simulated data for making prioritizations.
#'
#' \describe{
#'
#' \item{`sim_pu_data`}{Planning units are represented as tabular data.}
#'
#' \item{`sim_features_data`}{Features are represented as tabular data.}
#'
#' \item{`sim_dist_features_data`}{The simulated distribution of four features.}
#'
#' \item{`sim_threats_data`}{Threats are represented as tabular data.}
#'
#' \item{`sim_dist_threats_data`}{The simulated threats of two threats.}
#'
#' \item{`sim_sensitivity_data`}{Sensitivity of features to threats as tabular data.}
#'
#' \item{`sim_boundary_data`}{Boundary data between one hundred planning units.}
#'
#' }
#'
#' @docType data
#'
#' @aliases sim_pu_data sim_features_data sim_dist_features_data sim_threats_data
#' sim_dist_threats_data sim_sensitivity_data sim_boundary_data
#'
#' @usage data(sim_pu_data)
#'
#' @usage data(sim_features_data)
#'
#' @usage data(sim_dist_features_data)
#
#' @usage data(sim_threats_data)
#'
#' @usage data(sim_dist_threats_data)
#'
#' @usage data(sim_sensitivity_data)
#'
#' @usage data(sim_boundary_data)
#'
#' @format
#' \describe{
#'
#' \item{sim_pu_data}{[data.frame]
#'   object.}
#'
#' \item{sim_features_data}{[data.frame]
#'   object.}
#'
#' \item{sim_dist_features_data}{[data.frame]
#'   object.}
#'
#' \item{sim_threats_data}{[data.frame]
#'   object.}
#'
#' \item{sim_dist_threats_data}{[data.frame]
#'   object.}
#'
#' \item{sim_sensitivity_data}{[data.frame]
#'   object.}
#'
#' \item{sim_boundary_data}{[data.frame]
#'   object.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' # load data
#' data(sim_pu_data, sim_features_data, sim_dist_features_data,
#' sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
#' sim_boundary_data)
#'
#' # plot examples
#' library(raster)
#' r <- raster::raster(ncol=10, nrow=10, xmn=0, xmx=10, ymn=0, ymx=10)
#'
#' # plot cost of pu's
#' values(r) <- sim_pu_data$cost
#' plot(r)
#'
#' # plot feature distribution of feature 1
#' features <- reshape2::dcast(sim_dist_features_data, pu~feature,value.var = "amount", fill = 0)
#' values(r) <- features$'1'
#' plot(r)
#'
#' # plot feature distribution of threat 1
#' threats <- reshape2::dcast(sim_dist_threats_data, pu~threat,value.var = "amount", fill = 0)
#' values(r) <- threats$'1'
#' plot(r)
#'
#' @name sim_data
NULL
