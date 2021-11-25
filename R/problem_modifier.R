#' @title problem_modifier
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
problem_modifier <- function(x, ...) UseMethod("problem_modifier", x)

#' @rdname problem_modifier
#' @method problem_modifier OptimizationProblem
#' @noRd
problem_modifier.OptimizationProblem <- function(x, budget = NULL, blm = NULL, prop_target = NULL, ...) {

  assertthat::assert_that(inherits(x, "OptimizationProblem"))

  ## Getting data
  pu <- x$ConservationClass$data$pu
  features <- x$ConservationClass$data$features
  dist_features <- x$ConservationClass$data$dist_features
  threats <- x$ConservationClass$data$threats
  dist_threats <- x$ConservationClass$data$dist_threats
  sensitivity <- x$ConservationClass$data$sensitivities
  boundary <- x$ConservationClass$data$boundary
  curve <- x$data$args$curve
  segments <- x$data$args$segments
  model_type <- x$data$args$model_type

  # verifying arguments
  if(is.null(blm)){
    internal_blm <- x$data$args$blm
  }
  else{
    internal_blm <- blm
  }
  if(is.null(budget)){
    internal_budget <- x$data$args$budget
  }
  else{
    internal_budget <- budget
  }

  ##blm
  if (abs(internal_blm) <= 1e-10 && !is.null(boundary)) {
    warning("The blm argument was set to 0, so the boundary data has no effect",call.=FALSE, immediate. = TRUE)
  }

  if (abs(internal_blm) > 1e-50 && is.null(boundary)) {
    warning("No boundary data supplied so the blm argument has no effect",call.=FALSE, immediate. = TRUE)
  }

  ##blm_actions
  assertthat::assert_that(all(is.finite(threats$blm_actions)))
  if (all(threats$blm_actions <= 1e-10) && !is.null(boundary)) {
    warning("Some blm_actions argument were set to 0, so the boundary data has no effect for these cases",call.=FALSE, immediate. = TRUE)
  }

  ## blm_actions
  if (any(threats$blm_actions > 1e-10) && is.null(boundary)) {
    warning("No boundary data supplied so the blm_actions arguments has no effect",call.=FALSE, immediate. = TRUE)
  }

  ## curve
  if (!curve %in% c(1, 2, 3)) {
    stop("invalid curve type")
  }
  else if(curve == 1){
    segments = 1
  }
  if(curve != 1 && any(dist_threats$amount < 1)){
    curve = 1
    warning("Curve set to 1 because there are non-binary values in threat amount for a recovery target",call.=FALSE, immediate. = TRUE)
  }

  ## segments
  if (!segments %in% c(1, 2, 3, 4, 5)) {
    stop("invalid number of segments for linearization")
  }

  # Presolve---------------------------------------------------------
  if(model_type == "minimizeCosts"){
    features <- presolve(x$ConservationClass, model_type = model_type, features = features)
  }
  else if(model_type == "maximizeBenefits"){
    internal_budget <- presolve(x$ConservationClass, model_type = model_type, budget = internal_budget)
  }

  #-------------------------------------------------------------------

  if(!is.null(budget)){
    rhs_size <- length(x$data$rhs)
    x$data$rhs[rhs_size] <- internal_budget
  }

  if(!is.null(prop_target)){
    rhs_size <- length(x$data$rhs)
    features_size <- x$ConservationClass$getFeatureAmount()
    potential_benefit <- getPotentialBenefit(x$ConservationClass)
    maximum_target_recovery <- potential_benefit$maximum.recovery.benefit
    maximum_target_conservation <- potential_benefit$maximum.conservation.benefit
    new_recovery_target <- maximum_target_recovery * prop_target
    new_conservation_target <- maximum_target_conservation * prop_target

    x$data$rhs[(rhs_size - 2*features_size + 1):rhs_size] <- as.numeric(rbind(new_conservation_target, new_recovery_target))
  }

  if(!is.null(blm)){
    blm_last <- x$data$args$blm
    monitoring_costs <- x$ConservationClass$data$pu$monitoring_cost
    pu_size <- x$ConservationClass$getPlanningUnitsAmount()
    actions_size <- x$ConservationClass$getActionsAmount()
    dist_features_size <- nrow(x$ConservationClass$data$dist_features)
    boundary_size <- x$data$boundary_size

    if(x$data$args$curve != 1){
      var_conn_start <- pu_size + actions_size + 2*dist_features_size
    }
    else{
      var_conn_start <- pu_size + actions_size + dist_features_size
    }

    if(model_type == "minimizeCosts"){
      x$data$obj[1:pu_size] <- ((x$data$obj[1:pu_size] - monitoring_costs)/blm_last)*internal_blm
    }
    else if(model_type == "maximizeBenefits"){
      x$data$obj[1:pu_size] <- ((x$data$obj[1:pu_size])/blm_last)*internal_blm
    }
    x$data$obj[(var_conn_start + 1):(var_conn_start + boundary_size)] <- ((x$data$obj[(var_conn_start + 1):(var_conn_start + boundary_size)])/blm_last)*internal_blm
  }
  return(x)
}
