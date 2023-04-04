#' @include presolve.R
#' @import Matrix Rcpp
NULL

#' @title Create mathematical model
#'
#' @description Create an optimization model for the multi-action conservation
#'   planning problem, following the mathematical formulations used in
#'   Salgado-Rojas *et al.* (2020).
#'
#' @param x [data-class] object. Data used in a problem of
#'   prioritization of multiple conservation actions. This object must be created using the
#'   [problem()] function.
#'
#' @param model_type `character`. Name of the type of model to create. With two possible values:
#'   `minimizeCosts` and `maximizeBenefits`.
#'
#' @param blm `numeric`. Weight factor applied to the sum of connectivity penalties
#'   for missed connections in a solution, similar to Boundary Length Modifier (BLM) in *Marxan*. This argument
#'   only has an effect when the `boundary` is available.
#'
#' @param budget `numeric`. Maximum budget allowed. This field is used only if a
#' model of the type `maximizeBenefits` is applied.
#'
#' @param curve `integer`. Type of continuous curve used to represent benefit expression. It can
#'   be a linear (`1`), quadratic (`2`) or cubic (`3`) function.
#'   See **Details** for more information.
#'
#' @param segments `integer`. Number of segments (`1`, `2`, `3`, `4` or `5`) used to approximate the non-linear
#'  expression (`curve`) in the calculate benefits. See **Details** for more information.
#'
#' @name problem
#'
#' @return An object of class [optimizationProblem-class].
#'
#' @details Currently the problem function allows you to create two types of mathematical programming models:
#'\describe{
#' \item{**minimize cost (minimizeCosts)**:}{ This model seeks to find the set of actions that
#' minimizes the overall planning costs, while meeting a set of representation targets
#' for the conservation features.
#'
#'
#' This model can be expressed mathematically for a set of planning units
#'\eqn{I} indexed by \eqn{i} a set of features \eqn{S} indexed by \eqn{s}, and
#'a set of threats \eqn{K} indexed by \eqn{k} as:
#' \deqn{
#' \min \space \sum_{i \in I}\sum_{k \in K_i} x_{ik} c_{ik} + \sum_{i \in I} x_{i \cdot} c'_{i} + blm \cdot connectivity\\
#' \mathit{s.t.} \\
#'\sum_{i \in I_s} p_{is} r_{is} \geq t_s \space \forall \space s \in S
#' }
#' Where, \eqn{x_{ik}} is a decisions variable that specifies whether an action to abate
#' threat \eqn{k} in planning unit \eqn{i} has been selected (1) or not (0), \eqn{c_{ik}}
#' is the cost of the action to abate the threat \eqn{k} in the planning unit \eqn{i},
#' \eqn{c'_{i}} is the monitoring cost of planning unit \eqn{i},
#' \eqn{p_{is}} is the probability of persistence of the feature \eqn{s} in the planning unit \eqn{i}
#' (ranging between 0 and 1), \eqn{r_{is}} is the amount of
#' feature \eqn{s} in planning unit \eqn{i}. \eqn{t_s} is the **recovery target** for feature \eqn{s}.
#' In the case of working with **conservation target**, the following constraint is necessary:
#'
#' \deqn{
#'\sum_{i \in I_s: |K_{s} \cap K_{i}| \neq 0} z_{is} r_{is} \geq t'_s \space \forall \space s \in S
#' }
#' With, \eqn{z_{is}}  as the probability of persistence by conservation of the feature s in the planning unit i
#' (ranging between 0 and 1). It is only present when there is no spatial co-occurrence between
#' a feature and its threats (i.e. \eqn{|K_{s} \cap K_{i}| \neq 0}). In the case of binary threat
#' intensities it is assumed as 1.  \eqn{t'_s} is the
#' **conservation target** for feature \eqn{s}.
#'}
#'
#'\item{**maximize benefits (maximizeBenefits)**:}{ The maximize benefits model seeks
#' to find the set of actions that maximizes the sum of benefits of
#' all features, while the cost of performing actions and monitoring does not exceed a certain budget.
#' Using the terminology presented above, this model can be expressed mathematically as:
#'
#' \deqn{
#' \max \space \sum_{i \in I}\sum_{s \in S_i} b_{is} - blm \cdot connectivity\\
#' \mathit{s.t.} \\
#'\sum_{i \in I} \sum_{k \in K_i} x_{ik} c_{ik} + \sum_{i \in I} x_{i \cdot} c'_{i} \leq budget
#' }
#'
#' }}
#'
#' Where \eqn{b_{is}} is the benefit of the feature \eqn{s} in a planning unit \eqn{i} and it
#' is calculated by multiplying the probability of persistence of the feature in the
#' unit by its corresponding amount, i.e., \eqn{b_{is} = p_{is} r_{is}}. When we talk about
#' recovering, the probability of persistence is a measure of the number of actions taken against the threats that
#' affect said feature. For more information on its calculation, see the
#' `getSolutionBenefit()` or `getPotentialBenefit()` functions references.
#'
#' As a way of including the risk associated with calculating our probability of
#' persistence of the features and in turn, avoiding that many low probabilities
#' of persistence end up reaching the proposed targets, is that we add the `curve`
#' parameter. That incorporates an exponent (values of 1: linear, 2: quadratic
#' or 3: cubic) to the calculation of the probability of persistence. Thus penalizing
#' the low probabilities in the sum of the benefits achieved.
#' Since `prioriactions` works with linear models, we use a piecewise linearization strategy to
#' work with non-linear curves in \eqn{b_{is}}. The `segments` parameter indicates how well the expression
#' approximates the curved used in \eqn{b_{is}}. A higher number implies a better
#' approximation but increases the resolution complexity. Note that for a linear curve
#' (`curve` = 1) it is not necessary to set a `segment` parameter.
#'
#' Parameters `blm` and `blm_actions` allow controlling the spatial connectivity
#' of the selected units and of the deployed actions, respectively (similar to BLM in Marxan).
#'
#' @seealso For more information regarding the arguments
#'  `curve` and `segments`, see the supplementary material
#'  of Salgado-Rojas *et al.* (2020)..
#'
#' @examples
#' ## This example uses input files included into package.
#'
#' ## set seed for reproducibility
#' set.seed(14)
#'
#' ## Load data
#' data(sim_pu_data, sim_features_data, sim_dist_features_data,
#' sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
#' sim_boundary_data)
#'
#' ## Create data instance
#' problem_data <- inputData(
#'   pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
#' )
#'
#' ## Create minimizeCosts model
#' model_min <- problem(x = problem_data, blm = 1, model_type = "minimizeCosts")
#'
#' #' ## Create maximazeBenefits model
#' model_max <- problem(x = problem_data, model_type = "maximizeBenefits", budget = 100)

#' @rdname problem
#' @export
problem <- function(x, model_type = "minimizeCosts", budget = 0, blm = 0, curve = 1, segments = 3) {

  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, "Data"),
    assertthat::is.scalar(blm),
    is.finite(blm),
    assertthat::is.scalar(curve),
    is.finite(curve),
    assertthat::is.scalar(segments),
    is.finite(segments),
    is.numeric(budget),
    assertthat::is.scalar(budget))

  #Verifying name models
  if (!model_type %in% c("minimizeCosts", "maximizeBenefits")) {
    stop("invalid name model")
  }

  # Rounding numeric parameters
  budget <- base::round(budget, 3)
  #blm <- base::round(blm, 3).

  ## Getting data
  pu <- x$getData("pu")
  features <- x$getData("features")
  dist_features <- x$getData("dist_features")
  threats <- x$getData("threats")
  dist_threats <- x$getData("dist_threats")
  sensitivity <- x$getData("sensitivity")
  boundary <- x$getData("boundary")

  pu <- pu[, c("internal_id", "monitoring_cost", "status")]
  dist_features <- dist_features[, c("internal_pu", "internal_feature", "amount")]
  threats <- threats[, c("internal_id", "blm_actions")]
  dist_threats <- dist_threats[, c("internal_pu", "internal_threat", "amount", "action_cost", "status")]
  sensitivity <- sensitivity[, c("internal_feature", "internal_threat", "delta1", "delta2", "delta3", "delta4")]

  if (!is.null(boundary)) {
    boundary <- boundary[, c("internal_id1", "internal_id2", "boundary")]
  }

  ##blm
  if (abs(blm) <= 1e-10 && !is.null(boundary)) {
    warning("The blm argument was set to 0, so the boundary data has no effect",call.=FALSE, immediate. = TRUE)
  }

  if (abs(blm) > 1e-50 && is.null(boundary)) {
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
    features <- presolve(x, model_type = model_type, features = features)
    x$data$features$target_recovery <- features$target_recovery
    x$data$features$target_conservation <- features$target_conservation
  }
  else if(model_type == "maximizeBenefits"){
    budget <- presolve(x, model_type = model_type, budget = budget)
  }
  #-------------------------------------------------------------------

  #Creating mathematical model----------------------------------------
  op <- rcpp_new_optimization_problem()

  rcpp_constraint_benefit(op, pu, features, dist_features, threats, dist_threats, sensitivity)
  rcpp_constraint_activation(op, pu, threats, dist_threats)
  rcpp_constraint_lock(op, pu, dist_threats)

  if(model_type == "minimizeCosts"){
    rcpp_objective_min_set(op, pu, features, dist_features, threats, dist_threats, boundary, blm, curve)
    rcpp_constraint_target(op, pu, features, dist_features, dist_threats, threats, sensitivity, curve)
  }
  else if(model_type == "maximizeBenefits"){
    rcpp_objective_max_coverage(op, pu, features, dist_features, threats, dist_threats, boundary, blm, curve)
    rcpp_constraint_budget(op, pu, dist_threats, budget)
  }
  #Getting model from cpp----------------------------------------------

  model <- rcpp_optimization_problem_as_list(op)
  args <- list(blm = blm, curve = curve,
               segments = segments, budget = budget,
               model_type = model_type)

  model$A <- Matrix::sparseMatrix(i = model$A_i + 1, j = model$A_j + 1, x = model$A_x)

  #create list of curve items------------------------------------------
  if(curve != 1){
    genconpow <- list()

    for(i in seq_len(length(model$xvar)))
      genconpow[[i]] <- list(xvar = model$xvar[i] + 1, yvar = model$yvar[i] + 1, a = curve)
  }
  else{
    genconpow <- NULL
  }

  # create Optimization Problem object----------------------------------
  pproto(NULL, OptimizationProblem,
         data = list(
           obj = model$obj, rhs = model$rhs, sense = model$sense, vtype = model$vtype,
           A = model$A, bounds = model$bounds,
           modelsense = model$modelsense,
           genconpow = genconpow,
           boundary_size = model$boundary_size,
           args = args
         ),
         ConservationClass = x
  )
}
