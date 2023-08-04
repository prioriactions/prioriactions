#' This is an exact copy of the prioritizr code ------------
#' No extra arguments
#'
#' Check that no additional unused arguments have been supplied to a function
#' through the `...`.
#'
#' @param ... arguments that are not used.
#'
#' @return `logical` indicating success.
#'
#' @noRd
no_extra_arguments <- function(...) {
  return(length(list(...)) == 0)
}

assertthat::on_failure(no_extra_arguments) <- function(call, env) {
  "unused arguments"
}

#' Verify if assertion is met
#'
#' Verify if an assertion is met and throw a [base::warning()] if it
#' is not. This function is equivalent to [assertthat::assert_that()]
#' except that it throws warnings and not errors.
#'
#' @param x `logical` condition.
#'
#' @return `logical` if assertion is met and a `warning` if it is not.
#'
#' @noRd
verify_that <- function(..., env = parent.frame()) {
  res <- assertthat::validate_that(..., env = env)
  if (isTRUE(res)) {
    return(TRUE)
  }
  warning(res, immediate. = TRUE)
  FALSE
}

#' Atomic representation
#'
#' Return a pretty character representation of an object with elements and
#  names.
#'
#' @param x `object`.
#'
#' @return `character` object.
#'
#' @examples
#' repr_atomic(letters)
#' repr_atomic(letters, "characters")
#' @noRd
repr_atomic <- function(x, description = "") {
  n <- length(x)
  if (nchar(description) > 0) {
    description <- paste0(" ", description)
  }
  if (length(x) <= 4) {
    x <- x[seq_len(min(length(x), 4))]
  } else {
    x <- c(x[seq_len(min(length(x), 3))], "...")
  }
  paste0(paste(x, collapse = ", "), " (", n, description, ")")
}

#' Create a new `pproto` object
#'
#' Construct a new object with `pproto`. This object system is inspired
#' from the `ggproto` system used in the `ggplot2` package.
#'
#' @param _class Class name to assign to the object. This is stored as the class
#'   attribute of the object. This is optional: if `NULL` (the default),
#'   no class name will be added to the object.
#'
#' @param _inherit `pproto` object to inherit from. If `NULL`, don"t
#'   inherit from any object.
#'
#' @param ... A list of members to add to the new `pproto` object.
#'
#' @examples
#' Adder <- pproto("Adder",
#'   x = 0,
#'   add = function(self, n) {
#'     self$x <- self$x + n
#'     self$x
#'   }
#' )
#'
#' Adder$add(10)
#' Adder$add(10)
#'
#' Abacus <- pproto("Abacus", Adder,
#'   subtract = function(self, n) {
#'     self$x <- self$x - n
#'     self$x
#'   }
#' )
#' Abacus$add(10)
#' Abacus$subtract(10)
#' @noRd
pproto <- function(`_class` = NULL, `_inherit` = NULL, ...) {
  assertthat::assert_that(
    assertthat::is.string(`_class`) || is.null(`_class`),
    inherits(`_inherit`, "pproto") || is.null(`_inherit`)
  )
  # copy objects from one proto to another proto
  assign_fields <- function(p1, p2) {
    if (!inherits(p2, "proto")) {
      return()
    }
    for (i in p2$ls()) {
      if (inherits(p2[[i]], "proto")) {
        p1[[i]] <- proto::proto()
        class(p1[[i]]) <- class(p2[[i]])
        assign_fields(p1[[i]], p2[[i]])
      } else {
        p1[[i]] <- p2[[i]]
      }
    }
    assign_fields(p1, p2$.super)
  }
  # create new proto
  p <- proto::proto()
  if (!is.null(`_inherit`)) {
    # assign inherited members
    assign_fields(p, `_inherit`)
    # assign inherited classes
    class(p) <- class(`_inherit`)
  } else {
    # assign pproto class
    class(p) <- c("pproto", class(p))
  }
  # assign members to new proto
  assign_fields(p, proto::proto(...))
  # assign new class if specified
  if (!is.null(`_class`)) {
    class(p) <- c(`_class`, class(p))
  }
  # return value
  p
}


#' Get status
#'
#' Construct a new object with `pproto`. This object system is inspired
#' from the `ggproto` system used in the `ggplot2` package.
#'
#' @param _class Class name to assign to the object. This is stored as the class
#'   attribute of the object. This is optional: if `NULL` (the default),
#'   no class name will be added to the object.
#'
#' @param _inherit `pproto` object to inherit from. If `NULL`, don"t
#'   inherit from any object.
#'
#' @param ... A list of members to add to the new `pproto` object.
#'
#' @examples
#' # set seed for reproducibility
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
#' ## Create optimization model
#' problem_model <- problem(x = problem_data, blm = 1)
#'
#' ## Solve the optimization model
#' s <- solve(a = problem_model, solver = "gurobi", gap_limit = 0.01, output_file = FALSE)
#'
#' ## get status of solution
#' getStatus(s)
#'

#' @noRd
getStatus <- function(x) {
  # assert argument is valid
  assertthat::assert_that(inherits(x, c("Solution")))

  statusCode <- x$data$status
  gap <- x$data$arg$gap
  time_limit <- x$data$arg$timelimit
  if(statusCode == 0L){
    return(paste0("Optimal solution (according to gap tolerance: ", gap,")"))
  }
  else if(statusCode == 1L){
    return("No solution (model was proven to be infeasible or unbounded)")
  }
  else if(statusCode == 2L){
    return(paste0("Feasible solution (according to time limit: ", time_limit, " sec)"))
  }
  else if(statusCode == 3L){
    return(paste0("No solution (according to time limit: ", time_limit, " sec)"))
  }
  else if(statusCode == 4L){
    return("First feasible solution")
  }
  else{
    return("No solution information available")
  }
}


#' Check if solvers are working
#'
#' Provides the status of solver. Being TRUE if it's working fine and FALSE in otherwise.
#'
#' @param package `character` object. Posible values: "gurobi", "cplex", and "symphony".
#'
#' @examples
#' available_to_solve("cplex")
#'

#' @noRd
available_to_solve <- function(package = ""){

  # define primitive data
  nPlants     <- 1
  nWarehouses <- 1
  # Warehouse demand in thousands of units
  Demand      <- c(10)
  # Plant capacity in thousands of units
  Capacity    <- c(20)
  # Fixed costs for each plant
  FixedCosts  <- c(100)
  # Transportation costs per thousand units
  TransCosts  <- c(100)

  flowidx <- function(w, p) {nPlants * (w-1) + p}

   # Build model
  model <- list()
  model$modelname <- 'facility'
  model$modelsense <- 'min'

  # initialize data for variables
  model$lb       <- 0
  model$ub       <- c(rep(1, nPlants),   rep(Inf, nPlants * nWarehouses))
  model$vtype    <- c(rep('B', nPlants), rep('C', nPlants * nWarehouses))
  model$obj      <- c(FixedCosts, TransCosts)
  model$varnames <- c(paste0(rep('Open',nPlants),1:nPlants),
                      sprintf('Trans%d,%d',
                              c(mapply(rep,1:nWarehouses,nPlants)),
                              1:nPlants))

  # build production constraint matrix
  A1 <- Matrix::spMatrix(nPlants, nPlants, i = c(1:nPlants), j = (1:nPlants), x = -Capacity)
  A2 <- Matrix::spMatrix(nPlants, nPlants * nWarehouses,
                 i = c(mapply(rep, 1:nPlants, nWarehouses)),
                 j = mapply(flowidx,1:nWarehouses,c(mapply(rep,1:nPlants,nWarehouses))),
                 x = rep(1, nWarehouses * nPlants))
  A3 <- Matrix::spMatrix(nWarehouses, nPlants)
  A4 <- Matrix::spMatrix(nWarehouses, nPlants * nWarehouses,
                 i = c(mapply(rep, 1:nWarehouses, nPlants)),
                 j = mapply(flowidx,c(mapply(rep,1:nWarehouses,nPlants)),1:nPlants),
                 x = rep(1, nPlants * nWarehouses))
  model$A           <- rbind(cbind(A1, A2), cbind(A3, A4))
  model$rhs         <- c(rep(0, nPlants),   Demand)
  model$sense       <- c(rep('<=', nPlants), rep('==', nWarehouses))
  model$constrnames <- c(sprintf('Capacity%d',1:nPlants),
                         sprintf('Demand%d',1:nWarehouses))


  if(package == "gurobi"){
    # set parameters
    params <- list()
    params$TimeLimit <- 0.01
    params$LogToConsole <- 0
    model$sense <- replace(model$sense, model$sense == "==", "=")

    sol <- invisible(try(gurobi::gurobi(model, params), silent = TRUE))
  }
  else if(package == "cbc"){
    # set parameters
    cbc_args <- list()
    cbc_args$sec <- "0.01"
    cbc_args$log <- "0"

    constraints_minus_equal <- which(model$sense != "<=")
    constraints_plus_equal <- which(model$sense == "<=")
    row_ub <- model$rhs
    row_ub[constraints_minus_equal] <- Inf
    row_lb <- model$rhs
    row_lb[constraints_plus_equal] <- -Inf

    sol <- invisible(try(rcbc::cbc_solve(obj = model$obj,
                                         mat = model$A,
                                         is_integer = ifelse(model$vtype == "B", TRUE, FALSE),
                                         row_ub = row_ub,
                                         row_lb = row_lb,
                                         col_lb = rep(0, length(model$vtype)),
                                         col_ub = model$ub,
                                         max = ifelse(model$modelsense == "min", FALSE, TRUE),
                                         cbc_args = cbc_args), silent = TRUE))
  }
  else if(package == "cplex"){
    model$sense[model$sense == ">="] <- "G"
    model$sense[model$sense == "=="] <- "E"
    model$sense[model$sense == "<="] <- "L"

    # set parameters
    params <- list()
    params$tilim <- 0.01
    params$trace <- 0

    sol <- try(Rcplex::Rcplex(cvec = model$obj,
                              Amat = model$A,
                              bvec = model$rhs,
                              lb = model$lb,
                              ub = model$ub,
                              objsense = model$modelsense,
                              sense = model$sense,
                              vtype = model$vtype,
                              control = params),
               silent = TRUE)
  }
  else if(package == "symphony"){
    model$mat <- model$A
    model$dir <- model$sense
    model$max <- ifelse(model$modelsense == "min", FALSE, TRUE)
    model$types <- model$vtype

    sol <- invisible(try(Rsymphony::Rsymphony_solve_LP(model$obj,
                                             model$mat,
                                             model$dir,
                                             model$rhs,
                                             model$bounds,
                                             model$types,
                                             model$max,
                                             gap_limit = 100,
                                             time_limit = 0.01),
               silent = TRUE))

  }
  if(inherits(sol, "try-error")){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}

