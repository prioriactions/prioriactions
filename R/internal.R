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
#' Provides the status of solver at the end of the optimization period.
#'
#' @param x [solution-class] or [portfolio-class] object.
#'
#' `GetStatus` can have five states:
#'
#' 1) *Optimal solution (according to gap tolerance)* : When the resolution of the model stop
#' when the quality of the solution (*gap*) is less than or equal to
#' `gap_limit` (parameter of the `solve` function).
#' 2) *No solution (model was proven to be infeasible or unbounded)*: When the model is infeasible.
#' 3) *Feasible solution (according to time limit)*: When the resolution of the model
#' stops when a `time_limit` has been reached finding a feasible solution
#' (parameter of the `solve` function).
#' 4) *No solution (according to time limit)*: When the resolution of the model
#' stops when a `time_limit` has been reached without finding a feasible solution
#' (parameter of the `solve` function).
#' 5) *First feasible solution*: When the resolution of the model stops when it has
#' found the first feasible solution (`solution_limit = TRUE` parameter in `solve` function).
#' 6) *No solution information is available*: For any other case.
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

