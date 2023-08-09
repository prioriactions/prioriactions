#' Skip if no commercial solvers
#'
#' Skip a test if no commercial solvers are installed.
#'
#' @param packages `character` vector containing the package dependencies
#'   for commercial solvers. Defaults to `"gurobi"` and `"Rcplex"`.
#'
#' @return `logical` indicating success.
skip_if_no_commercial_solvers_installed <- function(packages = c("gurobi", "Rcplex")) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(packages), assertthat::noNA(packages))
  # check if any dependencies present
  result <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  # skip if none installed
  if (any(result)) {
    return(invisible(TRUE))
  }
  testthat::skip("No commercial solvers installed")
}

