#' @include presolve.R optimization_problem.R createtxt.R
NULL

#' @title solve
#'
#' @description Solve the optimization model associated with the multi-action
#'   conservation planning problem. This function is used to internally solve
#'   the mathematical model associated with the problem in reference, specifying
#'   the type of solver and providing a set of parameters to control the
#'   optimization process and the user's machine settings.
#'
#'   This function requires the \emph{\pkg{Rsymphony}} package, the
#'   \emph{\pkg{Rglpk}} package or the \emph{\pkg{gurobi}} package, which host
#'   the solvers inside. These packages are installed by default when the
#'   \emph{\link{prioriactions}} package is installed. Of these three,
#'   \strong{only the Gurobi solver needs a license to operate} .
#'
#' @param a Object of class \code{\link{OptimizationProblem-class}} that
#'   specifies the custom-tailor optimization model created for the problem of
#'   prioritization of multiple conservation actions. This object must be
#'   created using the \code{\link{min_costs}} function.
#'
#' @param solver A \code{string} label that indicates which solver to use to
#'   solve the model created for the problem of prioritization of multiple
#'   conservation actions. The following solvers are supported:
#'   \href{http://gurobi.com}{\code{"gurobi"}}, and
#'   \href{https://projects.coin-or.org/SYMPHONY}{\code{"symphony"}}.
#'   \strong{The default argument is \code{"gurobi"} (requires a license)}.
#'
#' @param gap_limit A \code{numeric} value (between 0 and 1) that represents the gap
#'   to optimality, i.e., a relative number that cause the optimizer to
#'   terminate when the difference between the upper and lower objective
#'   function bounds is less than the gap times the upper bound. For example, a
#'   value of 0.01 will result in the optimizer stopping when the difference
#'   between the bounds is 1 percent of the upper bound. \strong{The default
#'   argument is 0.0}.
#'
#' @param time_limit A \code{numeric} value (non negative) that indicates the
#'   time limit to run the optimizer (in seconds). The solver will return the
#'   current best solution when this time limit is exceeded. \strong{The default
#'   argument is the \emph{maximum} time the user's machine can hold out}.
#'
#' @param solution_limit A \code{logical} value that indicates if the solution process
#' should be stopped after the first feasible solution is found (\code{\link{TRUE}}),
#' or if it should continue its normal course until the optimal solution is
#' found (\code{\link{FALSE}}). \strong{The default argument is \code{FALSE}}.
#'
#' @param cores An \code{integer} value that indicates the number of parallel
#'   cores to use in the machine. \strong{The default argument is 1 (only one
#'   core in use)}.
#'
#' @param verbose A \code{logical} value that indicates if the solver
#'   information is displayed while solving the optimization model
#'   (\code{\link{TRUE}}), or if it is not displayed (\code{\link{FALSE}}).
#'   \strong{The default argument is \code{TRUE}}.
#'
#' @name solve
#'
#' @return An object of class \code{\link{Solution-class}}.
#'
#' @details The solvers supported by the \code{\link{solve}} function are
#'   described below. \describe{
#'   \item{\code{Default solver}}{This solver uses the best software currently
#'   installed on the system.}
#'
#'   \item{\code{Gurobi solver}}{ \href{http://gurobi.com}{\emph{Gurobi}} is a
#'   state-of-the-art commercial optimization software with an R package
#'   interface. It is by far the fastest of the solvers available in this
#'   package, however, it is also the only solver that is not freely available.
#'   That said, licenses are available to academics at no cost. The \pkg{gurobi}
#'   package is distributed with the \emph{Gurobi} software suite. This solver
#'   uses the \pkg{gurobi} package to solve problems.}
#'
#'   \item{\code{Symphony solver}}{
#'   \href{https://projects.coin-or.org/SYMPHONY}{\emph{SYMPHONY}} is an
#'   open-source integer programming solver that is part of the Computational
#'   Infrastructure for Operations Research (COIN-OR) project, an initiative to
#'   promote development of open-source tools for operations research (a field
#'   that includes linear programming). The \pkg{Rsymphony} package provides an
#'   interface to COIN-OR and is available on CRAN. This solver uses the
#'   \pkg{Rsymphony} package to solve problems.}}
#'
#' @seealso For more information on how to install and obtain an academic
#'   license of the Gurobi solver, see the \emph{Gurobi installation guide},
#'   which can be found online at
#'   \url{https://prioritizr.net/articles/gurobi_installation.html}.
#'
#' @examples
#' ## Solve a conservation problem using the associated optimization model and some solver
#' ## that the user has available. This example uses input files included into package.
#'
#' ## Load data
#' data(example_pu_data, example_features_data, example_rij_data, example_threats_data, example_sensitivity_data, example_bound_data)
#'
#' ## Create data instance
#' problem_data <- problem(
#'   pu = example_pu_data, features = example_features_data, rij = example_rij_data,
#'   threats = example_threats_data, sensitivity = example_sensibility_data,
#'   bound = example_bound_data
#' )
#'
#' ## Create optimization model
#' problem_model <- min_costs(x = problem_data, blm = 1, blm_actions = 1)
#'
#' ## Solve the optimization model using a default solver
#' model_solution <- solve(a = problem_model, verbose = FALSE)
#' model_solution
#'
#' ## Solve the optimization model using the Gurobi solver
#' ## NOTE: The Gurobi solver must be previously installed and must have a valid license!
#' model_solution <- solve(a = problem_model, solver = "gurobi")
#' model_solution
#'
#' ## Solve the optimization model using the Symphony solver
#' model_solution <- solve(a = problem_model, solver = "symphony")
#' model_solution
#'
#' ## Solution summary
#' print(model_solution)
#'
#' ## Report the solution to the problem you are working on:
#' ## (i) total cost of the conservation plan;
#' solution_report01 <- model_solution$getObjectiveValue()
#' print(solution_report01)
#'
#' ## (ii) planning units suggested to be included (value 1) and not included (value 0)
#' ## within the conservation plan;
#' solution_report02 <- model_solution$getSolutionUnits()
#' head(solution_report02)
#'
#' ## (iii) conservation actions that are suggested to be carried out (value 1)
#' ## and not carried out (value 0) within the conservation plan.
#' solution_report03 <- model_solution$getSolutionActions()
#' head(solution_report03)
#' @export
NULL

#' @name solve
#'
#' @rdname solve
methods::setMethod(
  "solve",
  signature(a = "OptimizationProblem", b = "missing"),
  function(a, b, solver = "", gap_limit = 0.0, time_limit = .Machine$integer.max, solution_limit = FALSE, cores = 2,
           verbose = TRUE, name_output_file = "prioriaction_output", name_log = "gurobi_log", output_file = TRUE, log_file = FALSE, ...) {
    # assert that arguments are valid
    assertthat::assert_that(
      inherits(a, "OptimizationProblem"),
      isTRUE(all(is.finite(gap_limit))),
      assertthat::is.scalar(gap_limit),
      isTRUE(gap_limit >= 0), isTRUE(all(is.finite(time_limit))),
      assertthat::is.count(time_limit),
      assertthat::is.flag(solution_limit),
      isTRUE(all(is.finite(cores))),
      assertthat::is.count(cores),
      isTRUE(cores <= parallel::detectCores(TRUE)),
      assertthat::is.flag(verbose),
      assertthat::is.flag(output_file),
      assertthat::is.flag(log_file),
      assertthat::is.string(name_output_file),
      assertthat::is.string(name_log),
      no_extra_arguments(...)
    )

    if (requireNamespace("gurobi", quietly = TRUE)) {
      solver_default <- "gurobi"
    } else if (requireNamespace("Rsymphony", quietly = TRUE)) {
      solver_default <- "symphony"
      #} else if (requireNamespace("Rglpk", quietly = TRUE)) {
      #  solver_default <- "glpk"
    } else {
      stop("No optimization problem solvers found on system")
    }

    if (solver == "") {
      solver <- solver_default
    }
    else {
      if (!solver %in% c("gurobi", "symphony")) {
        stop("Solver not available")
      }
      else if (identical(solver, "gurobi") && !requireNamespace("gurobi", quietly = TRUE)) {
        stop("Gurobi solver not found")
      }
      else if (identical(solver, "symphony") && !requireNamespace("Rsymphony", quietly = TRUE)) {
        stop("SYMPHONY solver not found")
      }
      #else if (identical(solver, "glpk") && !requireNamespace("Rglpk", quietly = TRUE)) {
      #  stop("GLPK solver not found")
      #}
    }

    #arguments
    arg_solve <- list(gap = gap_limit, timelimit = time_limit, cores = cores, verbose = verbose,
                      solver = solver, solution_limit = solution_limit, name_output_file = name_output_file,
                      name_log = name_log, output_file = output_file, log_file = log_file)

    ## Solving
    model <- a$getDataList()
    ## Gurobi solver
    if (solver == "gurobi") {
      ## Gurobi model
      model$sense <- replace(model$sense, model$sense == "==", "=")
      model$lb <- a$getData("bounds")$lower$val
      model$ub <- a$getData("bounds")$upper$val

      ## Gurobi parameters
      params <- list()

      #Display interval
      params$DisplayInterval <- 1

      params$Threads <- cores
      params$LogToConsole <- as.integer(verbose)
      # Stop condition: Relative MIP optimality gap
      params$MIPGap <- gap_limit
      # Stop condition: Time limit
      params$TimeLimit <- time_limit

      #log gurobi
      if(isTRUE(log_file)){
        params$LogFile <- paste0(name_log,".txt")
      }

      #params$SolutionLimit <- solution_limit
      # Stop condition: MIP feasible solution limit
      if (!isTRUE(solution_limit)) {
        params$SolutionLimit <- NULL
      } else {
        params$SolutionLimit <- 1
      }

      if(model$settings$curve != 1){
        params$FuncPieces <- 1
        params$FuncPieceLength <- round(1/model$settings$segments, digits = 1)
      }

      solution <- gurobi::gurobi(model, params)

      solution$status_code <- dplyr::case_when(
        solution$status == "OPTIMAL" ~ 0L,
        (solution$status == "INF_OR_UNBD" || solution$status == "INFEASIBLE" || solution$status == "UNBOUNDED") ~ 1L,
        (solution$status == "TIME_LIMIT" && !is.null(solution$objval)) ~ 2L,
        (solution$status == "TIME_LIMIT" && is.null(solution$objval)) ~ 3L,
        (solution$status == "SOLUTION_LIMIT") ~ 4L,
        TRUE ~ 999L
      )

      s <- pproto(NULL, Solution,
                  data = list(
                    objval = solution$objval, sol = solution$x, gap = solution$mipgap,
                    status = solution$status_code, runtime = solution$runtime, arg = arg_solve
                  ),
                  OptimizationClass = a
      )
    }
    ## SYMPHONY solver
    #else if (solver == "symphony") {
    else {
      ## SYMPHONY solver
      model$mat <- model$A
      model$dir <- model$sense
      model$max <- ifelse(model$modelsense == "min", FALSE, TRUE)
      model$types <- model$vtype

      ## SYMPHONY parameters
      verbose_mod <- as.integer(verbose) - 2

      runtime_symphony <- system.time(
        solution <- Rsymphony::Rsymphony_solve_LP(model$obj, model$mat, model$dir, model$rhs, model$bounds, model$types,
                                                  model$max, gap_limit = gap_limit, time_limit = time_limit, verbosity = verbose_mod, first_feasible = solution_limit
        )
      )[[1]]
      #Time_limit
      # if(time_limit != .Machine$integer.max){
      #   solution$runtime <- time_limit
      # }
      # else{
      #   solution$runtime <- "No reported"
      # }

      ## Optimization status codes from SYMPHONY solver
      ## "TM_OPTIMAL_SOLUTION_FOUND"  = 0L
      ## "TM_TARGET_GAP_ACHIEVED"     = 231L
      ## "TM_NO_SOLUTION"             = 226L
      ## "TM_UNBOUNDED"               = 237L
      ## "TM_TIME_LIMIT_EXCEEDED"     = 228L
      ## "TM_FEASIBLE_SOLUTION_FOUND" = 235L
      ## "TM_FOUND_FIRST_FEASIBLE"    = 232L

      solution$status_code <- dplyr::case_when(
        (solution$status == 0L || solution$status == 231L) ~ 0L,
        (solution$status == 226L || solution$status == 237L) ~ 1L,
        (solution$status == 235L) ~ 2L,
        #(solution$status == 228L) ~ paste0("Feasible or unfeasible solution (according to time limit: ", time_limit, "[s])"),
        (solution$status == 232L) ~ 4L,
        TRUE ~ 999L
      )

      #Gap_limit
      if(isTRUE(solution$status == 0L)){
        solution$gap <- gap_limit
      }
      else{
        solution$gap <- "No reported"
      }

      s <- pproto(NULL, Solution,
                  data = list(objval = solution$objval, sol = solution$solution, gap = solution$gap, status = solution$status_code,
                              runtime = runtime_symphony, arg = arg_solve),
                  OptimizationClass = a)

    } ## END IF (SYMPHONY Solver)

    ## GLPK solver
    # else {
    #   model$mat <- model$A
    #   model$dir <- model$sense
    #   model$max <- ifelse(model$modelsense == "min", FALSE, TRUE)
    #   model$types <- model$vtype
    #
    #   ## GLPK parameters
    #   control <- list()
    #   control$verbose <- verbose ## GLPK default: FALSE
    #   control$presolve <- FALSE ## GLPK default: FALSE
    #   if (isTRUE(time_limit == .Machine$integer.max)) {
    #     control$tm_limit <- 0L ## GLPK default: 0L
    #   } else {
    #     control$tm_limit <- time_limit * 1000 ## GLPK's time limit originally works in milliseconds
    #   }
    #   control$canonicalize_status <- TRUE ## GLPK default: TRUE
    #
    #
    #   if (!isTRUE(gap_limit == 0.0)) {
    #     warning("The GLPK solver does not support a gap limit. The gap limit set in 0%.", call. = FALSE)
    #     gap_limit <- 0.0
    #   }
    #   if (isTRUE(solution_limit)) {
    #     warning("The GLPK solver does not support a solution limit. The solution limit set in FALSE.", call. = FALSE)
    #     solution_limit <- FALSE
    #   }
    #
    #   solution <- Rglpk::Rglpk_solve_LP(
    #     model$obj, model$mat, model$dir, model$rhs, model$bounds, model$types,
    #     model$max, control
    #   )
    #   status_mip <- solution$status
    #   status_solution <- dplyr::case_when(
    #     (status_mip == 0L) ~ paste0("Optimal solution (according to gap tolerance: ", gap_limit * 100, "%)"),
    #     TRUE ~ "No solution information is available"
    #   )
    #
    #   if (isTRUE(status_mip == 0L)) {
    #     s <- pproto(NULL, Solution,
    #       data = list(objval = solution$optimum, sol = solution$solution, gap = gap_limit, status = status_solution)
    #     )
    #   } else {
    #     s <- pproto(NULL, Solution,
    #       data = list(objval = solution$optimum, sol = solution$solution, gap = "No reported", status = status_solution)
    #     )
    #   }
    # } ## END IF (GLPK solver)

    if (s$data$status == 0L || s$data$status == 2L || s$data$status == 4L) {

      # Getting unit solution
      pus <- a$ConservationClass$getPlanningUnitsAmount()
      pu_data <- a$ConservationClass$getData("pu")
      pu_data <- pu_data[!names(pu_data) %in% c("internal_id")]
      pu_data$solution <- s$data$sol[1:pus]
      s$data$sol_pu <- pu_data

      # Getting actions solution

      ## REDUCED
      actions <- a$ConservationClass$getActionsAmount()
      threats_data <- a$ConservationClass$getData("dist_threats")
      threats_data <- threats_data[!names(threats_data) %in% c("internal_pu", "internal_threats")]
      threats_data$solution <- s$data$sol[(pus + 1):(pus + actions)]
      s$data$sol_actions_reduced <- threats_data

      ## EXTENDED
      threats_data <- threats_data[names(threats_data) %in% c("pu","threats","solution")]
      actions_extended <- reshape2::dcast(threats_data, pu~threats,value.var = "solution")
      actions_extended[is.na(actions_extended)] <- 0
      actions_extended <- round(actions_extended,digits = 1)

      s$data$sol_actions_extended <- actions_extended

      # Getting local benefits
      dist_features_data <- a$ConservationClass$getData("dist_features")
      number_of_dist_features <- nrow(dist_features_data)
      dist_features_data <- dist_features_data[!names(dist_features_data) %in% c("amount","internal_species","internal_pu")]
      dist_features_data$local_benefit <- s$data$sol[(pus + actions +1):(pus + actions + number_of_dist_features)]
      s$data$local_benefits <- dist_features_data


      # Creating txt output
      if(isTRUE(output_file)){
        createtxt(s, name = name_output_file)
      }
    }

    s
  }
)
