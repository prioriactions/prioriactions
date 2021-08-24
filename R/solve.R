#' @include presolve.R writeOutputs.R
NULL

#' @title Solve mathematical models
#'
#' @description Solve the optimization model associated with the multi-action
#'   conservation planning problem. This function is used to solve
#'   the mathematical model created by the `minimizeCosts()` or `maximizeBenefits()`
#'   functions.
#'
#' @param a [optimizationProblem-class] object. Optimization model created
#'   for the problem of prioritization of multiple conservation actions. This object must be
#'   created using the `minimizeCosts()` or `maximizeBenefits()` functions.
#'
#' @param solver `string`. Name of solver to use to
#'   solve the model. The following solvers are supported:
#'   [`"gurobi"`](http://gurobi.com)(requires the \pkg{gurobi} package), and
#'   [`"symphony"`](https://projects.coin-or.org/SYMPHONY)(requires the \pkg{Rsymphony} package).
#'   We highly recommend using gurobi (for more information on how to obtain an academic license
#'   [here](https://prioritizr.net/articles/gurobi_installation.html)).
#'
#' @param gap_limit `numeric`. Value between 0 and 1 that represents the gap
#'   to optimality, i.e., a relative number that cause the optimizer to
#'   terminate when the difference between the upper and lower objective
#'   function bounds is less than the gap times the upper bound. For example, a
#'   value of 0.01 will result in the optimizer stopping when the difference
#'   between the bounds is 1 percent of the upper bound.
#'
#' @param time_limit `numeric`. Time limit to run the optimizer (in seconds).
#'   The solver will return the current best solution when this time limit is exceeded.
#'
#' @param solution_limit `logical`. Indicates if the solution process
#' should be stopped after the first feasible solution is found (`TRUE`),
#' or not (`FALSE`).
#'
#' @param cores `integer`. Number of parallel cores to use in the machine to solve the problem.
#'
#' @param verbose `logical`. Indicates if the solver information is displayed while
#' solving the optimization model (`TRUE`), or if it is not displayed (`FALSE`).
#'
#' @param name_output_file `string`. Prefix of all output names.
#'
#' @param output_file `logical`. Indicates if the outputs are exported as .csv files (`TRUE`),
#' or they are not exported (`FALSE`). Currently, 5 files are exported. The
#' distribution of actions in the solution, the distribution of the selected
#' planning units, the benefits achieved by the features, the parameters used,
#' and the optimization engine log.
#'
#' @name solve
#'
#' @return An object of class [solution-class].
#'
#' @details The solvers supported by the [solve()] function are
#'   described below. \describe{
#'
#'   \item{`Gurobi solver`}{ [*Gurobi*](http://gurobi.com) is a
#'   state-of-the-art commercial optimization software with an R package
#'   interface. It is by far the fastest of the solvers available in this
#'   package, however, it is also the only solver that is not freely available.
#'   That said, licenses are available to academics at no cost. The \pkg{gurobi}
#'   package is distributed with the *Gurobi* software suite. This solver
#'   uses the \pkg{gurobi} package to solve problems.}
#'
#'   \item{`Symphony solver`}{
#'   [*SYMPHONY*](https://projects.coin-or.org/SYMPHONY) is an
#'   open-source integer programming solver that is part of the Computational
#'   Infrastructure for Operations Research (COIN-OR) project, an initiative to
#'   promote development of open-source tools for operations research (a field
#'   that includes linear programming). The \pkg{Rsymphony} package provides an
#'   interface to COIN-OR and is available on CRAN. This solver uses the
#'   \pkg{Rsymphony} package to solve problems.}}
#'
#' @seealso For more information on how to install and obtain an academic
#'   license of the Gurobi solver, see the *Gurobi installation guide*,
#'   which can be found online at
#'   [prioritizr vignette](<https://prioritizr.net/articles/gurobi_installation.html>).
#'
#' @examples
#' ## This example uses input files included into package.
#'
#' ## Load data
#' data(sim_pu_data, sim_features_data, sim_dist_features_data,
#' sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
#' sim_boundary_data)
#'
#' ## Create data instance
#' problem_data <- problem(
#'   pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
#' )
#'
#' ## Create optimization model
#' problem_model <- minimizeCosts(x = problem_data, blm = 1)
#'
#' ## Solve the optimization model using a gap_limit and gurobi solver
#' ## NOTE: The Gurobi solver must be previously installed and must have a valid license!
#' s <- solve(a = problem_model, solver = "gurobi", gap_limit = 0.01, output_file = FALSE)
#'
#' print(s)
#'
#' ## Solve the optimization model using a time_limit and gurobi solver

#' s <- solve(a = problem_model, solver = "gurobi", time_limit = 10, output_file = FALSE)
#'
#' print(s)
#'
#' @export
NULL

#' @name solve
#'
#' @rdname solve

solve <- function(a, solver = "", gap_limit = 0.0, time_limit = .Machine$integer.max,
                  solution_limit = FALSE, cores = 2, verbose = TRUE,
                  name_output_file = "output", output_file = TRUE) {

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
      assertthat::is.string(name_output_file)
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
    args <- list(gap = gap_limit, timelimit = time_limit, cores = cores, verbose = verbose,
                      solver = solver, solution_limit = solution_limit, name_output_file = name_output_file,
                      output_file = output_file)

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
      if(isTRUE(output_file)){
        params$LogFile <- paste0(name_output_file,"_log.txt")
      }

      #params$SolutionLimit <- solution_limit
      # Stop condition: MIP feasible solution limit
      if (!isTRUE(solution_limit)) {
        params$SolutionLimit <- NULL
      } else {
        params$SolutionLimit <- 1
      }

      if(model$args$curve != 1){
        params$FuncPieces <- 1
        params$FuncPieceLength <- round(1/model$args$segments, digits = 1)
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
                    status = solution$status_code, runtime = solution$runtime, args = args
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
                              runtime = runtime_symphony, args = args),
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


    # Getting unit solution
    pus <- a$ConservationClass$getPlanningUnitsAmount()
    pu_data <- a$ConservationClass$getData("pu")
    pu_data <- pu_data[!names(pu_data) %in% c("cost", "status", "internal_id")]

    # Getting actions solution

    ## REDUCED
    actions <- a$ConservationClass$getActionsAmount()
    threats_data <- a$ConservationClass$getData("dist_threats")
    threats_data <- threats_data[names(threats_data) %in% c("pu","threat")]

    # Getting local benefits
    dist_features_data <- a$ConservationClass$getData("dist_features")
    number_of_dist_features <- nrow(dist_features_data)
    dist_features_data <- dist_features_data[!names(dist_features_data) %in% c("amount","internal_feature","internal_pu")]

    dist_features_data <- dist_features_data[order(dist_features_data$feature), ]


    if (s$data$status == 0L || s$data$status == 2L || s$data$status == 4L) {
      pu_data$solution <- base::round(s$data$sol[1:pus])
      threats_data$solution <- base::round(s$data$sol[(pus + 1):(pus + actions)])
      dist_features_data$local_benefit <- s$data$sol[(pus + actions +1):(pus + actions + number_of_dist_features)]
    }
    else{
      pu_data$solution <- NA
      threats_data$solution <- NA
      dist_features_data$local_benefit <- NA
    }

    s$data$sol_pu <- pu_data
    s$data$sol_actions_reduced <- threats_data

    dist_features_data <- dist_features_data[order(dist_features_data$pu), ]
    s$data$local_benefits <- dist_features_data

    ## EXTENDED
    threats_data <- threats_data[names(threats_data) %in% c("pu","threat","solution")]
    actions_extended <- reshape2::dcast(threats_data, pu~threat,value.var = "solution")
    actions_extended[is.na(actions_extended)] <- 0
    actions_extended <- round(actions_extended,digits = 1)

    s$data$sol_actions_extended <- actions_extended

    # Creating txt output
    if(isTRUE(output_file)){
      writeOutputs(s, name = name_output_file)
    }

    s
  }

