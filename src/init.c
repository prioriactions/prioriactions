#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP _prioriactions_rcpp_new_optimization_problem(SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_optimization_problem_as_list(SEXP);
extern SEXP _prioriactions_rcpp_get_optimization_problem_ncol(SEXP);
extern SEXP _prioriactions_rcpp_get_optimization_problem_nrow(SEXP);
extern SEXP _prioriactions_rcpp_get_optimization_problem_ncell(SEXP);
extern SEXP _prioriactions_rcpp_get_optimization_problem_A(SEXP);
extern SEXP _prioriactions_rcpp_objective_min_set(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_objective_max_coverage(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_constraint_benefit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_constraint_activation(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_constraint_lock(SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_stats_connectivity_units(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_stats_connectivity_actions(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_stats_costs_units(SEXP, SEXP);
extern SEXP _prioriactions_rcpp_stats_costs_actions(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_constraint_target(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_constraint_budget(SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_stats_benefit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _prioriactions_rcpp_stats_recovery(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_prioriactions_rcpp_new_optimization_problem", (DL_FUNC) &_prioriactions_rcpp_new_optimization_problem, 3},
  {"_prioriactions_rcpp_optimization_problem_as_list", (DL_FUNC) &_prioriactions_rcpp_optimization_problem_as_list, 1},
  {"_prioriactions_rcpp_get_optimization_problem_ncol", (DL_FUNC) &_prioriactions_rcpp_get_optimization_problem_ncol, 1},
  {"_prioriactions_rcpp_get_optimization_problem_nrow", (DL_FUNC) &_prioriactions_rcpp_get_optimization_problem_nrow, 1},
  {"_prioriactions_rcpp_get_optimization_problem_ncell", (DL_FUNC) &_prioriactions_rcpp_get_optimization_problem_ncell, 1},
  {"_prioriactions_rcpp_get_optimization_problem_A", (DL_FUNC) &_prioriactions_rcpp_get_optimization_problem_A, 1},
  {"_prioriactions_rcpp_objective_min_set", (DL_FUNC) &_prioriactions_rcpp_objective_min_set, 9},
  {"_prioriactions_rcpp_objective_max_coverage", (DL_FUNC) &_prioriactions_rcpp_objective_max_coverage, 9},
  {"_prioriactions_rcpp_constraint_benefit", (DL_FUNC) &_prioriactions_rcpp_constraint_benefit, 7},
  {"_prioriactions_rcpp_constraint_activation", (DL_FUNC) &_prioriactions_rcpp_constraint_activation, 4},
  {"_prioriactions_rcpp_constraint_lock", (DL_FUNC) &_prioriactions_rcpp_constraint_lock, 3},
  {"_prioriactions_rcpp_stats_connectivity_units", (DL_FUNC) &_prioriactions_rcpp_stats_connectivity_units, 5},
  {"_prioriactions_rcpp_stats_connectivity_actions", (DL_FUNC) &_prioriactions_rcpp_stats_connectivity_actions, 5},
  {"_prioriactions_rcpp_stats_costs_units", (DL_FUNC) &_prioriactions_rcpp_stats_costs_units, 2},
  {"_prioriactions_rcpp_stats_costs_actions", (DL_FUNC) &_prioriactions_rcpp_stats_costs_actions, 4},
  {"_prioriactions_rcpp_constraint_target", (DL_FUNC) &_prioriactions_rcpp_constraint_target, 8},
  {"_prioriactions_rcpp_constraint_budget", (DL_FUNC) &_prioriactions_rcpp_constraint_budget, 4},
  {"_prioriactions_rcpp_stats_benefit", (DL_FUNC) &_prioriactions_rcpp_stats_benefit, 7},
  {"_prioriactions_rcpp_stats_recovery", (DL_FUNC) &_prioriactions_rcpp_stats_recovery, 7},
  {NULL, NULL, 0}
};

void R_init_prioriactions(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
