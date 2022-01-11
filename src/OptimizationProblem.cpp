#include "OptimizationProblem.h"

// [[Rcpp::export]]
SEXP rcpp_new_optimization_problem(std::size_t nrow = 1000000,
                                   std::size_t ncol = 1000000,
                                   std::size_t ncell= 100000) {
  //
  OptimizationProblem* x = new OptimizationProblem(nrow, ncol, ncell);
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::XPtr<OptimizationProblem>(x,
                                                                        true);
  return(op);
}

// [[Rcpp::export]]
Rcpp::List rcpp_optimization_problem_as_list(SEXP x) {
  // initialization
  Rcpp::XPtr<OptimizationProblem> op =
    Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  // create bounds
  List bounds = List::create(Rcpp::Named("lower") = List::create(Rcpp::Named("ind") = seq(1,op->_obj.size()),
                        Rcpp::Named("val") = op->_lb),
                        Rcpp::Named("upper") = List::create(Rcpp::Named("ind") = seq(1,op->_obj.size()),
                        Rcpp::Named("val") = op->_ub));

  // create list
  return Rcpp::List::create(
    Rcpp::Named("modelsense") = op->_modelsense,
    Rcpp::Named("A_i") = op->_A_i,
    Rcpp::Named("A_j") = op->_A_j,
    Rcpp::Named("A_x") = op->_A_x,
    Rcpp::Named("obj") = op->_obj,
    Rcpp::Named("bounds") = bounds,
    Rcpp::Named("rhs") = op->_rhs,
    Rcpp::Named("sense") = op->_sense,
    Rcpp::Named("vtype") = op->_vtype,
    Rcpp::Named("name") = op->_name,
    Rcpp::Named("yvar") = op->_id_pow_variables,
    Rcpp::Named("xvar") = op->_id_variables,
    Rcpp::Named("boundary_size") = op->_boundary_size);
}

// [[Rcpp::export]]
std::size_t rcpp_get_optimization_problem_ncol(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x)->ncol());
}

// [[Rcpp::export]]
std::size_t rcpp_get_optimization_problem_nrow(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x)->nrow());
}

// [[Rcpp::export]]
std::size_t rcpp_get_optimization_problem_ncell(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x)->ncell());
}

// [[Rcpp::export]]
Rcpp::List rcpp_get_optimization_problem_A(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x)->A());
}
