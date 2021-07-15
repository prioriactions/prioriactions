#include "OptimizationProblem.h"

// [[Rcpp::export]]
SEXP rcpp_new_optimization_problem(std::size_t nrow = 1000000,
                                   std::size_t ncol = 1000000,
                                   std::size_t ncell= 100000) {

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
  // create list
  return Rcpp::List::create(
    Rcpp::Named("modelsense") = op->_sense,
    Rcpp::Named("A_i") = Rcpp::IntegerVector(op->_A_i.begin(),
                op->_A_i.end()),
    Rcpp::Named("A_j") = Rcpp::IntegerVector(op->_A_j.begin(),
                            op->_A_j.end()),
    Rcpp::Named("A_x") = op->_A_x,
    Rcpp::Named("obj") = op->_obj,
    Rcpp::Named("lb") = op->_lb,
    Rcpp::Named("ub") = op->_ub,
    Rcpp::Named("rhs") = op->_rhs,
    Rcpp::Named("sense") = op->_sense,
    Rcpp::Named("vtype") = op->_vtype);
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
