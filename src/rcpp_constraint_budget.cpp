#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_constraint_budget(SEXP x,
                            DataFrame pu_data,
                            DataFrame dist_threats_data,
                            double budget){

  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with w[i] variables) ----------------------
  //------------------------------------------------------------------------------------------

  //variables
  int number_of_units = pu_data.nrows();
  std::size_t row_constraint = op->_rhs.size();
  std::size_t col_constraint = 0;
  int number_of_actions = dist_threats_data.nrows();

  std::vector<double> action_costs = dist_threats_data["action_cost"];
  std::vector<double> unit_costs = pu_data["monitoring_cost"];

  for(int i = 0; i < number_of_units; i++){
    op->_A_i.push_back(row_constraint);
    op->_A_j.push_back(col_constraint);
    op->_A_x.push_back(unit_costs[i]);

    col_constraint = col_constraint + 1;
  }

  for(int a = 0; a < number_of_actions; a++){
    op->_A_i.push_back(row_constraint);
    op->_A_j.push_back(col_constraint);
    op->_A_x.push_back(action_costs[a]);

    col_constraint = col_constraint + 1;
  }
  op->_rhs.push_back(budget);
  op->_sense.push_back("<=");

  return true;
}
