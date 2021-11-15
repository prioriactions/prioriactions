#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_constraint_lock(SEXP x,
                          DataFrame pu_data,
                          DataFrame dist_threats_data){
  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  //units-----------------------------------------------------------------------

  //variables
  NumericVector status_units = pu_data["status"];
  int number_of_units = pu_data.nrows();
  int row_constraint = op->_rhs.size();

  for(int i = 0; i < number_of_units; i++){
    if(status_units[i] != 0){
      op->_A_i.push_back(row_constraint);
      op->_A_j.push_back(i);
      op->_A_x.push_back(1);

      if(status_units[i] == 2){
        op->_rhs.push_back(1);
      }
      else if(status_units[i] == 3){
        op->_rhs.push_back(0);
      }

      op->_sense.push_back("==");
      row_constraint++;
    }
  }

  //actions---------------------------------------------------------------------

  //variables
  NumericVector status_actions = dist_threats_data["status"];
  int number_of_actions = dist_threats_data.nrows();
  row_constraint = op->_rhs.size();

  for(int a = 0; a < number_of_actions; a++){
    if(status_actions[a] != 0){
      op->_A_i.push_back(row_constraint);
      op->_A_j.push_back(number_of_units + a);
      op->_A_x.push_back(1);

      if(status_actions[a] == 2){
        op->_rhs.push_back(1);
      }
      else if(status_actions[a] == 3){
        op->_rhs.push_back(0);
      }

      op->_sense.push_back("==");
      row_constraint++;
    }
  }
  return true;
}
