#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_constraint_activation(SEXP x,
                             DataFrame pu_data,
                             DataFrame threats_data,
                             DataFrame dist_threats_data){
  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  //variables
  int number_of_units = pu_data.nrows();
  int number_of_threats = threats_data.nrows();
  int row_constraint = op->_rhs.size();
  int threat_id = 0;
  int subset_size = 0;
  int col_action = 0;

  arma::sp_mat dist_threats_extended = create_dist_threats_extended(dist_threats_data,
                                                                    number_of_units,
                                                                    number_of_threats,
                                                                    dist_threats_data["amount"]);

  arma::sp_mat actions_extended = create_actions_extended(dist_threats_data, number_of_units, number_of_threats);

  for(int i = 0; i < number_of_units; i++){
    subset_size = 0;

    for (auto it_threats = dist_threats_extended.begin_row(i);
         it_threats != dist_threats_extended.end_row(i); ++it_threats) {
      threat_id = it_threats.col();
      col_action = number_of_units + actions_extended(i, threat_id) - 1;

      op->_A_i.push_back(row_constraint + i);
      op->_A_j.push_back(col_action);
      op->_A_x.push_back(1);

      subset_size = subset_size + 1;
    }

    op->_A_i.push_back(row_constraint + i);
    op->_A_j.push_back(i);
    op->_A_x.push_back(-1*subset_size);

    op->_rhs.push_back(0);
    op->_sense.push_back("<=");
  }

  return true;
}
