#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
NumericVector rcpp_stats_costs_actions(DataFrame pu_data,
                                       DataFrame threats_data,
                                       DataFrame dist_threats_data,
                                       std::vector<double> solution){
  // initialization
  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with w[i] variables) ----------------------
  //------------------------------------------------------------------------------------------

  //variables
  int number_of_units = pu_data.nrows();
  int number_of_actions = dist_threats_data.nrows();
  int number_of_threats = threats_data.nrows();
  NumericVector action_costs = dist_threats_data["action_cost"];
  NumericVector costs_action_solution(number_of_actions);

  for(int a = 0; a < number_of_actions; a++){
    costs_action_solution[a] = costs_action_solution[a] + action_costs[a]*solution[a];
  }

  // Getting sum of actions
  NumericVector costs_by_actions(number_of_threats);
  arma::sp_mat dist_threats_extended_connectivity = create_dist_threats_extended(dist_threats_data,
                                                                                 number_of_units,
                                                                                 number_of_threats,
                                                                                 costs_action_solution);

  for(int t = 0; t < number_of_threats; t++){
    for (auto it = dist_threats_extended_connectivity.begin_col(t);
         it != dist_threats_extended_connectivity.end_col(t); ++it) {

      costs_by_actions[t] = costs_by_actions[t] + (*it);
    }
  }

  return costs_by_actions;
}
