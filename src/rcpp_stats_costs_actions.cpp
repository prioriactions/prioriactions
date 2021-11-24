#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
double rcpp_stats_costs_actions(DataFrame pu_data,
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
  return sum(costs_action_solution);
}
