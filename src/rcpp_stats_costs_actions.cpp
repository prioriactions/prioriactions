#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
NumericVector rcpp_stats_costs_actions(DataFrame dist_threats_data,
                                       std::vector<double> solution){

  // initialization

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with w[i] variables) ----------------------
  //------------------------------------------------------------------------------------------

  //variables
  int number_of_actions = dist_threats_data.nrows();
  NumericVector action_costs = dist_threats_data["cost"];
  NumericVector costs_action_solution(number_of_actions);

  for(int a = 0; a < number_of_actions; a++){
    costs_action_solution[a] = costs_action_solution[a] + action_costs[a]*solution[a];
  }

  return costs_action_solution;
}
