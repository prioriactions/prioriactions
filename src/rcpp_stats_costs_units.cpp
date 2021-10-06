#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
double rcpp_stats_costs_units(DataFrame pu_data,
                               std::vector<double> solution){
  // initialization
  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with w[i] variables) ----------------------
  //------------------------------------------------------------------------------------------

  //variables
  int number_of_units = pu_data.nrows();
  NumericVector unit_costs = pu_data["monitoring_cost"];
  NumericVector costs_units_solution(number_of_units);

  for(int i = 0; i < number_of_units; i++){
    costs_units_solution[i] = costs_units_solution[i] + unit_costs[i]*solution[i];
  }
  return sum(costs_units_solution);
}
