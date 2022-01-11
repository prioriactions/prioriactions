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
  double costs_units_solution = 0.0;

  for(int i = 0; i < number_of_units; i++){
    costs_units_solution = costs_units_solution + unit_costs[i]*solution[i];
  }

  return costs_units_solution;
}
