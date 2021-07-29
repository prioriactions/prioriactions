#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
NumericVector rcpp_stats_connectivity_units(DataFrame pu_data,
                            DataFrame boundary_data,
                            std::vector<double> solution){

  // initialization

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with w[i] variables) ----------------------
  //------------------------------------------------------------------------------------------

  //variables
  int number_of_units = pu_data.nrows();
  int boundary_size = boundary_data.nrows();
  NumericVector connectivity_units(number_of_units);
  NumericVector connectivity_units_solution(number_of_units);

  arma::sp_mat matrix_boundary_extended;

  if(boundary_size != 0){
    matrix_boundary_extended = create_boundary_matrix_extended(boundary_data, number_of_units);

    IntegerVector boundary_data_id1 = boundary_data["internal_id1"];
    IntegerVector pu_id1 = clone(boundary_data_id1);
    pu_id1 = pu_id1 - 1;
    IntegerVector boundary_data_id2 = boundary_data["internal_id2"];
    IntegerVector pu_id2 = clone(boundary_data_id2);
    pu_id2 = pu_id2 - 1;
    NumericVector bound = boundary_data["boundary"];

    for(int i = 0; i < boundary_size; i++){
      connectivity_units[pu_id1[i]] = connectivity_units[pu_id1[i]] + bound[i];
      connectivity_units[pu_id2[i]] = connectivity_units[pu_id2[i]] + bound[i];
    }
  }

  for(int i = 0; i < number_of_units; i++){
    connectivity_units_solution[i] = connectivity_units[i]*solution[i];
  }

  return connectivity_units_solution;
}