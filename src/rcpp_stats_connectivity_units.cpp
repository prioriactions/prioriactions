#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
double rcpp_stats_connectivity_units(DataFrame pu_data,
                            DataFrame boundary_data,
                            DataFrame dist_threats_data,
                            DataFrame dist_features_data,
                            std::vector<double> solution){
  // initialization
  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with w[i] variables) ----------------------
  //------------------------------------------------------------------------------------------

  //variables
  int number_of_units = pu_data.nrows();
  //int boundary_size = boundary_data.nrows();
  NumericVector connectivity_units(number_of_units);
  NumericVector connectivity_units_solution(number_of_units);
  arma::sp_mat matrix_boundary_extended;

  matrix_boundary_extended = create_boundary_matrix_extended(boundary_data, number_of_units);
  arma::sp_mat z = matrix_boundary_extended.t();

  for(arma::sp_mat::const_iterator it = z.begin(); it != z.end(); ++it) {
    if(it.row() != it.col()){
      connectivity_units[it.col()] = connectivity_units[it.col()] + (*it);
    }
  }

  for(int i = 0; i < number_of_units; i++){
    connectivity_units_solution[i] = connectivity_units[i]*solution[i];
  }

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with y[i1,i2] variables) ------------------
  // auxiliary variables to normalize no-linear objective function
  //------------------------------------------------------------------------------------------
  double connectivityCoeff = 0.0;

  for(arma::sp_mat::const_iterator it = z.begin(); it != z.end(); ++it) {
    if(it.row() != it.col()){
      connectivityCoeff = -1*(*it);

      if(solution[it.row()] > 0.99 && solution[it.col()] > 0.99){
        connectivity_units_solution[it.row()] = connectivity_units_solution[it.row()] + connectivityCoeff;
      }
    }
  }
  return sum(connectivity_units_solution);
}
