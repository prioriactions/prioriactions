#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
NumericVector rcpp_stats_connectivity_units(DataFrame pu_data,
                            DataFrame boundary_data,
                            DataFrame dist_threats_data,
                            DataFrame dist_features_data,
                            std::vector<double> solution,
                            std::vector<double> connectivity,
                            double blm,
                            int curve){

  // initialization

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with w[i] variables) ----------------------
  //------------------------------------------------------------------------------------------

  //variables
  int number_of_units = pu_data.nrows();
  int number_of_actions = dist_threats_data.nrows();
  int number_of_dist_features = dist_features_data.nrows();
  int connectivity_size = connectivity.size();
  NumericVector connectivity_units_solution(connectivity_size);


  for(int i = 0; i < number_of_units; i++){
    connectivity_units_solution[i] = connectivity[i]*solution[i];
  }


  if(blm != 0){

    for(int i = number_of_units; i < connectivity_size; i++){

      if(curve != 1){
        connectivity_units_solution[i] = connectivity[i]*solution[number_of_actions + 2*number_of_dist_features + i];
      }
      else{
        connectivity_units_solution[i] = connectivity[i]*solution[number_of_actions + number_of_dist_features + i];
      }

    }
  }
  else{
    //------------------------------------------------------------------------------------------
    //--------------------- (coefficients associated with y[i1,i2] variables) ------------------
    // auxiliary variables to normalize no-linear objective function
    //------------------------------------------------------------------------------------------
    arma::sp_mat matrix_boundary_extended;
    matrix_boundary_extended = create_boundary_matrix_extended(boundary_data, number_of_units);

    int col = 0;

    arma::sp_mat z = matrix_boundary_extended.t();

    for(arma::sp_mat::const_iterator it = z.begin(); it != z.end(); ++it) {
      if(it.row() != it.col()){

        if(solution[it.row()] > 0.99 && solution[it.col()] > 0.99){

          connectivity_units_solution[number_of_units + col] = connectivity[number_of_units + col];
        }

        col = col + 1;
      }
    }

  }

  return connectivity_units_solution;

  /*
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

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with y[i1,i2] variables) ------------------
  // auxiliary variables to normalize no-linear objective function
  //------------------------------------------------------------------------------------------


  double connectivityCoeff;

  if(boundary_size != 0){
    arma::sp_mat z = matrix_boundary_extended.t();

    for(arma::sp_mat::const_iterator it = z.begin(); it != z.end(); ++it) {
      if(it.row() != it.col()){
        connectivityCoeff = -1*(*it);

        if(solution[it.row()] > 0.99 && solution[it.col()] > 0.99){
          connectivity_units_solution[it.row()] = connectivity_units_solution[it.row()] + connectivityCoeff;
        }
      }
    }
  }

  return connectivity_units_solution;
   */
}
