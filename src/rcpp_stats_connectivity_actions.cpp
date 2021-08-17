#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
NumericVector rcpp_stats_connectivity_actions(DataFrame pu_data,
                                              DataFrame threats_data,
                                              DataFrame dist_threats_data,
                                              DataFrame boundary_data,
                                              std::vector<double> solution){

  // initialization

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with w[i] variables) ----------------------
  //------------------------------------------------------------------------------------------

   //variables
  int number_of_units = pu_data.nrows();
  int number_of_threats = threats_data.nrows();
  int number_of_actions = dist_threats_data.nrows();
  int boundary_size = boundary_data.nrows();
  arma::sp_mat matrix_boundary_extended;
  NumericVector connectivity_actions(number_of_actions);
  NumericVector connectivity_actions_solution(number_of_actions);

  IntegerVector dist_threats_data_pu_id = dist_threats_data["internal_pu"];
  IntegerVector pu_id1_threat = clone(dist_threats_data_pu_id);
  pu_id1_threat = pu_id1_threat - 1;

  IntegerVector dist_threats_data_threat_id = dist_threats_data["internal_threat"];
  IntegerVector threat_id = clone(dist_threats_data_threat_id);
  threat_id = threat_id - 1;

  arma::sp_mat dist_threats_extended = create_dist_threats_extended(dist_threats_data, number_of_units, number_of_threats);


  for(int a = 0; a < number_of_actions; a++){

    if(boundary_size != 0){
      matrix_boundary_extended = create_boundary_matrix_extended(boundary_data, number_of_units);

      int pu_id2_threat;

      for (auto it = dist_threats_extended.begin_col(threat_id[a]);
           it != dist_threats_extended.end_col(threat_id[a]); ++it) {

        pu_id2_threat = it.row();

        if(pu_id1_threat[a] != pu_id2_threat && matrix_boundary_extended(pu_id1_threat[a], pu_id2_threat) != 0){
          connectivity_actions[a] = connectivity_actions[a] + matrix_boundary_extended(pu_id1_threat[a], pu_id2_threat);
        }
      }
    }

    connectivity_actions_solution[a] = connectivity_actions_solution[a] + connectivity_actions[a]*solution[a];
  }

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with p[i1,i2,k] variables) ------------------
  // auxiliary variables to normalize no-linear objective function
  //------------------------------------------------------------------------------------------

  double connectivityCoeff;
  int id_action2;
  arma::sp_mat actions_extended = create_actions_extended(dist_threats_data, number_of_units, number_of_threats);

  for(int a = 0; a < number_of_actions; a++){

    if(boundary_size != 0){

      int pu_id2_threat;

      for (auto it = dist_threats_extended.begin_col(threat_id[a]);
           it != dist_threats_extended.end_col(threat_id[a]); ++it) {

        pu_id2_threat = it.row();

        if(pu_id1_threat[a] != pu_id2_threat && matrix_boundary_extended(pu_id1_threat[a], pu_id2_threat) != 0){
          connectivityCoeff = -1*matrix_boundary_extended(pu_id1_threat[a], pu_id2_threat);

          id_action2 = actions_extended(pu_id2_threat, threat_id[a]) - 1;

          if(solution[a] > 0.99 && solution[id_action2] > 0.99){
            connectivity_actions_solution[a] = connectivity_actions_solution[a] + connectivityCoeff;
          }
        }
      }
    }
  }

  return connectivity_actions_solution;
}
