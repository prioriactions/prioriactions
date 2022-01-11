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
  //int boundary_size = boundary_data.nrows();
  arma::sp_mat matrix_boundary_extended;
  NumericVector connectivity_actions(number_of_actions);
  NumericVector connectivity_actions_solution(number_of_actions);

  IntegerVector dist_threats_data_pu_id = dist_threats_data["internal_pu"];
  IntegerVector pu_id1_threat = clone(dist_threats_data_pu_id);
  pu_id1_threat = pu_id1_threat - 1;

  IntegerVector dist_threats_data_threat_id = dist_threats_data["internal_threat"];
  IntegerVector threat_id = clone(dist_threats_data_threat_id);
  threat_id = threat_id - 1;

  arma::sp_mat dist_threats_extended = create_dist_threats_extended(dist_threats_data,
                                                                    number_of_units,
                                                                    number_of_threats,
                                                                    dist_threats_data["amount"]);

  double connectivityCoeff = 0.0;
  int id_action2 = 0;
  arma::sp_mat actions_extended = create_actions_extended(dist_threats_data, number_of_units, number_of_threats);

  for(int a = 0; a < number_of_actions; a++){
    matrix_boundary_extended = create_boundary_matrix_extended(boundary_data, number_of_units);

    int pu_id2_threat = 0;

    for (auto it = dist_threats_extended.begin_col(threat_id[a]);
         it != dist_threats_extended.end_col(threat_id[a]); ++it) {
      pu_id2_threat = it.row();

      if(pu_id1_threat[a] != pu_id2_threat && matrix_boundary_extended(pu_id1_threat[a], pu_id2_threat) != 0){
        connectivity_actions[a] = connectivity_actions[a] + matrix_boundary_extended(pu_id1_threat[a], pu_id2_threat);

        connectivityCoeff = -1*matrix_boundary_extended(pu_id1_threat[a], pu_id2_threat);
        id_action2 = actions_extended(pu_id2_threat, threat_id[a]) - 1;

        if(solution[a] > 0.99 && solution[id_action2] > 0.99){
          connectivity_actions_solution[a] = connectivity_actions_solution[a] + connectivityCoeff;
        }
      }
    }
    connectivity_actions_solution[a] = connectivity_actions_solution[a] + connectivity_actions[a]*solution[a];
  }

  // Getting sum of actions

  NumericVector connectivity_by_actions(number_of_threats);
  arma::sp_mat dist_threats_extended_connectivity = create_dist_threats_extended(dist_threats_data,
                                                                    number_of_units,
                                                                    number_of_threats,
                                                                    connectivity_actions_solution);

  for(int t = 0; t < number_of_threats; t++){
    for (auto it = dist_threats_extended_connectivity.begin_col(t);
         it != dist_threats_extended_connectivity.end_col(t); ++it) {

      connectivity_by_actions[t] = connectivity_by_actions[t] + (*it);
    }
  }

  return connectivity_by_actions;
}
