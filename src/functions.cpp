#include "functions.h"

/*
 * ...........................................................................................
 * METHODS....................................................................................
 * ...........................................................................................
 */

/*
 * Function that creates the original matrix "cv[i1, i2]" from the information of the
 * dataframe "boundary_Data" (dense matrix with tuple data).
 * Function only needs the number of planning units, the type the data distribution
 * that you want (symmetric or asymmetric), and the data (tuple data), obviously.
 */
arma::sp_mat create_boundary_matrix_extended(DataFrame boundary_data, int units){
  IntegerVector boundary_data_id1 = boundary_data["internal_id1"];
  IntegerVector id1 = clone(boundary_data_id1);
  id1 = id1 - 1;
  IntegerVector boundary_data_id2 = boundary_data["internal_id2"];
  IntegerVector id2 = clone(boundary_data_id2);
  id2 = id2 - 1;
  NumericVector bound = boundary_data["boundary"];

    //nrow = units, ncol = units (quadrate matrix)
  arma::sp_mat boundary_matrix_extended(units, units);
  int boundary_data_size = id1.size();

  for(int l = 0; l < boundary_data_size; l++){
    if(boundary_matrix_extended(id1[l], id2[l]) == 0){
      boundary_matrix_extended(id1[l], id2[l]) = bound[l];
      boundary_matrix_extended(id2[l], id1[l]) = bound[l];
    }
    else{
      boundary_matrix_extended(id1[l], id2[l]) = bound[l];
    }
  }
  return boundary_matrix_extended;
}

arma::sp_mat create_dist_threats_extended(DataFrame dist_threats_data, int units, int threats, NumericVector amount){
  IntegerVector dist_threats_data_pu_id = dist_threats_data["internal_pu"];
  IntegerVector pu_id = clone(dist_threats_data_pu_id);
  pu_id = pu_id - 1;
  IntegerVector dist_threats_data_threat_id = dist_threats_data["internal_threat"];
  IntegerVector threat_id = clone(dist_threats_data_threat_id);
  threat_id = threat_id - 1;

  //NumericVector action_amount = dist_threats_data["amount"];
  NumericVector action_amount = amount;
  int number_of_actions = dist_threats_data.nrows();

  arma::sp_mat dist_threats_extended(units, threats);

  for(int a = 0; a < number_of_actions; a++){
    dist_threats_extended(pu_id[a], threat_id[a]) = action_amount[a];
  }
  return dist_threats_extended;
}

arma::sp_mat create_dist_features_extended(DataFrame dist_features_data, int units, int features){
  IntegerVector dist_features_data_pu_id = dist_features_data["internal_pu"];
  IntegerVector pu_id = clone(dist_features_data_pu_id);
  pu_id = pu_id - 1;
  IntegerVector dist_features_data_feature_id = dist_features_data["internal_feature"];
  IntegerVector feature_id = clone(dist_features_data_feature_id);
  feature_id = feature_id - 1;

  NumericVector feature_amount = dist_features_data["amount"];
  arma::sp_mat dist_features_extended(units, features);

  for(int i = 0; i < dist_features_data.nrows(); i++){
    dist_features_extended(pu_id[i], feature_id[i]) = feature_amount[i];
  }
  return dist_features_extended;
}

arma::sp_mat create_sensitivity_extended(DataFrame sensitivity_data, int features, int threats){
  IntegerVector sensitivity_data_feature_id = sensitivity_data["internal_feature"];
  IntegerVector feature_id = clone(sensitivity_data_feature_id);
  feature_id = feature_id - 1;
  IntegerVector sensitivity_data_threat_id = sensitivity_data["internal_threat"];
  IntegerVector threat_id = clone(sensitivity_data_threat_id);
  threat_id = threat_id - 1;

  arma::sp_mat sensitivity_extended(features, threats);

  for(int i = 0; i < sensitivity_data.nrows(); i++){
    sensitivity_extended(feature_id[i], threat_id[i]) = 1;
  }
  return sensitivity_extended;
}

arma::sp_mat create_sensitivity_param_extended(DataFrame sensitivity_data, int features, int threats, String param){
  IntegerVector sensitivity_data_feature_id = sensitivity_data["internal_feature"];
  IntegerVector feature_id = clone(sensitivity_data_feature_id);
  feature_id = feature_id - 1;
  IntegerVector sensitivity_data_threat_id = sensitivity_data["internal_threat"];
  IntegerVector threat_id = clone(sensitivity_data_threat_id);
  threat_id = threat_id - 1;

  arma::sp_mat sensitivity_param_extended(features, threats);
  int size_sensitivity_data = sensitivity_data.nrows();
  NumericVector sensitivity_param(size_sensitivity_data);

  if(param == "delta1"){
    sensitivity_param = sensitivity_data["delta1"];
  }
  else if( param == "delta2"){
    sensitivity_param = sensitivity_data["delta2"];
  }
  else if( param == "delta3"){
    sensitivity_param = sensitivity_data["delta3"];
  }
  else if( param == "delta4"){
    sensitivity_param = sensitivity_data["delta4"];
  }

  for(int i = 0; i < size_sensitivity_data; i++){
    sensitivity_param_extended(feature_id[i], threat_id[i]) = sensitivity_param[i];
  }
  return sensitivity_param_extended;
}

arma::sp_mat create_actions_extended(DataFrame dist_threats_data, int units, int threats){
  IntegerVector dist_threats_data_pu_id = dist_threats_data["internal_pu"];
  IntegerVector pu_id = clone(dist_threats_data_pu_id);
  pu_id = pu_id - 1;
  IntegerVector dist_threats_data_threat_id = dist_threats_data["internal_threat"];
  IntegerVector threat_id = clone(dist_threats_data_threat_id);
  threat_id = threat_id - 1;

  int number_of_actions = dist_threats_data.nrows();
  arma::sp_mat actions_extended(units, threats);

  for(int a = 0; a < number_of_actions; a++){
    actions_extended(pu_id[a], threat_id[a]) = a + 1;
  }
  return actions_extended;
}

