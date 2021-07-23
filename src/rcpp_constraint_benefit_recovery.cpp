#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_constraint_benefit_recovery(SEXP x,
                             DataFrame pu_data,
                             DataFrame features_data,
                             DataFrame dist_features_data,
                             DataFrame threats_data,
                             DataFrame dist_threats_data,
                             DataFrame sensitivity_data,
                             int curve,
                             int segments){

  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  //variables
  int number_of_threats = threats_data.nrows();
  int number_of_units = pu_data.nrows();
  int number_of_features = features_data.nrows();
  int number_of_actions = dist_threats_data.nrows();

  arma::sp_mat dist_threats_extended = create_dist_threats_extended(dist_threats_data, number_of_units, number_of_threats);
  arma::sp_mat dist_features_extended = create_dist_features_extended(dist_features_data, number_of_units, number_of_features);
  arma::sp_mat sensitivity_extended = create_sensitivity_extended(sensitivity_data, number_of_features, number_of_threats);
  arma::sp_mat sensitivity_a_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "a");
  arma::sp_mat sensitivity_b_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "b");
  arma::sp_mat sensitivity_c_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "c");
  arma::sp_mat sensitivity_d_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "d");

  arma::sp_mat actions_extended = create_actions_extended(dist_threats_data, number_of_units, number_of_threats);

  int pu_id;
  int threat_id;
  int row_constraint = op->_rhs.size();
  int col_constraint = number_of_units + number_of_actions;
  int col_action = 0;

  double response_coef_variable;
  double response_coef_constant;
  double alpha;
  double sum_alpha;
  double param_a;
  double param_b;
  double param_c;
  double param_d;
  double threat_intensity;
  double feature_intensity;

  for(int s = 0; s < number_of_features; s++){

    for (auto it_species = dist_features_extended.begin_col(s);
         it_species != dist_features_extended.end_col(s); ++it_species) {

      pu_id = it_species.row();
      feature_intensity = dist_features_extended(pu_id, s);

      sum_alpha = 0.0;
      alpha = 0.0;

      // b_is
      op->_A_i.push_back(row_constraint);
      op->_A_j.push_back(col_constraint);
      op->_A_x.push_back(1);
      op->_rhs.push_back(0);
      op->_sense.push_back("==");

      for (auto it_threats = dist_threats_extended.begin_row(pu_id);
           it_threats != dist_threats_extended.end_row(pu_id); ++it_threats) {


        threat_id = it_threats.col();

        if(sensitivity_extended(s, threat_id) == 1){


          threat_intensity = dist_threats_extended(pu_id, threat_id);

          //calculate alpha value
          threat_intensity = dist_threats_extended(pu_id, threat_id);
          param_a = sensitivity_a_extended(s, threat_id);
          param_b = sensitivity_b_extended(s, threat_id);
          param_c = sensitivity_c_extended(s, threat_id);
          param_d = sensitivity_d_extended(s, threat_id);


          if(threat_intensity <= param_a){

            // intensity below or equal to a
            response_coef_constant = param_d;
            alpha = 1 - response_coef_constant;
          }
          else if(threat_intensity >= param_b){

            // intensity above or equal to b
            response_coef_constant = param_c;
            alpha = 1 - response_coef_constant;
          }
          else{

            // intensity between a and b
            response_coef_constant = (double) (param_c*(threat_intensity - param_a) - param_d*(threat_intensity - param_b))/(param_b - param_a);
            alpha = 1 - response_coef_constant;
          }
          sum_alpha = sum_alpha + alpha;
        }
      }

      for (auto it_threats = dist_threats_extended.begin_row(pu_id);
           it_threats != dist_threats_extended.end_row(pu_id); ++it_threats) {


        threat_id = it_threats.col();

        if(sensitivity_extended(s, threat_id) == 1){

          threat_intensity = dist_threats_extended(pu_id, threat_id);

          //calculate alpha value
          threat_intensity = dist_threats_extended(pu_id, threat_id);
          param_a = sensitivity_a_extended(s, threat_id);
          param_b = sensitivity_b_extended(s, threat_id);
          param_c = sensitivity_c_extended(s, threat_id);
          param_d = sensitivity_d_extended(s, threat_id);


          if(threat_intensity <= param_a){

            // intensity below or equal to a
            response_coef_variable = 0.0;
            response_coef_constant = param_d;
            alpha = 1 - response_coef_constant;
          }
          else if(threat_intensity >= param_b){

            // intensity above or equal to b
            response_coef_variable = param_d - param_c;
            response_coef_constant = param_c;
            alpha = 1 - response_coef_constant;
          }
          else{

            // intensity between a and b
            response_coef_variable = (double) ((param_a - threat_intensity)*(param_c - param_d))/(param_b - param_a);
            response_coef_constant = (double) (param_c*(threat_intensity - param_a) - param_d*(threat_intensity - param_b))/(param_b - param_a);
            alpha = 1 - response_coef_constant;
          }

          if(sum_alpha != 0.0){
            // x_ik
            col_action = number_of_units + actions_extended(pu_id, threat_id) - 1;

            op->_A_i.push_back(row_constraint);
            op->_A_j.push_back(col_action);
            op->_A_x.push_back(-1*(response_coef_variable * alpha)/sum_alpha);
          }
        }
      }

      if(sum_alpha == 0.0){

      }

      row_constraint = row_constraint + 1;
      col_constraint = col_constraint + 1;
    }
  }


  //targets

  row_constraint = op->_rhs.size();
  col_constraint = number_of_units + number_of_actions;

  NumericVector targets = features_data["target"];

  for(int s = 0; s < number_of_features; s++){

    for (auto it_species = dist_features_extended.begin_col(s);
         it_species != dist_features_extended.end_col(s); ++it_species) {

      pu_id = it_species.row();

      // b_is
      op->_A_i.push_back(row_constraint + s);
      op->_A_j.push_back(col_constraint);
      op->_A_x.push_back(feature_intensity);

      col_constraint = col_constraint + 1;
    }

    op->_rhs.push_back(targets[s]);
    op->_sense.push_back(">=");
  }



  return true;
}
