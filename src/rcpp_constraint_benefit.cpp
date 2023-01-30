#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_constraint_benefit(SEXP x,
                            DataFrame pu_data,
                            DataFrame features_data,
                            DataFrame dist_features_data,
                            DataFrame threats_data,
                            DataFrame dist_threats_data,
                            DataFrame sensitivity_data){
  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  //variables
  int number_of_threats = threats_data.nrows();
  int number_of_units = pu_data.nrows();
  int number_of_features = features_data.nrows();
  int number_of_actions = dist_threats_data.nrows();
  //int number_of_dist_features = dist_features_data.nrows();

  arma::sp_mat dist_threats_extended = create_dist_threats_extended(dist_threats_data,
                                                                    number_of_units,
                                                                    number_of_threats,
                                                                    dist_threats_data["amount"]);
  arma::sp_mat dist_features_extended = create_dist_features_extended(dist_features_data, number_of_units, number_of_features);
  arma::sp_mat sensitivity_extended = create_sensitivity_extended(sensitivity_data, number_of_features, number_of_threats);
  arma::sp_mat sensitivity_a_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "delta1");
  arma::sp_mat sensitivity_b_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "delta2");
  arma::sp_mat sensitivity_c_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "delta3");
  arma::sp_mat sensitivity_d_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "delta4");
  arma::sp_mat actions_extended = create_actions_extended(dist_threats_data, number_of_units, number_of_threats);

  int pu_id = 0;
  int threat_id = 0;
  int row_constraint = op->_rhs.size();
  int col_constraint = number_of_units + number_of_actions;
  int col_action = 0;
  //int iter = 0;

  double response_coef_variable = 0.0;
  double response_coef_constant = 0.0;
  //double coef_constant;
  double alpha = 0.0;
  double sum_alpha = 0.0;
  double param_a = 0.0;
  double param_b = 0.0;
  double param_c = 0.0;
  double param_d = 0.0;
  double threat_intensity = 0.0;

  for(int s = 0; s < number_of_features; s++){
    for (auto it_species = dist_features_extended.begin_col(s);
         it_species != dist_features_extended.end_col(s); ++it_species) {
      pu_id = it_species.row();
      sum_alpha = 0.0;
      alpha = 0.0;
      //iter = 0;

      // b_is
      op->_id_variables.push_back(col_constraint);
      op->_A_i.push_back(row_constraint);
      op->_A_j.push_back(col_constraint);
      op->_A_x.push_back(1);
      op->_rhs.push_back(0);
      op->_sense.push_back("<=");


      for (auto it_threats = dist_threats_extended.begin_row(pu_id);
           it_threats != dist_threats_extended.end_row(pu_id); ++it_threats) {
        threat_id = it_threats.col();

        if(sensitivity_extended(s, threat_id) == 1){
          threat_intensity = dist_threats_extended(pu_id, threat_id);

          //calculate alpha value
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

      //coef_constant = 0;
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
            //if(iter == 0){
            //  // b_is_second
            //  op->_A_i.push_back(row_constraint + 1);
            //  op->_A_j.push_back(col_constraint);
            //  op->_A_x.push_back(1);
            //  op->_rhs.push_back(1);
            //  op->_sense.push_back("<=");

            //  // w_i_second
            //  op->_A_i.push_back(row_constraint + 1);
            //  op->_A_j.push_back(pu_id);
            //  op->_A_x.push_back(1);

            //  iter++;
            //}

            // w_i
            //coef_constant = coef_constant + (response_coef_constant * alpha)/sum_alpha;

            // x_ik
            col_action = number_of_units + actions_extended(pu_id, threat_id) - 1;

            op->_A_i.push_back(row_constraint);
            op->_A_j.push_back(col_action);
            op->_A_x.push_back(-1*(response_coef_variable * alpha)/sum_alpha);

            ////x_ik_second
            //op->_A_i.push_back(row_constraint + 1);
            //op->_A_j.push_back(col_action);
            //op->_A_x.push_back(-1);
          }
        }
      }

      if(sum_alpha == 0.0){
        //z variables
        op->_A_i.push_back(row_constraint);
        op->_A_j.push_back(pu_id);
        op->_A_x.push_back(-1);

      }
      else{
        //row_constraint++;

        // w_i
        //op->_A_i.push_back(row_constraint);
        //op->_A_j.push_back(pu_id);
        //op->_A_x.push_back(-1*coef_constant);

        //row_constraint = row_constraint + 2;
      }

      row_constraint++;
      col_constraint++;
    }
  }
  return true;
}
