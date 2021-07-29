#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
DataFrame rcpp_stats_benefit(DataFrame pu_data,
                             DataFrame features_data,
                             DataFrame dist_features_data,
                             DataFrame threats_data,
                             DataFrame dist_threats_data,
                             DataFrame sensitivity_data,
                             std::vector<double> solution){

  // initialization

  //variables
  int number_of_threats = threats_data.nrows();
  int number_of_units = pu_data.nrows();
  int number_of_features = features_data.nrows();
  int large_solution = solution.size();

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
  int col_action = 0;
  int sol_action_id;

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

  NumericVector specie_distribution(number_of_features);
  NumericVector specie_distribution_threatened(number_of_features);
  NumericVector benefit_nothing(number_of_features);
  NumericVector benefit_maximum_recovery(number_of_features);
  NumericVector benefit_solution_nothing(number_of_features);
  NumericVector benefit_solution_recovery(number_of_features);

  for(int s = 0; s < number_of_features; s++){

    for (auto it_species = dist_features_extended.begin_col(s);
         it_species != dist_features_extended.end_col(s); ++it_species) {

      pu_id = it_species.row();
      feature_intensity = dist_features_extended(pu_id, s);
      specie_distribution[s] = specie_distribution[s] + feature_intensity;

      sum_alpha = 0.0;
      alpha = 0.0;

      for (auto it_threats = dist_threats_extended.begin_row(pu_id);
           it_threats != dist_threats_extended.end_row(pu_id); ++it_threats) {

        threat_id = it_threats.col();

        if(sensitivity_extended(s, threat_id) == 1){

          threat_intensity = dist_threats_extended(pu_id, threat_id);

          if(sum_alpha == 0){
            specie_distribution_threatened[s] = specie_distribution_threatened[s] + feature_intensity;
          }

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
            sol_action_id = number_of_units + actions_extended(pu_id, threat_id) - 1;

            // x_ik
            benefit_nothing[s] = benefit_nothing[s] + ((response_coef_constant * alpha)/sum_alpha)*feature_intensity;
            benefit_maximum_recovery[s] = benefit_maximum_recovery[s] + ((response_coef_variable * alpha)/sum_alpha)*feature_intensity;

            if(large_solution != 1){
              benefit_solution_nothing[s] = benefit_solution_nothing[s] + solution[pu_id]*(((response_coef_constant * alpha)/sum_alpha)*feature_intensity);
              benefit_solution_recovery[s] = benefit_solution_recovery[s] + solution[sol_action_id]*(((response_coef_variable * alpha)/sum_alpha)*feature_intensity);
            }
          }
        }
      }

      if(sum_alpha == 0.0){

        //z variables
        benefit_nothing[s] = benefit_nothing[s] + 1;

        if(large_solution != 1){
          benefit_solution_nothing[s] = benefit_solution_nothing[s] + solution[pu_id];
        }
      }
    }
  }

  //creating DataFrame

  DataFrame df;

  if(large_solution != 1){
    df = DataFrame::create(Named("specie") = features_data["internal_id"],
                                     Named("distribution") = specie_distribution,
                                     Named("distribution_threatened") = specie_distribution_threatened,
                                     Named("maximum_benefit_nothing") = benefit_nothing,
                                     Named("maximum_benefit_recovery") = benefit_maximum_recovery,
                                     Named("target") = features_data["target"],
                                     Named("sol_benefit_nothing") = benefit_solution_nothing,
                                     Named("sol_benefit_recovery") = benefit_solution_recovery);
  }
  else{
    df = DataFrame::create(Named("specie") = features_data["internal_id"],
                                     Named("distribution") = specie_distribution,
                                     Named("distribution_threatened") = specie_distribution_threatened,
                                     Named("maximum_benefit_nothing") = benefit_nothing,
                                     Named("maximum_benefit_recovery") = benefit_maximum_recovery,
                                     Named("target") = features_data["target"]);
  }

  return df;
}

