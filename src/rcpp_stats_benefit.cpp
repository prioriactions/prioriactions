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
  NumericVector status_units = pu_data["status"];

  arma::sp_mat dist_threats_extended = create_dist_threats_extended(dist_threats_data,
                                                                    number_of_units,
                                                                    number_of_threats,
                                                                    dist_threats_data["amount"]);
  arma::sp_mat dist_threats_extended_status = create_dist_threats_extended(dist_threats_data,
                                                                    number_of_units,
                                                                    number_of_threats,
                                                                    dist_threats_data["status"]);
  arma::sp_mat dist_features_extended = create_dist_features_extended(dist_features_data, number_of_units, number_of_features);
  arma::sp_mat sensitivity_extended = create_sensitivity_extended(sensitivity_data, number_of_features, number_of_threats);
  arma::sp_mat sensitivity_a_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "delta1");
  arma::sp_mat sensitivity_b_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "delta2");
  arma::sp_mat sensitivity_c_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "delta3");
  arma::sp_mat sensitivity_d_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "delta4");
  arma::sp_mat actions_extended = create_actions_extended(dist_threats_data, number_of_units, number_of_threats);

  int pu_id = 0;
  int threat_id = 0;
  int sol_action_id = 0;
  int action_status = 0;

  double response_coef_variable = 0.0;
  double response_coef_constant = 0.0;
  double alpha = 0.0;
  double sum_alpha = 0.0;
  double param_a = 0.0;
  double param_b = 0.0;
  double param_c = 0.0;
  double param_d = 0.0;
  double threat_intensity = 0.0;
  double feature_intensity = 0.0;

  NumericVector specie_distribution(number_of_features);
  NumericVector specie_distribution_threatened(number_of_features);
  NumericVector benefit_maximum_nothing(number_of_features);
  NumericVector benefit_maximum_recovery(number_of_features);
  NumericVector benefit_solution_nothing(number_of_features);
  NumericVector benefit_solution_recovery(number_of_features);

  for(int s = 0; s < number_of_features; s++){
    for (auto it_species = dist_features_extended.begin_col(s);
         it_species != dist_features_extended.end_col(s); ++it_species) {
      pu_id = it_species.row();
      feature_intensity = dist_features_extended(pu_id, s);
      specie_distribution[s]++;

      sum_alpha = 0.0;
      alpha = 0.0;

      if(status_units[pu_id] != 3){

        for (auto it_threats = dist_threats_extended.begin_row(pu_id);
             it_threats != dist_threats_extended.end_row(pu_id); ++it_threats) {
          threat_id = it_threats.col();

          if(sensitivity_extended(s, threat_id) == 1){
            threat_intensity = dist_threats_extended(pu_id, threat_id);

            if(sum_alpha == 0){
              specie_distribution_threatened[s]++;
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
            action_status = dist_threats_extended_status(pu_id, threat_id);
            sol_action_id = number_of_units + actions_extended(pu_id, threat_id) - 1;

            if(sum_alpha != 0.0 && action_status != 3){

              //calculate alpha value
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

              benefit_maximum_recovery[s] = benefit_maximum_recovery[s] + ((response_coef_variable * alpha)/sum_alpha)*feature_intensity;

              if(large_solution != 1){
                benefit_solution_recovery[s] = benefit_solution_recovery[s] + solution[sol_action_id]*(((response_coef_variable * alpha)/sum_alpha)*feature_intensity);
              }
            }
          }
        }

        if(sum_alpha == 0.0){
          //z variables
          benefit_maximum_nothing[s] = benefit_maximum_nothing[s] + 1;

          if(large_solution != 1){
            benefit_solution_nothing[s] = benefit_solution_nothing[s] + solution[pu_id];
          }
        }
      }
    }
  }

  if(large_solution != 1){
    DataFrame df = DataFrame::create(Named("solution_name") = "",
                               Named("feature") = features_data["id"],
                               Named("benefit.conservation") = Rcpp::round(benefit_solution_nothing, 4),
                               Named("benefit.recovery") = Rcpp::round(benefit_solution_recovery,4),
                               Named("benefit.total") = Rcpp::round(benefit_solution_recovery + benefit_solution_nothing, 4));
    return df;
  }
  else{
    DataFrame df = DataFrame::create(Named("feature") = features_data["id"],
                             Named("dist") = specie_distribution,
                             Named("dist_threatened") = specie_distribution_threatened,
                             Named("maximum.conservation.benefit") = Rcpp::round(benefit_maximum_nothing, 4),
                             Named("maximum.recovery.benefit") = Rcpp::round(benefit_maximum_recovery, 4),
                             Named("maximum.benefit") = Rcpp::round(benefit_maximum_recovery + benefit_maximum_nothing, 4));
    return df;
  }
}
