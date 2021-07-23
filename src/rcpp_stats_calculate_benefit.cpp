#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
DataFrame rcpp_stats_calculate_benefit(DataFrame pu_data,
                             DataFrame features_data,
                             DataFrame dist_features_data,
                             DataFrame threats_data,
                             DataFrame dist_threats_data,
                             DataFrame sensitivity_data){

  // initialization

  //variables
  int number_of_threats = threats_data.nrows();
  int number_of_units = pu_data.nrows();
  int number_of_features = features_data.nrows();

  arma::sp_mat dist_threats_extended = create_dist_threats_extended(dist_threats_data, number_of_units, number_of_threats);
  arma::sp_mat dist_features_extended = create_dist_features_extended(dist_features_data, number_of_units, number_of_features);
  arma::sp_mat sensitivity_extended = create_sensitivity_extended(sensitivity_data, number_of_features, number_of_threats);

  int pu_id;
  int threat_id;
  double alpha;
  double alpha_recovery;
  double sum_alpha;
  double threat_intensity;
  double feature_intensity;

  NumericVector specie_distribution(number_of_features);
  NumericVector specie_distribution_threatened(number_of_features);
  NumericVector benefit_nothing(number_of_features);
  NumericVector benefit_maximum_recovery(number_of_features);

  for(int s = 0; s < number_of_features; s++){

    for (auto it_species = dist_features_extended.begin_col(s);
         it_species != dist_features_extended.end_col(s); ++it_species) {

      pu_id = it_species.row();
      feature_intensity = dist_features_extended(pu_id, s);

      specie_distribution[s] = specie_distribution[s] + feature_intensity;

      alpha = 0.0;
      alpha_recovery = 0.0;
      sum_alpha = 0.0;

      for (auto it_threats = dist_threats_extended.begin_row(pu_id);
           it_threats != dist_threats_extended.end_row(pu_id); ++it_threats) {

        threat_id = it_threats.col();

        if(sensitivity_extended(s, threat_id) == 1){

          threat_intensity = dist_threats_extended(pu_id, threat_id);

          if(sum_alpha == 0){
            specie_distribution_threatened[s] = specie_distribution_threatened[s] + feature_intensity;
          }


          sum_alpha = sum_alpha + threat_intensity;
          alpha = alpha + threat_intensity*(1 - threat_intensity);
          alpha_recovery = alpha_recovery + threat_intensity*threat_intensity;
        }
      }

      if(sum_alpha != 0){
        benefit_nothing[s] = benefit_nothing[s] + (alpha/sum_alpha)*feature_intensity;
        benefit_maximum_recovery[s] = benefit_maximum_recovery[s] + (alpha_recovery/sum_alpha)*feature_intensity;
      }
    }
  }

  //creating DataFrame

  DataFrame df = DataFrame::create(Named("specie") = features_data["internal_id"],
                                   Named("distribution") = specie_distribution,
                                   Named("distribution threatened") = specie_distribution_threatened,
                                   Named("benefit nothing") = benefit_nothing,
                                   Named("benefit recovery") = benefit_maximum_recovery);

  return df;
}
