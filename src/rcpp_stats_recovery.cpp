#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
List rcpp_stats_recovery(std::vector<double> solution,
                         DataFrame pu_data,
                         DataFrame features_data,
                         DataFrame dist_features_data,
                         DataFrame dist_threats_data,
                         DataFrame threats_data,
                         DataFrame sensitivity_data){

  //targets
  int number_of_units = pu_data.nrows();
  int number_of_features = features_data.nrows();
  int number_of_actions = dist_threats_data.nrows();
  int number_of_threats = threats_data.nrows();
  int number_of_dist_features = dist_features_data.nrows();
  int pu_id = 0;
  int threat_id = 0;
  int count_threats;
  int iter;
  NumericVector recovery_solution(number_of_dist_features);
  NumericVector conservation_solution(number_of_dist_features);

  arma::sp_mat dist_features_extended = create_dist_features_extended(dist_features_data, number_of_units, number_of_features);
  arma::sp_mat dist_threats_extended = create_dist_threats_extended(dist_threats_data,
                                                                    number_of_units,
                                                                    number_of_threats,
                                                                    dist_threats_data["amount"]);
  arma::sp_mat sensitivity_extended = create_sensitivity_extended(sensitivity_data, number_of_features, number_of_threats);

  iter = 0;

  for(int s = 0; s < number_of_features; s++){
    for (auto it_species = dist_features_extended.begin_col(s);
         it_species != dist_features_extended.end_col(s); ++it_species) {

      pu_id = it_species.row();
      count_threats = 0;

      for (auto it_threats = dist_threats_extended.begin_row(pu_id);
           it_threats != dist_threats_extended.end_row(pu_id); ++it_threats) {
        threat_id = it_threats.col();

        if(sensitivity_extended(s, threat_id) == 1){
          count_threats++;
          break;
        }
      }

      if(count_threats == 0){
        // conservation
        conservation_solution[iter] = conservation_solution[iter] + solution[number_of_units + number_of_actions + iter];
      }
      else{
        //recovery
        recovery_solution[iter] = recovery_solution[iter] + solution[number_of_units + number_of_actions + iter];
      }
      iter++;
    }
  }

  return Rcpp::List::create(Rcpp::Named("recovery") = recovery_solution,
                            Rcpp::Named("conservation") = conservation_solution);
}
