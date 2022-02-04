#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_constraint_target(SEXP x,
                            DataFrame pu_data,
                            DataFrame features_data,
                            DataFrame dist_features_data,
                            DataFrame dist_threats_data,
                            DataFrame threats_data,
                            DataFrame sensitivity_data,
                            int curve){
  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  //targets
  int number_of_units = pu_data.nrows();
  int number_of_features = features_data.nrows();
  int number_of_actions = dist_threats_data.nrows();
  int number_of_threats = threats_data.nrows();
  int number_of_dist_features = dist_features_data.nrows();
  int row_constraint = 0;
  int col_constraint = 0;
  //int col_constraint_conservation = 0;
  int pu_id = 0;
  int threat_id = 0;
  int count_threats = 0;
  double feature_intensity = 0.0;
  int number_of_variables_conservation = 0;
  int number_of_variables_recovery = 0;

  arma::sp_mat dist_features_extended = create_dist_features_extended(dist_features_data, number_of_units, number_of_features);
  NumericVector targets_recovery = features_data["target_recovery"];
  NumericVector targets_conservation = features_data["target_conservation"];

  arma::sp_mat dist_threats_extended = create_dist_threats_extended(dist_threats_data,
                                                                    number_of_units,
                                                                    number_of_threats,
                                                                    dist_threats_data["amount"]);
  arma::sp_mat sensitivity_extended = create_sensitivity_extended(sensitivity_data, number_of_features, number_of_threats);

  row_constraint = op->_rhs.size();

  if(curve == 1){
    col_constraint = number_of_units + number_of_actions;
  }
  else{
    col_constraint = number_of_units + number_of_actions + number_of_dist_features;
  }

  for(int s = 0; s < number_of_features; s++){
    number_of_variables_conservation = 0;
    number_of_variables_recovery = 0;

    for (auto it_species = dist_features_extended.begin_col(s);
         it_species != dist_features_extended.end_col(s); ++it_species) {

      pu_id = it_species.row();
      count_threats = 0;
      feature_intensity = dist_features_extended(pu_id, s);

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
        op->_A_i.push_back(row_constraint);
        number_of_variables_conservation++;
      }
      else{
        //recovery
        op->_A_i.push_back(row_constraint + 1);
        number_of_variables_recovery++;
      }
      op->_A_j.push_back(col_constraint);
      op->_A_x.push_back(feature_intensity);

      col_constraint++;

    }

    if(number_of_variables_conservation > 0){
      op->_rhs.push_back(targets_conservation[s]);
      op->_sense.push_back(">=");
    }
    else{
      op->_A_i.push_back(row_constraint);
      op->_A_j.push_back(1);
      op->_A_x.push_back(0.0);
      op->_rhs.push_back(0.0);
      op->_sense.push_back(">=");
    }

    if(number_of_variables_recovery > 0){
      op->_rhs.push_back(targets_recovery[s]);
      op->_sense.push_back(">=");
    }
    else{
      op->_A_i.push_back(row_constraint + 1);
      op->_A_j.push_back(1);
      op->_A_x.push_back(0.0);
      op->_rhs.push_back(0.0);
      op->_sense.push_back(">=");
    }

    row_constraint = row_constraint + 2;
  }

  return true;
}
