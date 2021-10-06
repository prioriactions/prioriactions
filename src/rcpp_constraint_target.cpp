#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_constraint_target(SEXP x,
                            DataFrame pu_data,
                            DataFrame features_data,
                            DataFrame dist_features_data,
                            DataFrame dist_threats_data,
                            int curve){
  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  //targets
  int number_of_units = pu_data.nrows();
  int number_of_features = features_data.nrows();
  int number_of_actions = dist_threats_data.nrows();
  int number_of_dist_features = dist_features_data.nrows();
  int row_constraint;
  int col_constraint;
  int col_constraint_conservation;
  int pu_id;
  double feature_intensity;

  arma::sp_mat dist_features_extended = create_dist_features_extended(dist_features_data, number_of_units, number_of_features);
  NumericVector targets_recovery = features_data["target_recovery"];
  NumericVector targets_conservation = features_data["target_conservation"];

  row_constraint = op->_rhs.size();
  col_constraint_conservation = number_of_units + number_of_actions + number_of_dist_features;

  if(curve == 1){
    col_constraint = number_of_units + number_of_actions;
  }
  else{
    col_constraint = number_of_units + number_of_actions + 2*number_of_dist_features;
  }

  for(int s = 0; s < number_of_features; s++){
    for (auto it_species = dist_features_extended.begin_col(s);
         it_species != dist_features_extended.end_col(s); ++it_species) {

      pu_id = it_species.row();
        feature_intensity = dist_features_extended(pu_id, s);

      // z_is
      op->_A_i.push_back(row_constraint);
      op->_A_j.push_back(col_constraint_conservation);
      op->_A_x.push_back(feature_intensity);

      col_constraint_conservation++;
    }
    op->_rhs.push_back(targets_conservation[s]);
    op->_sense.push_back(">=");
    row_constraint++;
  }

  for(int s = 0; s < number_of_features; s++){
    for (auto it_species = dist_features_extended.begin_col(s);
         it_species != dist_features_extended.end_col(s); ++it_species) {
      pu_id = it_species.row();
      feature_intensity = dist_features_extended(pu_id, s);

      // b_is
      op->_A_i.push_back(row_constraint);
      op->_A_j.push_back(col_constraint);
      op->_A_x.push_back(feature_intensity);

      col_constraint++;
    }
    op->_rhs.push_back(targets_recovery[s]);
    op->_sense.push_back(">=");
    row_constraint++;
  }

  return true;
}
