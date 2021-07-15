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
                            DataFrame sensitivity_data,
                            int curve,
                            int segments){

  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  //variables
  int number_of_threats = threats_data.nrows();
  int number_of_units = pu_data.nrows();
  int number_of_features = features_data.nrows();

  arma::sp_mat dist_threats_extended = create_dist_threats_extended(dist_threats_data, number_of_units, number_of_threats);
  arma::sp_mat dist_features_extended = create_dist_features_extended(dist_features_data, number_of_units, number_of_features);
  arma::sp_mat sensitivity_extended = create_sensitivity_extended(sensitivity_data, number_of_features, number_of_threats);

  arma::sp_mat sensitivity_a_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "a");
  arma::sp_mat sensitivity_b_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "b");
  arma::sp_mat sensitivity_c_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "c");
  arma::sp_mat sensitivity_d_extended = create_sensitivity_param_extended(sensitivity_data, number_of_features, number_of_threats, "d");

  arma::sp_mat actions_extended = create_actions_extended(dist_threats_data, number_of_units, number_of_threats);
  arma::sp_mat dist_features_no_dangered_extended(number_of_units, number_of_features);

  int pu_id;
  int threat_id;
  double response_coef;
  double alpha;
  double sum_alpha;
  double threat_intensity;
  double param_a;
  double param_b;
  double param_c;
  double param_d;

  for(int s = 0; s < number_of_features; s++){

    arma::sp_mat intersection_threats_features = create_intersection_threats_features(dist_threats_extended,
                                                                                      dist_features_extended,
                                                                                      sensitivity_extended, s);

    for (auto it_species = dist_features_extended.begin_col(s);
         it_species != dist_features_extended.end_col(s); ++it_species) {

      pu_id = it_species.row();
      alpha = 0.0;
      sum_alpha = 0.0;

      for (auto it_threats = intersection_threats_features.begin_row(pu_id);
           it_threats != intersection_threats_features.end_row(pu_id); ++it_threats) {

        threat_id = it_threats.col();

        //calculate alpha value
        threat_intensity = dist_threats_extended(pu_id, threat_id);
        param_a = sensitivity_a_extended(s, threat_id);
        param_b = sensitivity_b_extended(s, threat_id);
        param_c = sensitivity_c_extended(s, threat_id);
        param_d = sensitivity_d_extended(s, threat_id);


        if(threat_intensity <= param_a){
          // intensity below or equal to a
          alpha = 1 - param_d;
        }
        else if(threat_intensity >= param_b){
          // intensity above or equal to b
          alpha = 1 - param_c;
        }
        else{
          // intensity between a and b
          alpha = 1 - param_c;
        }
        sum_alpha = sum_alpha + alpha;




        //alpha = 1 - (sensitivity_data_c[]);


      }

      if(threat_id == -1){
        //z variables
        std::size_t obj_size = op->_obj.size();
        dist_features_no_dangered_extended(pu_id, s) = obj_size;

        op->_obj.push_back(0);
        op->_vtype.push_back("B");
        op->_lb.push_back(0);
        op->_ub.push_back(1);

        op->_A_i.push_back(s);
        op->_A_j.push_back(obj_size);
        op->_A_x.push_back(1);
      }
    }
  }

  return true;
}
