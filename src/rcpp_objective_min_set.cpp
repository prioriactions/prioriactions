#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_objective_min_set(SEXP x,
                            DataFrame pu_data,
                            DataFrame threats_data,
                            DataFrame dist_threats_data,
                            DataFrame boundary_data,
                            double blm){

  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with w[i] variables) ----------------------
  //------------------------------------------------------------------------------------------

  //variables
  int number_of_units = pu_data.nrows();
  int boundary_size = boundary_data.nrows();
  NumericVector unit_costs = pu_data["cost"];
  NumericVector connectivity_units(number_of_units);

  arma::sp_mat matrix_boundary_extended;

  if(boundary_size != 0 && blm != 0){

    IntegerVector boundary_data_id1 = boundary_data["internal_id1"];
    IntegerVector pu_id1 = clone(boundary_data_id1);
    pu_id1 = pu_id1 - 1;
    IntegerVector boundary_data_id2 = boundary_data["internal_id2"];
    IntegerVector pu_id2 = clone(boundary_data_id2);
    pu_id2 = pu_id2 - 1;
    NumericVector bound = boundary_data["boundary"];

    matrix_boundary_extended = create_boundary_matrix_extended(boundary_data, number_of_units);

    for(int i = 0; i < boundary_size; i++){
      connectivity_units[pu_id1[i]] = connectivity_units[pu_id1[i]] + bound[i];
      connectivity_units[pu_id2[i]] = connectivity_units[pu_id2[i]] + bound[i];
    }

  }

  for(int i = 0; i < number_of_units; i++){
    op->_obj.push_back(blm*connectivity_units[i] + unit_costs[i]);
    op->_vtype.push_back("B");
    op->_lb.push_back(0);
    op->_ub.push_back(1);
  }

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with x[i,k] variables) --------------------
  //------------------------------------------------------------------------------------------

  //variables
  int number_of_threats = threats_data.nrows();
  int number_of_actions = dist_threats_data.nrows();
  NumericVector action_costs = dist_threats_data["cost"];
  NumericVector action_amount = dist_threats_data["amount"];
  NumericVector blm_actions = threats_data["blm_actions"];
  NumericVector connectivity_actions(number_of_actions);

  IntegerVector dist_threats_data_pu_id = dist_threats_data["internal_pu"];
  IntegerVector pu_id1_threat = clone(dist_threats_data_pu_id);
  pu_id1_threat = pu_id1_threat - 1;

  IntegerVector dist_threats_data_threat_id = dist_threats_data["internal_threats"];
  IntegerVector threat_id = clone(dist_threats_data_threat_id);
  threat_id = threat_id - 1;

  arma::sp_mat dist_threats_extended = create_dist_threats_extended(dist_threats_data, number_of_units, number_of_threats);


  for(int a = 0; a < number_of_actions; a++){

    if(boundary_size != 0 && blm_actions[threat_id[a]] != 0){

      int pu_id2_threat;

      for (auto it = dist_threats_extended.begin_col(threat_id[a]);
          it != dist_threats_extended.end_col(threat_id[a]); ++it) {

        pu_id2_threat = it.row();

        if(pu_id1_threat[a] != pu_id2_threat && matrix_boundary_extended(pu_id1_threat[a], pu_id2_threat) != 0){
          connectivity_actions[a] = connectivity_actions[a] + matrix_boundary_extended(pu_id1_threat[a], pu_id2_threat);
        }
      }
    }

    op->_obj.push_back(blm_actions[threat_id[a]]*connectivity_actions[a] + action_costs[a]);
    op->_vtype.push_back("B");
    op->_lb.push_back(0);
    op->_ub.push_back(1);

  }

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with y[i1,i2] variables) ------------------
  // auxiliary variables to normalize no-linear objective function
  //------------------------------------------------------------------------------------------

  double connectivityCoeff;

  if(boundary_size != 0 && blm != 0){
    arma::sp_mat z = matrix_boundary_extended.t();

    for(arma::sp_mat::const_iterator it = z.begin(); it != z.end(); ++it) {
      if(it.row() != it.col()){
        connectivityCoeff = -1*(*it);

        op->_obj.push_back(blm*connectivityCoeff);
        op->_vtype.push_back("B");
        op->_lb.push_back(0);
        op->_ub.push_back(1);

      }
    }
  }

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with p[i1,i2,k] variables) ------------------
  // auxiliary variables to normalize no-linear objective function
  //------------------------------------------------------------------------------------------

  for(int a = 0; a < number_of_actions; a++){

    if(boundary_size != 0 && blm_actions[threat_id[a]] != 0){

      int pu_id2_threat;

      for (auto it = dist_threats_extended.begin_col(threat_id[a]);
           it != dist_threats_extended.end_col(threat_id[a]); ++it) {

        pu_id2_threat = it.row();

        if(pu_id1_threat[a] != pu_id2_threat && matrix_boundary_extended(pu_id1_threat[a], pu_id2_threat) != 0){
          connectivityCoeff = -1*matrix_boundary_extended(pu_id1_threat[a], pu_id2_threat);

          op->_obj.push_back(blm_actions[threat_id[a]]*connectivityCoeff);
          op->_vtype.push_back("B");
          op->_lb.push_back(0);
          op->_ub.push_back(1);
        }
      }
    }
  }


  return true;
}
