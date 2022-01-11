#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
bool rcpp_objective_min_set(SEXP x,
                            DataFrame pu_data,
                            DataFrame features_data,
                            DataFrame dist_features_data,
                            DataFrame threats_data,
                            DataFrame dist_threats_data,
                            DataFrame boundary_data,
                            double blm,
                            int curve){
  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);
  op->_modelsense = "min";

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with w[i] variables) ----------------------
  //------------------------------------------------------------------------------------------

  //variables
  int number_of_units = pu_data.nrows();
  int boundary_size = boundary_data.nrows();
  NumericVector unit_costs = pu_data["monitoring_cost"];
  NumericVector connectivity_units(number_of_units);
  arma::sp_mat matrix_boundary_extended;

  if(boundary_size != 0){
    matrix_boundary_extended = create_boundary_matrix_extended(boundary_data, number_of_units);
  }

  if(boundary_size != 0 && blm != 0){
    arma::sp_mat z = matrix_boundary_extended.t();

    for(arma::sp_mat::const_iterator it = z.begin(); it != z.end(); ++it) {
      if(it.row() != it.col()){
        connectivity_units[it.col()] = connectivity_units[it.col()] + (*it);
      }
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
  NumericVector action_costs = dist_threats_data["action_cost"];
  NumericVector blm_actions = threats_data["blm_actions"];
  NumericVector connectivity_actions(number_of_actions);

  IntegerVector dist_threats_data_pu_id = dist_threats_data["internal_pu"];
  IntegerVector pu_id1_threat = clone(dist_threats_data_pu_id);
  pu_id1_threat = pu_id1_threat - 1;

  IntegerVector dist_threats_data_threat_id = dist_threats_data["internal_threat"];
  IntegerVector threat_id = clone(dist_threats_data_threat_id);
  threat_id = threat_id - 1;

  arma::sp_mat dist_threats_extended = create_dist_threats_extended(dist_threats_data,
                                                                    number_of_units,
                                                                    number_of_threats,
                                                                    dist_threats_data["amount"]);
  for(int a = 0; a < number_of_actions; a++){
    if(boundary_size != 0 && blm_actions[threat_id[a]] != 0){
      int pu_id2_threat = 0;

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
  //--------------------- (coefficients associated with b[i,s] variables) --------------------
  //------------------------------------------------------------------------------------------

  //variables
  int number_of_features = features_data.nrows();
  arma::sp_mat dist_features_extended = create_dist_features_extended(dist_features_data, number_of_units, number_of_features);

  for(int s = 0; s < number_of_features; s++){
    for (auto it_species = dist_features_extended.begin_col(s);
         it_species != dist_features_extended.end_col(s); ++it_species) {

      //b_is
      op->_obj.push_back(0);
      op->_vtype.push_back("C");
      op->_lb.push_back(0);
      op->_ub.push_back(1);

    }
  }

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with b'[i,s] variables) --------------------
  //------------------------------------------------------------------------------------------

  //variables
  int col_constraint = op->_obj.size();

  if(curve != 1){
    for(int s = 0; s < number_of_features; s++){
      for (auto it_species = dist_features_extended.begin_col(s);
           it_species != dist_features_extended.end_col(s); ++it_species) {
        op->_obj.push_back(0);
        op->_vtype.push_back("C");
        op->_lb.push_back(0);
        op->_ub.push_back(1);

        //curve
        op->_id_pow_variables.push_back(col_constraint);

        col_constraint++;
      }
    }
  }

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with y[i1,i2] variables) ------------------
  // auxiliary variables to normalize no-linear objective function
  //------------------------------------------------------------------------------------------

  double connectivityCoeff = 0.0;
  int row_constraint = op->_rhs.size();
  int number_y_variables = 0;
  col_constraint = op->_obj.size();

  if(boundary_size != 0  && blm != 0){
    arma::sp_mat z = matrix_boundary_extended.t();

    for(arma::sp_mat::const_iterator it = z.begin(); it != z.end(); ++it) {
      if(it.row() != it.col()){
        number_y_variables++;
        connectivityCoeff = -1*(*it);

        // objective vector
        op->_obj.push_back(blm*connectivityCoeff);
        op->_vtype.push_back("B");
        op->_lb.push_back(0);
        op->_ub.push_back(1);

        //matrix A
        //Constraint number 1 (Y[i1,i2] - W[i1] <= 0)
        op->_A_i.push_back(row_constraint);
        op->_A_j.push_back(col_constraint);
        op->_A_x.push_back(1);

        op->_A_i.push_back(row_constraint);
        op->_A_j.push_back(it.row());
        op->_A_x.push_back(-1);

        op->_rhs.push_back(0);
        op->_sense.push_back("<=");

        row_constraint++;

        //Constraint number 2 (Y[i1,i2] - W[i2] <= 0)
        op->_A_i.push_back(row_constraint);
        op->_A_j.push_back(col_constraint);
        op->_A_x.push_back(1);

        op->_A_i.push_back(row_constraint);
        op->_A_j.push_back(it.col());
        op->_A_x.push_back(-1);

        op->_rhs.push_back(0);
        op->_sense.push_back("<=");

        row_constraint++;

        //Constraint number 3 (Y[i1,i2] - W[i1] - W[i2] => -1)
        op->_A_i.push_back(row_constraint);
        op->_A_j.push_back(col_constraint);
        op->_A_x.push_back(1);

        op->_A_i.push_back(row_constraint);
        op->_A_j.push_back(it.row());
        op->_A_x.push_back(-1);
        op->_A_i.push_back(row_constraint);
        op->_A_j.push_back(it.col());
        op->_A_x.push_back(-1);

        op->_rhs.push_back(-1);
        op->_sense.push_back(">=");

        row_constraint++;
        col_constraint++;
      }
    }
  }
  op->_boundary_size = number_y_variables;

  //------------------------------------------------------------------------------------------
  //--------------------- (coefficients associated with p[i1,i2,k] variables) ------------------
  // auxiliary variables to normalize no-linear objective function
  //------------------------------------------------------------------------------------------

  row_constraint = op->_rhs.size();
  col_constraint = op->_obj.size();
  int col_action = 0;
  arma::sp_mat actions_extended = create_actions_extended(dist_threats_data, number_of_units, number_of_threats);

  for(int a = 0; a < number_of_actions; a++){
    if(boundary_size != 0 && blm_actions[threat_id[a]] != 0){
      int pu_id2_threat = 0;

      for (auto it = dist_threats_extended.begin_col(threat_id[a]);
           it != dist_threats_extended.end_col(threat_id[a]); ++it) {
        pu_id2_threat = it.row();

        if(pu_id1_threat[a] != pu_id2_threat && matrix_boundary_extended(pu_id1_threat[a], pu_id2_threat) != 0){
          connectivityCoeff = -1*matrix_boundary_extended(pu_id1_threat[a], pu_id2_threat);

          // objective vector
          op->_obj.push_back(blm_actions[threat_id[a]]*connectivityCoeff);
          op->_vtype.push_back("B");
          op->_lb.push_back(0);
          op->_ub.push_back(1);

          //matrix A
          //Constraint number 1 (P[i1,i2,k] - X[i1,k] <= 0)
          col_action = number_of_units + actions_extended(pu_id1_threat[a], threat_id[a]) - 1;

          op->_A_i.push_back(row_constraint);
          op->_A_j.push_back(col_constraint);
          op->_A_x.push_back(1);

          op->_A_i.push_back(row_constraint);
          op->_A_j.push_back(col_action);
          op->_A_x.push_back(-1);

          op->_rhs.push_back(0);
          op->_sense.push_back("<=");

          row_constraint++;

          //Constraint number 2 (P[i1,i2,k] - X[i2,k] <= 0)
          col_action = number_of_units + actions_extended(pu_id2_threat, threat_id[a]) - 1;

          op->_A_i.push_back(row_constraint);
          op->_A_j.push_back(col_constraint);
          op->_A_x.push_back(1);

          op->_A_i.push_back(row_constraint);
          op->_A_j.push_back(col_action);
          op->_A_x.push_back(-1);

          op->_rhs.push_back(0);
          op->_sense.push_back("<=");

          row_constraint++;

          //Constraint number 3 (P[i1,i2,k] - X[i1,k] - X[i2,k] => -1)
          op->_A_i.push_back(row_constraint);
          op->_A_j.push_back(col_constraint);
          op->_A_x.push_back(1);

          col_action = number_of_units + actions_extended(pu_id1_threat[a], threat_id[a]) - 1;
          op->_A_i.push_back(row_constraint);
          op->_A_j.push_back(col_action);
          op->_A_x.push_back(-1);

          col_action = number_of_units + actions_extended(pu_id2_threat, threat_id[a]) - 1;
          op->_A_i.push_back(row_constraint);
          op->_A_j.push_back(col_action);
          op->_A_x.push_back(-1);

          op->_rhs.push_back(-1);
          op->_sense.push_back(">=");

          row_constraint++;
          col_constraint++;
        }
      }
    }
  }
  return true;
}
