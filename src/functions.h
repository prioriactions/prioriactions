#pragma once
#ifndef GUARD_functions_h
#define GUARD_functions_h

#include "Package.h"

/*
 * ...........................................................................................
 * METHODS....................................................................................
 * ...........................................................................................
 */

/*
 * Function that creates the original matrix "cv[i1, i2]" from the information of the
 * dataframe "boundary_Data" (dense matrix with tuple data).
 * Function only needs the number of planning units, the type the data distribution
 * that you want (symmetric or asymmetric), and the data (tuple data), obviously.
 */
arma::sp_mat create_boundary_matrix_extended(DataFrame, int);
arma::sp_mat create_dist_threats_extended(DataFrame, int, int, NumericVector);
arma::sp_mat create_dist_features_extended(DataFrame, int, int);
arma::sp_mat create_sensitivity_extended(DataFrame, int, int);
arma::sp_mat create_sensitivity_param_extended(DataFrame, int, int, String);
arma::sp_mat create_actions_extended(DataFrame, int, int);


#endif
