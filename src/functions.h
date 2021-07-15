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

arma::sp_mat create_dist_threats_extended(DataFrame, int, int);

arma::sp_mat create_dist_features_extended(DataFrame, int, int);

arma::sp_mat create_sensitivity_extended(DataFrame, int, int);

arma::sp_mat create_sensitivity_param_extended(DataFrame, int, int, String);


arma::sp_mat create_intersection_threats_features(arma::sp_mat, arma::sp_mat, arma::sp_mat, int);

arma::sp_mat create_actions_extended(DataFrame, int, int);




NumericVector concatenate(NumericVector, NumericVector);

StringVector concatenateString(StringVector, StringVector);

IntegerVector manualWhich(IntegerVector, int);

int getSizeStatusUnits(DataFrame);

int getSizeStatusActions(DataFrame);

//It gets you an 'INT Number' from: Planning Units (returns the number of planning units).
int getUnits(DataFrame);

//It gets you an 'INT Number' from: Species (returns the number of species).
int getSpecies(DataFrame);

//It gets you an 'INT Number' from: Threats (returns the number of threats).
int getThreats(DataFrame);

//New methods for linearization of the measure of the "local benefit of the species" (constraint MAMP.2)
double getExponent(DataFrame);

int getSegments(DataFrame);


int getBreakpoints(DataFrame);


NumericVector get_bp(DataFrame);

NumericVector get_bp3(DataFrame);

NumericVector get_slope(DataFrame);

//It gives you an 'INT Number' from: parameter "beta1".
double getBlm(DataFrame);

//It gives you an 'INT Number' from: parameter 'beta2'.
double getBetaActions(DataFrame);

/*
 * Function sets the name of a variable "W" with is respective index "i" (i.e, "W[i]").
 */
String variableW(int);

/*
 * Function that creates a vector with the name of all variables W,
 * (i.e, vectorVariablesW = {"W[1]","W[2]", ..., "W[n]"}).
 */
CharacterVector createVectorVarW(int);

/*
 * Function sets the name of a variable "Y" with is respective index "i1" and "i2" (i.e, "Y[i1,i2]").
 */
String variableY(int, int);

/*
 * Function sets the name of a variable "X" with is respective index "i" and "k" (i.e, "X[i,k]").
 */
String variableX(int, int);

/*
 * Function sets the name of a variable "Z" with is respective index "i" and "s" (i.e, "Z[i,s]").
 */
String variableZ(int, int);

/*
 * Function sets the name of a variable "B" with is respective index "i" and "s" (i.e, "B[i,s]").
 */
String variableB(int, int);

/*
 * Function sets the name of a variable "Lambda" with is respective sub-index "i", "s" and "m" (i.e, "Lambda[i,s,m]").
 */
String variableLambda(int, int, int);

/*
 * Function sets the name of a variable "V" with is respective sub-index "i", "s" and "m" (i.e, "V[i,s,m]").
 */
String variableV(int, int, int);

/*
 * Function sets the name of a variable "P" with is respective sub-index "i1", "i2" and "k" (i.e, "P[i1,i2,k]").
 */
String variableP(int, int, int);


IntegerVector getSubset(DataFrame, String, int);

//It gets you a 'MAP<int,std::map<int,bool>>' from: Planning Units|Species|Amount.
//Corresponds to the set S[i] (only create S[i] subsets that are NOT empty!).
std::map<int,std::map<int,double>> getSpeciesDistribution(DataFrame);


//It gets you a 'MAP<int,std::map<int,bool>>' from: Species|Planning Units|Amount.
//Corresponds to the "transpose" of the species distribution data (i.e. set I[s]),
//but only create I[s] subsets that are NOT empty!
std::map<int,std::map<int,double>> getSpeciesDistribution_t(DataFrame);

//It gets you a 'MAP<int,std::map<int,bool>>' from: Planning Units|Threats|Amount.
//Corresponds to the set K[i] (only create K[i] subsets that are NOT empty!).
std::map<int,std::map<int,double>> getThreatsDistribution(DataFrame);

//It gets you a 'MAP<int,std::map<int,bool>>' from: Species|Threats|Sensibility.
//Corresponds to the set K[s] (only create K[s] subsets that are NOT empty!).
std::map<int,std::map<int,double>> getSensibility(DataFrame);


//It gets you a LIST with the set that you required, for example,
//"Ki" gives you the set K[i] (the options are: "Si", "Is", "Ki", "Ks").
//The method only needs the name of the set (Rcpp::String).
List getSet(DataFrame, DataFrame, DataFrame, DataFrame, DataFrame, DataFrame, String);


List get_UnitStatus(DataFrame);


#endif
