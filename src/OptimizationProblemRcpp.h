#ifndef GUARD_OptimizationProblemRcppClass_h
#define GUARD_OptimizationProblemRcppClass_h
#include "Package.h"

/*
 * *************************************************************
 * ********************** CLASS: MAMPData **********************
 * *************************************************************
 */

//CLASS DECLARATION.
class OptimizationProblemRcpp{
public:
  //MAMPData();
  OptimizationProblemRcpp(); //Constructor
  // DataFrame target_Data; //Equivalent to target_Data
  // DataFrame unitCost_Data; //Equivalent to unitCost_Data
  // DataFrame boundary_Data; //Equivalent to boundary_Data
  // DataFrame speciesDistribution_Data; //Equivalent to speciesDistribution_Data
  // //New input data for the MAMP problem
  // DataFrame threatsDistribution_Data; //Equivalent to threatsDistribution_Data
  // DataFrame sensibility_Data; //Equivalent to sensibility_Data
  // List settings_Data;      //Equivalent to settings_Data
  //

  // deconstructor
  ~OptimizationProblemRcpp(){};

  //Methods
  Rcpp::List Create_new_optimization_problem(DataFrame, DataFrame, DataFrame, DataFrame, DataFrame, DataFrame, List);







private:
  // Member variables
  std::vector<double> _C;
  std::vector<std::size_t> _A_i;
  std::vector<std::size_t> _A_j;
  std::vector<double> _A_x;
  std::vector<double> _rhs;
  std::vector<std::string> _vtype;
  std::vector<double> _lb;
  std::vector<double> _ub;
  std::vector<std::string> _sense;

  // int getUnits2();
  // int units = getUnits2();
  //
  // int getSpecies2();
  // int species = getUnits2();
  //
  // int getThreats2();
  // int threats = getThreats2();
  //
  // std::map<int,std::map<int,double>> getSpeciesDistribution();
  // std::map<int,std::map<int,double>> getSpeciesDistribution_t();
  // std::map<int,std::map<int,double>> getThreatsDistribution();
  // std::map<int,std::map<int,double>> getSensibility();
  //
  // List getSet(String setName);
  // List get_UnitStatus();
  // int getSizeStatusUnits();
  // int getSizeStatusActions();
  //
  // List Ki = getSet("Ki");
  // List Si = getSet("Si");
  // List Ks = getSet("Ks");
  // List UnitStatus = get_UnitStatus();
  //
};

#endif
