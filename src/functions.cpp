#include "functions.h"

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
arma::sp_mat create_boundary_matrix_extended(DataFrame boundary_data, int units){

  IntegerVector boundary_data_id1 = boundary_data["internal_id1"];
  IntegerVector id1 = clone(boundary_data_id1);
  id1 = id1 - 1;
  IntegerVector boundary_data_id2 = boundary_data["internal_id2"];
  IntegerVector id2 = clone(boundary_data_id2);
  id2 = id2 - 1;
  NumericVector bound = boundary_data["boundary"];

    //nrow = units, ncol = units (quadrate matrix)
  arma::sp_mat boundary_matrix_extended(units, units);
  int boundary_data_size = id1.size();

  for(int l = 0; l < boundary_data_size; l++){

    if(boundary_matrix_extended(id1[l], id2[l]) == 0){
      boundary_matrix_extended(id1[l], id2[l]) = bound[l];
      boundary_matrix_extended(id2[l], id1[l]) = bound[l];

    }
    else{
      boundary_matrix_extended(id1[l], id2[l]) = bound[l];
    }
  }
  return boundary_matrix_extended;
}



arma::sp_mat create_dist_threats_extended(DataFrame dist_threats_data, int units, int threats){

  IntegerVector dist_threats_data_pu_id = dist_threats_data["internal_pu"];
  IntegerVector pu_id = clone(dist_threats_data_pu_id);
  pu_id = pu_id - 1;
  IntegerVector dist_threats_data_threat_id = dist_threats_data["internal_threats"];
  IntegerVector threat_id = clone(dist_threats_data_threat_id);
  threat_id = threat_id - 1;

  NumericVector action_amount = dist_threats_data["amount"];
  int number_of_actions = dist_threats_data.nrows();

  arma::sp_mat dist_threats_extended(units, threats);


  for(int a = 0; a < number_of_actions; a++){

    dist_threats_extended(pu_id[a], threat_id[a]) = action_amount[a];

  }
  return dist_threats_extended;
}



arma::sp_mat create_dist_features_extended(DataFrame dist_features_data, int units, int features){

  IntegerVector dist_features_data_pu_id = dist_features_data["internal_pu"];
  IntegerVector pu_id = clone(dist_features_data_pu_id);
  pu_id = pu_id - 1;
  IntegerVector dist_features_data_feature_id = dist_features_data["internal_species"];
  IntegerVector feature_id = clone(dist_features_data_feature_id);
  feature_id = feature_id - 1;

  NumericVector feature_amount = dist_features_data["amount"];


  arma::sp_mat dist_features_extended(units, features);


  for(int i = 0; i < dist_features_data.nrows(); i++){

    dist_features_extended(pu_id[i], feature_id[i]) = feature_amount[i];

  }
  return dist_features_extended;
}


arma::sp_mat create_sensitivity_extended(DataFrame sensitivity_data, int features, int threats){

  IntegerVector sensitivity_data_feature_id = sensitivity_data["internal_species"];
  IntegerVector feature_id = clone(sensitivity_data_feature_id);
  feature_id = feature_id - 1;
  IntegerVector sensitivity_data_threat_id = sensitivity_data["internal_threats"];
  IntegerVector threat_id = clone(sensitivity_data_threat_id);
  threat_id = threat_id - 1;

  arma::sp_mat sensitivity_extended(features, threats);

  for(int i = 0; i < sensitivity_data.nrows(); i++){

    sensitivity_extended(feature_id[i], threat_id[i]) = 1;

  }
  return sensitivity_extended;
}

arma::sp_mat create_sensitivity_param_extended(DataFrame sensitivity_data, int features, int threats, String param){

  IntegerVector sensitivity_data_feature_id = sensitivity_data["internal_species"];
  IntegerVector feature_id = clone(sensitivity_data_feature_id);
  feature_id = feature_id - 1;
  IntegerVector sensitivity_data_threat_id = sensitivity_data["internal_threats"];
  IntegerVector threat_id = clone(sensitivity_data_threat_id);
  threat_id = threat_id - 1;

  arma::sp_mat sensitivity_param_extended(features, threats);
  NumericVector sensitivity_param;

  if(param == "a"){
    sensitivity_param = sensitivity_data["a"];
  }
  else if( param == "b"){
    sensitivity_param = sensitivity_data["b"];
  }
  else if( param == "c"){
    sensitivity_param = sensitivity_data["c"];
  }
  else if( param == "d"){
    sensitivity_param = sensitivity_data["d"];
  }

  for(int i = 0; i < sensitivity_data.nrows(); i++){

    sensitivity_param_extended(feature_id[i], threat_id[i]) = sensitivity_param[i];

  }
  return sensitivity_param_extended;
}



arma::sp_mat create_intersection_threats_features(arma::sp_mat dist_threats_extended,
                                                  arma::sp_mat dist_features_extended,
                                                  arma::sp_mat sensitivity_extended,
                                                  int feature){

  arma::sp_mat intersection_threats_features = dist_threats_extended;

  int pu_id;
  int threat_id;

  for (auto it = intersection_threats_features.begin();
       it != intersection_threats_features.end(); ++it) {

    pu_id = it.row();
    threat_id = it.col();

    if(dist_features_extended(pu_id, feature) == 0){
      intersection_threats_features(pu_id, threat_id) = 0;
    }
    else if(sensitivity_extended(feature, threat_id) == 0){
      intersection_threats_features(pu_id, threat_id) = 0;
    }
  }

  return intersection_threats_features;
}


arma::sp_mat create_actions_extended(DataFrame dist_threats_data, int units, int threats){

  IntegerVector dist_threats_data_pu_id = dist_threats_data["internal_pu"];
  IntegerVector pu_id = clone(dist_threats_data_pu_id);
  pu_id = pu_id - 1;
  IntegerVector dist_threats_data_threat_id = dist_threats_data["internal_threats"];
  IntegerVector threat_id = clone(dist_threats_data_threat_id);
  threat_id = threat_id - 1;

  int number_of_actions = dist_threats_data.nrows();

  arma::sp_mat actions_extended(units, threats);


  for(int a = 0; a < number_of_actions; a++){

    actions_extended(pu_id[a], threat_id[a]) = a + 1;

  }
  return actions_extended;
}














NumericVector concatenate(NumericVector vectorA, NumericVector vectorB){

  for(NumericVector::iterator i = vectorB.begin(); i != vectorB.end(); ++i){
    vectorA.push_back(*i);
  }

  return vectorA;
}

StringVector concatenateString(StringVector vectorA, StringVector vectorB){

  for(int i = 0; i < vectorB.size(); i++){
    vectorA.push_back(vectorB[i]);
  }

  return vectorA;
}

IntegerVector manualWhich(IntegerVector vector, int index){

  IntegerVector subset;
  for(int i = 0; i < vector.size(); i++){
    if(vector[i] == index){
      subset.push_back(i);
    }
  }
  return subset;
}



int getSizeStatusUnits(DataFrame unitCost_Data){
  int size = 0;
  IntegerVector statusVector = unitCost_Data["status"];

  for(int i = 0; i < unitCost_Data.size(); i++){
    if(statusVector[i] != 0){
      size += 1;
    }
  }

  return size;
}

int getSizeStatusActions(DataFrame threatsDistribution_Data){
  int size = 0;
  IntegerVector statusVector = threatsDistribution_Data["status"];
  IntegerVector amountVector = threatsDistribution_Data["amount"];

  for(int i = 0; i < threatsDistribution_Data.size(); i++){
    if(statusVector[i] != 0 && amountVector[i] != 0){
      size += 1;
    }
  }

  return size;
}


//METHOD: It gets you an 'INT Number' from: Planning Units (returns the number of planning units).
int getUnits(DataFrame pu_data){
  int units = pu_data.nrows();
  return units;
}

//METHOD: It gets you an 'INT Number' from: Species (returns the number of species).
int getSpecies(DataFrame target_Data){
  int species = target_Data.nrows();
  return species;
}


//METHOD: It gets you an 'INT Number' from: Threats (returns the number of threats).
int getThreats(DataFrame threats_data){
  int threats = threats_data.nrows();
  return threats;
}

//New methods for linearization of the measure of the "local benefit of the species" (constraint MAMP.2)
//METHOD:
double getExponent(DataFrame settings_Data){
  double exponent = settings_Data(2);
  return exponent;
}

//METHOD:
int getSegments(DataFrame settings_Data){
  int segments = settings_Data(3);
  return segments;
}

//METHOD:
int getBreakpoints(DataFrame settings_Data){
  int breakpoints = getSegments(settings_Data) + 1;
  return breakpoints;
}



//METHOD:
NumericVector get_bp(DataFrame settings_Data){
  //"bp" is a numerical vector of cardinality equal to the number of breakpoints.
  int breakpoints = getBreakpoints(settings_Data);
  int segments = breakpoints - 1;

  NumericVector bp(breakpoints);
  double valueX;

  for(int m = 1; m <= breakpoints; m++){
    valueX    = (1.0/segments)*(m - 1);
    bp(m - 1) = valueX;
  }
  return bp;
}

//METHOD:
NumericVector get_bp3(DataFrame settings_Data){
  //"bp3" is a numerical vector of cardinality equal to the number of breakpoints.
  int breakpoints = getBreakpoints(settings_Data);
  //int exponent = getExponent(settings_Data);
  double exponent = getExponent(settings_Data);


  NumericVector bp3(breakpoints);
  NumericVector bp = get_bp(settings_Data);
  double valueY;

  for(int m = 1; m <= breakpoints; m++){
    valueY     = pow( bp(m - 1) , exponent );
    bp3(m - 1) = valueY;
  }
  return bp3;
}

//METHOD:
NumericVector get_slope(DataFrame settings_Data){
  //"slope" is a numerical vector of cardinality equal to the number of segments.
  int segments = getSegments(settings_Data);

  NumericVector slope(segments);
  NumericVector bp  = get_bp(settings_Data);
  NumericVector bp3 = get_bp3(settings_Data);
  double deltaY = 0.0;
  double deltaX = 0.0;

  for(int m = 0; m < segments; m++){
    deltaY = bp3(m + 1) - bp3(m);
    deltaX = bp(m + 1) - bp(m);
    slope(m) = deltaY/deltaX;
  }
  return slope;
}

//METHOD: It gives you an 'INT Number' from: parameter "beta1".
double getBlm(DataFrame settings_Data){
  //'beta1' is a boundary length modiﬁer: it can be varied for more or less connected reserve systems.
  double blm = settings_Data(0); //'beta1' = 1 by default!
  return blm;
}

//METHOD: It gives you an 'INT Number' from: parameter 'beta2'.
double getBetaActions(DataFrame settings_Data){
  //'betaActions' is a penalty factor associated to the spatial fragmentation of actions, which has the same goal than "beta1" in MAMP model.
  double betaActions = settings_Data(1); //'beta2' = 1 by default!
  return betaActions;
}

/*
 * Function sets the name of a variable "W" with is respective index "i" (i.e, "W[i]").
 */
String variableW(int indexI){
  std::string index_I       = std::to_string(indexI);
  std::string parenthesis_1 = "[";
  std::string parenthesis_2 = "]";
  std::string varW          = "W" + parenthesis_1 + index_I + parenthesis_2;
  return varW;
}

/*
 * Function that creates a vector with the name of all variables W,
 * (i.e, vectorVariablesW = {"W[1]","W[2]", ..., "W[n]"}).
 */
CharacterVector createVectorVarW(int units){
  CharacterVector vectorVariablesW;
  std::string varW;

  for(int i = 0; i < units; i++){
    varW = variableW(i+1);
    vectorVariablesW.push_back(varW);
    //I use ".push_back()" because I don't prefix the length of the vector "vectorVariablesW".
  }
  return vectorVariablesW;
}

/*
 * Function sets the name of a variable "Y" with is respective index "i1" and "i2" (i.e, "Y[i1,i2]").
 */
String variableY(int indexI, int indexJ){
  std::string index_I       = std::to_string(indexI);
  std::string index_J       = std::to_string(indexJ);
  std::string i_NameIndex   = "i";
  std::string parenthesis_1 = "[";
  std::string parenthesis_2 = "]";
  std::string comma         = ",";
  //std::string varY          = "Y" + parenthesis_1 + i_NameIndex + index_I + comma + i_NameIndex + index_J + parenthesis_2;
  std::string varY          =  "Y" + parenthesis_1 + index_I + comma + index_J + parenthesis_2;
  return varY;
}

/*
 * Function sets the name of a variable "X" with is respective index "i" and "k" (i.e, "X[i,k]").
 */
String variableX(int indexI, int indexK){
  std::string index_I       = std::to_string(indexI);
  std::string index_K       = std::to_string(indexK);
  std::string parenthesis_1 = "[";
  std::string parenthesis_2 = "]";
  std::string comma         = ",";
  std::string varX          = "X" + parenthesis_1 + index_I + comma + index_K + parenthesis_2;
  return varX;
}

/*
 * Function sets the name of a variable "Z" with is respective index "i" and "s" (i.e, "Z[i,s]").
 */
String variableZ(int indexI, int indexS){
  std::string index_I       = std::to_string(indexI);
  std::string index_S       = std::to_string(indexS);
  std::string parenthesis_1 = "[";
  std::string parenthesis_2 = "]";
  std::string comma         = ",";
  std::string varZ          = "Z" + parenthesis_1 + index_I + comma + index_S + parenthesis_2;
  return varZ;
}

/*
 * Function sets the name of a variable "B" with is respective index "i" and "s" (i.e, "B[i,s]").
 */
String variableB(int indexI, int indexS){
  std::string index_I       = std::to_string(indexI);
  std::string index_S       = std::to_string(indexS);
  std::string parenthesis_1 = "[";
  std::string parenthesis_2 = "]";
  std::string comma         = ",";
  std::string varB          = "B" + parenthesis_1 + index_I + comma + index_S + parenthesis_2;
  return varB;
}

/*
 * Function sets the name of a variable "Lambda" with is respective sub-index "i", "s" and "m" (i.e, "Lambda[i,s,m]").
 */
String variableLambda(int indexI, int indexS, int indexM){
  std::string index_I       = std::to_string(indexI);
  std::string index_S       = std::to_string(indexS);
  std::string index_M       = std::to_string(indexM);
  std::string parenthesis_1 = "[";
  std::string parenthesis_2 = "]";
  std::string comma         = ",";
  std::string varLambda     = "Lambda" + parenthesis_1 + index_I + comma + index_S + comma + index_M + parenthesis_2;
  return varLambda;
}

/*
 * Function sets the name of a variable "V" with is respective sub-index "i", "s" and "m" (i.e, "V[i,s,m]").
 */
String variableV(int indexI, int indexS, int indexM){
  std::string index_I       = std::to_string(indexI);
  std::string index_S       = std::to_string(indexS);
  std::string index_M       = std::to_string(indexM);
  std::string parenthesis_1 = "[";
  std::string parenthesis_2 = "]";
  std::string comma         = ",";
  std::string varV          = "V" + parenthesis_1 + index_I + comma + index_S + comma + index_M + parenthesis_2;
  return varV;
}

/*
 * Function sets the name of a variable "P" with is respective sub-index "i1", "i2" and "k" (i.e, "P[i1,i2,k]").
 */
String variableP(int indexI1, int indexI2, int indexK){
  std::string index_I1      = std::to_string(indexI1);
  std::string index_I2      = std::to_string(indexI2);
  std::string index_K       = std::to_string(indexK);
  std::string parenthesis_1 = "[";
  std::string parenthesis_2 = "]";
  std::string comma         = ",";
  std::string varP          = "P" + parenthesis_1 + index_I1 + comma + index_I2 + comma + index_K + parenthesis_2;
  return varP;
}


IntegerVector getSubset(DataFrame Data, String setName, int index){
  IntegerVector setRequired;
  int setCardinality;
  IntegerVector set_vector;
  IntegerVector subset_vector;

  if(setName == "Ki"){
    setCardinality = Data.nrows();
    set_vector = Data["pu"];
    subset_vector = Data["threats"];

    for(int i = 0; i < setCardinality; i++){
      if(set_vector[i] == index){
        setRequired.push_back(subset_vector[i]);
      }
    }
  }
  if(setName == "Si"){
    setCardinality = Data.nrows();
    set_vector = Data["pu"];
    subset_vector = Data["species"];

    for(int i = 0; i < setCardinality; i++){
      if(set_vector[i] == index){
        setRequired.push_back(subset_vector[i]);
      }
    }
  }
  if(setName == "Ks"){
    setCardinality = Data.nrows();
    set_vector = Data["species"];
    subset_vector = Data["threats"];

    for(int i = 0; i < setCardinality; i++){
      if(set_vector[i] == index){
        setRequired.push_back(subset_vector[i]);
      }
    }
  }
  return setRequired;
}

//METHOD: It gets you a 'MAP<int,std::map<int,bool>>' from: Planning Units|Species|Amount.
//Corresponds to the set S[i] (only create S[i] subsets that are NOT empty!).
std::map<int,std::map<int,double>> getSpeciesDistribution(DataFrame speciesDistribution_Data){
  std::map<int,std::map<int,double>> speciesDistributionData; //It stores the value of species and their quantity in the respective planning unit.

  IntegerVector vectorAux05 = speciesDistribution_Data["internal_pu"]; //To identify the planning units.
  IntegerVector vectorAux06 = speciesDistribution_Data["internal_species"]; //To identify the species.
  NumericVector vectorAux07 = speciesDistribution_Data["amount"]; //To identify the quantity of each species.
  int dataSize = speciesDistribution_Data.nrows();
  int iAux     = 0;
  int jAux     = 0;
  double kAux    = 0;

  for(int r = 0; r < dataSize; r++){
    iAux = vectorAux05[r] - 1; //Unit in specific.
    jAux = vectorAux06[r] - 1; //Specie in specific.
    kAux = vectorAux07[r]; //Is specie j in unit i? k = TRUE if so, k = FALSE if it isn't.
    speciesDistributionData[iAux][jAux] = kAux;
  }

  return speciesDistributionData;
}

//METHOD: It gets you a 'MAP<int,std::map<int,bool>>' from: Species|Planning Units|Amount.
//Corresponds to the "transpose" of the species distribution data (i.e. set I[s]),
//but only create I[s] subsets that are NOT empty!
std::map<int,std::map<int,double>> getSpeciesDistribution_t(DataFrame speciesDistribution_Data){
  std::map<int,std::map<int,double>> speciesDistributionData_t; //It stores the value of species and their quantity in the respective planning unit.

  IntegerVector vectorAux05 = speciesDistribution_Data["internal_species"]; //To identify the species.
  IntegerVector vectorAux06 = speciesDistribution_Data["internal_pu"]; //To identify the planning units.
  NumericVector vectorAux07 = speciesDistribution_Data["amount"]; //To identify the quantity of each species.
  int dataSize = speciesDistribution_Data.nrows();
  int iAux     = 0;
  int jAux     = 0;
  double kAux    = 0;

  for(int r = 0; r < dataSize; r++){
    iAux = vectorAux05[r] - 1; //Specie in specific.
    jAux = vectorAux06[r] - 1; //Unit in specific.
    kAux = vectorAux07[r]; //Unit i is part of the habitat of species j? k = TRUE if so, k = FALSE if it isn't.
    speciesDistributionData_t[iAux][jAux] = kAux;
  }
  return speciesDistributionData_t;
}

//METHOD: It gets you a 'MAP<int,std::map<int,bool>>' from: Planning Units|Threats|Amount.
//Corresponds to the set K[i] (only create K[i] subsets that are NOT empty!).
std::map<int,std::map<int,double>> getThreatsDistribution(DataFrame threatsDistribution_Data){
  std::map<int,std::map<int,double>> threatsDistributionData; //It stores the value of threats and their quantity in the respective planning unit.

  IntegerVector vectorAux05 = threatsDistribution_Data["internal_pu"]; //To identify the planning units.
  IntegerVector vectorAux06 = threatsDistribution_Data["internal_threats"]; //To identify the threats.
  NumericVector vectorAux07 = threatsDistribution_Data["amount"]; //To identify the quantity of each threats.
  int dataSize = threatsDistribution_Data.nrows();
  int iAux     = 0;
  int jAux     = 0;
  double kAux    = 0;

  for(int r = 0; r < dataSize; r++){
    iAux = vectorAux05[r] - 1; //Unit in specific.
    jAux = vectorAux06[r] - 1; //Threat in specific.
    kAux = vectorAux07[r]; //Is threat j in unit i? k = TRUE if so, k = FALSE if it isn't.
    threatsDistributionData[iAux][jAux] = kAux;
  }

  return threatsDistributionData;
}

//METHOD: It gets you a 'MAP<int,std::map<int,bool>>' from: Species|Threats|Sensibility.
//Corresponds to the set K[s] (only create K[s] subsets that are NOT empty!).
std::map<int,std::map<int,double>> getSensibility(DataFrame sensibility_Data){
  std::map<int,std::map<int,double>> sensibilityData; //It stores the sensibility of species to threats.

  IntegerVector vectorAux05 = sensibility_Data["internal_species"]; //To identify the species.
  IntegerVector vectorAux06 = sensibility_Data["internal_threats"]; //To identify the threats.
  //NumericVector vectorAux07 = sensibility_Data["amount"]; //To identify the threats that affect the species s.
  int dataSize = sensibility_Data.nrows();
  int iAux     = 0;
  int jAux     = 0;
  double kAux    = 0;

  for(int r = 0; r < dataSize; r++){
    iAux = vectorAux05[r] - 1; //Specie in specific.
    jAux = vectorAux06[r] - 1; //Threat in specific.
    kAux = 1; //Does threat j affect the species i? k = TRUE if so, k = FALSE if it isn't.
    sensibilityData[iAux][jAux] = kAux;
  }

  return sensibilityData;
}


//METHOD: It gets you a LIST with the set that you required, for example,
//"Ki" gives you the set K[i] (the options are: "Si", "Is", "Ki", "Ks").
//The method only needs the name of the set (Rcpp::String).
List getSet(DataFrame unitCost_Data, DataFrame target_Data, DataFrame speciesDistribution_Data, DataFrame threatsDistribution_Data, DataFrame sensibility_Data, DataFrame threats_data, String setName){
  std::map<int,std::map<int,double>> setData; //To identify the data input that is needed.
  int setCardinality = 0;    //To identify the expected cardinality of the set.
  int subsetCardinality = 0; //To identify the expected cardinality of the subsets.
  List setRequired;
  StringVector namesList;
  int units = getUnits(unitCost_Data);
  int species = getSpecies(target_Data);
  int threats = getThreats(threats_data);
  double filterCondition;

  if(setName == "Si"){
    setData = getSpeciesDistribution(speciesDistribution_Data);
    setCardinality = units;
    subsetCardinality = species;
  }
  if(setName == "Is"){
    setData = getSpeciesDistribution_t(speciesDistribution_Data);
    setCardinality = species;
    subsetCardinality = units;
  }
  if(setName == "Ki"){
    setData = getThreatsDistribution(threatsDistribution_Data);
    setCardinality = units;
    subsetCardinality = threats;
  }
  if(setName == "Ks"){
    setData = getSensibility(sensibility_Data);
    setCardinality = species;
    subsetCardinality = threats;
  }

  for(int i = 0; i < setCardinality; i++){
    namesList.push_back(std::to_string(i));
    IntegerVector listValues;
    for(int j = 0; j< subsetCardinality; j++){
      filterCondition = setData[i][j];
      if(filterCondition != 0){
        listValues.push_back(j);
      }
    }
    setRequired.push_back(listValues);
  }//END external for!

  setRequired.names() = namesList;

  return setRequired;
}



//METHOD:
List get_UnitStatus(DataFrame unitCost_Data){
  IntegerVector unitsUnrestricted; //Unrestricted planning units.
  IntegerVector unitsLockedIn;     //Pre-included planning units.
  IntegerVector unitsLockedOut;    //Pre-excluded planning units.
  IntegerVector vectorAux01 = unitCost_Data["internal_id"]; //To identify the planning unit i.
  IntegerVector vectorAux02 = unitCost_Data["status"]; //To identify the "status" of planning unit i (variable W[i]).
  int dataSize = unitCost_Data.nrows();

  for(int r = 0; r < dataSize; r++){
    int filterCondition = vectorAux02[r];
    if(filterCondition == 0){
      int unitIndex = vectorAux01[r];
      unitsUnrestricted.push_back(unitIndex);
    }
    if(filterCondition == 2){ //If the unit status equals 2, it means the unit is "locked-in"
      int unitIndex = vectorAux01[r];
      unitsLockedIn.push_back(unitIndex);
    }
    if(filterCondition == 3){ //If the unit status equals 3, it means the unit is "locked-out"
      int unitIndex = vectorAux01[r];
      unitsLockedOut.push_back(unitIndex);
    }
  }

  List unitStatus = List::create(Named("Unrestricted") = Rcpp::sort_unique(unitsUnrestricted), _["LockedIn"] = Rcpp::sort_unique(unitsLockedIn),_["LockedOut"] = Rcpp::sort_unique(unitsLockedOut));
  return unitStatus;
}
