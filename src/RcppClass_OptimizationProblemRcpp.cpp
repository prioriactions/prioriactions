  #include "OptimizationProblemRcpp.h"

  /*
   * *************************************************************
   * ********************** CLASS: MAMPData **********************
   * *************************************************************
   */
  //RCPP 'EXPOSURE BLOCK'.
  RCPP_MODULE(OptimizationProblemRcppmodule){
    Rcpp::class_<OptimizationProblemRcpp>( "OptimizationProblemRcpp" )
    //.constructor("documentation for default constructor")
      .constructor("documentation for constructor")

    .method("Create_new_optimization_problem", &OptimizationProblemRcpp::Create_new_optimization_problem, "documentation")
    ;
  }


  // //RCPP 'EXPOSURE BLOCK'.
  // RCPP_MODULE(OptimizationProblemRcppmodule){
  //   Rcpp::class_<OptimizationProblemRcpp>( "OptimizationProblemRcpp" )
  //   //.constructor("documentation for default constructor")
  //     .constructor<DataFrame,DataFrame,DataFrame,DataFrame,DataFrame,DataFrame,List>("documentation for constructor")
  //     .field( "target_Data", &OptimizationProblemRcpp::target_Data, "documentation for data01")
  //     .field( "unitCost_Data", &OptimizationProblemRcpp::unitCost_Data, "documentation for data02")
  //     .field( "boundary_Data", &OptimizationProblemRcpp::boundary_Data, "documentation for data03")
  //     .field( "speciesDistribution_Data", &OptimizationProblemRcpp::speciesDistribution_Data, "documentation for data04")
  //     .field( "threatsDistribution_Data", &OptimizationProblemRcpp::threatsDistribution_Data, "documentation for data05")
  //     .field( "sensibility_Data", &OptimizationProblemRcpp::sensibility_Data, "documentation for data06")
  //     .field( "settings_Data", &OptimizationProblemRcpp::settings_Data, "documentation for data07")
  //
  //   .method("Create_new_optimization_problem", &OptimizationProblemRcpp::Create_new_optimization_problem, "documentation")
  //   //.method("getUnits2", &OptimizationProblemRcpp::getUnits2, "documentation")
  //
  //   ; //ATENTION! With ";"
  // }


  //CLASS DEFINITION.
  //CONSTRUCTORS.
  //MAMPData::MAMPData() :x(0), y(0) { } //To set a default constructor!
  //(NOTE: it's advisable to change the name of data01 -> _data01)
  //OptimizationProblemRcpp::OptimizationProblemRcpp(DataFrame _target_Data, DataFrame _unitCost_Data, DataFrame _boundary_Data, DataFrame _speciesDistribution_Data, DataFrame _threatsDistribution_Data, DataFrame _sensibility_Data, List _settings_Data) : target_Data(_target_Data), unitCost_Data(_unitCost_Data), boundary_Data(_boundary_Data), speciesDistribution_Data(_speciesDistribution_Data), threatsDistribution_Data(_threatsDistribution_Data), sensibility_Data(_sensibility_Data), settings_Data(_settings_Data) { }

  OptimizationProblemRcpp::OptimizationProblemRcpp(){
    this->_C.reserve(100000);
    this->_A_i.reserve(100000);
    this->_A_j.reserve(100000);
    this->_A_x.reserve(100000);
    this->_rhs.reserve(100000);
    this->_vtype.reserve(100000);
    this->_lb.reserve(100000);
    this->_ub.reserve(100000);
    this->_sense.reserve(100000);
  }
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
  arma::sp_mat originalMatrix_cv(DataFrame boundary_Data, int units){
    IntegerVector vectorAux01 = boundary_Data[0]; //id1
    IntegerVector vectorAux02 = boundary_Data[1]; //id2
    NumericVector vectorAux03 = boundary_Data[2]; //boundary
    arma::sp_mat matrix_cv(units, units); //nrow = units, ncol = units (quadrate matrix)
    int boundary_Data_size = vectorAux01.size();

    int index_i1 = 0;
    int index_i2 = 0;
    double value;

    for(int l = 0; l < boundary_Data_size; l++){
      index_i1 = vectorAux01[l] -1;
      index_i2 = vectorAux02[l] -1;
      value    = vectorAux03[l];

      if(matrix_cv(index_i1, index_i2) == 0){
        matrix_cv(index_i1, index_i2) = value;
        matrix_cv(index_i2, index_i1) = value;

      }
      else{
        matrix_cv(index_i1, index_i2) = value;
      }
    }
    return matrix_cv;
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
  int getUnits(DataFrame unitCost_Data){
    int units = unitCost_Data.nrows();
    return units;
  }

  //METHOD: It gets you an 'INT Number' from: Species (returns the number of species).
  int getSpecies(DataFrame target_Data){
    int species = target_Data.nrows();
    return species;
  }


  //METHOD: It gets you an 'INT Number' from: Threats (returns the number of threats).
  int getThreats(DataFrame threatsDistribution_Data){
    IntegerVector vectorAux = threatsDistribution_Data[1];
    int threats = *std::max_element(vectorAux.begin(), vectorAux.end()); //t
    //I've used asterisk (*) to convert an 'iterator' type (default return from max_element() function) in an 'int' type.
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
    IntegerVector vectorAux06 = threatsDistribution_Data["threats"]; //To identify the threats.
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
    IntegerVector vectorAux06 = sensibility_Data["threats"]; //To identify the threats.
    NumericVector vectorAux07 = sensibility_Data["amount"]; //To identify the threats that affect the species s.
    int dataSize = sensibility_Data.nrows();
    int iAux     = 0;
    int jAux     = 0;
    double kAux    = 0;

    for(int r = 0; r < dataSize; r++){
      iAux = vectorAux05[r] - 1; //Specie in specific.
      jAux = vectorAux06[r] - 1; //Threat in specific.
      kAux = vectorAux07[r]; //Does threat j affect the species i? k = TRUE if so, k = FALSE if it isn't.
      sensibilityData[iAux][jAux] = kAux;
    }

    return sensibilityData;
  }


  //METHOD: It gets you a LIST with the set that you required, for example,
  //"Ki" gives you the set K[i] (the options are: "Si", "Is", "Ki", "Ks").
  //The method only needs the name of the set (Rcpp::String).
  List getSet(DataFrame unitCost_Data, DataFrame target_Data, DataFrame speciesDistribution_Data, DataFrame threatsDistribution_Data, DataFrame sensibility_Data, String setName){
    std::map<int,std::map<int,double>> setData; //To identify the data input that is needed.
    int setCardinality = 0;    //To identify the expected cardinality of the set.
    int subsetCardinality = 0; //To identify the expected cardinality of the subsets.
    List setRequired;
    StringVector namesList;
    int units = getUnits(unitCost_Data);
    int species = getSpecies(target_Data);
    int threats = getThreats(threatsDistribution_Data);
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


  /*
   * ...........................................................................................
   * STARTING MAIN FUNCTION.....................................................................
   * ...........................................................................................
   */

  Rcpp::List OptimizationProblemRcpp::Create_new_optimization_problem(DataFrame target_Data, DataFrame unitCost_Data,
                                                                      DataFrame boundary_Data, DataFrame speciesDistribution_Data,
                                                                      DataFrame threatsDistribution_Data, DataFrame sensibility_Data,
                                                                      List settings_Data){

    Timer timer;
    timer.step("start");
    //Global parameters
    int n = 1000000000;
    int numberUnits = getUnits(unitCost_Data);
    int numberThreats = getThreats(threatsDistribution_Data);
    int numberSpecies = getSpecies(target_Data);
    double beta1 = getBlm(settings_Data);
    double beta2 = getBetaActions(settings_Data);
    int boundarySize = boundary_Data.nrows();

    //Define variables
    StringVector vectorVarType;
    arma::sp_mat matrix_cv;

    timer.step("C_parameters1_created");

    List Ki = getSet(unitCost_Data, target_Data, speciesDistribution_Data, threatsDistribution_Data, sensibility_Data,"Ki");

    timer.step("C_parameters2_created");

    List Si = getSet(unitCost_Data, target_Data, speciesDistribution_Data, threatsDistribution_Data, sensibility_Data,"Si");

    timer.step("C_parameters3_created");

      List Ks = getSet(unitCost_Data, target_Data, speciesDistribution_Data, threatsDistribution_Data, sensibility_Data,"Ks");

    timer.step("C_parameters4_created");

    List UnitStatus = get_UnitStatus(unitCost_Data);



    if(boundarySize != 0){
      matrix_cv = originalMatrix_cv(boundary_Data, numberUnits);
    }

    timer.step("C_parameters_created");

    //------------------------------------------------------------------------------------------
    //---------------- Vector C - Coefficients associated with the planning cost ---------------
    //--------------------- (coefficients associated with W[i] variables) ----------------------
    //------------------------------------------------------------------------------------------
    //Variable w
    //NumericVector vectorC;
    NumericVector vectorC_VarW;
    int break_W = 0;


    //StringVector VariablesNames = createVectorVarW(numberUnits);

    //Internal parameters
    NumericVector vector_UnitCost  = unitCost_Data["cost"];
    int index_id1_corrected;
    int index_id2_corrected;
    double connectivityCoeff;
    NumericVector connectivityVector(numberUnits);
    NumericVector connectivityVector_actions(numberUnits);

    if(boundarySize != 0){

      IntegerVector id1_vector = boundary_Data["internal_id1"];
      IntegerVector id2_vector = boundary_Data["internal_id2"];
      NumericVector bound_vector = boundary_Data["boundary"];

      for(int i = 0; i < boundarySize; i++){
        index_id1_corrected = id1_vector[i] - 1;
        index_id2_corrected = id2_vector[i] - 1;
        connectivityCoeff = bound_vector[i];

        connectivityVector[index_id1_corrected] = connectivityVector[index_id1_corrected] + connectivityCoeff;
        connectivityVector[index_id2_corrected] = connectivityVector[index_id2_corrected] + connectivityCoeff;
        //This allows you to specify in a set, the values associated with W[i] that are linked to Y[i1,i2]
      }//END external 'for'

      for(int i = 0; i < numberUnits; i++){
        this->_C.push_back(beta1*connectivityVector[i] + vector_UnitCost[i]);
        this->_vtype.push_back("B");
        this->_lb.push_back(0);
        this->_ub.push_back(1);
      }
    }
    else{
      for(int i = 0; i < numberUnits; i++){
        this->_C.push_back(vector_UnitCost[i]);
        this->_vtype.push_back("B");
        this->_lb.push_back(0);
        this->_ub.push_back(1);
      }
    }

    timer.step("C_vector_cost_w_created");
    //Observation: "vectorC_VarW", vector with the associated coefficients of each variable W[i],
    //(number of positions equivalent to the number of planning units)


    //------------------------------------------------------------------------------------------
    //----------------- Vector C - Coefficients associated with the action cost ----------------
    //--------------------- (coefficients associated with X[i,k], P[i1,i2,k] variables) --------------------
    //------------------------------------------------------------------------------------------
    //Variable x
    int break_X = this->_C.size();

    //NumericVector vectorC_VarX;
    NumericVector vectorC_VarP;
    //StringVector vectorVariablesP;

    //Internal parameters
    IntegerVector pu_threats_data = threatsDistribution_Data["internal_pu"];
    IntegerVector id_threats_data = threatsDistribution_Data["threats"];
    NumericVector vector_ActionCost = threatsDistribution_Data["cost"];
    NumericVector vector_ActionAmount = threatsDistribution_Data["amount"];
    IntegerVector subset_Ki;
    int index_id_corrected = 0;
    int index_k_corrected = 0;
    int actions_size = pu_threats_data.size();


    for(int i = 0; i < actions_size; i++){
      index_id_corrected = pu_threats_data[i] -1;
      index_k_corrected = id_threats_data[i] - 1;
      double action_cost = vector_ActionCost[i];

      if(vector_ActionAmount[i] != 0){
        //std::string varX = variableX(pu_threats_data[i], id_threats_data[i]);
        //VariablesNames.push_back(varX);
        if(boundarySize != 0){
          double connectivityCoeffAgreggate = 0;
          for(int j = 0; j < numberUnits; j++){
            if(matrix_cv(index_id_corrected,j) != 0 && index_id_corrected != j){
              subset_Ki = Ki(j);

              auto result1 = std::find(std::begin(subset_Ki), std::end(subset_Ki), index_k_corrected);

              if (result1 != std::end(subset_Ki)) {
                connectivityCoeffAgreggate = connectivityCoeffAgreggate + matrix_cv(index_id_corrected,j);
                vectorC_VarP.push_back(-1*matrix_cv(index_id_corrected,j));

                //std::string varP = variableP(pu_threats_data[i], j + 1, id_threats_data[i]);
                //vectorVariablesP.push_back(varP);
              }
            }
          }
          this->_C.push_back(beta2*connectivityCoeffAgreggate + action_cost);
          this->_vtype.push_back("B");
          this->_lb.push_back(0);
          this->_ub.push_back(1);

          //Statistics
          connectivityVector_actions.push_back(connectivityCoeffAgreggate);
        }
        else{
          this->_C.push_back(action_cost);
          this->_vtype.push_back("B");
          this->_lb.push_back(0);
          this->_ub.push_back(1);

          //Statistics
          connectivityVector_actions.push_back(0);
        }
        //Statistics
        connectivityVector.push_back(0);
      }
    }
    timer.step("C_vector_cost_x_and_p_created");
    //------------------------------------------------------------------------------------------
    //-------------- Vector C - Coefficients associated with the connectivity cost -------------
    //-------------------- (coefficients associated with Y[i1,i2] variables) -------------------
    //------------------------------------------------------------------------------------------
    // Observation: vector with the associated coefficients of each variable Y[i1,i2]
    //(number of positions equivalent to the number of planning units)

    //-------------------------------------------------------NEW-----------------------------------------
    //Variable y
    int break_Y = this->_C.size();

    //int contador = 0;
    if(boundarySize != 0){
      arma::sp_mat z = matrix_cv.t();

      for(arma::sp_mat::const_iterator it = z.begin(); it != z.end(); ++it) {
        if(it.row() != it.col()){
          connectivityCoeff = -1*(*it);

          this->_C.push_back(beta1*connectivityCoeff);
          this->_vtype.push_back("B");
          this->_lb.push_back(0);
          this->_ub.push_back(1);

          //Statistics
          connectivityVector.push_back(connectivityCoeff);
          connectivityVector_actions.push_back(0);

          //contador = contador + 1;
        }
      }
    }



    //-------------------------------------------------------OLD-----------------------------------------
    //Variable y
    // int break_Y = this->_C.size();
    // int contador = 0;
    //
    // if(boundarySize != 0){
    //   for(int i = 0; i < numberUnits; i++){
    //     for(int j = 0; j < numberUnits; j++){
    //       if(matrix_cv(i,j) != 0 && i != j){
    //         //std::string varY = variableY(i + 1, j + 1);
    //         connectivityCoeff = -1*matrix_cv(i,j);
    //
    //         //vectorVariablesY.push_back(varY);
    //         this->_C.push_back(beta1*connectivityCoeff);
    //         this->_vtype.push_back("B");
    //         this->_lb.push_back(0);
    //         this->_ub.push_back(1);
    //
    //         //Statistics
    //         connectivityVector.push_back(connectivityCoeff);
    //         connectivityVector_actions.push_back(0);
    //
    //         contador = contador + 1;
    //       }
    //     }
    //   }
    // }
    //Rcout << "largo del bound: " << contador << "\n";

    //-------------------------------------------------------------------------------------


    timer.step("C_vector_cost_y_created");
    //------------------------------------------------------------------------------------------
    //------------ Vector C - Coefficients associated with the auxiliary variable Z ------------
    //--------------------- (coefficients associated with Z[i,s], B[i,s], Lambda[i,s,m], V[i,s,m] variables) --------------------
    //------------------------------------------------------------------------------------------

   //  //-----------------------------NEW------------------------------------------
   //  int break_P = this->_C.size();
   //
   //  //this->_C = concatenate(this->_C, vectorC_VarP);
   //  for(int i = 0; i < vectorC_VarP.size(); i++){
   //    this->_C.push_back(beta2*vectorC_VarP[i]);
   //    this->_vtype.push_back("B");
   //    this->_lb.push_back(0);
   //    this->_ub.push_back(1);
   //
   //    //Statistics
   //    connectivityVector_actions.push_back(vectorC_VarP[i]);
   //  }
   //
   //  //Internal parameters
   //  int intersectionSize = 0;
   //  double exponent = getExponent(settings_Data);
   //  int numberSegments = getSegments(settings_Data);
   //  IntegerVector subset_Si;
   //  IntegerVector subset_Ks;
   //  IntegerVector subset_Ki_Ks;
   //  NumericVector vectorIndex_i_Bis;
   //  NumericVector vectorIndex_s_Bis;
   //
   //  IntegerVector vectorC_VarLambda;
   //  IntegerVector vectorC_VarZ;
   //  IntegerVector vectorC_VarV;
   //  IntegerVector vectorC_VarB;
   //
   //  //Variable z
   //  IntegerVector vectorIndex_i_Zis;
   //  int vectorC_VarZ_size;
   //
   //  int break_Z = this->_C.size();
   //
   //  for(int i = 0; i < numberUnits; i++){
   //    subset_Si = Si(i);
   //    subset_Ki = Ki(i);
   //    for(int s = 0; s < subset_Si.size(); s++){
   //      subset_Ks = Ks(subset_Si[s]);
   //      subset_Ki_Ks     = Rcpp::intersect(subset_Ki,subset_Ks);
   //      intersectionSize = subset_Ki_Ks.size();
   //
   //      if(intersectionSize == 0){
   //        vectorIndex_i_Zis.push_back(i);
   //
   //        this->_C.push_back(0);
   //        this->_vtype.push_back("B");
   //        this->_lb.push_back(0);
   //        this->_ub.push_back(1);
   //
   //        vectorC_VarZ.push_back(0);
   //      }
   //    }
   //  }
   //
   //  //Variable B
   //  int break_B = this->_C.size();
   //  int vectorC_VarB_size;
   //
   //  for(int i = 0; i < numberUnits; i++){
   //    subset_Si = Si(i);
   //    subset_Ki = Ki(i);
   //    for(int s = 0; s < subset_Si.size(); s++){
   //      subset_Ks = Ks(subset_Si[s]);
   //      subset_Ki_Ks     = Rcpp::intersect(subset_Ki,subset_Ks);
   //      intersectionSize = subset_Ki_Ks.size();
   //
   //      if(intersectionSize == 0){
   //
   //      }
   //      else if(exponent != 1){
   //
   //        vectorIndex_i_Bis.push_back(i);
   //        vectorIndex_s_Bis.push_back(subset_Si[s]);
   //
   //        this->_C.push_back(0);
   //        this->_vtype.push_back("C");
   //        this->_lb.push_back(0);
   //        this->_ub.push_back(1);
   //
   //        vectorC_VarB.push_back(0);
   //      }
   //    }
   //  }
   //
   //
   //  //Variable Lambda
   //  int break_Lambda = this->_C.size();
   //  int vectorC_VarLambda_size;
   //
   //  for(int i = 0; i < numberUnits; i++){
   //    subset_Si = Si(i);
   //    subset_Ki = Ki(i);
   //    for(int s = 0; s < subset_Si.size(); s++){
   //      subset_Ks = Ks(subset_Si[s]);
   //      subset_Ki_Ks     = Rcpp::intersect(subset_Ki,subset_Ks);
   //      intersectionSize = subset_Ki_Ks.size();
   //
   //      if(intersectionSize == 0){
   //
   //      }
   //      else if(exponent != 1){
   //
   //        for(int m = 0; m < numberSegments; m++){
   //
   //          this->_C.push_back(0);
   //          this->_vtype.push_back("C");
   //          this->_lb.push_back(0);
   //          this->_ub.push_back(1);
   //
   //          vectorC_VarLambda.push_back(0);
   //        }
   //      }
   //    }
   //  }
   //
   // //Variable V
   // int break_V = this->_C.size();
   // int vectorC_VarV_size;
   //
   // for(int i = 0; i < numberUnits; i++){
   //   subset_Si = Si(i);
   //   subset_Ki = Ki(i);
   //   for(int s = 0; s < subset_Si.size(); s++){
   //     subset_Ks = Ks(subset_Si[s]);
   //     subset_Ki_Ks     = Rcpp::intersect(subset_Ki,subset_Ks);
   //     intersectionSize = subset_Ki_Ks.size();
   //
   //     if(intersectionSize == 0){
   //
   //     }
   //     else if(exponent != 1){
   //       for(int m = 0; m < numberSegments; m++){
   //
   //         this->_C.push_back(0);
   //         this->_vtype.push_back("B");
   //         this->_lb.push_back(0);
   //         this->_ub.push_back(1);
   //
   //         vectorC_VarV.push_back(0);
   //       }
   //     }
   //   }
   // }


    //timer.step("C_vector_cost_auxiliary_builded");



//-------------------------------------------OLD-------------------------------------------
    //Variable z
    //IntegerVector vectorC_VarZ;
    int vectorC_VarZ_size = 0;
    IntegerVector vectorIndex_i_Zis;
    //StringVector vectorVariablesZ;

    //Variable B
    int vectorC_VarB_size = 0;
    //IntegerVector vectorC_VarB;
    //StringVector vectorVariablesB;

    //Variable Lambda
    int vectorC_VarLambda_size = 0;
    //IntegerVector vectorC_VarLambda;
    //StringVector vectorVariablesLambda;

    //Variable Lambda
    int vectorC_VarV_size = 0;
    //IntegerVector vectorC_VarV;
    //StringVector vectorVariablesV;

    //Internal parameters
    int intersectionSize = 0;
    double exponent = getExponent(settings_Data);
    int numberSegments = getSegments(settings_Data);


    IntegerVector subset_Si;
    IntegerVector subset_Ks;
    IntegerVector subset_Ki_Ks;
    IntegerVector vectorIndex_i_Bis;
    IntegerVector vectorIndex_s_Bis;

    for(int i = 0; i < numberUnits; i++){
      subset_Si = Si(i);
      subset_Ki = Ki(i);
      for(int s = 0; s < subset_Si.size(); s++){
        subset_Ks = Ks(subset_Si[s]);
        subset_Ki_Ks     = Rcpp::intersect(subset_Ki,subset_Ks);
        intersectionSize = subset_Ki_Ks.size();

        if(intersectionSize == 0){
          //std::string varZ = variableZ(i + 1, subset_Si[s]);
          //vectorVariablesZ.push_back(varZ);
          //Rcout << "i: " << i + 1 << ", s: "<< subset_Si[s] + 1 <<"\n";
          vectorIndex_i_Zis.push_back(i);

          vectorC_VarZ_size = vectorC_VarZ_size + 1;
          //vectorC_VarZ.push_back(0);
        }
        else if(exponent != 1.0){
          //std::string varB = variableB(i + 1, subset_Si[s]);
          //vectorVariablesB.push_back(varB);
          //timer.step("C_vector_cost_before_subset");
          vectorC_VarB_size = vectorC_VarB_size + 1;

          //vectorC_VarB.push_back(0);
          vectorIndex_i_Bis.push_back(i);
          vectorIndex_s_Bis.push_back(subset_Si[s]);
          //timer.step("C_vector_cost_after_subset");
          //Rcout << "i: " << i + 1 << ", s: "<< subset_Si[s] + 1 <<"\n";
          if(numberSegments != 1.0){
            for(int m = 0; m < numberSegments; m++){
              vectorC_VarLambda_size = vectorC_VarLambda_size + 1;
              vectorC_VarV_size = vectorC_VarV_size + 1;

              //vectorC_VarLambda.push_back(0);
              //vectorC_VarV.push_back(0);

              //std::string varlambda = variableLambda(i + 1, subset_Si[s], m + 1);
              //vectorVariablesLambda.push_back(varlambda);

              //std::string varV = variableV(i + 1, subset_Si[s], m + 1);
              //vectorVariablesV.push_back(varV);
            }
          }
        }
      }
    }
    timer.step("C_vector_cost_auxiliary_builded");

    int break_P = this->_C.size();

    //this->_C = concatenate(this->_C, vectorC_VarP);
    for(int i = 0; i < vectorC_VarP.size(); i++){
      this->_C.push_back(beta2*vectorC_VarP[i]);
      this->_vtype.push_back("B");
      this->_lb.push_back(0);
      this->_ub.push_back(1);

      //Statistics
      connectivityVector_actions.push_back(vectorC_VarP[i]);
    }

    int break_Z = this->_C.size();
    //this->_C = concatenate(this->_C, vectorC_VarZ);
    for(int i = 0; i < vectorC_VarZ_size; i++){
      this->_C.push_back(0);
      this->_vtype.push_back("B");
      this->_lb.push_back(0);
      this->_ub.push_back(1);

    }

    int break_B = this->_C.size();
    //this->_C = concatenate(this->_C, vectorC_VarB);
    for(int i = 0; i < vectorC_VarB_size; i++){
      this->_C.push_back(0);
      this->_vtype.push_back("C");
      this->_lb.push_back(0);
      this->_ub.push_back(1);

    }

    int break_Lambda = this->_C.size();
    //this->_C = concatenate(this->_C, vectorC_VarLambda);
    for(int i = 0; i < vectorC_VarLambda_size; i++){
      this->_C.push_back(0);
      this->_vtype.push_back("C");
      this->_lb.push_back(0);
      this->_ub.push_back(1);

    }

    int break_V = this->_C.size();
    //this->_C = concatenate(this->_C, vectorC_VarV);
    for(int i = 0; i < vectorC_VarV_size; i++){
      this->_C.push_back(0);
      this->_vtype.push_back("B");
      this->_lb.push_back(0);
      this->_ub.push_back(1);

    }

//--------------------------------------------------------------------------------------------

    // VariablesNames = concatenateString(VariablesNames, vectorVariablesY);
    // VariablesNames = concatenateString(VariablesNames, vectorVariablesP);
    // VariablesNames = concatenateString(VariablesNames, vectorVariablesZ);
    // VariablesNames = concatenateString(VariablesNames, vectorVariablesB);
    // VariablesNames = concatenateString(VariablesNames, vectorVariablesLambda);
    // VariablesNames = concatenateString(VariablesNames, vectorVariablesV);

    timer.step("C created");


    //------------------------------------------------------------------------------------------
    //----- Matrix A - Coefficients associated with the variables of the first restriction -----
    //--------------------------------------- (MAMP. 2) ----------------------------------------
    //------------------------------------------------------------------------------------------

    //arma::sp_mat matrixA(numberSpecies, this->_C.size());
    //NumericVector vectorRhs;
    //CharacterVector vectorSense;

    int posX = 0;
    int posY = 0;
    int posY_acumulated = numberSpecies;
    double coeff = 0;
    double benefit_den = 0;
    int countervariablesB = 0;
    int countervariablesZ = 0;
    std::map<int,std::map<int,double>> SpeciesDistribution = getSpeciesDistribution(speciesDistribution_Data);


    for(int i = 0; i < numberUnits; i++){
      subset_Si = Si(i);
      subset_Ki = Ki(i);

      for(int s = 0; s < subset_Si.size(); s++){
        subset_Ks = Ks(subset_Si[s]);
        subset_Ki_Ks     = Rcpp::intersect(subset_Ki,subset_Ks);
        intersectionSize = subset_Ki_Ks.size();

        if(intersectionSize != 0){
          if(exponent == 1){
            benefit_den = 0.0;
            for(int k = 0; k < intersectionSize; k++){
              for(int i_threats = 0; i_threats < pu_threats_data.size(); i_threats++){
                index_id_corrected = pu_threats_data[i_threats] - 1;
                index_k_corrected =  id_threats_data[i_threats] - 1;

                if(index_id_corrected == i  && index_k_corrected == subset_Ki_Ks[k]){

                  benefit_den = benefit_den + vector_ActionAmount[i_threats];
                  break;
                }
              }
            }
            for(int k = 0; k < intersectionSize; k++){
              for(int i_threats = 0; i_threats < pu_threats_data.size(); i_threats++){
                index_id_corrected = pu_threats_data[i_threats] - 1;
                index_k_corrected =  id_threats_data[i_threats] - 1;

                if(index_id_corrected == i  && index_k_corrected == subset_Ki_Ks[k]){
                  posX = break_X + i_threats;
                  coeff = (double) (vector_ActionAmount[i_threats])/benefit_den;
                  break;
                }
              }
              posY = subset_Si[s];
              //matrixA(posY, posX) = coeff;
              this->_A_i.push_back(posY);
              this->_A_j.push_back(posX);
              this->_A_x.push_back(coeff);
            }
          }
          else{
            posX = break_B + countervariablesB;
            posY = subset_Si[s];
            //matrixA(posY, posX) = 1;
            this->_A_i.push_back(posY);
            this->_A_j.push_back(posX);
            this->_A_x.push_back(1);

            countervariablesB += 1;
          }
        }
        else{
          posX = break_Z + countervariablesZ;
          posY = subset_Si[s];
          //matrixA(posY, posX) = 1;
          this->_A_i.push_back(posY);
          this->_A_j.push_back(posX);
          this->_A_x.push_back(1);

          countervariablesZ += 1;
        }
      }
    }

    ///////////////////////////////RHS/////////////////////////////////////////////////////////
    NumericVector vectorRhs = target_Data["target"];
    for(int i = 0; i < numberSpecies; i++){
      this->_rhs.push_back(vectorRhs[i]);
      this->_sense.push_back(">=");
    }

    timer.step("MAMP2");

    //------------------------------------------------------------------------------------------
    //---- Matrix A - Coefficients associated with the variables of the second restriction -----
    //--------------------------------------- (MAMP. 3) ----------------------------------------
    //------------------------------------------------------------------------------------------
    //arma::sp_mat matrixA_MAMP3(numberUnits, vectorC.size());

    for(int i = 0; i < numberUnits; i++){
      //subset_Ki = getSubset(threatsDistribution_Data, "Ki", i + 1);
      subset_Ki = Ki(i);
      coeff  = -1*subset_Ki.size();
      //matrixA_MAMP3(i, i) = coeff;
      this->_A_i.push_back(posY_acumulated + i);
      this->_A_j.push_back(i);
      this->_A_x.push_back(coeff);

      //vectorRhs.push_back(0);
      this->_rhs.push_back(0);
      this->_sense.push_back("<=");
      //vectorSense.push_back("<=");
    }

    for(int j = 0; j < pu_threats_data.size(); j++){
      index_id_corrected = pu_threats_data[j] -1;
      posX = break_X + j;
      posY = posY_acumulated + index_id_corrected;
      //matrixA_MAMP3(posY, posX) = 1;
      this->_A_i.push_back(posY);
      this->_A_j.push_back(posX);
      this->_A_x.push_back(1);
    }
    //matrixA = join_cols(matrixA, matrixA_MAMP3);

    posY_acumulated += numberUnits;
    timer.step("MAMP3");

    //------------------------------------------------------------------------------------------
    //---- Matrix A - Coefficients associated with the variables of the third restriction ------
    //--------------------------------------- (MAMP. 4) ----------------------------------------
    //------------------------------------------------------------------------------------------

    if(vectorC_VarZ_size != 0){
      IntegerVector auxSet_unitsZis;
      IntegerVector subset_Si_t;
      auxSet_unitsZis = Rcpp::sort_unique(vectorIndex_i_Zis);
      posX = 0;
      posY = 0;

      //arma::sp_mat matrixA_MAMP4(auxSet_unitsZis.size(), vectorC.size());


      for(int i = 0; i < auxSet_unitsZis.size(); i++){
        subset_Si_t = manualWhich(vectorIndex_i_Zis, auxSet_unitsZis[i]);
        coeff = -1*subset_Si_t.size();

        posX = auxSet_unitsZis[i];
        posY = posY_acumulated + i;
        //matrixA_MAMP4(posY, posX) = coeff;
        this->_A_i.push_back(posY);
        this->_A_j.push_back(posX);
        this->_A_x.push_back(coeff);

        //Rcout << "row: " << posY << ", col: " << posX << ", value: " << coeff << "\n";

        for(int j = 0; j < subset_Si_t.size(); j++){
          coeff = 1;
          posX = break_Z + subset_Si_t[j];
          posY = posY_acumulated + i;
          //matrixA_MAMP4(posY, posX) = coeff;
          this->_A_i.push_back(posY);
          this->_A_j.push_back(posX);
          this->_A_x.push_back(coeff);
        }
        //vectorRhs.push_back(0);
        //vectorSense.push_back("<=");
        this->_rhs.push_back(0);
        this->_sense.push_back("<=");
      }
      //matrixA = join_cols(matrixA, matrixA_MAMP4);
      posY_acumulated += auxSet_unitsZis.size();
    }

    timer.step("MAMP4");


    //
    // arma::sp_mat matrixA_MAMP4(1, vectorC.size());
    // countervariablesZ = 0;
    // int i_aux = 9999;
    //
    // if(vectorC_VarZ.size() != 0){
    //   for(int i = 0; i < numberUnits; i++){
    //     //subset_Si = getSubset(speciesDistribution_Data, "Si", i + 1); //The subset is a vector of integers following its definition in C ++.
    //     subset_Si = Si(i);
    //     //subset_Ki = getSubset(threatsDistribution_Data, "Ki", i + 1); //The subset is a vector of integers following its definition in C ++.
    //     subset_Ki = Ki(i);
    //
    //     for(int s = 0; s < subset_Si.size(); s++){
    //       //subset_Ks        = getSubset(sensibility_Data, "Ks", subset_Si[s]); //The subset is a vector of integers following its definition in C ++.
    //       subset_Ks = Ks(s);
    //       subset_Ki_Ks     = Rcpp::intersect(subset_Ki,subset_Ks);
    //       intersectionSize = subset_Ki_Ks.size();
    //
    //       if(intersectionSize == 0){
    //         if(i_aux == 9999 && i != i_aux){
    //           i_aux = 0;
    //           coeff = -subset_Si.size();
    //           matrixA_MAMP4(i_aux, i) = coeff;
    //
    //           posX  = break_Z + countervariablesZ;
    //           posY  = i_aux;
    //           matrixA_MAMP4[posY, posX] = 1;
    //           countervariablesZ += 1;
    //         }
    //         else if(i != i_aux){
    //           arma::sp_mat matrixA_MAMP4_aux(1, vectorC.size());
    //           i_aux += 1;
    //           coeff = -subset_Si.size();
    //           matrixA_MAMP4_aux(i_aux, i) = coeff;
    //
    //           posX  = break_Z + countervariablesZ;
    //           posY  = i_aux;
    //           matrixA_MAMP4_aux[posY, posX] = 1;
    //           countervariablesZ += 1;
    //           arma::sp_mat matrixA_MAMP4 = join_cols(matrixA_MAMP4, matrixA_MAMP4_aux);
    //         }
    //       }
    //     }
    //   }
    //
    // }

    //------------------------------------------------------------------------------------------
    // Matrix A - Coefficients associated the linearisation of the MAMP model objective function
    //------------------ (MAMP.6 - 3 restrictions for each variable Y[i1,i2]) ------------------
    //------------------------------------------------------------------------------------------

    //arma::sp_mat matrixA_MAMP6(3*vectorC_VarY.size(), vectorC.size());

    if(beta1 !=0 && boundarySize != 0){
      int countervariablesY = 0;

      for(int i = 0; i < numberUnits; i++){
        for(int j = 0; j < numberUnits; j++){
          if(matrix_cv(i,j) != 0 && i != j){

            posX = break_Y  +  countervariablesY;
            posY = posY_acumulated + countervariablesY*3;

            //Constraint number 1 (Y[i1,i2] - W[i1] <= 0)
            //matrixA_MAMP6( posY , posX ) =  1;
            this->_A_i.push_back(posY);
            this->_A_j.push_back(posX);
            this->_A_x.push_back(1);

            //matrixA_MAMP6( posY , i ) = -1;
            this->_A_i.push_back(posY);
            this->_A_j.push_back(i);
            this->_A_x.push_back(-1);

            //vectorRhs.push_back(0);
            //vectorSense.push_back("<=");
            this->_rhs.push_back(0);
            this->_sense.push_back("<=");

            //Constraint number 2 (Y[i1,i2] - W[i2] <= 0)
            //matrixA_MAMP6( posY + 1 , posX ) =  1;
            this->_A_i.push_back(posY + 1);
            this->_A_j.push_back(posX);
            this->_A_x.push_back(1);

            //matrixA_MAMP6( posY + 1 , j ) = -1;
            this->_A_i.push_back(posY + 1);
            this->_A_j.push_back(j);
            this->_A_x.push_back(-1);

            //vectorRhs.push_back(0);
            //vectorSense.push_back("<=");
            this->_rhs.push_back(0);
            this->_sense.push_back("<=");

            //Constraint number 3 (Y[i1,i2] - W[i1] - W[i2] => -1)
            //matrixA_MAMP6( posY + 2 , posX ) =  1;
            this->_A_i.push_back(posY + 2);
            this->_A_j.push_back(posX);
            this->_A_x.push_back(1);

            //matrixA_MAMP6( posY + 2 , i ) = -1;
            this->_A_i.push_back(posY + 2);
            this->_A_j.push_back(i);
            this->_A_x.push_back(-1);

            //matrixA_MAMP6( posY + 2 , j ) = -1;
            this->_A_i.push_back(posY + 2);
            this->_A_j.push_back(j);
            this->_A_x.push_back(-1);

            //vectorRhs.push_back(-1);
            //vectorSense.push_back(">=");
            this->_rhs.push_back(-1);
            this->_sense.push_back(">=");

            countervariablesY += 1;
          }
        }
      }
      posY_acumulated += 3*(break_P - break_Y);
      //matrixA = join_cols(matrixA, matrixA_MAMP6);
    }

    timer.step("MAMP6");

    //------------------------------------------------------------------------------------------
    //- Matrix A - Coefficients associated the linearisation of the MAMP model cubic constraint
    //--------------------------------------- (MAMP. 7) ----------------------------------------
    //------------------------------------------------------------------------------------------
    //arma::sp_mat matrixA_MAMP7(vectorC_VarLambda.size(), vectorC.size());


    for(int i = 0; i < vectorC_VarLambda_size; i++){

      posX = break_Lambda + i;
      posY = posY_acumulated + i;
      //matrixA_MAMP7(posY, posX) = 1;
      this->_A_i.push_back(posY);
      this->_A_j.push_back(posX);
      this->_A_x.push_back(1);

      posX = break_V + i;
      //matrixA_MAMP7(posY, posX) = -1;
      this->_A_i.push_back(posY);
      this->_A_j.push_back(posX);
      this->_A_x.push_back(-1);

      //vectorRhs.push_back(0);
      //vectorSense.push_back("<=");
      this->_rhs.push_back(0);
      this->_sense.push_back("<=");
    }
    posY_acumulated += vectorC_VarLambda_size;
    //Rcout << "posY_acumulated MAMP 7: " << posY_acumulated << "\n";
    //matrixA = join_cols(matrixA, matrixA_MAMP7);

    timer.step("MAMP7");

    //------------------------------------------------------------------------------------------
    //- Matrix A - Coefficients associated the linearisation of the MAMP model cubic constraint
    //--------------------------------------- (MAMP. 8) ----------------------------------------
    //------------------------------------------------------------------------------------------
    //arma::sp_mat matrixA_MAMP8(vectorC_VarB.size(), vectorC.size());

    for(int i = 0; i < vectorC_VarB_size; i++){
      posY = posY_acumulated + i;
      for(int m = 0; m < numberSegments; m++)
      {
        posX = break_V + vectorC_VarB_size*m + i;
        //break_V + vectorC_VarB.size()*m + i
        //matrixA_MAMP8(posY, posX) = 1;
        this->_A_i.push_back(posY);
        this->_A_j.push_back(posX);
        this->_A_x.push_back(1);
      }
      //vectorRhs.push_back(1);
      //vectorSense.push_back("==");
      this->_rhs.push_back(1);
      this->_sense.push_back("==");
    }
    posY_acumulated += vectorC_VarB_size;
    //matrixA = join_cols(matrixA, matrixA_MAMP8);

    timer.step("MAMP8");
    //Rcout << "posY_acumulated MAMP 8: " << posY_acumulated << "\n";

    //------------------------------------------------------------------------------------------
    //- Matrix A - Coefficients associated the linearisation of the MAMP model cubic constraint
    //--------------------------------------- (MAMP. 9) ----------------------------------------
    //------------------------------------------------------------------------------------------
    //arma::sp_mat matrixA_MAMP9(vectorC_VarB.size(), vectorC.size());

    countervariablesB = 0;
    NumericVector bp = get_bp(settings_Data);

    for(int l = 0; l < vectorC_VarB_size; l++){

      int i = vectorIndex_i_Bis[l];
      int s = vectorIndex_s_Bis[l];

      subset_Ki = Ki(i);
      subset_Ks = Ks(s);
      subset_Ki_Ks     = Rcpp::intersect(subset_Ki,subset_Ks);
      intersectionSize = subset_Ki_Ks.size();
      benefit_den = 0.0;

      for(int k = 0; k < intersectionSize; k++){
        for(int i_threats = 0; i_threats < pu_threats_data.size(); i_threats++){
          index_id_corrected = pu_threats_data[i_threats] - 1;
          index_k_corrected =  id_threats_data[i_threats] - 1;

          if(index_id_corrected == i  && index_k_corrected == subset_Ki_Ks[k]){

            benefit_den = benefit_den + vector_ActionAmount[i_threats];
            break;
          }
        }
      }
      for(int k = 0; k < intersectionSize; k++){
        for(int i_threats = 0; i_threats < pu_threats_data.size(); i_threats++){
          index_id_corrected = pu_threats_data[i_threats] - 1;
          index_k_corrected =  id_threats_data[i_threats] - 1;

          if(index_id_corrected == i  && index_k_corrected == subset_Ki_Ks[k]){
            //Rcout << "i: "<< i + 1 <<", k:"<< subset_Ki_Ks[k] + 1 <<", s: "<< subset_Si[s] + 1 << ", n=" << countervariablesB << ", index: "<<i_threats << "\n";
            posX = break_X + i_threats;
            coeff = (double) (vector_ActionAmount[i_threats])/benefit_den;
            break;
          }
        }
        posY = posY_acumulated + l;
        //matrixA_MAMP9(posY, posX) = coeff;
        this->_A_i.push_back(posY);
        this->_A_j.push_back(posX);
        this->_A_x.push_back(coeff);
      }
      for(int m = 0; m < numberSegments; m++){
        posY = posY_acumulated + l;
        posX = break_Lambda + vectorC_VarB_size*m + l;
        coeff = -1*( bp[m + 1] - bp[m]);
        //matrixA_MAMP9(posY, posX) = coeff;
        this->_A_i.push_back(posY);
        this->_A_j.push_back(posX);
        this->_A_x.push_back(coeff);

        posX = break_V + vectorC_VarB_size*m + l;
        coeff = -1*( bp[m]);
        //matrixA_MAMP9(posY, posX) = coeff;
        this->_A_i.push_back(posY);
        this->_A_j.push_back(posX);
        this->_A_x.push_back(coeff);
      }
      this->_rhs.push_back(0);
      this->_sense.push_back("==");
    }
    posY_acumulated += vectorC_VarB_size;
    //Rcout << "posY_acumulated MAMP 9: " << posY_acumulated << "\n";
    timer.step("MAMP9");

    // for(int i = 0; i < numberUnits; i++){
    //   //subset_Si = getSubset(speciesDistribution_Data, "Si", i + 1); //The subset is a vector of integers following its definition in C ++.
    //   subset_Si = Si(i);
    //   //subset_Ki = getSubset(threatsDistribution_Data, "Ki", i + 1); //The subset is a vector of integers following its definition in C ++.
    //   subset_Ki = Ki(i);
    //
    //   for(int s = 0; s < subset_Si.size(); s++){
    //     //subset_Ks        = getSubset(sensibility_Data, "Ks", subset_Si[s]); //The subset is a vector of integers following its definition in C ++.
    //     subset_Ks = Ks(subset_Si[s]);
    //     subset_Ki_Ks     = Rcpp::intersect(subset_Ki,subset_Ks);
    //     intersectionSize = subset_Ki_Ks.size();
    //
    //     if(intersectionSize != 0){
    //       if(exponent != 1){
    //
    //         coeff = (double) 1/intersectionSize;
    //         for(int k = 0; k < intersectionSize; k++){
    //           for(int i_threats = 0; i_threats < pu_threats_data.size(); i_threats++){
    //             index_id_corrected = pu_threats_data[i_threats] - 1;
    //             index_k_corrected =  id_threats_data[i_threats] - 1;
    //
    //             if(index_id_corrected == i  && index_k_corrected == subset_Ki_Ks[k]){
    //               Rcout << "i: "<< i + 1 <<", k:"<< subset_Ki_Ks[k] + 1 <<", s: "<< subset_Si[s] + 1 << ", n=" << countervariablesB << ", index: "<<i_threats << "\n";
    //               posX = break_X + i_threats;
    //               break;
    //             }
    //           }
    //           posY = countervariablesB;
    //           matrixA_MAMP9(posY, posX) = coeff;
    //         }
    //
    //         for(int m = 0; m < numberSegments; m++)
    //         {
    //           posY = countervariablesB;
    //           posX = break_Lambda + vectorC_VarB.size()*m + countervariablesB;
    //           coeff = -1*( bp[m + 1] - bp[m]);
    //           matrixA_MAMP9(posY, posX) = coeff;
    //
    //           posX = break_V + vectorC_VarB.size()*m + countervariablesB;
    //           coeff = -1*( bp[m]);
    //           matrixA_MAMP9(posY, posX) = coeff;
    //         }
    //         countervariablesB += 1;
    //       }
    //     }
    //   }
    // }
    //matrixA = join_cols(matrixA, matrixA_MAMP9);

    //------------------------------------------------------------------------------------------
    //- Matrix A - Coefficients associated the linearisation of the MAMP model cubic constraint
    //--------------------------------------- (MAMP. 10) ---------------------------------------
    //------------------------------------------------------------------------------------------

    //arma::sp_mat matrixA_MAMP10(vectorC_VarB.size(), vectorC.size());
    NumericVector bp3 = get_bp3(settings_Data);

    for(int i = 0; i < vectorC_VarB_size; i++){
      posY = posY_acumulated + i;
      posX = break_B + i;
      //matrixA_MAMP10(posY, posX) = 1;
      this->_A_i.push_back(posY);
      this->_A_j.push_back(posX);
      this->_A_x.push_back(1);

      //vectorRhs.push_back(0);
      //vectorSense.push_back("==");
      this->_rhs.push_back(0);
      this->_sense.push_back("==");

      for(int m = 0; m < numberSegments; m++)
      {
        posX = break_Lambda + vectorC_VarB_size*m + i;
        coeff = -1*( bp3[m + 1] - bp3[m] );
        //matrixA_MAMP10(posY, posX) = coeff;
        this->_A_i.push_back(posY);
        this->_A_j.push_back(posX);
        this->_A_x.push_back(coeff);

        posX = break_V + vectorC_VarB_size*m + i;
        coeff = -1*( bp3[m] );
        //matrixA_MAMP10(posY, posX) = coeff;
        this->_A_i.push_back(posY);
        this->_A_j.push_back(posX);
        this->_A_x.push_back(coeff);
      }
    }
    posY_acumulated += vectorC_VarB_size;
    //matrixA = join_cols(matrixA, matrixA_MAMP10);
    //Rcout << "posY_acumulated MAMP 10: " << posY_acumulated << "\n";
    timer.step("MAMP10");

    //------------------------------------------------------------------------------------------
    //- Matrix A - Coeffs. associated the linearisation of the MAMP-E model objective function -
    //---------------- (MAMP.11 - 3 restrictions for each variable P[i1,i2,k]) -----------------
    //------------------------------------------------------------------------------------------

    //arma::sp_mat matrixA_MAMP11(3*vectorC_VarP.size(), vectorC.size());
    int posX_varX = 0;
    int index_k2_corrected = 0;



    if(beta2 !=0 && boundarySize != 0){
      int countervariablesP = 0;

      for(int i = 0; i < pu_threats_data.size(); i++){
        index_id_corrected = pu_threats_data[i] -1;
        index_k_corrected = id_threats_data[i] - 1;

        for(int j = 0; j < numberUnits; j++){
          if(matrix_cv(index_id_corrected,j) != 0 && index_id_corrected != j){

            //subset_Ki = getSubset(threatsDistribution_Data, "Ki", j + 1);
            subset_Ki = Ki(j);

            auto result1 = std::find(std::begin(subset_Ki), std::end(subset_Ki), index_k_corrected);

            if (result1 != std::end(subset_Ki)) {

              for(int i_threats = 0; i_threats < pu_threats_data.size(); i_threats++){
                index_id2_corrected = pu_threats_data[i_threats] - 1;
                index_k2_corrected =  id_threats_data[i_threats] - 1;

                if(index_id2_corrected == j  && index_k2_corrected == index_k_corrected){
                  posX_varX = break_X + i_threats;
                  break;
                }
              }

              posY = posY_acumulated + countervariablesP*3;
              posX = break_P + countervariablesP;

              //Constraint number 1 (P[i1,i2,k] - X[i1,k] <= 0)
              //matrixA_MAMP11( posY, posX ) =  1;
              this->_A_i.push_back(posY);
              this->_A_j.push_back(posX);
              this->_A_x.push_back(1);

              //matrixA_MAMP11( posY , break_X + i ) = -1;
              this->_A_i.push_back(posY);
              this->_A_j.push_back(break_X + i);
              this->_A_x.push_back(-1);

              //vectorRhs.push_back(0);
              //vectorSense.push_back("<=");
              this->_rhs.push_back(0);
              this->_sense.push_back("<=");

              //Constraint number 2 (P[i1,i2,k] - X[i2,k] <= 0)
              //matrixA_MAMP11( posY + 1, posX ) =  1;
              this->_A_i.push_back(posY + 1);
              this->_A_j.push_back(posX);
              this->_A_x.push_back(1);

              //matrixA_MAMP11( posY + 1 , posX_varX ) = -1;
              this->_A_i.push_back(posY + 1);
              this->_A_j.push_back(posX_varX);
              this->_A_x.push_back(-1);

              //vectorRhs.push_back(0);
              //vectorSense.push_back("<=");
              this->_rhs.push_back(0);
              this->_sense.push_back("<=");

              //Constraint number 3 (P[i1,i2,k] - X[i1,k] - X[i2,k] => -1)
              //matrixA_MAMP11( posY + 2, posX ) =  1;
              this->_A_i.push_back(posY + 2);
              this->_A_j.push_back(posX);
              this->_A_x.push_back(1);

              //matrixA_MAMP11( posY + 2 , break_X + i ) = -1;
              this->_A_i.push_back(posY + 2);
              this->_A_j.push_back(break_X + i);
              this->_A_x.push_back(-1);

              //matrixA_MAMP11( posY + 2, posX_varX ) = -1;
              this->_A_i.push_back(posY + 2);
              this->_A_j.push_back(posX_varX);
              this->_A_x.push_back(-1);

              //vectorRhs.push_back(-1);
              //vectorSense.push_back(">=");
              this->_rhs.push_back(-1);
              this->_sense.push_back(">=");

              countervariablesP += 1;
            }
          }
        }
      }
      posY_acumulated += 3*vectorC_VarP.size();
      //matrixA = join_cols(matrixA, matrixA_MAMP11);
    }

    timer.step("MAMP11");
    //------------------------------------------------------------------------------------------
    //------- Matrix A - Coeffs. associated with the initial solution of the MAMP model --------
    //- (MAMP.12 - Restr. for each variable W[i] given by the "status" of the planning units) --
    //------------------------------------------------------------------------------------------

    int SizeUnits = getSizeStatusUnits(unitCost_Data);
    //arma::sp_mat matrixA_MAMP12(SizeUnits, vectorC.size());
    int counterStatusUnits = 0;

    if(SizeUnits != 0){

      NumericVector vectorStatusUnits = unitCost_Data["status"];
      for(int i = 0; i < numberUnits; i++){
        if(vectorStatusUnits[i] != 0){
          posX = i;
          posY = posY_acumulated + counterStatusUnits;
          //matrixA_MAMP12(posY, posX) = 1;
          this->_A_i.push_back(posY);
          this->_A_j.push_back(posX);
          this->_A_x.push_back(1);

          if(vectorStatusUnits[i] == 2){
            //vectorRhs.push_back(1);
            //vectorSense.push_back("==");
            this->_rhs.push_back(1);
            this->_sense.push_back("==");
          }
          if(vectorStatusUnits[i] == 3){
            //vectorRhs.push_back(0);
            //vectorSense.push_back("==");
            this->_rhs.push_back(0);
            this->_sense.push_back("==");
          }

          counterStatusUnits += 1;
        }
      }
      posY_acumulated += SizeUnits;
      //matrixA = join_cols(matrixA, matrixA_MAMP12);
    }
    timer.step("MAMP12");

    //------------------------------------------------------------------------------------------
    //------- Matrix A - Coeffs. associated with the initial solution of the MAMP model --------
    //---- (MAMP.13 - Restr. for each variable X[i,k] given by the "status" of the actions) ----
    //------------------------------------------------------------------------------------------

    int SizeActions = getSizeStatusActions(threatsDistribution_Data);
    //arma::sp_mat matrixA_MAMP13(SizeActions, vectorC.size());
    int counterStatusActions = 0;
    int counterVariableX = 0;

    if(SizeActions != 0){
      NumericVector vectorAmountActions = threatsDistribution_Data["amount"];
      NumericVector vectorStatusActions = threatsDistribution_Data["status"];

      for(int i = 0; i < vectorAmountActions.size(); i++){
        if(vectorAmountActions[i] != 0){
          if(vectorStatusActions[i] != 0){
            posX = break_X + counterVariableX;
            posY = posY_acumulated + counterStatusActions;
            //matrixA_MAMP13(posY, posX) = 1;
            this->_A_i.push_back(posY);
            this->_A_j.push_back(posX);
            this->_A_x.push_back(1);

            if(vectorStatusActions[i] == 2){
              //vectorRhs.push_back(1);
              //vectorSense.push_back("==");
              this->_rhs.push_back(1);
              this->_sense.push_back("==");
            }
            if(vectorStatusActions[i] == 3){
              //vectorRhs.push_back(0);
              //vectorSense.push_back("==");
              this->_rhs.push_back(0);
              this->_sense.push_back("==");
            }
            counterStatusActions += 1;
          }
          counterVariableX += 1;
        }
      }
      posY_acumulated += SizeActions;
      //matrixA = join_cols(matrixA, matrixA_MAMP13);
    }
    timer.step("MAMP13");


    //------------------------------------------------------------------------------------------
    //----------------------------------- BOUNDS -----------------------------------------------
    //------------------------------------------------------------------------------------------
    List vectorBounds;
    //NumericVector lowerBounds;
    //NumericVector upperBounds;

    //lowerBounds = rep(0, vectorC.size());
    //upperBounds = rep(1, vectorC.size());

    vectorBounds = List::create(Rcpp::Named("lower") = List::create(Rcpp::Named("ind") = seq(1,this->_C.size()),
                                            Rcpp::Named("val") = this->_lb),
                                            Rcpp::Named("upper") = List::create(Rcpp::Named("ind") = seq(1,this->_C.size()),
                                                        Rcpp::Named("val") = this->_ub));


    //------------------------------------------------------------------------------------------
    //----------------------------------- STATISTICS -----------------------------------------------
    //------------------------------------------------------------------------------------------
    List statistics;
    //-------------------------------------------Timer------------------------------------
    NumericVector res(timer);   //
    for (int i=0; i<res.size(); i++) {
      res[i] = res[i] / n;
    }



    statistics = List::create(Rcpp::Named("timer") = res,
                              Rcpp::Named("unitConnectivityVector") = connectivityVector,
                              Rcpp::Named("actionConnectivityVector") = connectivityVector_actions
      );







    return Rcpp::List::create(Rcpp::Named("C") = this->_C,
                              Rcpp::Named("A_i") = this->_A_i,
                              Rcpp::Named("A_j") = this->_A_j,
                              Rcpp::Named("A_x") = this->_A_x,
                              Rcpp::Named("Rhs") = this->_rhs,
                              Rcpp::Named("Sense") = this->_sense,
                              Rcpp::Named("Bounds") = vectorBounds,
                              Rcpp::Named("Type") = this->_vtype,
                              Rcpp::Named("Modelsense") = "min",
                              Rcpp::Named("Statistics") = statistics
    );
  }

  /*
   * ...........................................................................................
   * FINISHING MAIN FUNCTION....................................................................
   * ...........................................................................................
   */













