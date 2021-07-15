#include "Package.h"
#include "functions.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
Rcpp::List rcpp_min_set(SEXP x,
                               DataFrame features_data,
                               DataFrame pu_data,
                               DataFrame bound_data,
                               DataFrame dist_features_data,
                               DataFrame dist_threats_data,
                               DataFrame sensitivity_data,
                               DataFrame threats_data,
                               List settings_Data){

  // initialization
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

 Timer timer;
 timer.step("start");
 //Global parameters
 int n = 1000000000;
 int numberUnits = getUnits(pu_data);
 int numberThreats = getThreats(threats_data);
 int numberSpecies = getSpecies(features_data);
 double beta1 = getBlm(settings_Data);
 double beta2 = getBetaActions(settings_Data);
 int boundarySize = bound_data.nrows();

 //Define variables
 StringVector vectorVarType;
 arma::sp_mat matrix_cv;

 timer.step("C_parameters1_created");

 List Ki = getSet(pu_data, features_data, dist_features_data, dist_threats_data, sensitivity_data, threats_data, "Ki");

 timer.step("C_parameters2_created");

 List Si = getSet(pu_data, features_data, dist_features_data, dist_threats_data, sensitivity_data, threats_data, "Si");

 timer.step("C_parameters3_created");

 List Ks = getSet(pu_data, features_data, dist_features_data, dist_threats_data, sensitivity_data, threats_data, "Ks");

 timer.step("C_parameters4_created");

 List UnitStatus = get_UnitStatus(pu_data);



 if(boundarySize != 0){
   matrix_cv = create_boundary_matrix_extended(bound_data, numberUnits);
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
 NumericVector vector_UnitCost  = pu_data["cost"];
 int index_id1_corrected;
 int index_id2_corrected;
 double connectivityCoeff;
 NumericVector connectivityVector(numberUnits);
 NumericVector connectivityVector_actions(numberUnits);

 if(boundarySize != 0){

   IntegerVector id1_vector = bound_data["internal_id1"];
   IntegerVector id2_vector = bound_data["internal_id2"];
   NumericVector bound_vector = bound_data["boundary"];

   for(int i = 0; i < boundarySize; i++){
     index_id1_corrected = id1_vector[i] - 1;
     index_id2_corrected = id2_vector[i] - 1;
     connectivityCoeff = bound_vector[i];

     connectivityVector[index_id1_corrected] = connectivityVector[index_id1_corrected] + connectivityCoeff;
     connectivityVector[index_id2_corrected] = connectivityVector[index_id2_corrected] + connectivityCoeff;
     //This allows you to specify in a set, the values associated with W[i] that are linked to Y[i1,i2]
   }//END external 'for'

   for(int i = 0; i < numberUnits; i++){
     op->_obj.push_back(beta1*connectivityVector[i] + vector_UnitCost[i]);
     op->_vtype.push_back("B");
     op->_lb.push_back(0);
     op->_ub.push_back(1);
   }
 }
 else{
   for(int i = 0; i < numberUnits; i++){
     op->_obj.push_back(vector_UnitCost[i]);
     op->_vtype.push_back("B");
     op->_lb.push_back(0);
     op->_ub.push_back(1);
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
 int break_X = op->_obj.size();

 //NumericVector vectorC_VarX;
 NumericVector vectorC_VarP;
 //StringVector vectorVariablesP;

 //Internal parameters
 IntegerVector pu_threats_data = dist_threats_data["internal_pu"];
 IntegerVector id_threats_data = dist_threats_data["internal_threats"];
 NumericVector vector_ActionCost = dist_threats_data["cost"];
 NumericVector vector_ActionAmount = dist_threats_data["amount"];
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
     if(boundarySize != 0 && beta2 != 0){
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
       op->_obj.push_back(beta2*connectivityCoeffAgreggate + action_cost);
       op->_vtype.push_back("B");
       op->_lb.push_back(0);
       op->_ub.push_back(1);

       //Statistics
       connectivityVector_actions.push_back(connectivityCoeffAgreggate);
     }
     else{
       op->_obj.push_back(action_cost);
       op->_vtype.push_back("B");
       op->_lb.push_back(0);
       op->_ub.push_back(1);

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
 int break_Y = op->_obj.size();

 //int contador = 0;
 if(boundarySize != 0 && beta1 != 0){
   arma::sp_mat z = matrix_cv.t();

   for(arma::sp_mat::const_iterator it = z.begin(); it != z.end(); ++it) {
     if(it.row() != it.col()){
       connectivityCoeff = -1*(*it);

       op->_obj.push_back(beta1*connectivityCoeff);
       op->_vtype.push_back("B");
       op->_lb.push_back(0);
       op->_ub.push_back(1);

       //Statistics
       connectivityVector.push_back(connectivityCoeff);
       connectivityVector_actions.push_back(0);

       //contador = contador + 1;
     }
   }
 }



 //-------------------------------------------------------OLD-----------------------------------------
 //Variable y
 // int break_Y = op->_obj.size();
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
 //         op->_obj.push_back(beta1*connectivityCoeff);
 //         op->_vtype.push_back("B");
 //         op->_lb.push_back(0);
 //         op->_ub.push_back(1);
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
 //  int break_P = op->_obj.size();
 //
 //  //op->_obj = concatenate(op->_obj, vectorC_VarP);
 //  for(int i = 0; i < vectorC_VarP.size(); i++){
 //    op->_obj.push_back(beta2*vectorC_VarP[i]);
 //    op->_vtype.push_back("B");
 //    op->_lb.push_back(0);
 //    op->_ub.push_back(1);
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
 //  int break_Z = op->_obj.size();
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
 //        op->_obj.push_back(0);
 //        op->_vtype.push_back("B");
 //        op->_lb.push_back(0);
 //        op->_ub.push_back(1);
 //
 //        vectorC_VarZ.push_back(0);
 //      }
 //    }
 //  }
 //
 //  //Variable B
 //  int break_B = op->_obj.size();
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
 //        op->_obj.push_back(0);
 //        op->_vtype.push_back("C");
 //        op->_lb.push_back(0);
 //        op->_ub.push_back(1);
 //
 //        vectorC_VarB.push_back(0);
 //      }
 //    }
 //  }
 //
 //
 //  //Variable Lambda
 //  int break_Lambda = op->_obj.size();
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
 //          op->_obj.push_back(0);
 //          op->_vtype.push_back("C");
 //          op->_lb.push_back(0);
 //          op->_ub.push_back(1);
 //
 //          vectorC_VarLambda.push_back(0);
 //        }
 //      }
 //    }
 //  }
 //
 // //Variable V
 // int break_V = op->_obj.size();
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
 //         op->_obj.push_back(0);
 //         op->_vtype.push_back("B");
 //         op->_lb.push_back(0);
 //         op->_ub.push_back(1);
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

 int break_P = op->_obj.size();

 //op->_obj = concatenate(op->_obj, vectorC_VarP);
 for(int i = 0; i < vectorC_VarP.size(); i++){
   op->_obj.push_back(beta2*vectorC_VarP[i]);
   op->_vtype.push_back("B");
   op->_lb.push_back(0);
   op->_ub.push_back(1);

   //Statistics
   connectivityVector_actions.push_back(vectorC_VarP[i]);
 }

 int break_Z = op->_obj.size();
 //op->_obj = concatenate(op->_obj, vectorC_VarZ);
 for(int i = 0; i < vectorC_VarZ_size; i++){
   op->_obj.push_back(0);
   op->_vtype.push_back("B");
   op->_lb.push_back(0);
   op->_ub.push_back(1);

 }

 int break_B = op->_obj.size();
 //op->_obj = concatenate(op->_obj, vectorC_VarB);
 for(int i = 0; i < vectorC_VarB_size; i++){
   op->_obj.push_back(0);
   op->_vtype.push_back("C");
   op->_lb.push_back(0);
   op->_ub.push_back(1);

 }

 int break_Lambda = op->_obj.size();
 //op->_obj = concatenate(op->_obj, vectorC_VarLambda);
 for(int i = 0; i < vectorC_VarLambda_size; i++){
   op->_obj.push_back(0);
   op->_vtype.push_back("C");
   op->_lb.push_back(0);
   op->_ub.push_back(1);

 }

 int break_V = op->_obj.size();
 //op->_obj = concatenate(op->_obj, vectorC_VarV);
 for(int i = 0; i < vectorC_VarV_size; i++){
   op->_obj.push_back(0);
   op->_vtype.push_back("B");
   op->_lb.push_back(0);
   op->_ub.push_back(1);

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

 //arma::sp_mat matrixA(numberSpecies, op->_obj.size());
 //NumericVector vectorRhs;
 //CharacterVector vectorSense;

 int posX = 0;
 int posY = 0;
 int posY_acumulated = numberSpecies;
 double coeff = 0;
 double benefit_den = 0;
 int countervariablesB = 0;
 int countervariablesZ = 0;
 std::map<int,std::map<int,double>> SpeciesDistribution = getSpeciesDistribution(dist_features_data);


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

               //Rcout << "i: " << i << ", s: " << subset_Si[s] << ", value = " << SpeciesDistribution[i][subset_Si[s]] <<"\n";
               coeff = (double) (vector_ActionAmount[i_threats] * vector_ActionAmount[i_threats] * SpeciesDistribution[i][subset_Si[s]])/benefit_den;
               break;
             }
           }
           posY = subset_Si[s];
           //matrixA(posY, posX) = coeff;
           op->_A_i.push_back(posY);
           op->_A_j.push_back(posX);
           op->_A_x.push_back(coeff);
         }
       }
       else{
         posX = break_B + countervariablesB;
         posY = subset_Si[s];
         //matrixA(posY, posX) = 1;
         op->_A_i.push_back(posY);
         op->_A_j.push_back(posX);
         op->_A_x.push_back(1);

         countervariablesB += 1;
       }
     }
     else{
       posX = break_Z + countervariablesZ;
       posY = subset_Si[s];
       //matrixA(posY, posX) = 1;
       op->_A_i.push_back(posY);
       op->_A_j.push_back(posX);
       op->_A_x.push_back(1);

       countervariablesZ += 1;
     }
   }
 }

 ///////////////////////////////RHS/////////////////////////////////////////////////////////
 NumericVector vectorRhs = features_data["target"];
 for(int i = 0; i < numberSpecies; i++){
   op->_rhs.push_back(vectorRhs[i]);
   op->_sense.push_back(">=");
 }

 timer.step("MAMP2");

 //------------------------------------------------------------------------------------------
 //---- Matrix A - Coefficients associated with the variables of the second restriction -----
 //--------------------------------------- (MAMP. 3) ----------------------------------------
 //------------------------------------------------------------------------------------------
 //arma::sp_mat matrixA_MAMP3(numberUnits, vectorC.size());

 for(int i = 0; i < numberUnits; i++){
   //subset_Ki = getSubset(dist_threats_data, "Ki", i + 1);
   subset_Ki = Ki(i);
   coeff  = -1*subset_Ki.size();
   //matrixA_MAMP3(i, i) = coeff;
   op->_A_i.push_back(posY_acumulated + i);
   op->_A_j.push_back(i);
   op->_A_x.push_back(coeff);

   //vectorRhs.push_back(0);
   op->_rhs.push_back(0);
   op->_sense.push_back("<=");
   //vectorSense.push_back("<=");
 }

 for(int j = 0; j < pu_threats_data.size(); j++){
   index_id_corrected = pu_threats_data[j] -1;
   posX = break_X + j;
   posY = posY_acumulated + index_id_corrected;
   //matrixA_MAMP3(posY, posX) = 1;
   op->_A_i.push_back(posY);
   op->_A_j.push_back(posX);
   op->_A_x.push_back(1);
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
     op->_A_i.push_back(posY);
     op->_A_j.push_back(posX);
     op->_A_x.push_back(coeff);

     //Rcout << "row: " << posY << ", col: " << posX << ", value: " << coeff << "\n";

     for(int j = 0; j < subset_Si_t.size(); j++){
       coeff = 1;
       posX = break_Z + subset_Si_t[j];
       posY = posY_acumulated + i;
       //matrixA_MAMP4(posY, posX) = coeff;
       op->_A_i.push_back(posY);
       op->_A_j.push_back(posX);
       op->_A_x.push_back(coeff);
     }
     //vectorRhs.push_back(0);
     //vectorSense.push_back("<=");
     op->_rhs.push_back(0);
     op->_sense.push_back("<=");
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
 //     //subset_Si = getSubset(dist_features_data, "Si", i + 1); //The subset is a vector of integers following its definition in C ++.
 //     subset_Si = Si(i);
 //     //subset_Ki = getSubset(dist_threats_data, "Ki", i + 1); //The subset is a vector of integers following its definition in C ++.
 //     subset_Ki = Ki(i);
 //
 //     for(int s = 0; s < subset_Si.size(); s++){
 //       //subset_Ks        = getSubset(sensitivity_data, "Ks", subset_Si[s]); //The subset is a vector of integers following its definition in C ++.
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
         op->_A_i.push_back(posY);
         op->_A_j.push_back(posX);
         op->_A_x.push_back(1);

         //matrixA_MAMP6( posY , i ) = -1;
         op->_A_i.push_back(posY);
         op->_A_j.push_back(i);
         op->_A_x.push_back(-1);

         //vectorRhs.push_back(0);
         //vectorSense.push_back("<=");
         op->_rhs.push_back(0);
         op->_sense.push_back("<=");

         //Constraint number 2 (Y[i1,i2] - W[i2] <= 0)
         //matrixA_MAMP6( posY + 1 , posX ) =  1;
         op->_A_i.push_back(posY + 1);
         op->_A_j.push_back(posX);
         op->_A_x.push_back(1);

         //matrixA_MAMP6( posY + 1 , j ) = -1;
         op->_A_i.push_back(posY + 1);
         op->_A_j.push_back(j);
         op->_A_x.push_back(-1);

         //vectorRhs.push_back(0);
         //vectorSense.push_back("<=");
         op->_rhs.push_back(0);
         op->_sense.push_back("<=");

         //Constraint number 3 (Y[i1,i2] - W[i1] - W[i2] => -1)
         //matrixA_MAMP6( posY + 2 , posX ) =  1;
         op->_A_i.push_back(posY + 2);
         op->_A_j.push_back(posX);
         op->_A_x.push_back(1);

         //matrixA_MAMP6( posY + 2 , i ) = -1;
         op->_A_i.push_back(posY + 2);
         op->_A_j.push_back(i);
         op->_A_x.push_back(-1);

         //matrixA_MAMP6( posY + 2 , j ) = -1;
         op->_A_i.push_back(posY + 2);
         op->_A_j.push_back(j);
         op->_A_x.push_back(-1);

         //vectorRhs.push_back(-1);
         //vectorSense.push_back(">=");
         op->_rhs.push_back(-1);
         op->_sense.push_back(">=");

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
   op->_A_i.push_back(posY);
   op->_A_j.push_back(posX);
   op->_A_x.push_back(1);

   posX = break_V + i;
   //matrixA_MAMP7(posY, posX) = -1;
   op->_A_i.push_back(posY);
   op->_A_j.push_back(posX);
   op->_A_x.push_back(-1);

   //vectorRhs.push_back(0);
   //vectorSense.push_back("<=");
   op->_rhs.push_back(0);
   op->_sense.push_back("<=");
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
     op->_A_i.push_back(posY);
     op->_A_j.push_back(posX);
     op->_A_x.push_back(1);
   }
   //vectorRhs.push_back(1);
   //vectorSense.push_back("==");
   op->_rhs.push_back(1);
   op->_sense.push_back("==");
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
         coeff = (double) (vector_ActionAmount[i_threats] * vector_ActionAmount[i_threats]* SpeciesDistribution[i][s])/benefit_den;
         break;
       }
     }
     posY = posY_acumulated + l;
     //matrixA_MAMP9(posY, posX) = coeff;
     op->_A_i.push_back(posY);
     op->_A_j.push_back(posX);
     op->_A_x.push_back(coeff);
   }
   for(int m = 0; m < numberSegments; m++){
     posY = posY_acumulated + l;
     posX = break_Lambda + vectorC_VarB_size*m + l;
     coeff = -1*( bp[m + 1] - bp[m]);
     //matrixA_MAMP9(posY, posX) = coeff;
     op->_A_i.push_back(posY);
     op->_A_j.push_back(posX);
     op->_A_x.push_back(coeff);

     posX = break_V + vectorC_VarB_size*m + l;
     coeff = -1*( bp[m]);
     //matrixA_MAMP9(posY, posX) = coeff;
     op->_A_i.push_back(posY);
     op->_A_j.push_back(posX);
     op->_A_x.push_back(coeff);
   }
   op->_rhs.push_back(0);
   op->_sense.push_back("==");
 }
 posY_acumulated += vectorC_VarB_size;
 //Rcout << "posY_acumulated MAMP 9: " << posY_acumulated << "\n";
 timer.step("MAMP9");

 // for(int i = 0; i < numberUnits; i++){
 //   //subset_Si = getSubset(dist_features_data, "Si", i + 1); //The subset is a vector of integers following its definition in C ++.
 //   subset_Si = Si(i);
 //   //subset_Ki = getSubset(dist_threats_data, "Ki", i + 1); //The subset is a vector of integers following its definition in C ++.
 //   subset_Ki = Ki(i);
 //
 //   for(int s = 0; s < subset_Si.size(); s++){
 //     //subset_Ks        = getSubset(sensitivity_data, "Ks", subset_Si[s]); //The subset is a vector of integers following its definition in C ++.
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
   op->_A_i.push_back(posY);
   op->_A_j.push_back(posX);
   op->_A_x.push_back(1);

   //vectorRhs.push_back(0);
   //vectorSense.push_back("==");
   op->_rhs.push_back(0);
   op->_sense.push_back("==");

   for(int m = 0; m < numberSegments; m++)
   {
     posX = break_Lambda + vectorC_VarB_size*m + i;
     coeff = -1*( bp3[m + 1] - bp3[m] );
     //matrixA_MAMP10(posY, posX) = coeff;
     op->_A_i.push_back(posY);
     op->_A_j.push_back(posX);
     op->_A_x.push_back(coeff);

     posX = break_V + vectorC_VarB_size*m + i;
     coeff = -1*( bp3[m] );
     //matrixA_MAMP10(posY, posX) = coeff;
     op->_A_i.push_back(posY);
     op->_A_j.push_back(posX);
     op->_A_x.push_back(coeff);
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

         //subset_Ki = getSubset(dist_threats_data, "Ki", j + 1);
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
           op->_A_i.push_back(posY);
           op->_A_j.push_back(posX);
           op->_A_x.push_back(1);

           //matrixA_MAMP11( posY , break_X + i ) = -1;
           op->_A_i.push_back(posY);
           op->_A_j.push_back(break_X + i);
           op->_A_x.push_back(-1);

           //vectorRhs.push_back(0);
           //vectorSense.push_back("<=");
           op->_rhs.push_back(0);
           op->_sense.push_back("<=");

           //Constraint number 2 (P[i1,i2,k] - X[i2,k] <= 0)
           //matrixA_MAMP11( posY + 1, posX ) =  1;
           op->_A_i.push_back(posY + 1);
           op->_A_j.push_back(posX);
           op->_A_x.push_back(1);

           //matrixA_MAMP11( posY + 1 , posX_varX ) = -1;
           op->_A_i.push_back(posY + 1);
           op->_A_j.push_back(posX_varX);
           op->_A_x.push_back(-1);

           //vectorRhs.push_back(0);
           //vectorSense.push_back("<=");
           op->_rhs.push_back(0);
           op->_sense.push_back("<=");

           //Constraint number 3 (P[i1,i2,k] - X[i1,k] - X[i2,k] => -1)
           //matrixA_MAMP11( posY + 2, posX ) =  1;
           op->_A_i.push_back(posY + 2);
           op->_A_j.push_back(posX);
           op->_A_x.push_back(1);

           //matrixA_MAMP11( posY + 2 , break_X + i ) = -1;
           op->_A_i.push_back(posY + 2);
           op->_A_j.push_back(break_X + i);
           op->_A_x.push_back(-1);

           //matrixA_MAMP11( posY + 2, posX_varX ) = -1;
           op->_A_i.push_back(posY + 2);
           op->_A_j.push_back(posX_varX);
           op->_A_x.push_back(-1);

           //vectorRhs.push_back(-1);
           //vectorSense.push_back(">=");
           op->_rhs.push_back(-1);
           op->_sense.push_back(">=");

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

 int SizeUnits = getSizeStatusUnits(pu_data);
 //arma::sp_mat matrixA_MAMP12(SizeUnits, vectorC.size());
 int counterStatusUnits = 0;

 if(SizeUnits != 0){

   NumericVector vectorStatusUnits = pu_data["status"];
   for(int i = 0; i < numberUnits; i++){
     if(vectorStatusUnits[i] != 0){
       posX = i;
       posY = posY_acumulated + counterStatusUnits;
       //matrixA_MAMP12(posY, posX) = 1;
       op->_A_i.push_back(posY);
       op->_A_j.push_back(posX);
       op->_A_x.push_back(1);

       if(vectorStatusUnits[i] == 2){
         //vectorRhs.push_back(1);
         //vectorSense.push_back("==");
         op->_rhs.push_back(1);
         op->_sense.push_back("==");
       }
       if(vectorStatusUnits[i] == 3){
         //vectorRhs.push_back(0);
         //vectorSense.push_back("==");
         op->_rhs.push_back(0);
         op->_sense.push_back("==");
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

 int SizeActions = getSizeStatusActions(dist_threats_data);
 //arma::sp_mat matrixA_MAMP13(SizeActions, vectorC.size());
 int counterStatusActions = 0;
 int counterVariableX = 0;

 if(SizeActions != 0){
   NumericVector vectorAmountActions = dist_threats_data["amount"];
   NumericVector vectorStatusActions = dist_threats_data["status"];

   for(int i = 0; i < vectorAmountActions.size(); i++){
     if(vectorAmountActions[i] != 0){
       if(vectorStatusActions[i] != 0){
         posX = break_X + counterVariableX;
         posY = posY_acumulated + counterStatusActions;
         //matrixA_MAMP13(posY, posX) = 1;
         op->_A_i.push_back(posY);
         op->_A_j.push_back(posX);
         op->_A_x.push_back(1);

         if(vectorStatusActions[i] == 2){
           //vectorRhs.push_back(1);
           //vectorSense.push_back("==");
           op->_rhs.push_back(1);
           op->_sense.push_back("==");
         }
         if(vectorStatusActions[i] == 3){
           //vectorRhs.push_back(0);
           //vectorSense.push_back("==");
           op->_rhs.push_back(0);
           op->_sense.push_back("==");
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

 vectorBounds = List::create(Rcpp::Named("lower") = List::create(Rcpp::Named("ind") = seq(1,op->_obj.size()),
                                         Rcpp::Named("val") = op->_lb),
                                         Rcpp::Named("upper") = List::create(Rcpp::Named("ind") = seq(1,op->_obj.size()),
                                                     Rcpp::Named("val") = op->_ub));


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




 return Rcpp::List::create(Rcpp::Named("C") = op->_obj,
                           Rcpp::Named("A_i") = op->_A_i,
                           Rcpp::Named("A_j") = op->_A_j,
                           Rcpp::Named("A_x") = op->_A_x,
                           Rcpp::Named("Rhs") = op->_rhs,
                           Rcpp::Named("Sense") = op->_sense,
                           Rcpp::Named("Bounds") = vectorBounds,
                           Rcpp::Named("Type") = op->_vtype,
                           Rcpp::Named("Modelsense") = "min",
                           Rcpp::Named("Statistics") = statistics

  );
}
