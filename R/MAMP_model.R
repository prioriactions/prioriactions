#' @title Put title here!
#'
#' @description Put description here!
#'
#' @param x Put description of a parameter here!
#'
#' @name MAMP_model
#'
#' @return Put the "return" object here!
#'
#' @details Put details here!
#'
#' @seealso Put "seealso" comments!
#'
#' @examples
#' ## Put examples here!
#' @references ## Put references here!
#' @noRd
MAMP_model <- function(features, pu, bound, rij, threats, sensibility, settings_Data, x) {
  preprocessingTime_1 <- Sys.time()
  # Reading input data for MARXAN and/or MAMP models (instance size: 10 units/ 5 species/ 3 threats)
  #------------------------------------------------------------------------------------------
  target_Data <- features
  unitCost_Data <- pu
  boundary_Data <- bound
  speciesDistribution_Data <- rij
  # New data required by the MAMP problem (threats distribution, species-threats sensibility and action cost)
  threatsDistribution_Data <- threats
  sensibility_Data <- sensibility

  #------------------------------------------------------------------------------------------
  #---------------------- Implementation of the MAMP and MAMP-e models ----------------------
  #------------------------------------------------------------------------------------------
  # Observation: "MAMPData" Class was writen in the C++ language.
  problemData <- methods::new(MAMPData, target_Data, unitCost_Data, boundary_Data, speciesDistribution_Data, threatsDistribution_Data, sensibility_Data, settings_Data)

  beta1 <- problemData$getBeta1() # beta1 set in 1 for default!
  beta2 <- problemData$getBeta2() # beta2 set in 1 for default!
  exponent <- problemData$getExponent() # exponent set in 3 for default!
  numberBreakpoints <- problemData$getBreakpoints() # numberBreakpoints set in 4 for default!
  numberSegments <- problemData$getSegments() # numberSegments = (numberBreakpoints - 1) for default!

  # NEW! Parameters for linearization of the measure of the "local benefit of the species" (constraint MAMP.2)
  bp <- problemData$get_bp()
  bp3 <- problemData$get_bp3()
  slope <- problemData$get_slope()
  # NEW! Status of planning units and actions (i.e.: unrestricted [0], pre-included [2] or pre-excluded [3])
  unitStatus <- problemData$get_UnitStatus()
  actionStatus <- problemData$get_ActionStatus()

  numberUnits <- problemData$getUnits()
  numberSpecies <- problemData$getSpecies()
  numberThreats <- problemData$getThreats()
  actionCost <- problemData$getActionCost()
  ts <- problemData$getTarget() # Conservation target of each species!
  # hola          = problemData$getBoundary()

  Si <- problemData$getSet("Si") # It corresponds to the set Si (Si belong to set S).
  Ki <- problemData$getSet("Ki") # It corresponds to the set Ki (Ki belong to set K).
  Ks <- problemData$getSet("Ks") # It corresponds to the set Ks (Ks belong to set K).
  Is <- problemData$getSet("Is") # It corresponds to the set Is (Is belong to set I).


  # matrix_cv corresponds to the original quadrate matrix "cv[i1,i2]", the symmetric one!
  # matrix_speciesDistribution, matrix_threatsDistribution and matrix_c corresponds to the original matrices
  # (that is, not the dense matrix in its tuple version).
  # Observation: "originalMatrix_cv()", "originalMatrix_Distribution()" and "createMatrix_c()" functions were written in the C++ language.
  if (!is.null(boundary_Data)) {
    matrix_cv <- originalMatrix_cv(type = "symmetric", data = boundary_Data, units = numberUnits)
  }

  matrix_speciesDistribution <- originalMatrix_Distribution(data = speciesDistribution_Data, units = numberUnits, species = numberSpecies)
  matrix_threatsDistribution <- originalMatrix_Distribution(data = threatsDistribution_Data, units = numberUnits, species = numberThreats)
  matrix_c <- createMatrix_c(data = threatsDistribution_Data, units = numberUnits, threats = numberThreats)

  # utils::write.csv(matrix_cv, file="NEW_cv_Matrix.csv", row.names = F)
  # utils::write.csv(matrix_speciesDistribution, file="NEW_speciesDistribution_Matrix.csv", row.names = F)
  # utils::write.csv(matrix_threatsDistribution, file="NEW_threatsDistribution_Matrix.csv", row.names = F)
  # utils::write.csv(matrix_c, file="NEW_actionCost_Matrix.csv", row.names = F)

  #------------------------------------------------------------------------------------------
  #---------------- Vector C - Coefficients associated with the planning cost ---------------
  #--------------------- (coefficients associated with W[i] variables) ----------------------
  #------------------------------------------------------------------------------------------
  vectorC_UnitCost <- as.vector(x = unitCost_Data[, 2], mode = "numeric")

  if (beta1 != 0 && !is.null(boundary_Data)) {
    boundarySize <- nrow(boundary_Data)
    index_i1 <- 1
    index_i2 <- 1
    connectivityCoeff <- 0
    connectivityVector <- vector(mode = "numeric", length = numberUnits)
    auxCoeff <- 0

    for (l in 1:boundarySize) { # OJO!:VER COMO OPTIMIZAR ESTA PARTE CUANDO beta1 == 0
      index_i1 <- boundary_Data[l, 1]
      index_i2 <- boundary_Data[l, 2]
      connectivityCoeff <- beta1 * getValueTuple_2(index_i1, index_i2, boundary_Data)

      for (i in 1:numberUnits) {
        # Here we assume that the "cv[i1,i2]" matrix is symmetric!
        if (index_i1 == i) {
          auxCoeff <- connectivityVector[i]
          connectivityVector[i] <- auxCoeff + connectivityCoeff
          # This allows you to specify in a set, the values associated with W[i] that are linked to Y[i1,i2]
          # Here, we suppose that the original quadrate matrix cv[i1,i2], is symmetric!
        }
        if (index_i2 == i) {
          auxCoeff <- connectivityVector[i]
          connectivityVector[i] <- auxCoeff + connectivityCoeff
        }
      } # END internal 'for'
    } # END external 'for'
    vectorC_VarW <- vectorC_UnitCost + connectivityVector
  }
  else {
    vectorC_VarW <- vectorC_UnitCost
  }


  # Observation: "vectorC_VarW", vector with the associated coefficients of each variable W[i],
  # (number of positions equivalent to the number of planning units)



  numberVariablesW <- numberUnits

  #------------------------------------------------------------------------------------------
  #-------------- Vector C - Coefficients associated with the connectivity cost -------------
  #-------------------- (coefficients associated with Y[i1,i2] variables) -------------------
  #------------------------------------------------------------------------------------------
  # Observation: vector with the associated coefficients of each variable Y[i1,i2]
  # (number of positions equivalent to the number of planning units)
  vectorC_VarY <- c()

  # Creation of vectors containing the name of the decision variables (variables W[i] and Y[i1,i2]).
  # Observation: "createVectorVarW()" Function was written in C++ language.
  vectorVariablesW <- createVectorVarW(units = numberUnits)
  vectorVariablesY <- c()
  vectorIndex_i1_Yij <- c()
  vectorIndex_i2_Yij <- c()

  # Important observation!
  # If beta1 = 0, the Y[i1,i2] variables and the linearization restrictions associated with these variables are not required!
  if (beta1 != 0 && !is.null(boundary_Data)) {
    for (i in 1:numberUnits) {
      for (j in 1:numberUnits) {
        if (i != j && matrix_cv[i, j] != 0) {
          # Observation: "variableY()" Function was written in C++ language.
          varY <- variableY(i, j)

          vectorIndex_i1_Yij <- c(vectorIndex_i1_Yij, i)
          vectorIndex_i2_Yij <- c(vectorIndex_i2_Yij, j)
          vectorVariablesY <- c(vectorVariablesY, varY)

          connectivityCoeff <- -beta1 * matrix_cv[i, j]
          vectorC_VarY <- c(vectorC_VarY, connectivityCoeff)
        }
      }
    }
  } # END If

  numberVariablesY <- length(vectorVariablesY)

  #------------------------------------------------------------------------------------------
  #----------------- Vector C - Coefficients associated with the action cost ----------------
  #--------------------- (coefficients associated with X[i,k] variables) --------------------
  #------------------------------------------------------------------------------------------
  vectorC_VarX <- c()
  vectorVariablesX <- c()
  vectorIndexI_Xik <- c()
  vectorIndexK_Xik <- c()
  actionCostCoeff <- 0

  for (i in 1:numberUnits) {
    for (k in 1:numberThreats) {
      if (matrix_c[k, i] > 0) {
        varX <- variableX(i, k)

        vectorVariablesX <- c(vectorVariablesX, varX)
        vectorIndexI_Xik <- c(vectorIndexI_Xik, i)
        vectorIndexK_Xik <- c(vectorIndexK_Xik, k)

        actionCostCoeff <- matrix_c[k, i]
        vectorC_VarX <- c(vectorC_VarX, actionCostCoeff)
      }
    }
  }


  #------------------------------------------------------------------------------------------
  #------------ Vector C - Coefficients associated with the auxiliary variable Z ------------
  #--------------------- (coefficients associated with Z[i,s] variables) --------------------
  #------------------------------------------------------------------------------------------
  vectorVariablesZ <- c()
  vectorIndex_i_Zis <- c()
  vectorIndex_s_Zis <- c()

  for (i in 1:numberUnits) {
    subset_Si <- Si[[i]] # The subset is a vector of integers following its definition in C ++.
    subset_Ki <- Ki[[i]] # The subset is a vector of integers following its definition in C ++.
    for (s in subset_Si) {
      subset_Ks <- Ks[[s]] # The subset is a vector of integers following its definition in C ++.
      subset_Ki_Ks <- dplyr::intersect(subset_Ki, subset_Ks)
      intersectionSize <- length(subset_Ki_Ks)
      if (intersectionSize == 0) {
        varZ <- variableZ(i, s)
        vectorVariablesZ <- c(vectorVariablesZ, varZ)
        vectorIndex_i_Zis <- c(vectorIndex_i_Zis, i)
        vectorIndex_s_Zis <- c(vectorIndex_s_Zis, s)
      }
    }
  }

  numberVariablesZ <- length(vectorVariablesZ)
  vectorC_auxVarZ <- vector(mode = "numeric", length = numberVariablesZ)

  #------------------------------------------------------------------------------------------
  #----------- Vector C - Coefficients associated with the the auxiliary variable B ---------
  #-------------------- (coefficients associated with B[i,s] variables) ---------------------
  #------------------------------------------------------------------------------------------
  vectorIndex_i_Bis <- c()
  vectorIndex_s_Bis <- c()

  for (s in 1:numberSpecies) {
    subset_Is <- Is[[s]] # The subset is a vector of integers following its definition in C ++.
    subset_Ks <- Ks[[s]]
    for (i in subset_Is) {
      subset_Ki <- Ki[[i]]
      subset_Ki_Ks <- dplyr::intersect(subset_Ki, subset_Ks)
      intersectionSize <- length(subset_Ki_Ks)

      if (intersectionSize != 0) {
        vectorIndex_i_Bis <- c(vectorIndex_i_Bis, i)
        vectorIndex_s_Bis <- c(vectorIndex_s_Bis, s)
      } # END internal if
    } # END external_1 for (about Planning Units!)
  } # END external_2 for (about Species!)

  matrixIndex_Bis <- cbind(vectorIndex_i_Bis, vectorIndex_s_Bis)
  matrixIndex_Bis <- matrixIndex_Bis[order(matrixIndex_Bis[, 1], matrixIndex_Bis[, 2]), ] # The last comma "," is necessary!
  vectorVariablesB <- apply(X = matrixIndex_Bis, MARGIN = 1, FUN = function(x) variableB(x[1], x[2]))
  # Observation: "variableB()" Function was written in C++ language.

  numberVariablesB <- length(vectorVariablesB)
  vectorC_auxVarB <- vector(mode = "numeric", length = numberVariablesB)

  #------------------------------------------------------------------------------------------
  #-------- Vector C - Coefficients associated with the the auxiliary variable Lambda -------
  #---------------- (coefficients associated with Lambda[i,s,m] variables) ------------------
  #------------------------------------------------------------------------------------------
  vectorVariablesLambda <- c()
  vectorVariablesLambda_Seg <- vector(mode = "character", length = numberVariablesB)

  for (m in 1:numberSegments) {
    vectorVariablesLambda_Seg <- apply(X = matrixIndex_Bis, MARGIN = 1, FUN = function(x) variableLambda(x[1], x[2], m))
    vectorVariablesLambda <- c(vectorVariablesLambda, vectorVariablesLambda_Seg)
  }

  numberVariablesLambda <- numberVariablesB * numberSegments
  vectorC_auxVarLambda <- vector(mode = "numeric", length = numberVariablesLambda)

  #------------------------------------------------------------------------------------------
  #----------- Vector C - Coefficients associated with the the auxiliary variable V ---------
  #------------------- (coefficients associated with V[i,s,m] variables) --------------------
  #------------------------------------------------------------------------------------------
  vectorVariablesV <- c()
  vectorVariablesV_Seg <- vector(mode = "character", length = numberVariablesB)

  for (m in 1:numberSegments) {
    vectorVariablesV_Seg <- apply(X = matrixIndex_Bis, MARGIN = 1, FUN = function(x) variableV(x[1], x[2], m))
    vectorVariablesV <- c(vectorVariablesV, vectorVariablesV_Seg)
  }

  numberVariablesV <- numberVariablesB * numberSegments
  vectorC_auxVarV <- vector(mode = "numeric", length = numberVariablesV)

  #------------------------------------------------------------------------------------------
  #-- Vector C - Coefficients associated with the cost of spatial fragmentation of actions --
  #------------------- (coefficients associated with P[i1,i2,k] variables) ------------------
  #------------------------------------------------------------------------------------------
  # Important observation!
  # If beta2 = 0, the P[i1,i2,k] variables and the linearization restrictions associated with these variables are not required!
  if (beta2 != 0 && !is.null(boundary_Data)) {
    vectorIndex_i1_Yij <- c()
    vectorIndex_i2_Yij <- c()
    for (i in 1:numberUnits) {
      for (j in 1:numberUnits) {
        if (i != j && matrix_cv[i, j] != 0) {
          vectorIndex_i1_Yij <- c(vectorIndex_i1_Yij, i)
          vectorIndex_i2_Yij <- c(vectorIndex_i2_Yij, j)
        }
      }
    }

    # Matrix ordered by i1 and then by i2 (since its creation).
    matrixIndex_Yi1i12 <- cbind(vectorIndex_i1_Yij, vectorIndex_i2_Yij)
    matrixIndex_Pi1i12k <- matrix(data = 0, nrow = 0, ncol = 3)
    boundarySize <- nrow(matrixIndex_Yi1i12)
    vectorVariablesP <- c()
    vectorC_auxVarP <- c()

    for (l in 1:boundarySize) {
      i1 <- matrixIndex_Yi1i12[l, 1]
      i2 <- matrixIndex_Yi1i12[l, 2]
      subset_Ki1 <- Ki[[i1]]
      subset_Ki2 <- Ki[[i2]]
      subset_Ki1_Ki2 <- dplyr::intersect(subset_Ki1, subset_Ki2)
      intersectionSize <- length(subset_Ki1_Ki2)

      if (intersectionSize != 0) {
        for (k in subset_Ki1_Ki2) {
          vectorAux <- as.vector(x = cbind(i1, i2, k), mode = "integer")
          matrixIndex_Pi1i12k <- rbind(matrixIndex_Pi1i12k, vectorAux)
        } # END internal for
      } # END if
    } # END external for

    matrixIndex_Pi1i12k <- matrixIndex_Pi1i12k[order(matrixIndex_Pi1i12k[, 1], matrixIndex_Pi1i12k[, 2], matrixIndex_Pi1i12k[, 3]), ] # The last comma "," is necessary!
    colnames(matrixIndex_Pi1i12k) <- c("i1", "i2", "k")
    rownames(matrixIndex_Pi1i12k) <- c(1:nrow(matrixIndex_Pi1i12k))

    vectorVariablesP <- apply(X = matrixIndex_Pi1i12k, MARGIN = 1, FUN = function(x) variableP(x[1], x[2], x[3]))
    vectorC_auxVarP <- apply(X = matrixIndex_Pi1i12k, MARGIN = 1, FUN = function(x) -1 * beta2 * matrix_cv[x[1], x[2]])

    #------------------------------------------------------------------------------------------
    #-- Vector C - Coefficients associated with the cost of spatial fragmentation of actions --
    #--------------------- (coefficients associated with X[i,k] variables) --------------------
    #------------------------------------------------------------------------------------------
    numberVariablesP <- length(vectorVariablesP)
    for (l in 1:numberVariablesP) {
      i1 <- matrixIndex_Pi1i12k[l, 1]
      i2 <- matrixIndex_Pi1i12k[l, 2]
      k <- matrixIndex_Pi1i12k[l, 3]
      varX_i1k <- variableX(i1, k)
      coeff_Xi1k <- beta2 * matrix_cv[i1, i2]
      posX_Xi1k <- match(varX_i1k, vectorVariablesX)

      auxCoeff <- vectorC_VarX[posX_Xi1k]
      vectorC_VarX[posX_Xi1k] <- auxCoeff + coeff_Xi1k
    } # END external for
  } else {
    numberVariablesP <- 0
    vectorVariablesP <- vector(mode = "character", length = numberVariablesP)
    vectorC_auxVarP <- vector(mode = "numeric", length = numberVariablesP)
  }

  # utils::View(matrixIndex_Pi1i12k)
  # utils::View(vectorVariablesP)
  # utils::View(vectorC_auxVarP)
  # utils::View(vectorC_VarX)

  #------------------------------------------------------------------------------------------
  #-------------------------------------- Vector C - FINAL ----------------------------------
  #------------------------------------------------------------------------------------------
  # Number of variables W is equal to #N, where N is the set of {1,..., numberUnits}.
  # Number of variables Y can only be calculated after creating the vector that contains
  # all the names associated with the valid variables Y[i1,i2]. Otherwise, the number of variables Y
  # is equal to #N*#N - #N, where N is the set of {1,..., numberUnits}.
  # Number of variables X can only be calculated after creating the vector that contains
  # all the names associated with the valid variables X[i,k].
  numberVariablesW <- numberUnits
  numberVariablesY <- length(vectorVariablesY)
  numberVariablesX <- length(vectorVariablesX)
  numberVariables <- numberVariablesW + numberVariablesY + numberVariablesX + numberVariablesZ +
    numberVariablesB + numberVariablesLambda + numberVariablesV + numberVariablesP

  vectorVariables <- c(vectorVariablesW, vectorVariablesY, vectorVariablesX, vectorVariablesZ, vectorVariablesB, vectorVariablesLambda, vectorVariablesV, vectorVariablesP)
  vectorC_Final <- as.vector(x = c(vectorC_VarW, vectorC_VarY, vectorC_VarX, vectorC_auxVarZ, vectorC_auxVarB, vectorC_auxVarLambda, vectorC_auxVarV, vectorC_auxVarP), mode = "numeric")

  # Transform the vector into a "sparse" type
  # vectorC_Final = methods::as(object = vectorC_Final, Class = "sparseVector")

  # vectorC_Final_Sparse = Matrix::sparseVector(x = vectorC_Final,i = 1:numberVariables, length = numberVariables)
  # utils::object.size(vectorC_Final)
  # utils::View(vectorVariables)
  # utils::View(vectorC_Final)



  #------------------------------------------------------------------------------------------
  #----- Matrix A - Coefficients associated with the variables of the first restriction -----
  #--------------------------------------- (MAMP. 2) ----------------------------------------
  #------------------------------------------------------------------------------------------
  sizeRHS_MAMP2 <- numberSpecies
  # matrixA_MAMP2 <- matrix(data = 0, nrow = sizeRHS_MAMP2, ncol = numberVariables)
  matrixA_MAMP2 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP2, ncol = numberVariables, sparse = TRUE)

  # vectorIndexI_Xik  #Subindex of "i" variable X (I think it's not used! It's not necessary!)
  # vectorIndexK_Xik  #Subindex of "k" variable X
  # vectorIndex_i_Zis #Subindex of "i" variable Z
  # vectorIndex_s_Zis #Subindex of "s" variable Z

  posX_Xik <- 0
  posY_Xik <- 0
  posX_Zis <- 0
  posY_Zis <- 0
  # NEW!
  posX_Bis <- 0
  posY_Bis <- 0

  # Break points (x axis) of vector C, which indicate the separations of each set of variables that are within that vector.
  break_1 <- 0
  break_2 <- numberVariablesW
  break_3 <- numberVariablesW + numberVariablesY
  break_4 <- numberVariablesW + numberVariablesY + numberVariablesX
  break_5 <- numberVariablesW + numberVariablesY + numberVariablesX + numberVariablesZ
  # NEW!
  break_6 <- numberVariablesW + numberVariablesY + numberVariablesX + numberVariablesZ + numberVariablesB
  break_7 <- numberVariablesW + numberVariablesY + numberVariablesX + numberVariablesZ + numberVariablesB + numberVariablesLambda
  break_8 <- numberVariablesW + numberVariablesY + numberVariablesX + numberVariablesZ + numberVariablesB + numberVariablesLambda + numberVariablesV
  break_9 <- numberVariables


  for (s in 1:numberSpecies) {
    subset_Is <- Is[[s]] # The subset is a vector of integers following its definition in C ++.
    subset_Ks <- Ks[[s]]
    for (i in subset_Is) {
      subset_Ki <- Ki[[i]]
      subset_Ki_Ks <- dplyr::intersect(subset_Ki, subset_Ks)
      intersectionSize <- length(subset_Ki_Ks)

      if (intersectionSize != 0) {
        if (exponent == 1) { # If the exponent = 1, the "local benefit function" of WAMP.2 does NOT need a linearization strategy.
          coeff_Xik <- 1 / intersectionSize
          for (k in subset_Ki_Ks) {
            varX <- variableX(i, k)
            posX_Xik <- break_3 + match(varX, vectorVariablesX)
            posY_Xik <- s
            matrixA_MAMP2[posY_Xik, posX_Xik] <- coeff_Xik
          } # END internal for (about Threats!)
        } else { # When the exponent != 1
          coeff_Bis <- 1
          varB <- variableB(i, s)
          posX_Bis <- break_5 + match(varB, vectorVariablesB)
          posY_Bis <- s
          matrixA_MAMP2[posY_Bis, posX_Bis] <- coeff_Bis
        } # END internal_1 if-else (about the exponent!)
      } else { # When |Ki intersect Ks| = 0
        coeff_Zis <- 1
        varZ <- variableZ(i, s)
        posX_Zis <- break_4 + match(varZ, vectorVariablesZ)
        posY_Zis <- s
        matrixA_MAMP2[posY_Zis, posX_Zis] <- coeff_Zis
      } # END internal_2 if-else (about the intersection of subsets!)
    } # END external_1 for (about Planning Units!)
  } # END external_2 for (about Species!)


  # utils::View( vectorVariables[(break_3+1):numberVariables]  )
  # utils::View( matrixA_MAMP2[ , (break_3+1):numberVariables] )

  #------------------------------------------------------------------------------------------
  #---- Matrix A - Coefficients associated with the variables of the second restriction -----
  #--------------------------------------- (MAMP. 3) ----------------------------------------
  #------------------------------------------------------------------------------------------
  sizeRHS_MAMP3 <- numberUnits
  matrixA_MAMP3 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP3, ncol = numberVariables, sparse = TRUE)

  posX_Wi <- 0
  posY_Wi <- 0
  posX_Xik <- 0
  posY_Xik <- 0


  for (i in 1:numberUnits) {
    subset_Ki <- Ki[[i]]
    coeff_Wi <- -1 * length(subset_Ki)

    for (k in subset_Ki) {
      posX_Wi <- i
      posY_Wi <- i
      matrixA_MAMP3[posY_Wi, posX_Wi] <- coeff_Wi

      coeff_Xik <- 1
      varX <- variableX(i, k)
      posX_Xik <- break_3 + match(varX, vectorVariablesX)
      posY_Xik <- i
      matrixA_MAMP3[posY_Xik, posX_Xik] <- coeff_Xik
    }
  }

  # utils::View(matrixA_MAMP3)

  #------------------------------------------------------------------------------------------
  #---- Matrix A - Coefficients associated with the variables of the third restriction ------
  #--------------------------------------- (MAMP. 4) ----------------------------------------
  #------------------------------------------------------------------------------------------
  if (numberVariablesZ != 0) {
    # auxiliarySet_unitsZis is a set of planning units where there is an associated variable Z[i,s].
    auxSet_unitsZis <- unique(vectorIndex_i_Zis)
    sizeRHS_MAMP4 <- length(auxSet_unitsZis)
    matrixA_MAMP4 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP4, ncol = numberVariables, sparse = TRUE)

    posX_Wi <- 0
    posY_Wi <- 0
    posX_Zis <- 0
    posY_Zis <- 0

    for (i in auxSet_unitsZis) {
      subset_Si <- which(vectorIndex_i_Zis %in% i)
      coeff_Wi <- -1 * length(subset_Si)

      posX_Wi <- i
      posY_Wi <- which(auxSet_unitsZis %in% i)
      matrixA_MAMP4[posY_Wi, posX_Wi] <- coeff_Wi
      for (s in subset_Si) {
        coeff_Zis <- 1
        posX_Zis <- break_4 + s
        posY_Zis <- posY_Wi
        matrixA_MAMP4[posY_Zis, posX_Zis] <- coeff_Zis
      }
    }

    # utils::View(matrixA_MAMP4)
  } else {
    sizeRHS_MAMP4 <- 0
    matrixA_MAMP4 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP4, ncol = numberVariables, sparse = TRUE)
  }

  #------------------------------------------------------------------------------------------
  # Matrix A - Coefficients associated the linearisation of the MAMP model objective function
  #------------------ (MAMP.6 - 3 restrictions for each variable Y[i1,i2]) ------------------
  #------------------------------------------------------------------------------------------
  if (beta1 != 0 && !is.null(boundary_Data)) {
    # Linearity constraints for the fragmentation of planning units.
    sizeRHS_MAMP6 <- 3 * numberVariablesY
    matrixA_MAMP6 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP6, ncol = numberVariables, sparse = TRUE)

    auxNumberUnits <- numberUnits + 1
    posX_Yij <- 0 # I used the name "Yij" instead of the original one, "Yi1i2".
    posY_Yij_ct1 <- 0
    posY_Yij_ct2 <- 0
    posY_Yij_ct3 <- 0

    for (l in 1:numberVariablesY) {
      index_i1 <- vectorIndex_i1_Yij[l]
      index_i2 <- vectorIndex_i2_Yij[l]
      varY <- variableY(index_i1, index_i2)
      # print(varY)

      posX_Yij <- break_2 + match(varY, vectorVariablesY)
      # print(posX_Yij)
      posY_Yij_ct1 <- (posX_Yij - auxNumberUnits) * 3 + 1
      posY_Yij_ct2 <- (posX_Yij - auxNumberUnits) * 3 + 2
      posY_Yij_ct3 <- (posX_Yij - auxNumberUnits) * 3 + 3

      # Constraint number 1 (Y[i1,i2] - W[i1] <= 0)
      matrixA_MAMP6[posY_Yij_ct1, posX_Yij] <- 1
      matrixA_MAMP6[posY_Yij_ct1, index_i1] <- -1

      # Constraint number 2 (Y[i1,i2] - W[i2] <= 0)
      matrixA_MAMP6[posY_Yij_ct2, posX_Yij] <- 1
      matrixA_MAMP6[posY_Yij_ct2, index_i2] <- -1

      # Constraint number 3 (Y[i1,i2] - W[i1] - W[i2] => -1)
      matrixA_MAMP6[posY_Yij_ct3, posX_Yij] <- 1
      matrixA_MAMP6[posY_Yij_ct3, index_i1] <- -1
      matrixA_MAMP6[posY_Yij_ct3, index_i2] <- -1
    }
    # utils::View(matrixA_MAMP6)
  } else {
    sizeRHS_MAMP6 <- 0
    matrixA_MAMP6 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP6, ncol = numberVariables, sparse = TRUE)
  }


  #------------------------------------------------------------------------------------------
  #- Matrix A - Coefficients associated the linearisation of the MAMP model cubic constraint
  #--------------------------------------- (MAMP. 7) ----------------------------------------
  #------------------------------------------------------------------------------------------
  sizeRHS_MAMP7 <- numberVariablesLambda
  # matrixA_MAMP7 <- matrix(data = 0, nrow = sizeRHS_MAMP7, ncol = numberVariables)
  matrixA_MAMP7 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP7, ncol = numberVariables, sparse = TRUE)

  posX_LAMBDAism <- 0
  posY_LAMBDAism <- 0

  posX_Vism <- 0
  posY_Vism <- 0

  coeff_LAMBDAism <- 1
  coeff_Vism <- -1

  for (l in 1:numberVariablesLambda) {
    posX_LAMBDAism <- break_6 + l
    posY_LAMBDAism <- l

    posX_Vism <- break_7 + l
    posY_Vism <- l

    matrixA_MAMP7[posY_LAMBDAism, posX_LAMBDAism] <- coeff_LAMBDAism
    matrixA_MAMP7[posY_Vism, posX_Vism] <- coeff_Vism
  }
  # utils::View(matrixA_MAMP7)
  # utils::View( vectorVariables[(break_6+1):numberVariables]  )
  # utils::View( matrixA_MAMP7[ , (break_6+1):numberVariables] )

  #------------------------------------------------------------------------------------------
  #- Matrix A - Coefficients associated the linearisation of the MAMP model cubic constraint
  #--------------------------------------- (MAMP. 8) ----------------------------------------
  #------------------------------------------------------------------------------------------
  sizeRHS_MAMP8 <- numberVariablesB
  # matrixA_MAMP8 <- matrix(data = 0, nrow = sizeRHS_MAMP8, ncol = numberVariables)
  matrixA_MAMP8 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP8, ncol = numberVariables, sparse = TRUE)

  posX_Vism <- 0
  posY_Vism <- 0
  coeff_Vism <- 1

  for (l in 1:numberVariablesB) {
    posY_Vism <- l
    for (m in 1:numberSegments) {
      posX_Vism <- break_7 + (numberVariablesB * m - numberVariablesB) + l
      matrixA_MAMP8[posY_Vism, posX_Vism] <- coeff_Vism
    }
  }

  # utils::View( vectorVariables[(break_7+1):numberVariables] )
  # utils::View( matrixA_MAMP8[ ,(break_7+1):numberVariables] )

  #------------------------------------------------------------------------------------------
  #- Matrix A - Coefficients associated the linearisation of the MAMP model cubic constraint
  #--------------------------------------- (MAMP. 9) ----------------------------------------
  #------------------------------------------------------------------------------------------
  sizeRHS_MAMP9 <- numberVariablesB
  # matrixA_MAMP9 <- matrix(data = 0, nrow = sizeRHS_MAMP9, ncol = numberVariables)
  matrixA_MAMP9 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP9, ncol = numberVariables, sparse = TRUE)

  posX_LAMBDAism <- 0
  posY_LAMBDAism <- 0
  coeff_LAMBDAism <- 0

  posX_Vism <- 0
  posY_Vism <- 0
  coeff_Vism <- 0

  posX_Xik <- 0
  posY_Xik <- 0
  coeff_Xik <- 0

  # matrixIndex_Bis;

  for (l in 1:numberVariablesB) {
    posY_Xik <- l
    posY_LAMBDAism <- l
    posY_Vism <- l

    i <- as.integer(matrixIndex_Bis[l, 1])
    s <- as.integer(matrixIndex_Bis[l, 2])

    subset_Ki <- Ki[[i]] # The subset is a vector of integers following its definition in C ++.
    subset_Ks <- Ks[[s]]
    subset_Ki_Ks <- dplyr::intersect(subset_Ki, subset_Ks)
    intersectionSize <- length(subset_Ki_Ks)
    coeff_Xik <- 1 / intersectionSize

    for (k in subset_Ki_Ks) {
      varX <- variableX(i, k)
      posX_Xik <- break_3 + match(varX, vectorVariablesX)
      matrixA_MAMP9[posY_Xik, posX_Xik] <- coeff_Xik
    } # END internal_1 for

    for (m in 1:numberSegments) {
      posX_LAMBDAism <- break_6 + (numberVariablesB * m - numberVariablesB) + l
      posX_Vism <- break_7 + (numberVariablesB * m - numberVariablesB) + l

      coeff_LAMBDAism <- -1 * (bp[m + 1] - bp[m])
      coeff_Vism <- -1 * (bp[m])

      matrixA_MAMP9[posY_LAMBDAism, posX_LAMBDAism] <- coeff_LAMBDAism
      matrixA_MAMP9[posY_Vism, posX_Vism] <- coeff_Vism
    } # END internal_2 for
  } # END external for

  # utils::View( vectorVariables[(break_3+1):numberVariables]  )
  # utils::View( matrixA_MAMP9[ , (break_3+1):numberVariables] )

  #------------------------------------------------------------------------------------------
  #- Matrix A - Coefficients associated the linearisation of the MAMP model cubic constraint
  #--------------------------------------- (MAMP. 10) ---------------------------------------
  #------------------------------------------------------------------------------------------
  sizeRHS_MAMP10 <- numberVariablesB
  # matrixA_MAMP10  <- matrix(data = 0, nrow = sizeRHS_MAMP10, ncol = numberVariables)
  matrixA_MAMP10 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP10, ncol = numberVariables, sparse = TRUE)

  posX_Bis <- 0
  posY_Bis <- 0
  coeff_Bis <- 0

  posX_LAMBDAism <- 0
  posY_LAMBDAism <- 0
  coeff_LAMBDAism <- 0

  posX_Vism <- 0
  posY_Vism <- 0
  coeff_Vism <- 0

  for (l in 1:numberVariablesB) {
    posY_Bis <- l
    posY_LAMBDAism <- l
    posY_Vism <- l

    posX_Bis <- break_5 + l
    coeff_Bis <- 1
    matrixA_MAMP10[posY_Bis, posX_Bis] <- coeff_Bis

    for (m in 1:numberSegments) {
      posX_LAMBDAism <- break_6 + (numberVariablesB * m - numberVariablesB) + l
      posX_Vism <- break_7 + (numberVariablesB * m - numberVariablesB) + l

      coeff_LAMBDAism <- -1 * (bp3[m + 1] - bp3[m])
      coeff_Vism <- -1 * (bp3[m])

      matrixA_MAMP10[posY_LAMBDAism, posX_LAMBDAism] <- coeff_LAMBDAism
      matrixA_MAMP10[posY_Vism, posX_Vism] <- coeff_Vism
    } # END internal for
  } # END external for

  # utils::View( vectorVariables[(break_5+1):numberVariables]  )
  # utils::View( matrixA_MAMP10[ , (break_5+1):numberVariables] )


  #------------------------------------------------------------------------------------------
  #- Matrix A - Coeffs. associated the linearisation of the MAMP-E model objective function -
  #---------------- (MAMP.11 - 3 restrictions for each variable P[i1,i2,k]) -----------------
  #------------------------------------------------------------------------------------------
  if (beta2 != 0 && !is.null(boundary_Data)) {
    sizeRHS_MAMP11 <- 3 * numberVariablesP
    # matrixA_MAMP11 = matrix(data = 0, nrow = sizeRHS_MAMP11, ncol = numberVariables)
    matrixA_MAMP11 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP11, ncol = numberVariables, sparse = TRUE)

    posX_Pi1i2k <- 0
    posY_Pi1i2k_ct1 <- 0
    posY_Pi1i2k_ct2 <- 0
    posY_Pi1i2k_ct3 <- 0

    posX_Xi1k <- 0
    posX_Xi2k <- 0

    for (l in 1:numberVariablesP) {
      i1 <- matrixIndex_Pi1i12k[l, 1]
      i2 <- matrixIndex_Pi1i12k[l, 2]
      k <- matrixIndex_Pi1i12k[l, 3]
      varX_i1k <- variableX(i1, k)
      varX_i2k <- variableX(i2, k)

      posX_Pi1i2k <- break_8 + l
      posY_Pi1i2k_ct1 <- (posX_Pi1i2k - (break_8 + 1)) * 3 + 1
      posY_Pi1i2k_ct2 <- (posX_Pi1i2k - (break_8 + 1)) * 3 + 2
      posY_Pi1i2k_ct3 <- (posX_Pi1i2k - (break_8 + 1)) * 3 + 3

      posX_Xi1k <- break_3 + match(varX_i1k, vectorVariablesX)
      posX_Xi2k <- break_3 + match(varX_i2k, vectorVariablesX)

      # Constraint number 1 (P[i1,i2,k] - X[i1,k] <= 0)
      matrixA_MAMP11[posY_Pi1i2k_ct1, posX_Pi1i2k] <- 1
      matrixA_MAMP11[posY_Pi1i2k_ct1, posX_Xi1k] <- -1

      # Constraint number 2 (P[i1,i2,k] - X[i2,k] <= 0)
      matrixA_MAMP11[posY_Pi1i2k_ct2, posX_Pi1i2k] <- 1
      matrixA_MAMP11[posY_Pi1i2k_ct2, posX_Xi2k] <- -1

      # Constraint number 3 (P[i1,i2,k] - X[i1,k] - X[i2,k] => -1)
      matrixA_MAMP11[posY_Pi1i2k_ct3, posX_Pi1i2k] <- 1
      matrixA_MAMP11[posY_Pi1i2k_ct3, posX_Xi1k] <- -1
      matrixA_MAMP11[posY_Pi1i2k_ct3, posX_Xi2k] <- -1
    } # END for
  } else {
    sizeRHS_MAMP11 <- 0
    matrixA_MAMP11 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP11, ncol = numberVariables, sparse = TRUE)
  } # END if-else

  # utils::View( matrixA_MAMP11[ , (break_3+1):numberVariables] )


  #------------------------------------------------------------------------------------------
  #------- Matrix A - Coeffs. associated with the initial solution of the MAMP model --------
  #- (MAMP.12 - Restr. for each variable W[i] given by the "status" of the planning units) --
  #------------------------------------------------------------------------------------------
  # Is there an initial solution of the model?
  initialSolution_Units <- FALSE
  if (length(unitStatus$Unrestricted) != 0) {
    initialSolution_Units <- TRUE
  }

  # Are there pre-included ("locked-in") units?
  if (initialSolution_Units == TRUE && length(unitStatus$LockedIn) != 0) {
    sizeRHS_MAMP12_01 <- length(unitStatus$LockedIn)
    # matrixA_MAMP12_01 = matrix(data = 0, nrow = sizeRHS_MAMP12_01, ncol = numberVariables)
    matrixA_MAMP12_01 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP12_01, ncol = numberVariables, sparse = TRUE)

    for (i in 1:sizeRHS_MAMP12_01) {
      posX_Wi <- unitStatus$LockedIn[i]
      posY_Wi <- i
      # Constraint of type W[i] == 1
      matrixA_MAMP12_01[posY_Wi, posX_Wi] <- 1
    }
  } else {
    sizeRHS_MAMP12_01 <- 0
    matrixA_MAMP12_01 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP12_01, ncol = numberVariables, sparse = TRUE)
  }


  # Are there pre-excluded ("locked-out") units?
  if (initialSolution_Units == TRUE && length(unitStatus$LockedOut) != 0) {
    sizeRHS_MAMP12_02 <- length(unitStatus$LockedOut)
    # matrixA_MAMP12_02 = matrix(data = 0, nrow = sizeRHS_MAMP12_02, ncol = numberVariables)
    matrixA_MAMP12_02 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP12_02, ncol = numberVariables, sparse = TRUE)

    for (i in 1:sizeRHS_MAMP12_02) {
      posX_Wi <- unitStatus$LockedOut[i]
      posY_Wi <- i
      # Constraint of type W[i] == 0
      matrixA_MAMP12_02[posY_Wi, posX_Wi] <- 1
    }
  } else {
    sizeRHS_MAMP12_02 <- 0
    matrixA_MAMP12_02 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP12_02, ncol = numberVariables, sparse = TRUE)
  }

  #------------------------------------------------------------------------------------------
  #------- Matrix A - Coeffs. associated with the initial solution of the MAMP model --------
  #---- (MAMP.13 - Restr. for each variable X[i,k] given by the "status" of the actions) ----
  #------------------------------------------------------------------------------------------
  # Is there an initial solution of the model?
  initialSolution_Actions <- FALSE
  if (nrow(actionStatus$Unrestricted) != 0) {
    initialSolution_Actions <- TRUE
  }

  # Are there pre-included ("locked-in") actions?
  if (initialSolution_Actions == TRUE && nrow(actionStatus$LockedIn) != 0) {
    sizeRHS_MAMP13_01 <- nrow(actionStatus$LockedIn)
    # matrixA_MAMP13_01 = matrix(data = 0, nrow = sizeRHS_MAMP13_01, ncol = numberVariables)
    matrixA_MAMP13_01 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP13_01, ncol = numberVariables, sparse = TRUE)

    for (i in 1:sizeRHS_MAMP13_01) {
      varX_ik <- variableX(actionStatus$LockedIn[[i, 1]], actionStatus$LockedIn[[i, 2]])
      posX_Xik <- break_3 + match(varX_ik, vectorVariablesX)
      posY_Xik <- i
      # Constraint of type X[i,k] == 1
      matrixA_MAMP13_01[posY_Xik, posX_Xik] <- 1
    }
  } else {
    sizeRHS_MAMP13_01 <- 0
    matrixA_MAMP13_01 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP13_01, ncol = numberVariables, sparse = TRUE)
  } # END first If


  # Are there pre-excluded ("locked-out") actions?
  if (initialSolution_Actions == TRUE && nrow(actionStatus$LockedOut) != 0) {
    sizeRHS_MAMP13_02 <- nrow(actionStatus$LockedOut)
    # matrixA_MAMP13_02 = matrix(data = 0, nrow = sizeRHS_MAMP13_02, ncol = numberVariables)
    matrixA_MAMP13_02 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP13_02, ncol = numberVariables, sparse = TRUE)

    for (i in 1:sizeRHS_MAMP13_02) {
      varX_ik <- variableX(actionStatus$LockedOut[[i, 1]], actionStatus$LockedOut[[i, 2]])
      posX_Xik <- break_3 + match(varX_ik, vectorVariablesX)
      posY_Xik <- i
      # Constraint of type X[i,k] == 0
      matrixA_MAMP13_02[posY_Xik, posX_Xik] <- 1
    }
  } else {
    sizeRHS_MAMP13_02 <- 0
    matrixA_MAMP13_02 <- Matrix::Matrix(data = 0, nrow = sizeRHS_MAMP13_02, ncol = numberVariables, sparse = TRUE)
  } # END second If

  # utils::View( matrixA_MAMP13_01 [ , (break_3+1):(break_3+numberVariablesX)] )
  # utils::View( matrixA_MAMP13_02 [ , (break_3+1):(break_3+numberVariablesX)] )

  #------------------------------------------------------------------------------------------
  # Conditions for constructing matrix A.
  if (is.null(boundary_Data) || (beta1 == 0 && beta2 == 0)) {
    # The setting is: b1 equal to 0 and b2 equal to 0 (MAMP model without fragmentation of units).
    matrixA_Final <- rbind(matrixA_MAMP2, matrixA_MAMP3, matrixA_MAMP4, matrixA_MAMP7, matrixA_MAMP8, matrixA_MAMP9, matrixA_MAMP10)
  }
  else if (beta1 != 0) {
    if (beta2 != 0) {
      # The setting is: b1 different to 0 and b2 different to 0 (MAMP-E model).
      matrixA_Final <- rbind(matrixA_MAMP2, matrixA_MAMP3, matrixA_MAMP4, matrixA_MAMP6, matrixA_MAMP7, matrixA_MAMP8, matrixA_MAMP9, matrixA_MAMP10, matrixA_MAMP11)
    } else {
      # The setting is: b1 different to 0 and b2 equal to 0 (MAMP model).
      matrixA_Final <- rbind(matrixA_MAMP2, matrixA_MAMP3, matrixA_MAMP4, matrixA_MAMP6, matrixA_MAMP7, matrixA_MAMP8, matrixA_MAMP9, matrixA_MAMP10)
    }
  }
  else {
    if (beta2 != 0) {
      # The setting is: b1 equal to 0 and b2 different to 0 (MAMP-E model without fragmentation of units).
      matrixA_Final <- rbind(matrixA_MAMP2, matrixA_MAMP3, matrixA_MAMP4, matrixA_MAMP7, matrixA_MAMP8, matrixA_MAMP9, matrixA_MAMP10, matrixA_MAMP11)
    }
  }


  # NEW! Is there an initial solution of the model?
  # Are there pre-included ("locked-in") units?
  if (initialSolution_Units == TRUE && length(unitStatus$LockedIn) != 0) {
    matrixA_Final <- rbind(matrixA_Final, matrixA_MAMP12_01)
  }
  # Are there pre-excluded ("locked-out") units?
  if (initialSolution_Units == TRUE && length(unitStatus$LockedOut) != 0) {
    matrixA_Final <- rbind(matrixA_Final, matrixA_MAMP12_02)
  }

  # Are there pre-included ("locked-in") actions?
  if (initialSolution_Actions == TRUE && nrow(actionStatus$LockedIn) != 0) {
    matrixA_Final <- rbind(matrixA_Final, matrixA_MAMP13_01)
  }
  # Are there pre-excluded ("locked-out") actions?
  if (initialSolution_Actions == TRUE && nrow(actionStatus$LockedOut) != 0) {
    matrixA_Final <- rbind(matrixA_Final, matrixA_MAMP13_02)
  }


  # utils::View(matrixA_Final)

  #------------------------------------------------------------------------------------------
  #----------------- Vector b - Parameters associated with all restrictions -----------------
  #-------------------------------- (RHS, right-hand side) ----------------------------------
  #------------------------------------------------------------------------------------------

  vectorb_MAMP2 <- as.vector(x = ts, mode = "numeric")
  vectorb_MAMP3 <- vector(mode = "numeric", length = sizeRHS_MAMP3)
  vectorb_MAMP4 <- vector(mode = "numeric", length = sizeRHS_MAMP4)
  vectorb_MAMP6 <- rep(c(0, 0, -1), numberVariablesY)

  vectorb_MAMP7 <- vector(mode = "numeric", length = sizeRHS_MAMP7)
  vectorb_MAMP8 <- rep(1, sizeRHS_MAMP8)
  vectorb_MAMP9 <- vector(mode = "numeric", length = sizeRHS_MAMP9)
  vectorb_MAMP10 <- vector(mode = "numeric", length = sizeRHS_MAMP10)
  vectorb_MAMP11 <- rep(c(0, 0, -1), numberVariablesP)

  vectorb_MAMP12_01 <- rep(1, sizeRHS_MAMP12_01) # NEW!
  vectorb_MAMP12_02 <- rep(0, sizeRHS_MAMP12_02) # NEW!
  vectorb_MAMP13_01 <- rep(1, sizeRHS_MAMP13_01) # NEW!
  vectorb_MAMP13_02 <- rep(0, sizeRHS_MAMP13_02) # NEW!

  # Transform the vector into a "sparse" type
  vectorb_Final <- c(vectorb_MAMP2, vectorb_MAMP3, vectorb_MAMP4, vectorb_MAMP6, vectorb_MAMP7, vectorb_MAMP8, vectorb_MAMP9, vectorb_MAMP10, vectorb_MAMP11)
  # NEW! Is there an initial solution of the model?
  vectorb_Final <- c(vectorb_Final, vectorb_MAMP12_01, vectorb_MAMP12_02, vectorb_MAMP13_01, vectorb_MAMP13_02) # NEW!

  # vectorb_Final = methods::as(object = vectorb_Final, Class = "sparseVector")
  #------------------------------------------------------------------------------------------
  #------------------------ Others inputs parameters for the solver -------------------------
  #------------ (Vector of sense, Vector of bounds, and Vector of variables type ) ----------
  #------------------------------------------------------------------------------------------
  # Right-hand-side (RHS) of the restrictions.
  vectorSense_MAMP2 <- rep(">=", sizeRHS_MAMP2)
  vectorSense_MAMP3 <- rep("<=", sizeRHS_MAMP3)
  vectorSense_MAMP4 <- rep("<=", sizeRHS_MAMP4)
  vectorSense_MAMP6 <- rep(c("<=", "<=", ">="), numberVariablesY)
  vectorSense_MAMP7 <- rep("<=", sizeRHS_MAMP7) # NEW!
  vectorSense_MAMP8 <- rep("==", sizeRHS_MAMP8) # NEW!
  vectorSense_MAMP9 <- rep("==", sizeRHS_MAMP9) # NEW!
  vectorSense_MAMP10 <- rep("==", sizeRHS_MAMP10) # NEW!
  vectorSense_MAMP11 <- rep(c("<=", "<=", ">="), numberVariablesP) # NEW!

  vectorSense_MAMP12_01 <- rep("==", sizeRHS_MAMP12_01) # NEW!
  vectorSense_MAMP12_02 <- rep("==", sizeRHS_MAMP12_02) # NEW!
  vectorSense_MAMP13_01 <- rep("==", sizeRHS_MAMP13_01) # NEW!
  vectorSense_MAMP13_02 <- rep("==", sizeRHS_MAMP13_02) # NEW!

  vectorSense_Final <- c(vectorSense_MAMP2, vectorSense_MAMP3, vectorSense_MAMP4, vectorSense_MAMP6, vectorSense_MAMP7, vectorSense_MAMP8, vectorSense_MAMP9, vectorSense_MAMP10, vectorSense_MAMP11, vectorSense_MAMP12_01, vectorSense_MAMP12_02, vectorSense_MAMP13_01, vectorSense_MAMP13_02)

  # Bounds of the variables.
  lowerBound_W <- rep(0, numberVariablesW)
  lowerBound_Y <- rep(0, numberVariablesY)
  lowerBound_X <- rep(0, numberVariablesX)
  lowerBound_Z <- rep(0, numberVariablesZ)
  lowerBound_B <- rep(0, numberVariablesB)
  lowerBound_Lambda <- rep(0, numberVariablesLambda)
  lowerBound_V <- rep(0, numberVariablesV)
  lowerBound_P <- rep(0, numberVariablesP)
  lowerBounds <- c(lowerBound_W, lowerBound_Y, lowerBound_X, lowerBound_Z, lowerBound_B, lowerBound_Lambda, lowerBound_V, lowerBound_P)

  upperBound_W <- rep(1, numberVariablesW)
  upperBound_Y <- rep(1, numberVariablesY)
  upperBound_X <- rep(1, numberVariablesX)
  upperBound_Z <- rep(1, numberVariablesZ)
  upperBound_B <- rep(1, numberVariablesB)
  upperBound_Lambda <- rep(1, numberVariablesLambda)
  upperBound_V <- rep(1, numberVariablesV)
  upperBound_P <- rep(1, numberVariablesP)
  upperBounds <- c(upperBound_W, upperBound_Y, upperBound_X, upperBound_Z, upperBound_B, upperBound_Lambda, upperBound_V, upperBound_P)

  vectorBounds_Final <- list(
    lower = list(ind = seq(1:numberVariables), val = lowerBounds),
    upper = list(ind = seq(1:numberVariables), val = upperBounds)
  )

  # Type of the variables.
  # "C", "I", and "B" corresponding to continuous, integer, and binary, respectively, or NULL (default), taken as all-continuous.
  vectorVarType_W <- rep("B", numberVariablesW)
  vectorVarType_Y <- rep("B", numberVariablesY)
  vectorVarType_X <- rep("B", numberVariablesX)
  vectorVarType_Z <- rep("B", numberVariablesZ)
  vectorVarType_B <- rep("C", numberVariablesB)
  vectorVarType_Lambda <- rep("C", numberVariablesLambda)
  vectorVarType_V <- rep("B", numberVariablesV)
  vectorVarType_P <- rep("B", numberVariablesP)

  vectorVarType_Final <- c(vectorVarType_W, vectorVarType_Y, vectorVarType_X, vectorVarType_Z, vectorVarType_B, vectorVarType_Lambda, vectorVarType_V, vectorVarType_P)

  preprocessingTime_2 <- Sys.time()
  preprocessingTime <- preprocessingTime_2 - preprocessingTime_1

  #------------------------------------------------------------------------------------------
  #-------------------------- Solvers (GUROBI - Rsymphony - GLPK) ---------------------------
  #------------------------------------------------------------------------------------------

  # Inputs parameters for solvers (matrix notation of the MAMP mathematical model)
  # max      <- FALSE               #max <- TRUE means F.O = maximize / max <- FALSE means F.O = minimize
  # write_lp <- TRUE                #Optional!
  # write_lp <- TRUE                #Optional!


  # create OptimizationProblem object
  pproto(NULL, OptimizationProblem,
    data = list(
      obj = vectorC_Final, rhs = vectorb_Final, sense = vectorSense_Final, vtype = vectorVarType_Final,
      A = matrixA_Final, bounds = vectorBounds_Final, modelsense = "min", break_actions = numberVariablesW + numberVariablesY
    ),
    ConservationClass = x
  )
}
