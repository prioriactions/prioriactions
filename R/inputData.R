#' @include internal.R
NULL

#' @title Creates the multi-action planning problem
#'
#' @description
#' Create the [data-class] object with information about the multi-action
#' conservation planning problem. This function is used to specify all the data
#' that defines the spatial prioritization problem (planning units data, feature
#' data, threats data, and their spatial distributions.)
#'
#' @param pu Object of class [data.frame()] that specifies the planning units (PU)
#' of the corresponding instance and their corresponding monitoring cost and status. Each
#' row corresponds to a different planning unit. This file is inherited from the
#' *pu.dat* in *Marxan*. It must contain the following columns:
#' \describe{
#'    \item{`id`}{`integer` unique identifier for each planning unit.}
#'    \item{`monitoring_cost`}{`numeric` cost of including each planning unit in the reserve system.}
#'    \item{`status`}{`integer` (**optional**) value that indicate if each planning unit
#'    should be available to be selected (0), *locked-in* (2) as part of the
#'    solution, or *locked-out* (3) and excluded from the solution.}
#'    }
#'
#' @param features Object of class [data.frame()] that specifies the conservation
#' features to consider in the optimization problem. Each row corresponds to a different
#' feature. This file is inherited from marxan's *spec.dat*.
#'
#' The `prioriactions` package supports two types of purposes when optimizing: focus on
#' recovery of features threatened (through the **recovery target**), where only
#' take into account benefits when taking action against threats and there is no benefit
#' when selecting planning units where the features are not threatened;
#' or include the benefits of the features sites where they are not threatened
#' (through the **conservation target**).
#'
#' Note that by default only information on recovery targets is necessary,
#' while conservation targets equal to zero are assumed. The maximum values of
#' benefits to achieve both recovery and conservation per feature can be verified
#' with the `getPotentialBenefit()` function.
#' For more information on the implications of these targets in the solutions see
#' the [recovery](https://prioriactions.github.io/prioriactions/articles/objectives.html)
#' vignette.
#'
#' This file must contain
#' the following columns:
#' \describe{
#'    \item{`id`}{`integer` unique identifier for each conservation feature.}
#'    \item{`target_recovery`}{`numeric` amount of recovery target to achieve for each conservation feature.
#'    This field is **required** if a `minimizeCosts` model is used.}
#'    \item{`target_conservation`}{`numeric` (**optional**) amount of conservation target to achieve
#'    for each conservation feature.
#'    This field is used only if a model of the type `minimizeCosts` is applied.}
#'    \item{`name`}{`character` (**optional**) name for each conservation feature.}
#'    }
#'
#' @param dist_features Object of class [data.frame()] that specifies the spatial
#' distribution of conservation features across planning units. Each row corresponds
#' to a combination of planning unit and feature. This file is inherited from marxan's
#' *puvspr.dat*. It must contain the following columns:
#' \describe{
#'    \item{`pu`}{`integer` *id* of a planning unit where the conservation feature
#'    listed on the same row occurs.}
#'    \item{`feature`}{`integer` *id* of each conservation feature.}
#'    \item{`amount`}{`numeric` amount of the feature in the planning unit. Set
#'    to 1 to work with presence/absence.}
#'    }
#'
#' @param threats Object of class [data.frame()] that specifies the threats to consider in
#' the optimization exercise. Each row corresponds to a different threats. It must contain
#' the following columns:
#' \describe{
#'    \item{`id`}{`integer` unique identifier for each threat.}
#'    \item{`blm_actions`}{`numeric` (**optional**) penalty of connectivity between threats.
#'    Default is 0.}
#'    \item{`name`}{`character` (**optional**) name for each threat.}
#'    }
#'
#' @param dist_threats Object of class [data.frame()] that specifies the spatial
#' distribution of threats across planning units. Each row corresponds
#' to a combination of planning unit and threat. It must contain the following
#' columns:
#' \describe{
#'    \item{`pu`}{`integer` *id* of a planning unit where the threat listed on the
#'    same row occurs.}
#'    \item{`threat`}{`integer` *id* of each threat.}
#'    \item{`amount`}{`numeric` amount of the threat in the planning unit. Set
#'    to 1 to work with presence/absence. Continuous amount values require
#'    that feature sensitivities to threats be established (more info in
#'    [sensitivities](https://prioriactions.github.io/prioriactions/articles/sensitivities.html)
#'    vignette).}
#'    \item{`action_cost`}{`numeric` cost of an action to abate the threat
#'    in each planning unit.}
#'    \item{`status`}{`integer` (**optional**) value that indicates if each action
#'    to abate the threat is available to be selected (0), *locked-in* (2)
#'    as part of the solution, or *locked-out* (3) and therefore excluded from the solution.}
#'    }
#'
#' @param sensitivity (**optional**) Object of class [data.frame()] that specifies
#' the sensitivity of each feature to each threat. Each row corresponds
#' to a combination of feature and threat. If not informed, all features
#' are assumed to be sensitive to all threats.
#'
#' Sensitivity can be parameterized in two ways: **binary**; the feature is
#' sensitive or not, or **continuous**; with response curves of the probability of
#' persistence of the features to threats. For the first case, it is only necessary
#' to indicate the ids of the threats and the respective features sensitive to them.
#' In the second case, the response can be parameterized through four values: *\eqn{\delta_1}*, *\eqn{\delta_2}*, *\eqn{\delta_3}*
#' and *\eqn{\delta_4}*. See
#' [sensitivities](https://prioriactions.github.io/prioriactions/articles/sensitivities.html)
#' vignette for more information on continuous sensitivities. Then, the sensitivity input must contain the following columns:
#'    \describe{
#'    \item{`feature`}{`integer` *id* of each conservation feature.}
#'    \item{`threat`}{`integer` *id* of each threat.}
#'    \item{`delta1`}{`numeric` (**optional**) the minimum intensity of the threat at
#'    which the features probability of persistence starts to decline. The more
#'    sensitive the feature is to the threat, the lowest this value will be. Default
#'    is 0.}
#'    \item{`delta2`}{`numeric` (**optional**) the value of intensity of the threat
#'    over which the feature has a probability of persistence of 0. If it is not
#'    established,it is assumed as the **maximum value of the threat across all planning units**
#'    in the study area.
#'    Note that this might overestimate the sensitivity of features to threats,
#'    as they will only be assumed to disappear from planning units if the
#'    threats reach the maximum intensity value in the study area.}
#'    \item{`delta3`}{`numeric` (**optional**) minimum probability of persistence of a
#'    features when a threat reaches its maximum intensity value. Default is 0.}
#'    \item{`delta4`}{`numeric` (**optional**) maximum probability of persistence of a
#'    features in absence threats. Default is 1.}
#'    }
#'  Note that optional parameters *delta1*, *delta2*, *delta3* and *delta4* can be provided independently.
#'
#' @param boundary (**optional**) Object of class [data.frame()] that specifies
#' the spatial relationship between pair of planning units. Each row corresponds
#' to a combination of planning unit. This file is inherited from marxan's
#' *bound.dat*. It must contain the following columns:
#'   \describe{
#'   \item{`id1`}{`integer` *id* of each planning unit.}
#'   \item{`id2`}{`integer` *id* of each planning unit.}
#'   \item{`boundary`}{`numeric` penalty applied in the objective function
#'   when only one of the planning units is present in the solution.}
#'   }
#'
#' @param ... Unused arguments, reserved for future expansion.
#'
#' @name inputData
#'
#' @return An object of class [data-class].
#'
#' @seealso For more information on the correct format for *Marxan* input data, see the
#' [official *Marxan* website](https://marxansolutions.org) and Ball *et al.* (2009).
#'
#' @examples
#' ## set seed for reproducibility
#' set.seed(14)
#'
#' ## Set prioriactions path
#' prioriactions_path <- system.file("extdata/example_input/", package = "prioriactions")
#'
#' ## Load in planning unit data
#' pu_data <- data.table::fread(paste0(prioriactions_path,"/pu.dat"),
#'                              data.table = FALSE)
#' head(pu_data)
#'
#' ## Load in feature data
#' features_data <- data.table::fread(paste0(prioriactions_path,"/features.dat"),
#'                                    data.table = FALSE)
#' head(features_data)
#'
#' ## Load in planning unit vs feature data
#' dist_features_data <- data.table::fread(paste0(prioriactions_path,"/dist_features.dat"),
#'                                         data.table = FALSE)
#' head(dist_features_data)
#'
#' ## Load in the threats data
#' threats_data <- data.table::fread(paste0(prioriactions_path,"/threats.dat"),
#'                                   data.table = FALSE)
#' head(threats_data)
#'
#' ## Load in the threats distribution data
#' dist_threats_data <- data.table::fread(paste0(prioriactions_path,"/dist_threats.dat"),
#'                                        data.table = FALSE)
#' head(dist_threats_data)
#'
#' ## Load in the sensitivity data
#' sensitivity_data <- data.table::fread(paste0(prioriactions_path,"/sensitivity.dat"),
#'                                       data.table = FALSE)
#' head(sensitivity_data)
#'
#' ## Load in the boundary data
#' boundary_data <- data.table::fread(paste0(prioriactions_path,"/boundary.dat"),
#'                                    data.table = FALSE)
#' head(boundary_data)
#'
#' ## Create data instance
#' problem_data <- inputData(
#'   pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,
#'   threats = sim_threats_data, dist_threats = sim_dist_threats_data,
#'   sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
#' )
#'
#' ## Summary
#' print(problem_data)
#'
#' @references
#' \itemize{
#' \item Ball I, Possingham H, Watts, M. *Marxan and relatives: software for spatial
#' conservation prioritization*. Spatial conservation prioritisation: quantitative
#' methods and computational tools 2009.
#' }
#'
#' @export
methods::setGeneric("inputData",
                    signature = methods::signature("pu", "features", "dist_features", "threats", "dist_threats"),
                    function(pu, features, dist_features, threats, dist_threats, ...) standardGeneric("inputData")
)

#' @rdname inputData
methods::setMethod(
  "inputData",
  methods::signature(
    pu = "data.frame", features = "data.frame", dist_features = "data.frame",
    threats = "data.frame", dist_threats = "data.frame"
  ),
  function(pu, features, dist_features, threats, dist_threats,
           sensitivity = NULL, boundary = NULL) {

    ## pu
    assertthat::assert_that(
      inherits(pu, "data.frame"),
      assertthat::has_name(pu, "id"),
      is.numeric(pu$id),
      anyDuplicated(pu$id) == 0,
      nrow(pu) > 0,
      assertthat::noNA(pu$id),
      assertthat::has_name(pu, "monitoring_cost"),
      is.numeric(pu$monitoring_cost),
      assertthat::noNA(pu$monitoring_cost)
    )
    if ("status" %in% names(pu)) {
      assertthat::assert_that(
        is.numeric(pu$status),
        assertthat::noNA(pu$status),
        all(pu$status %in% c(0, 2, 3))
      )
    } else {
      pu$status <- 0
    }
    pu <- pu[order(pu$id),]

    ## features
    assertthat::assert_that(
      inherits(features, "data.frame"),
      assertthat::has_name(features, "id"),
      is.numeric(features$id),
      nrow(features) > 0,
      anyDuplicated(features$id) == 0,
      assertthat::noNA(features$id)
    )
    if ("name" %in% names(features)) {
      assertthat::assert_that(
        inherits(as.character(features$name), c("character", "factor")),
        anyDuplicated(as.character(features$name)) == 0,
        assertthat::noNA(as.character(features$name))
      )
    } else {
      features$name <- paste0("feature.", seq_len(nrow(features)))
    }

    ## Targets_recovery
    assertthat::assert_that(
      assertthat::has_name(features, "target_recovery"),
      is.numeric(features$target_recovery),
      assertthat::noNA(features$target_recovery)
    )

    if(assertthat::has_name(features, "target_conservation")){
      assertthat::assert_that(
        is.numeric(features$target_conservation),
        assertthat::noNA(features$target_conservation)
      )
    }
    else{
      features$target_conservation <- 0
    }


    ## dist_features
    assertthat::assert_that(
      inherits(dist_features, "data.frame"),
      assertthat::has_name(dist_features, "pu"),
      assertthat::has_name(dist_features, "feature"),
      assertthat::has_name(dist_features, "amount"),
      nrow(dist_features) > 0,
      is.numeric(dist_features$pu),
      is.numeric(dist_features$feature),
      is.numeric(dist_features$amount),
      assertthat::noNA(dist_features$pu),
      assertthat::noNA(dist_features$feature),
      assertthat::noNA(dist_features$amount),
      all(dist_features$amount >= 0),
      all(dist_features$pu %in% pu$id),
      all(dist_features$feature %in% features$id)
    )

    # eliminate features with amount equal to zero
    dist_features <- dist_features[!dist_features$amount == 0, ]

    # verify if exist rows duplicates
    dist_features_copy <- dist_features[,c("pu", "feature")]
    dist_features_copy <- dist_features_copy %>% dplyr::distinct()

    if(nrow(dist_features_copy) != nrow(dist_features)){
      stop("There are duplicate values of the pu and feature pair in the dis_features input file.", call. = FALSE, immediate. = TRUE)
    }

    ## threats
    assertthat::assert_that(
      inherits(threats, "data.frame"),
      assertthat::has_name(threats, "id"),
      is.numeric(threats$id),
      nrow(threats) > 0,
      anyDuplicated(threats$id) == 0,
      assertthat::noNA(threats$id)
    )
    if ("name" %in% names(threats)) {
      assertthat::assert_that(
        inherits(as.character(threats$name), c("character", "factor")),
        anyDuplicated(as.character(threats$name)) == 0,
        assertthat::noNA(as.character(threats$name))
      )
    } else {
      threats$name <- paste0("threat.", seq_len(nrow(threats)))
    }
    if ("blm_actions" %in% names(threats)) {
      assertthat::assert_that(
        is.numeric(threats$blm_actions),
        all(threats$blm_actions >= 0))
    }
    else{
      threats$blm_actions <- 0.0
    }

    ## dist_threats
    assertthat::assert_that(
      inherits(dist_threats, "data.frame"),
      assertthat::has_name(dist_threats, "pu"),
      assertthat::has_name(dist_threats, "threat"),
      assertthat::has_name(dist_threats, "action_cost"),
      assertthat::has_name(dist_threats, "amount"),
      nrow(dist_threats) > 0,
      is.numeric(dist_threats$pu),
      is.numeric(dist_threats$threat),
      is.numeric(dist_threats$action_cost),
      is.numeric(dist_threats$amount),
      assertthat::noNA(dist_threats$pu),
      assertthat::noNA(dist_threats$threat),
      assertthat::noNA(dist_threats$action_cost),
      all(dist_threats$amount >= 0),
      all(dist_threats$pu %in% pu$id)
    )
    if ("status" %in% names(dist_threats)) {
      assertthat::assert_that(
        is.numeric(dist_threats$status),
        assertthat::noNA(dist_threats$status),
        all(dist_threats$status %in% c(0, 2, 3))
      )

      status_lock_out <- pu$status == 3

      for(row in seq_along(pu$id)){
        if(isTRUE(status_lock_out[row])){

          status_locked_out <- dist_threats[dist_threats$pu == pu$id[row], ]$status
          status_incorrect <- any(status_locked_out == 2)

          if(isTRUE(status_incorrect)){
            warning(paste0("The pu ", pu$id[row], " was set as locked out so it is not possible to set actions on it (lock in). Therefore, all actions in that unit will be set as locked out"), call. = FALSE, immediate. = TRUE)
          }

          if(length(status_locked_out) != 0){
            dist_threats[dist_threats$pu == pu$id[row], ]$status <- 3
          }
        }
      }
    } else {
      pu$status <- 0
    }

    # eliminate threats with amount equal to zero
    dist_threats <- dist_threats[!dist_threats$amount == 0, ]

    # verify if exist rows duplicates
    dist_threats_copy <- dist_threats[,c("pu", "threat")]
    dist_threats_copy <- dist_threats_copy %>% dplyr::distinct()

    if(nrow(dist_threats_copy) != nrow(dist_threats)){
      stop("There are duplicate values of the pu and threat pair in the dis_threats input file.", call. = FALSE, immediate. = TRUE)
    }

    ## sensitivity
    assertthat::assert_that(inherits(sensitivity, c("NULL", "data.frame")))
    if (inherits(sensitivity, "data.frame")) {
      assertthat::assert_that(
        assertthat::has_name(sensitivity, "feature"),
        assertthat::has_name(sensitivity, "threat"),
        nrow(sensitivity) > 0,
        is.numeric(sensitivity$feature),
        is.numeric(sensitivity$threat),
        assertthat::noNA(sensitivity$feature),
        assertthat::noNA(sensitivity$threat)
      )
    }
    else{
      sensitivity <- base::expand.grid("feature" = features$id, "threat" = threats$id)
    }

    if ("delta1" %in% names(sensitivity)) {
      assertthat::assert_that(
        !is.character(sensitivity$delta1)
      )
      sensitivity$delta1[is.na(sensitivity$delta1)] = 0
    } else {
      sensitivity$delta1 <- 0
    }
    if ("delta2" %in% names(sensitivity)) {
      assertthat::assert_that(
        !is.character(sensitivity$delta2)
      )
    } else {
      sensitivity$delta2 <- NA
    }

    vector_max_intensities <- c()

    max_intensities <- dist_threats %>% dplyr::group_by(.data$threat) %>% dplyr::summarise(value = max(.data$amount))

    for(i in seq_len(nrow(max_intensities))){
      if(any(sensitivity$threat == max_intensities$threat[i][[1]])){
        vector_max_intensities[sensitivity$threat == max_intensities$threat[i][[1]]] <- max_intensities$value[i][[1]]
      }
    }
    sensitivity$delta2[is.na(sensitivity$delta2)] <- vector_max_intensities[is.na(sensitivity$delta2)]


    if ("delta3" %in% names(sensitivity)) {
      assertthat::assert_that(
        !is.character(sensitivity$delta3)
      )
      sensitivity$delta3[is.na(sensitivity$delta3)] = 0
    } else {
      sensitivity$delta3 <- 0
    }
    if ("delta4" %in% names(sensitivity)) {
      assertthat::assert_that(
        !is.character(sensitivity$delta4)
      )
      sensitivity$delta4[is.na(sensitivity$delta4)] = 1
    } else {
      sensitivity$delta4 <- 1
    }

    if(isFALSE(all(sensitivity$delta2 > sensitivity$delta1))){
      stop("Every value of delta1 parameter must be less than every value of delta2 parameter", call. = FALSE, immediate. = TRUE)
    }

    if(isFALSE(all(sensitivity$delta4 > sensitivity$delta3))){
      stop("Every value of delta3 parameter must be less than every value of delta4 parameter", call. = FALSE, immediate. = TRUE)
    }


    ## boundary
    assertthat::assert_that(inherits(boundary, c("NULL", "data.frame")))
    if (inherits(boundary, "data.frame")) {
      assertthat::assert_that(
        assertthat::has_name(boundary, "id1"),
        assertthat::has_name(boundary, "id2"),
        assertthat::has_name(boundary, "boundary"),
        is.numeric(boundary$id1),
        is.numeric(boundary$id2),
        is.numeric(boundary$boundary),
        assertthat::noNA(boundary$id1),
        assertthat::noNA(boundary$id2),
        assertthat::noNA(boundary$boundary)
        #all(boundary$id1 %in% pu$id), all(boundary$id2 %in% pu$id)
      )
      boundary$boundary <- base::round(boundary$boundary, 3)
    }


    ## Verification subsets
    verify_that(all(as.matrix(pu[, "monitoring_cost", drop = FALSE]) >= 0,
                    na.rm = TRUE
    ),
    msg = "argument to pu has negative monitoring cost data"
    )

    dif_pu <- setdiff(unique(pu$id), unique(dist_features$pu))
    if (length(dif_pu) != 0L) {
      warning(paste0("The following pu's do not contain features: ", paste(dif_pu, collapse = " ")), call. = FALSE, immediate. = TRUE)
    }

    dif_features <- setdiff(unique(features$id), unique(dist_features$feature))
    if (length(dif_features) != 0L) {

      warning(paste0("The following features are not represented (it'll not be considered in the model): ", paste(dif_features, collapse = " ")), call. = FALSE, immediate. = TRUE)

      # eliminate species not represented
      features <- features[!features$id %in% dif_features, ]
      sensitivity <- sensitivity[!sensitivity$feature %in% dif_features, ]
    }

    dif_species_threatened <- setdiff(unique(features$id), unique(sensitivity$feature))
    if (length(dif_species_threatened) != 0L) {
      warning(paste0("The following features are not threatened: ", paste(dif_species_threatened, collapse = " ")), call. = FALSE, immediate. = TRUE)

      # eliminate species not threatened

    }

    dif_threats <- setdiff(unique(threats$id), unique(dist_threats$threat))
    if (length(dif_threats) != 0L) {
      warning(paste0("The following threats are not represented (it'll not be considered in the model): ", paste(dif_threats, collapse = " ")), call. = FALSE, immediate. = TRUE)

      # eliminate species not represented
      threats <- threats[!threats$id %in% dif_threats, ]
      sensitivity <- sensitivity[!sensitivity$threat %in% dif_threats, ]
    }

    dif_threats_dangerous <- setdiff(unique(threats$id), unique(sensitivity$threat))
    if (length(dif_threats_dangerous) != 0L) {
      warning(paste0("The following threats are not dangerous to any features (it'll not be considered in the model): ", paste(dif_threats_dangerous, collapse = " ")), call. = FALSE, immediate. = TRUE)

      # eliminate threats not represented
      threats <- threats[!threats$id %in% dif_threats_dangerous, ]
      dist_threats <- dist_threats[!dist_threats$threat %in% dif_threats_dangerous, ]
    }
    if(!is.null(boundary)){
      if(!all(boundary$id1 %in% pu$id) || !all(boundary$id2 %in% pu$id)){
        warning("The boundary data contain pu which ids does not exist in pu data (it'll not be considered in the model)", call. = FALSE, immediate. = TRUE)

        rows_boundary_leftover <- c(which(!boundary$id1 %in% pu$id), which(!boundary$id2 %in% pu$id))
        boundary <- boundary[-rows_boundary_leftover,]
      }
    }



    ## Rounding numeric fields of input data
    pu$monitoring_cost <- base::round(pu$monitoring_cost, 3)
    features$target_recovery <- base::round(features$target_recovery, 3)
    features$target_conservation <- base::round(features$target_conservation, 3)
    dist_features$amount <- base::round(dist_features$amount, 3)
    threats$blm_actions <- base::round(threats$blm_actions, 3)
    dist_threats$amount <- base::round(dist_threats$amount, 3)
    dist_threats$action_cost <- base::round(dist_threats$action_cost, 3)
    sensitivity$delta1 <- base::round(sensitivity$delta1, 3)
    sensitivity$delta2 <- base::round(sensitivity$delta2, 3)
    sensitivity$delta3 <- base::round(sensitivity$delta3, 3)
    sensitivity$delta4 <- base::round(sensitivity$delta4, 3)



    ## Creating internal id's
    # pu
    pu$internal_id <- seq_len(nrow(pu))

    # features
    features$internal_id <- seq_len(nrow(features))

    # threats
    threats$internal_id <- seq_len(nrow(threats))

    # boundary
    if(!is.null(boundary)){
      internal_id1 <- dplyr::inner_join(boundary, pu, by = c("id1" = "id"))$internal_id
      internal_id2 <- dplyr::inner_join(boundary, pu, by = c("id2" = "id"))$internal_id
      boundary$internal_id1 <- internal_id1
      boundary$internal_id2 <- internal_id2
    }

    # dist_features
    internal_id <- dplyr::inner_join(dist_features, pu, by = c("pu" = "id"))$internal_id
    dist_features$internal_pu <- internal_id

    internal_feature <- dplyr::inner_join(dist_features, features, by = c("feature" = "id"))$internal_id
    dist_features$internal_feature <- internal_feature

    # dist_threats
    internal_id <- dplyr::inner_join(dist_threats, pu, by = c("pu" = "id"))$internal_id
    dist_threats$internal_pu <- internal_id

    internal_threat <- dplyr::inner_join(dist_threats, threats, by = c("threat" = "id"))$internal_id
    dist_threats$internal_threat <- internal_threat

    # sensitivity
    internal_feature <- dplyr::inner_join(sensitivity, features, by = c("feature" = "id"))$internal_id
    sensitivity$internal_feature <- internal_feature

    internal_threat <- dplyr::inner_join(sensitivity, threats, by = c("threat" = "id"))$internal_id
    sensitivity$internal_threat <- internal_threat


    ## Create Data object

    pproto(NULL, Data,
           data = list(
             pu = pu, features = features, dist_features = dist_features, dist_threats = dist_threats, threats = threats,
             sensitivity = sensitivity, boundary = boundary
           )
    )
  }
)
