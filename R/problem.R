#' @include internal.R
NULL

#' @title Multi-action conservation planning problem
#'
#' @description
#' Create a data instance for the multi-action conservation planning problem. This function is used to specify
#' the basic data used in a problem of prioritization of multiple conservation actions.
#' This includes, the spatial distribution of the planning units and their costs,
#' the features to be conserved (e.g. species, ecosystems) and their threats, and the
#' costs of implementing an action to remove such threats.
#'
#' @param pu Object of class [data.frame()] that specifies the planning units
#' to use in the reserve design exercise and their corresponding cost. It may be
#' desirable to exclude some planning units from the analysis, for example those outside
#' the study area. To exclude planning units you have two options:
#' (i) pre-process the `data.frame` before using it (i.e., *manually* exclude planning units);
#' or (ii) set the *status* of planning units to 3 not to include them. Each row corresponds
#' to a different planning unit, and each column corresponds to different information about the planning units.
#' The description of the columns is listed below.
#' \describe{
#'    \item{`"id"`}{`integer` unique identifier for each planning unit. This column is **mandatory**.}
#'    \item{`"cost"`}{`numeric` cost of including each planning unit in the reserve system. This column is **mandatory**.}
#'    \item{`"status"`}{`integer` indicating if each planning unit should not be locked in the solution (0) or if it should be *locked-in* (2) or *locked-out* (3) of the solution. This column is **optional**.}
#'    }
#'
#' @param features Object of class [data.frame()] that specifies the conservation
#' features to consider in the reserve design exercise, such as their name and representation
#' requirements (i.e., *targets*). Each row corresponds to a different feature and each column
#' corresponds to different information about the features. The description of the columns is listed below.
#' \describe{
#'    \item{`"id"`}{`integer` unique identifier for each conservation feature. This column is **mandatory**.}
#'    \item{`"target"`}{`numeric` target amount for each conservation feature. This column is **mandatory**.}
#'    \item{`"name"`}{`character` name for each conservation feature. This column is **optional**.}
#'    }
#'
#' @param dist_features Object of class [data.frame()] that specifies the spatial distribution of conservation features across planning units.
#' **It is recommended not to include cases where a feature does not occur in a planning unit (avoid rows with
#' `"amount"` equal to 0). Instead, it is recommended to omit these cases completely from the [data.frame()], for efficiency reasons.**
#' The description of the columns is listed below.
#' \describe{
#'    \item{`"pu"`}{`integer` *id* of a planning unit where the conservation feature listed on the same row occurs. This column is **mandatory**.}
#'    \item{`"species"`}{`integer` *id* of each conservation feature. This column is **mandatory**.}
#'    \item{`"amount"`}{`integer` indicating if the conservation feature occurring in the planning unit listed on the same row (1), or if it is not occurring (0). This column is **mandatory**.}
#'    }
#'
#' @param threats Object of class [data.frame()] that specifies the threats to consider in
#' the reserve design exercise, such as their name and penalty of connectivity (i.e., *blm_actions*). Each row corresponds to a different feature and each column
#' corresponds to different information about the threats The description of the columns is listed below.
#' \describe{
#'    \item{`"id"`}{`integer` unique identifier for each threat. This column is **mandatory**.}
#'    \item{`"name"`}{`character` name for each threat. This column is **optional**.}
#'    \item{`"blm_actions"`}{`numeric` penalty of connectivity between threats. This column is **optional**.}
#'    }
#'
#' @param dist_threats Object of class [data.frame()] that specifies the spatial distribution of threats across planning units
#' and the corresponding cost of applying an action against these threats. **It is recommended not to include cases where a threat does not occur
#' in a planning unit (avoid rows with `"amount"` equal to 0). Instead, it is recommended to omit these cases completely from the [data.frame()], for efficiency reasons.**
#' The description of the columns is listed below.
#' \describe{
#'    \item{`"pu"`}{`integer` *id* of a planning unit where the threat listed on the same row occurs. This column is **mandatory**.}
#'    \item{`"threats"`}{`integer` *id* of each threat. This column is **mandatory**.}
#'    \item{`"amount"`}{`integer` indicating if the threat occurring in the planning unit listed on the same row (1), or if it is not occurring (0). This column is **mandatory**.}
#'    \item{`"cost"`}{`numeric` cost of applying an action to eliminate the threat in the planning unit listed in the same row. This column is **mandatory**.}
#'    \item{`"status"`}{`integer` indicating if each action should not be locked in the solution (0) or if it should be *locked-in* (2) or *locked-out* (3) of the solution. This column is **optional**.}
#'    }
#'
#' @param sensitivity Object of class [data.frame()] that specifies the species-threats sensitivity,
#' i.e., which threats affect the persistence of endangered conservation features (e.g., species).
#' **It is recommended not to include cases where there is no species-threats sensitivity
#' (avoid rows with `"amount"` equal to 0). Instead, it is recommended to omit these cases completely
#' from the [data.frame()], for efficiency reasons.**
#' The description of the columns is listed below.
#'    \describe{
#'    \item{`"species"`}{`integer` *id* of each conservation feature. This column is **mandatory**.}
#'    \item{`"threats"`}{`integer` *id* of each threat. This column is **mandatory**.}
#'    \item{`"amount"`}{`integer` indicating if the threat againsting the conservation feature listed on the same row (1), or if it is not againsting (0). This column is **mandatory**.}
#'    }
#'
#' @param boundary Object of class [NULL()] (default) or an object of class [data.frame()].
#' `NULL` indicates that planning units' boundaries (i.e., *boundary data*) are not required for the multi-action conservation planning problem.
#' `data.frame` specifies the boundaries of the planning units, i.e., the spatial relationship between two units, such as the "length" of the shared boundary between them.
#' The description of the columns is listed below.
#'   \describe{
#'   \item{`"id1"`}{`integer` *id* of each planning unit. This column is **mandatory**.}
#'   \item{`"id2"`}{`integer` *id* of each planning unit. This column is **mandatory**.}
#'   \item{`"boundary"`}{`numeric` "length" of shared boundary between the planning units identified on the same row. This column is **mandatory**.}
#'   }
#'
#' @name problem
#'
#' @return An object of class [ConservationProblem-class()].
#'
#' @details If you are familiar with using conservation planning software called
#' *Marxan*, you should know that most of the arguments in this function
#' follow the conventions used by the Marxan input files. This means that,
#' `pu` corresponds to the data in the **Planning Unit File** (typically called `"pu.dat"`);
#' `features` corresponds to the data in the **Conservation Feature File** (typically
#' called `"spec.dat"`); `dist_features` corresponds to the data in the **Planning Unit versus
#' Conservation Feature File** (typically called `"dist_features.dat"`); and `boundary` corresponds
#' to the data in the **Boundary Length File** (typically called `"bound.dat"`).
#'
#' @seealso For more information on the correct format for *Marxan* input data, see the
#' [official *Marxan* website](https://marxansolutions.org) and Ball *et al.* (2009).
#'
#' @examples
#' ## Create a data instance for the multi-action conservation planning problem
#' ## using data.frames that have been loaded into R. This example uses input files included
#' ## into package.
#'
#' ## Set prioriactions path
#' prioriactions_path <- system.file("extdata/input/", package = "prioriactions")
#'
#' ## Load in planning unit data
#' pu_data <- data.table::fread(paste0(prioriactions_path,"pu.dat"), data.table = FALSE)
#' head(pu_data)
#'
#' ## Load in feature data
#' features_data <- data.table::fread(paste0(prioriactions_path,"features.dat"), data.table = FALSE)
#' head(spec_data)
#'
#' ## Load in planning unit vs feature data
#' dist_features_data <- data.table::fread(paste0(prioriactions_path,"dist_features.dat"), data.table = FALSE)
#' head(dist_features_data)
#'
#' ## Load in the threats data
#' threats_data <- data.table::fread(paste0(prioriactions_path,"threats.dat"), data.table = FALSE)
#' head(threats_data)
#'
#' ## Load in the threats distribution data
#' dist_threats_data <- data.table::fread(paste0(prioriactions_path,"dist_threats.dat"), data.table = FALSE)
#' head(dist_threats_data)
#'
#' ## Load in the sensitivity data
#' sensitivity_data <- data.table::fread(paste0(prioriactions_path,"sensibility.dat"), data.table = FALSE)
#' head(sensitivity_data)
#'
#' ## Load in the boundary data
#' boundary_data <- data.table::fread(paste0(prioriactions_path,"bound.dat"), data.table = FALSE)
#' head(boundary_data)
#'
#' ## Create data instance
#' problem_data <- problem(
#'   pu = pu_data, features = features_data, dist_features = dist_features_data,
#'   dist_threats = dist_threats_data, threats = threats_data, sensitivity = sensitivity_data,
#'   boundary = boundary_data
#' )
#'
#' ## Instance summary
#' print(problem_data)
#' @references
#' \itemize{
#' \item Ball I, Possingham H, Watts, M. *Marxan and relatives: software for spatial
#' conservation prioritization*. Spatial conservation prioritisation: quantitative
#' methods and computational tools 2009; 185<U+2012>195.
#' }
#'
#' @export
methods::setGeneric("problem",
                    signature = methods::signature("pu", "features", "dist_features", "threats", "dist_threats","sensitivity"),
                    function(pu, features, dist_features, threats, dist_threats, sensitivity, ...) standardGeneric("problem")
)

#' @name problem
#' @usage \S4method{problem}{data.frame}(pu, features, dist_features, threats, dist_threats, sensitivity, boundary = NULL)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(
    pu = "data.frame", features = "data.frame", dist_features = "data.frame",
    threats = "data.frame", dist_threats = "data.frame", sensitivity = "data.frame"
  ),
  function(pu, features, dist_features, threats, dist_threats, sensitivity, boundary = NULL, ...) {
    # This is an exact copy of the prioritizr code ------------

    # assert arguments are valid
    assertthat::assert_that(no_extra_arguments(...))

    ## pu
    assertthat::assert_that(
      inherits(pu, "data.frame"),
      assertthat::has_name(pu, "id"),
      is.numeric(pu$id),
      anyDuplicated(pu$id) == 0,
      nrow(pu) > 0,
      assertthat::noNA(pu$id),
      assertthat::has_name(pu, "cost"),
      is.numeric(pu$cost),
      assertthat::noNA(pu$cost)
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
      threats$name <- paste0("threats.", seq_len(nrow(threats)))
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
      assertthat::has_name(dist_threats, "cost"),
      assertthat::has_name(dist_threats, "amount"),
      nrow(dist_threats) > 0,
      is.numeric(dist_threats$pu),
      is.numeric(dist_threats$threat),
      is.numeric(dist_threats$cost),
      is.numeric(dist_threats$amount),
      assertthat::noNA(dist_threats$pu),
      assertthat::noNA(dist_threats$threat),
      assertthat::noNA(dist_threats$cost),
      all(dist_threats$amount >= 0),
      all(dist_threats$pu %in% pu$id)
    )
    if ("status" %in% names(dist_threats)) {
      assertthat::assert_that(
        is.numeric(dist_threats$status),
        assertthat::noNA(dist_threats$status),
        all(dist_threats$status %in% c(0, 2, 3))
      )
    } else {
      pu$status <- 0
    }

    # eliminate threats with amount equal to zero
    dist_threats <- dist_threats[!dist_threats$amount == 0, ]


    ## boundary
    assertthat::assert_that(inherits(boundary, c("NULL", "data.frame")))
    if (inherits(boundary, c("data.frame"))) {
      assertthat::assert_that(
        assertthat::has_name(boundary, "id1"),
        assertthat::has_name(boundary, "id2"),
        assertthat::has_name(boundary, "boundary"),
        is.numeric(boundary$id1),
        is.numeric(boundary$id2),
        is.numeric(boundary$boundary),
        assertthat::noNA(boundary$id1),
        assertthat::noNA(boundary$id2),
        assertthat::noNA(boundary$boundary),
        all(boundary$id1 %in% pu$id), all(boundary$id2 %in% pu$id)
      )
    }


    ## sensitivity
    assertthat::assert_that(
      inherits(sensitivity, "data.frame"),
      assertthat::has_name(sensitivity, "feature"),
      assertthat::has_name(sensitivity, "threat"),
      nrow(sensitivity) > 0,
      is.numeric(sensitivity$feature),
      is.numeric(sensitivity$threat),
      assertthat::noNA(sensitivity$feature),
      assertthat::noNA(sensitivity$threat),
      assertthat::noNA(sensitivity$amount)
    )
    if ("a" %in% names(sensitivity)) {
      assertthat::assert_that(
        is.numeric(sensitivity$a),
        assertthat::noNA(sensitivity$a)
      )
    } else {
      sensitivity$a <- 0
    }
    if ("b" %in% names(sensitivity)) {
      assertthat::assert_that(
        is.numeric(sensitivity$b),
        assertthat::noNA(sensitivity$b)
      )
    } else {
      max_intensities <- dist_threats %>% dplyr::group_by(threat) %>% dplyr::summarise(value = max(amount))

      sensitivity$b <- 1
      for(i in 1:nrow(max_intensities)){
        if(any(sensitivity$threat == max_intensities$threat[i][[1]])){
          sensitivity[sensitivity$threat == max_intensities$threat[i][[1]], ]$b <- max_intensities$value[i][[1]]
        }
      }
    }
    if ("c" %in% names(sensitivity)) {
      assertthat::assert_that(
        is.numeric(sensitivity$c),
        assertthat::noNA(sensitivity$c),
        all(sensitivity$c < 1.0)
      )
    } else {
      sensitivity$c <- 0
    }
    if ("d" %in% names(sensitivity)) {
      assertthat::assert_that(
        is.numeric(sensitivity$d),
        assertthat::noNA(sensitivity$d)
      )
    } else {
      sensitivity$d <- 1
    }

    if(isFALSE(all(sensitivity$b > sensitivity$a))){
      stop("Every value of a must be less than every value of b", call. = FALSE, immediate. = TRUE)
    }

    if(isFALSE(all(sensitivity$d > sensitivity$c))){
      stop("Every value of c parameter must be less than every value of d parameter", call. = FALSE, immediate. = TRUE)
    }


    ## Verification subsets
    verify_that(all(as.matrix(pu[, "cost", drop = FALSE]) >= 0,
                    na.rm = TRUE
    ),
    msg = "argument to pu has negative cost data"
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

    ## Creating internal id's

    # pu
    pu$internal_id <- seq(1:nrow(pu))

    # features
    features$internal_id <- seq(1:nrow(features))

    # threats
    threats$internal_id <- seq(1:nrow(threats))

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


    ## Create ConservationProblem object

    pproto(NULL, ConservationProblem,
           data = list(
             pu = pu, features = features, dist_features = dist_features, dist_threats = dist_threats, threats = threats,
             sensitivity = sensitivity, boundary = boundary
           )
    )
  }
)
