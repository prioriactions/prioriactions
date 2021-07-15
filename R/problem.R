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
#' @param pu Object of class \code{\link{data.frame}} that specifies the planning units
#' to use in the reserve design exercise and their corresponding cost. It may be
#' desirable to exclude some planning units from the analysis, for example those outside
#' the study area. To exclude planning units you have two options:
#' (i) pre-process the \code{data.frame} before using it (i.e., \emph{manually} exclude planning units);
#' or (ii) set the \emph{status} of planning units to 3 not to include them. Each row corresponds
#' to a different planning unit, and each column corresponds to different information about the planning units.
#' The description of the columns is listed below.
#' \describe{
#'    \item{\code{"id"}}{\code{integer} unique identifier for each planning unit. This column is \strong{mandatory}.}
#'    \item{\code{"cost"}}{\code{numeric} cost of including each planning unit in the reserve system. This column is \strong{mandatory}.}
#'    \item{\code{"status"}}{\code{integer} indicating if each planning unit should not be locked in the solution (0) or if it should be \emph{locked-in} (2) or \emph{locked-out} (3) of the solution. This column is \strong{optional}.}
#'    }
#'
#' @param features Object of class \code{\link{data.frame}} that specifies the conservation
#' features to consider in the reserve design exercise, such as their name and representation
#' requirements (i.e., \emph{targets}). Each row corresponds to a different feature and each column
#' corresponds to different information about the features. The description of the columns is listed below.
#' \describe{
#'    \item{\code{"id"}}{\code{integer} unique identifier for each conservation feature. This column is \strong{mandatory}.}
#'    \item{\code{"target"}}{\code{numeric} target amount for each conservation feature. This column is \strong{mandatory}.}
#'    \item{\code{"name"}}{\code{character} name for each conservation feature. This column is \strong{optional}.}
#'    }
#'
#' @param dist_features Object of class \code{\link{data.frame}} that specifies the spatial distribution of conservation features across planning units.
#' \strong{It is recommended not to include cases where a feature does not occur in a planning unit (avoid rows with
#' \code{"amount"} equal to 0). Instead, it is recommended to omit these cases completely from the \code{\link{data.frame}}, for efficiency reasons.}
#' The description of the columns is listed below.
#' \describe{
#'    \item{\code{"pu"}}{\code{integer} \emph{id} of a planning unit where the conservation feature listed on the same row occurs. This column is \strong{mandatory}.}
#'    \item{\code{"species"}}{\code{integer} \emph{id} of each conservation feature. This column is \strong{mandatory}.}
#'    \item{\code{"amount"}}{\code{integer} indicating if the conservation feature occurring in the planning unit listed on the same row (1), or if it is not occurring (0). This column is \strong{mandatory}.}
#'    }
#'
#' @param threats Object of class \code{\link{data.frame}} that specifies the threats to consider in
#' the reserve design exercise, such as their name and penalty of connectivity (i.e., \emph{blm_actions}). Each row corresponds to a different feature and each column
#' corresponds to different information about the threats The description of the columns is listed below.
#' \describe{
#'    \item{\code{"id"}}{\code{integer} unique identifier for each threat. This column is \strong{mandatory}.}
#'    \item{\code{"name"}}{\code{character} name for each threat. This column is \strong{optional}.}
#'    \item{\code{"blm_actions"}}{\code{numeric} penalty of connectivity between threats. This column is \strong{optional}.}
#'    }
#'
#' @param dist_threats Object of class \code{\link{data.frame}} that specifies the spatial distribution of threats across planning units
#' and the corresponding cost of applying an action against these threats. \strong{It is recommended not to include cases where a threat does not occur
#' in a planning unit (avoid rows with \code{"amount"} equal to 0). Instead, it is recommended to omit these cases completely from the \code{\link{data.frame}}, for efficiency reasons.}
#' The description of the columns is listed below.
#' \describe{
#'    \item{\code{"pu"}}{\code{integer} \emph{id} of a planning unit where the threat listed on the same row occurs. This column is \strong{mandatory}.}
#'    \item{\code{"threats"}}{\code{integer} \emph{id} of each threat. This column is \strong{mandatory}.}
#'    \item{\code{"amount"}}{\code{integer} indicating if the threat occurring in the planning unit listed on the same row (1), or if it is not occurring (0). This column is \strong{mandatory}.}
#'    \item{\code{"cost"}}{\code{numeric} cost of applying an action to eliminate the threat in the planning unit listed in the same row. This column is \strong{mandatory}.}
#'    \item{\code{"status"}}{\code{integer} indicating if each action should not be locked in the solution (0) or if it should be \emph{locked-in} (2) or \emph{locked-out} (3) of the solution. This column is \strong{optional}.}
#'    }
#'
#' @param sensitivity Object of class \code{\link{data.frame}} that specifies the species-threats sensitivity,
#' i.e., which threats affect the persistence of endangered conservation features (e.g., species).
#' \strong{It is recommended not to include cases where there is no species-threats sensitivity
#' (avoid rows with \code{"amount"} equal to 0). Instead, it is recommended to omit these cases completely
#' from the \code{\link{data.frame}}, for efficiency reasons.}
#' The description of the columns is listed below.
#'    \describe{
#'    \item{\code{"species"}}{\code{integer} \emph{id} of each conservation feature. This column is \strong{mandatory}.}
#'    \item{\code{"threats"}}{\code{integer} \emph{id} of each threat. This column is \strong{mandatory}.}
#'    \item{\code{"amount"}}{\code{integer} indicating if the threat againsting the conservation feature listed on the same row (1), or if it is not againsting (0). This column is \strong{mandatory}.}
#'    }
#'
#' @param boundary Object of class \code{\link{NULL}} (default) or an object of class \code{\link{data.frame}}.
#' \code{NULL} indicates that planning units' boundaries (i.e., \emph{boundary data}) are not required for the multi-action conservation planning problem.
#' \code{data.frame} specifies the boundaries of the planning units, i.e., the spatial relationship between two units, such as the "length" of the shared boundary between them.
#' The description of the columns is listed below.
#'   \describe{
#'   \item{\code{"id1"}}{\code{integer} \emph{id} of each planning unit. This column is \strong{mandatory}.}
#'   \item{\code{"id2"}}{\code{integer} \emph{id} of each planning unit. This column is \strong{mandatory}.}
#'   \item{\code{"boundary"}}{\code{numeric} "length" of shared boundary between the planning units identified on the same row. This column is \strong{mandatory}.}
#'   }
#'
#' @name problem
#'
#' @return An object of class \code{\link{ConservationProblem-class}}.
#'
#' @details If you are familiar with using conservation planning software called
#' \emph{Marxan}, you should know that most of the arguments in this function
#' follow the conventions used by the Marxan input files. This means that,
#' \code{pu} corresponds to the data in the \strong{Planning Unit File} (typically called \code{"pu.dat"});
#' \code{features} corresponds to the data in the \strong{Conservation Feature File} (typically
#' called \code{"spec.dat"}); \code{dist_features} corresponds to the data in the \strong{Planning Unit versus
#' Conservation Feature File} (typically called \code{"dist_features.dat"}); and \code{boundary} corresponds
#' to the data in the \strong{Boundary Length File} (typically called \code{"bound.dat"}).
#'
#' @seealso For more information on the correct format for \emph{Marxan} input data, see the
#' \href{https://marxansolutions.org}{official \emph{Marxan} website} and Ball \emph{et al.} (2009).
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
#' \item Ball I, Possingham H, Watts, M. \emph{Marxan and relatives: software for spatial
#' conservation prioritization}. Spatial conservation prioritisation: quantitative
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
      assertthat::has_name(dist_features, "species"),
      assertthat::has_name(dist_features, "amount"),
      nrow(dist_features) > 0,
      is.numeric(dist_features$pu),
      is.numeric(dist_features$species),
      is.numeric(dist_features$amount),
      assertthat::noNA(dist_features$pu),
      assertthat::noNA(dist_features$species),
      assertthat::noNA(dist_features$amount),
      all(dist_features$amount >= 0),
      all(dist_features$pu %in% pu$id),
      all(dist_features$species %in% features$id)
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
      assertthat::has_name(dist_threats, "threats"),
      assertthat::has_name(dist_threats, "cost"),
      assertthat::has_name(dist_threats, "amount"),
      nrow(dist_threats) > 0,
      is.numeric(dist_threats$pu),
      is.numeric(dist_threats$threats),
      is.numeric(dist_threats$cost),
      is.numeric(dist_threats$amount),
      assertthat::noNA(dist_threats$pu),
      assertthat::noNA(dist_threats$threats),
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
      assertthat::has_name(sensitivity, "species"),
      assertthat::has_name(sensitivity, "threats"),
      nrow(sensitivity) > 0,
      is.numeric(sensitivity$species),
      is.numeric(sensitivity$threats),
      assertthat::noNA(sensitivity$species),
      assertthat::noNA(sensitivity$threats),
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
      max_intensities <- dist_threats %>% dplyr::group_by(threats) %>% dplyr::summarise(value = max(amount))

      sensitivity$b <- 1
      for(i in 1:nrow(max_intensities)){
        if(any(sensitivity$threats == max_intensities$threats[i][[1]])){
          sensitivity[sensitivity$threats == max_intensities$threats[i][[1]], ]$b <- max_intensities$value[i][[1]]
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
      stop("Every value of a must be less than every value of b", call. = FALSE)
    }

    if(isFALSE(all(sensitivity$d > sensitivity$c))){
      stop("Every value of c parameter must be less than every value of d parameter", call. = FALSE)
    }


    ## Verification subsets
    verify_that(all(as.matrix(pu[, "cost", drop = FALSE]) >= 0,
                    na.rm = TRUE
    ),
    msg = "argument to pu has negative cost data"
    )

    dif_pu <- setdiff(unique(pu$id), unique(dist_features$pu))
    if (length(dif_pu) != 0L) {
      warning(paste0("The following pu's do not contain species: ", paste(dif_pu, collapse = " ")), call. = FALSE)
    }

    dif_features <- setdiff(unique(features$id), unique(dist_features$species))
    if (length(dif_features) != 0L) {

      warning(paste0("The following features are not represented (it'll not be considered in the model): ", paste(dif_features, collapse = " ")), call. = FALSE)

      # eliminate species not represented
      features <- features[!features$id %in% dif_features, ]
      sensitivity <- sensitivity[!sensitivity$species %in% dif_features, ]
    }

    dif_species_threatened <- setdiff(unique(features$id), unique(sensitivity$species))
    if (length(dif_species_threatened) != 0L) {
      warning(paste0("The following features are not threatened: ", paste(dif_species_threatened, collapse = " ")), call. = FALSE)

      # eliminate species not threatened

    }

    dif_threats <- setdiff(unique(threats$id), unique(dist_threats$threats))
    if (length(dif_threats) != 0L) {
      warning(paste0("The following threats are not represented (it'll not be considered in the model): ", paste(dif_threats, collapse = " ")), call. = FALSE)

      # eliminate species not represented
      threats <- threats[!threats$id %in% dif_threats, ]
      sensitivity <- sensitivity[!sensitivity$threats %in% dif_threats, ]
    }

    dif_threats_dangerous <- setdiff(unique(threats$id), unique(sensitivity$threats))
    if (length(dif_threats_dangerous) != 0L) {
      warning(paste0("The following threats are not dangerous to any features (it'll not be considered in the model): ", paste(dif_threats_dangerous, collapse = " ")), call. = FALSE)

      # eliminate threats not represented
      threats <- threats[!threats$id %in% dif_threats_dangerous, ]
      dist_threats <- dist_threats[!dist_threats$threats %in% dif_threats_dangerous, ]
    }

    message("Correctly loaded inputs")


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

    internal_species <- dplyr::inner_join(dist_features, features, by = c("species" = "id"))$internal_id
    dist_features$internal_species <- internal_species

    # dist_threats
    internal_id <- dplyr::inner_join(dist_threats, pu, by = c("pu" = "id"))$internal_id
    dist_threats$internal_pu <- internal_id

    internal_threats <- dplyr::inner_join(dist_threats, threats, by = c("threats" = "id"))$internal_id
    dist_threats$internal_threats <- internal_threats

    # sensitivity
    internal_species <- dplyr::inner_join(sensitivity, features, by = c("species" = "id"))$internal_id
    sensitivity$internal_species <- internal_species

    internal_threats <- dplyr::inner_join(sensitivity, threats, by = c("threats" = "id"))$internal_id
    sensitivity$internal_threats <- internal_threats


    ## Create ConservationProblem object

    pproto(NULL, ConservationProblem,
           data = list(
             pu = pu, features = features, dist_features = dist_features, dist_threats = dist_threats, threats = threats,
             sensitivity = sensitivity, boundary = boundary
           )
    )
  }
)
