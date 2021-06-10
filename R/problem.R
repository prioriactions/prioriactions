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
#' @param rij Object of class \code{\link{data.frame}} that specifies the spatial distribution of conservation features across planning units.
#' \strong{It is recommended not to include cases where a feature does not occur in a planning unit (avoid rows with
#' \code{"amount"} equal to 0). Instead, it is recommended to omit these cases completely from the \code{\link{data.frame}}, for efficiency reasons.}
#' The description of the columns is listed below.
#' \describe{
#'    \item{\code{"pu"}}{\code{integer} \emph{id} of a planning unit where the conservation feature listed on the same row occurs. This column is \strong{mandatory}.}
#'    \item{\code{"species"}}{\code{integer} \emph{id} of each conservation feature. This column is \strong{mandatory}.}
#'    \item{\code{"amount"}}{\code{integer} indicating if the conservation feature occurring in the planning unit listed on the same row (1), or if it is not occurring (0). This column is \strong{mandatory}.}
#'    }
#'
#' @param threats Object of class \code{\link{data.frame}} that specifies the spatial distribution of threats across planning units
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
#' @param bound Object of class \code{\link{NULL}} (default) or an object of class \code{\link{data.frame}}.
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
#' called \code{"spec.dat"}); \code{rij} corresponds to the data in the \strong{Planning Unit versus
#' Conservation Feature File} (typically called \code{"puvspr.dat"}); and \code{bound} corresponds
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
#' ## Load in planning unit data
#' pu_path <- system.file("extdata/input/pu.dat", package = "prioriactions")
#' pu_data <- data.table::fread(pu_path, data.table = FALSE)
#' head(pu_data)
#'
#' ## Load in feature data
#' spec_path <- system.file("extdata/input/spec.dat", package = "prioriactions")
#' spec_data <- data.table::fread(spec_path, data.table = FALSE)
#' head(spec_data)
#'
#' ## Load in planning unit vs feature data
#' puvspr_path <- system.file("extdata/input/puvspr.dat", package = "prioriactions")
#' puvspr_data <- data.table::fread(puvspr_path, data.table = FALSE)
#' head(puvspr_data)
#'
#' ## Load in the threats data
#' threats_path <- system.file("extdata/input/threats.dat", package = "prioriactions")
#' threats_data <- data.table::fread(threats_path, data.table = FALSE)
#' head(threats_data)
#'
#' ## Load in the sensitivity data
#' sensitivity_path <- system.file("extdata/input/sensibility.dat", package = "prioriactions")
#' sensitivity_data <- data.table::fread(sensitivity_path, data.table = FALSE)
#' head(sensitivity_data)
#'
#' ## Load in the boundary data
#' bound_path <- system.file("extdata/input/bound.dat", package = "prioriactions")
#' bound_data <- data.table::fread(bound_path, data.table = FALSE)
#' head(bound_data)
#'
#' ## Create data instance
#' problem_data <- problem(
#'   pu = pu_data, features = spec_data, rij = puvspr_data,
#'   threats = threats_data, sensitivity = sensitivity_data,
#'   bound = bound_data
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
                    signature = methods::signature("pu", "features", "rij", "threats", "sensitivity"),
                    function(pu, features, rij, threats, sensitivity, ...) standardGeneric("problem")
)

#' @name problem
#' @usage \S4method{problem}{data.frame}(pu, features, rij, threats, sensitivity, bound = NULL)
#' @rdname problem
methods::setMethod(
  "problem",
  methods::signature(
    pu = "data.frame", features = "data.frame", rij = "data.frame",
    threats = "data.frame", sensitivity = "data.frame"
  ),
  function(pu, features, rij, threats, sensitivity, bound = NULL, ...) {
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
    # x$internal_id <- seq(1,nrow(x))
    # create locked in data
    # if (assertthat::has_name(x, "status")) {
    #   x$locked_in <- x$status == 2
    #   x$locked_out <- x$status == 3
    # } else {
    #   x$locked_in <- FALSE
    #   x$locked_out <- FALSE
    # }


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


    # if (!do.call(xor, as.list(c("prop", "amount") %in% names(features)))) {
    #  stop(
    #    "argument to spec must have the column \"prop\" or \"amount\" and ",
    #    "not both"
    #  )
    # }

    ## rij
    assertthat::assert_that(
      inherits(rij, "data.frame"),
      assertthat::has_name(rij, "pu"),
      assertthat::has_name(rij, "species"),
      assertthat::has_name(rij, "amount"),
      nrow(rij) > 0,
      is.numeric(rij$pu),
      is.numeric(rij$species),
      is.numeric(rij$amount),
      assertthat::noNA(rij$pu),
      assertthat::noNA(rij$species),
      assertthat::noNA(rij$amount),
      all(rij$pu %in% pu$id),
      all(rij$species %in% features$id),
      #amount between 0-1
      all(rij$amount <= 1)
    )

    ## bound
    assertthat::assert_that(inherits(bound, c("NULL", "data.frame")))
    if (inherits(bound, c("data.frame"))) {
      assertthat::assert_that(
        assertthat::has_name(bound, "id1"),
        assertthat::has_name(bound, "id2"),
        assertthat::has_name(bound, "boundary"),
        is.numeric(bound$id1),
        is.numeric(bound$id2),
        is.numeric(bound$boundary),
        assertthat::noNA(bound$id1),
        assertthat::noNA(bound$id2),
        assertthat::noNA(bound$boundary),
        all(bound$id1 %in% pu$id), all(bound$id2 %in% pu$id)
      )
    }




    # new code (threats) ------------------------------------------------------

    ## threats
    assertthat::assert_that(
      inherits(threats, "data.frame"),
      assertthat::has_name(threats, "pu"),
      assertthat::has_name(threats, "threats"),
      assertthat::has_name(threats, "cost"),
      assertthat::has_name(threats, "amount"),
      nrow(threats) > 0,
      is.numeric(threats$pu),
      is.numeric(threats$threats),
      is.numeric(threats$cost),
      is.numeric(threats$amount),
      assertthat::noNA(threats$pu),
      assertthat::noNA(threats$threats),
      assertthat::noNA(threats$cost),
      all(threats$pu %in% pu$id),
      all(threats$amount <= 1)
    )
    if ("status" %in% names(threats)) {
      assertthat::assert_that(
        is.numeric(threats$status),
        assertthat::noNA(threats$status),
        all(threats$status %in% c(0, 2, 3))
      )
    } else {
      pu$status <- 0
    }
    # create locked in data
    # if (assertthat::has_name(threats, "status")) {
    #   threats$locked_in <- threats$status == 2
    #   threats$locked_out <- threats$status == 3
    # } else {
    #   threats$locked_in <- FALSE
    #   threats$locked_out <- FALSE
    # }

    ## sensitivity
    assertthat::assert_that(
      inherits(sensitivity, "data.frame"),
      assertthat::has_name(sensitivity, "species"),
      assertthat::has_name(sensitivity, "threats"),
      assertthat::has_name(sensitivity, "amount"),
      nrow(sensitivity) > 0,
      is.numeric(sensitivity$species),
      is.numeric(sensitivity$threats),
      is.numeric(sensitivity$amount),
      assertthat::noNA(sensitivity$species),
      assertthat::noNA(sensitivity$threats),
      assertthat::noNA(sensitivity$amount),
      all(sensitivity$species %in% features$id),
      all(sensitivity$threats %in% threats$threats)
    )


    ## Verification subsets (VERIFICARRRRRR)
    verify_that(all(rij$amount >= 0))
    verify_that(all(as.matrix(pu[, "cost", drop = FALSE]) >= 0,
                    na.rm = TRUE
    ),
    msg = "argument to pu has negative cost data"
    )

    # #Verifing valid units costs
    # if (assertthat::has_name(x, "status")) {
    #   inv_pu <- which(x$cost <= 0 & x$status != 3)
    # }
    # else {
    #   inv_pu <- which(x$cost <= 0)
    # }
    # x_edited <- x[-c(inv_pu), ]
    #
    # if (length(inv_pu) != 0L) {
    #   warning(paste0("The following pu's do contain invalid cost data:",
    #                  paste(x$id[inv_pu], collapse = " ")), call. = FALSE)
    #
    #   rij_edited <- rij[-c(which(rij$pu == inv_pu)), ]
    # }
    #
    # #Verifing valid actions costs
    # if (assertthat::has_name(threats, "status")) {
    #   inv_action <- which(threats$cost <= 0 & threats$status != 3)
    # }
    # else {
    #   inv_action <- which(threats$cost <= 0)
    # }
    # threats_edited <- threats[-c(inv_action), ]
    #
    # if (length(inv_action) != 0L) {
    #   warning(paste0("The following pu's do contain invalid cost data:",
    #                  paste(threats$id[inv_pu], collapse = " ")), call. = FALSE)
    #
    #   rij_edited <- rij[-c(which(rij$pu == inv_pu)), ]
    # }


    dif_pu <- setdiff(unique(pu$id), unique(rij$pu))
    if (length(dif_pu) != 0L) {
      warning(paste0("The following pu's do not contain species: ", paste(dif_pu, collapse = " ")), call. = FALSE)
    }

    dif_features <- setdiff(unique(features$id), unique(rij$species))
    if (length(dif_features) != 0L) {
      warning(paste0("The following species are not represented: ", paste(dif_features, collapse = " ")), call. = FALSE)
    }

    dif_threats <- setdiff(unique(features$id), unique(sensitivity$species))
    if (length(dif_threats) != 0L) {
      warning(paste0("The following species are not threatened: ", paste(dif_threats, collapse = " ")), call. = FALSE)
    }

    dif_threats <- setdiff(unique(threats$threats), unique(sensitivity$threats))
    if (length(dif_threats) != 0L) {
      warning(paste0("The following threats are not dangerous to any species: ", paste(dif_threats, collapse = " ")), call. = FALSE)
    }

    message("Correctly loaded inputs")

    # create ConservationProblem object

    # pu
    pu$internal_id <- seq(1:nrow(pu))

    # features
    features$internal_id <- seq(1:nrow(features))

    # bound
    if(!is.null(bound)){
      internal_id1 <- dplyr::inner_join(bound, pu, by = c("id1" = "id"))$internal_id
      internal_id2 <- dplyr::inner_join(bound, pu, by = c("id2" = "id"))$internal_id
      bound$internal_id1 <- internal_id1
      bound$internal_id2 <- internal_id2
    }

    # rij
    internal_id <- dplyr::inner_join(rij, pu, by = c("pu" = "id"))$internal_id
    rij$internal_pu <- internal_id

    internal_species <- dplyr::inner_join(rij, features, by = c("species" = "id"))$internal_id
    rij$internal_species <- internal_species


    # sensitivity
    internal_species <- dplyr::inner_join(sensitivity, features, by = c("species" = "id"))$internal_id
    sensitivity$internal_species <- internal_species

    # threats
    internal_id <- dplyr::inner_join(threats, pu, by = c("pu" = "id"))$internal_id
    threats$internal_pu <- internal_id




    pproto(NULL, ConservationProblem,
           data = list(
             pu = pu, features = features, rij = rij, threats = threats,
             sensitivity = sensitivity, bound = bound
           )
    )
  }
)
