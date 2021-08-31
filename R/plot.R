#' @title Plot portfolio
#'
#' @description Plot information about an group of solutions.
#'
#' @param x `Portfolio-class`.
#' @param type [character]. Type of plot. The following types
#' can be used: *connectivity_vs_cost*, *connectivity_vs_benefit* and
#' *cost_vs_benefit*.
#' @param ... Not used.
#'
#' @name plot
#'
#' @return None.
#'
#' @seealso [graphics::plot()].
#'
#' @aliases plot
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(14)
#'
#' ## Load data
#' inputs <- list(sim_pu_data, sim_features_data, sim_dist_features_data,
#' sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
#' sim_boundary_data)
#'
#' ## Create model and solve
#' port <- evalTarget(data = inputs, values = c(0.5, 0.6), gap_limit = 0.01, output_file = FALSE)
#'
#' plot(port)
#'

NULL

#' @rdname plot
#' @method plot Portfolio
#' @importFrom ggplot2 ggplot aes geom_text geom_point labs theme_bw scale_x_continuous scale_y_continuous expansion scale_colour_viridis_d
#' @importFrom graphics plot
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
plot.Portfolio <- function(x, type = "connectivity_vs_cost", ...){
  # assert argument is valid
  assertthat::assert_that(inherits(x, "Portfolio"))

  lab_names <- x$getNames()

  if(type == "connectivity_vs_cost"){

    plot_data <-   tibble::tibble(
      plot_x = getPlanningUnitsConnectivity(x),
      plot_y = getTotalCost(x),
      Scenarios = readr::parse_number(lab_names)
    )

    args <- list(xlab = "Connectivity", ylab = "Cost")
  }
  else if(type == "connectivity_vs_benefit"){

    plot_data <-   tibble::tibble(
      plot_x = getPlanningUnitsConnectivity(x),
      plot_y = getTotalBenefit(x),
      Scenarios = readr::parse_number(lab_names)
    )

    args <- list(xlab = "Connectivity", ylab = "Benefit")
  }
  else if(type == "cost_vs_benefit"){

    plot_data <-   tibble::tibble(
      plot_x = getTotalCost(x),
      plot_y = getTotalBenefit(x),
      Scenarios = readr::parse_number(lab_names)
    )

    args <- list(xlab = "Cost", ylab = "Benefit")
  }


  #general plot
  plot_data$Scenarios <- with(plot_data, reorder(Scenarios,
                                                 c(readr::parse_number(lab_names))))


  g <- ggplot(plot_data, aes(x = .data$plot_x, y = .data$plot_y, color = .data$Scenarios)) +
    geom_point(size = 2) +
    theme_bw() +
    scale_x_continuous(expand = expansion(c(0.1, 0.1))) +
    scale_y_continuous(expand = expansion(c(0.1, 0.1))) +
    scale_colour_viridis_d(name = strsplit(lab_names,
                                           readr::parse_number(lab_names))[[1]])+
    labs(x = args$xlab,
         y = args$ylab,
         caption = "Source: prioriactions")

  #if(text){
  #  g <- g + geom_text(label=x$getNames(), vjust = -0.6, check_overlap = TRUE)
  #}


  g
}
