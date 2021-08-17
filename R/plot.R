#' @title Plot
#'
#' @description Display information about an object.
#'
#' @param x Any object.
#' @param ... Not used.
#'
#' @name plot
#'
#' @return None.
#'
#' @seealso [graphics::plot()].
#'
#' @aliases plot

NULL

#' @rdname plot
#' @method plot Portfolio
#' @importFrom ggplot2 ggplot aes geom_text geom_point labs theme_bw scale_x_continuous scale_y_continuous expansion
#' @importFrom graphics plot
#' @export
plot.Portfolio <- function(x, type = "connectivity_vs_cost", text = TRUE,...){
  # assert argument is valid
  assertthat::assert_that(inherits(x, "Portfolio"))

  if(type == "connectivity_vs_cost"){

    plot_data <-   tibble::tibble(
      plot_x = getPlanningUnitsConnectivity(x),
      plot_y = getTotalCost(x)
    )

    args <- list(xlab = "Connectivity", ylab = "Cost")
  }
  else if(type == "connectivity_vs_benefit"){

    plot_data <-   tibble::tibble(
      plot_x = getPlanningUnitsConnectivity(x),
      plot_y = getTotalBenefit(x)
    )

    args <- list(xlab = "Connectivity", ylab = "Benefit")
  }
  else if(type == "cost_vs_benefit"){

    plot_data <-   tibble::tibble(
      plot_x = getTotalCost(x),
      plot_y = getTotalBenefit(x)
    )

    args <- list(xlab = "Cost", ylab = "Benefit")
  }


  #general plot
  g <- ggplot(plot_data, aes(x = .data$plot_x, y = .data$plot_y)) +
    geom_point(size = 2, color = "#041c54") +
    theme_bw() +
    scale_x_continuous(expand = expansion(c(0.1, 0.1))) +
    scale_y_continuous(expand = expansion(c(0.1, 0.1))) +
    labs(x = args$xlab,
         y = args$ylab,
         caption = "Source: prioriactions")

  if(text){
    g <- g + geom_text(label=x$getNames(), vjust = -0.6, check_overlap = TRUE)
  }


  g
}
