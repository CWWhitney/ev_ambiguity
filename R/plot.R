#' Plot Expected Value of Eliminating Causal Ambiguity (EVECA) Results
#'
#' Create a simple visualization of EVECA analysis results from `compute_evca()`.
#'
#' @param evca_result Output from `compute_evca()` function.
#' @param title Plot title (default: "EVECA Analysis").
#' @param xlab X-axis label (default: "Component").
#' @param ylab Y-axis label (default: "Expected Utility").
#' @param colors Vector of two colors for BMA Optimal and Perfect Information bars
#'   (default: c("#4E79A7", "#F28E2B")).
#'
#' @return A ggplot object that can be further customized or printed.
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' # Run example analysis
#' result <- example_evca()
#'
#' # Create plot
#' p <- plot_evca(result$evca_result)
#' print(p)
#'
#' # Customize plot
#' p <- plot_evca(result$evca_result,
#'   title = "EVECA Analysis for Treatment Selection",
#'   colors = c("#2E8B57", "#FF6347")
#' )
#' print(p)
#' }
plot_evca <- function(evca_result,
                      title = "EVECA Analysis",
                      xlab = "Component",
                      ylab = "Expected Utility",
                      colors = c("#4E79A7", "#F28E2B")) {
  # Validate input
  if (!is.list(evca_result) || !"evca" %in% names(evca_result)) {
    stop("evca_result must be output from compute_evca() function")
  }

  # Create plot data
  plot_data <- data.frame(
    Component = c("BMA Optimal", "Perfect Information"),
    Value = c(
      evca_result$optimal_utility_bma,
      evca_result$perfect_info_expected_utility
    )
  )

  # Create plot
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$Component, y = .data$Value, fill = .data$Component)
  ) +
    ggplot2::geom_bar(stat = "identity", width = 0.6) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "gray50"
    ) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      subtitle = sprintf("EVECA = %.4f", evca_result$evca)
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(face = "bold")
    )

  return(p)
}
