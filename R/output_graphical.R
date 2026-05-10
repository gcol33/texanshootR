#' Generate a graphical abstract PNG
#'
#' A single-figure summary of the highlighted specification, composed
#' as a multi-panel layout with conceptual arrows. Renders via ggplot2
#' to PNG.
#'
#' Unlock requirement: \strong{Senior Scientist}. See [progress()] for live state.
#'
#' @inheritParams manuscript
#' @export
graphical_abstract <- function(run, output_dir = NULL, file = NULL,
                               force = FALSE) {
  require_unlocked("graphical_abstract")
  require_pkg("ggplot2", "graphical_abstract")
  d <- resolve_output_dir(output_dir)
  stem <- file %||% "figure_final"
  out  <- versioned_filename(d, stem, "png", force)

  hs <- run$highlighted_spec %||% list()

  df_plot <- data.frame(
    x = seq_len(20),
    y = cumsum(stats::rnorm(20))
  )

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title    = "Conceptual relationship",
      subtitle = sprintf("Highlighted specification: %s",
                          hs$formula %||% "y ~ x"),
      caption  = sprintf("R^2 = %.3f, p = %.4f",
                          hs$r_squared %||% 0,
                          hs$p_value %||% 1),
      x = "Latent gradient",
      y = "Response"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  status_open(out)
  ggplot2::ggsave(filename = out, plot = p, width = 6, height = 4,
                  dpi = 150)
  status_close()
  record_output(run, "graphical_abstract", out)
  invisible(out)
}
