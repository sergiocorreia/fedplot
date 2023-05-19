#' Export figure
#' @export
save_plot <- function(
    # Standard options
  filename, # Excluding extension
  extension = c('pdf', 'eps', 'png', 'all'),
  path = NULL,
  wide = FALSE,
  # Advanced options
  scale = 1,
  height = 99,
  width = 174,
  dpi = 300,
  plot = last_plot()
) {

  extension <- match.arg(arg=extension)
  if (extension == "all") extension <- c('pdf', 'eps', 'png')

  # Override width if wide is TRUE
  if (wide) width <- 402

  if (!is.null(path)) {
    # Path of the directory where plot is saved; defaults to working directory
    filename <- file.path(path, filename)
  }

  # Parameters
  height_points <- 99
  width_points <- ifelse(wide, 402, 174)

  # Hack: enforce size of plot panel
  resized_plot <- plot +
    ggh4x::force_panelsizes(rows = unit(height, "bigpts"), cols = unit(width, "bigpts"))

  # Hack: compute plot width to save exactly that (else the plot has too much empty space around it)
  gt <- ggplot2::ggplotGrob(resized_plot)
  total_height <- sum(as.numeric(grid::convertUnit(gt$heights, "in"))) # * 60
  total_width <- sum(as.numeric(grid::convertUnit(gt$widths, "in"))) # * 40

  for (ext in extension) {
    full_fn <- glue::glue("{filename}.{ext}")

    # Hack to show PNGs with enough quality
    internal_dpi <- if (ext == "png") 2 * dpi else dpi

    # Select device
    device = NULL
    # Need to use Cairo as device; otherwise PDF won't embed fonts
    # https://www.andrewheiss.com/blog/2017/09/27/working-with-r-cairo-graphics-custom-fonts-and-ggplot/
    if (ext == "pdf") device = cairo_pdf
    if (ext == "eps") device = cairo_ps

    # Save PDF figure
    ggplot2::ggsave(
      filename = full_fn,
      scale = scale,
      plot = resized_plot,
      unit = 'in',
      height = total_height,
      width = total_width,
      bg = 'white',
      device = device,
      #family = "ITCFranklinGothic LT BookCn",
      # ITC Franklin Gothic LT Book Condensed
      #family = "ITC Franklin Gothic LT Book Condensed",
      #family = "Mistral",
      dpi = internal_dpi)

    print(glue::glue("saved '{full_fn}' ({height_points}x{width_points}; wide={wide}; dpi={internal_dpi})"))
  }

}

