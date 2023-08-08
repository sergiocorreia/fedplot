#' Export figure
#' @param filename filename to be saved, excluding the path and extension.
#' @param extension image extension; also defines the type of image. Valid values are c('pdf', 'eps', 'png', 'all'); note that 'all' will create all image types.
#' @param path path where the file(s) will be saved
#' @param scale rescaling done by [ggplot2::ggsave]; experimental.
#' @param dpi dots per inch of the output file(s). Typical values are 300 for print (the default) and rarely 72 for web image output. Note that for png images `save_plot()` multiples the dpi by 2, to compensate for the lower quality compared to vector formats (pdf, eps). Thus, the default dpi for png images is 600.
#' @param plot ggplot2 to save. By default will save the plot last printed to screen.
#' @param ... Other arguments passed on to the graphics device function.
#' @export
save_plot <- function(
      # Standard options
    filename, # Excluding extension
    extension = c('pdf', 'eps', 'png', 'all'),
    path = NULL,
    # Advanced options
    scale = 1,
    dpi = 300,
    plot = ggplot2::last_plot(),
    ...) {

  extension <- match.arg(arg=extension)
  if (extension == "all") extension <- c('pdf', 'eps', 'png')

  if (!is.null(path)) {
    # Path of the directory where plot is saved; defaults to working directory
    filename <- file.path(path, filename)
  }

  # Hack: compute plot size to save exactly that (else the plot has too much empty space around it)
  gt <- ggplot2::ggplotGrob(plot)
  total_height <- sum(as.numeric(grid::convertUnit(gt$heights, "in")))
  total_width <- sum(as.numeric(grid::convertUnit(gt$widths, "in")))
  #total_height <- grid::convertUnit(grid::unit(height, "bigpts"), "in")
  #total_width <- grid::convertUnit(grid::unit(width, "bigpts"), "in")

  for (ext in extension) {
    full_fn <- glue::glue("{filename}.{ext}")

    # Hack to show PNGs with enough quality
    internal_dpi <- if (ext == "png") 2 * dpi else dpi

    # Select device
    device <- NULL
    # Need to use Cairo as device; otherwise PDF won't embed fonts
    # https://www.andrewheiss.com/blog/2017/09/27/working-with-r-cairo-graphics-custom-fonts-and-ggplot/
    if (ext == "pdf") device <- grDevices::cairo_pdf
    if (ext == "eps") device <- grDevices::cairo_ps
    #if (ext == "pdf") device <- Cairo::CairoPDF
    #if (ext == "eps") device <- Cairo::CairoPS

    #https://www.andrewheiss.com/blog/2017/09/27/working-with-r-cairo-graphics-custom-fonts-and-ggplot/
    if (ext == "png") device <- ragg::agg_png

    # Save PDF figure
    ggplot2::ggsave(
      filename = full_fn,
      scale = scale,
      plot = plot,
      unit = 'in',
      height = total_height,
      width = total_width,
      bg = 'white',
      device = device,
      dpi = internal_dpi,
      ...)

    print(glue::glue("saved '{full_fn}' ({total_height}x{total_width}; dpi={internal_dpi})"))
  }

}
