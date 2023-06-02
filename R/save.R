#' Export figure
#' @export
save_plot <- function(
    # Standard options
  filename, # Excluding extension
  extension = c('pdf', 'eps', 'png', 'all'),
  path = NULL,
  size = c('narrow', 'wide', 'box_narrow', 'box_wide', 'slides', 'custom'),
  # Advanced options
  scale = 1,
  height = NULL,
  width = NULL,
  dpi = 300,
  plot = last_plot()
) {

  extension <- match.arg(arg=extension)
  size <- match.arg(size)
  if (extension == "all") extension <- c('pdf', 'eps', 'png')

  # Define height and width based on plot size
  if (is.null(width)) {
    width <- switch(size,
                    narrow = 192,
                    wide = 420,
                    box_narrow = 180,
                    box_wide = 396,
                    slides = 192)
  }
  if (is.null(height)) {
    height <- switch(size,
                    slides = 161,
                    99)
  }

  if (!is.null(path)) {
    # Path of the directory where plot is saved; defaults to working directory
    filename <- file.path(path, filename)
  }

  # Hack: enforce size of plot panel
  resized_plot <- plot +
    ggh4x::force_panelsizes(rows = unit(height, "bigpts"), cols = unit(width, "bigpts"))
  #print(plot + ggh4x::force_panelsizes(rows = 2 * unit(height, "bigpts"), cols = 2 * unit(width, "bigpts")))
  #print(resized_plot)

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

    print(glue::glue("saved '{full_fn}' ({height}x{width}; size={size}; dpi={internal_dpi})"))
  }

}

