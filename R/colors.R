# This code handles the color palettes used in FIRE/BSVR/FSR plots

# References --------------------------------------------------------------
# - Colors based on color_scheme.R
# - https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
# - For color naming ideas see: https://www.color-name.com/hex/948B3D



# Linewidth palettes ------------------------------------------------------

#' Linewidths used in the FSR.
#'
#' @export
# @keywords internal
# 0.375, 0.70, 1.125, 1.5, then just 0.375
fsr_linewidths <- 0.375 * getOption("fedplot.linewidth_adj") * c(seq(1, 4), rep(1, 10))

# -------------------------------------------------------------------------
#' Closure to generate a linewidth palette picker, given an input palette
#'
#' This internal function will take as an input a palette (a vector of numbers
#' representing linewidths) and return a function `f(n)` that produces a
#' smaller vector containing the first `n` linewidths of the
#' input palette.
#'
#' @param palette Vector of linewidths that will be used as a palette; must be larger than the maximum number of lines plotted.
#' @return function
#' @export
# -------------------------------------------------------------------------

fed_linewidth_pal <- function(palette=fsr_linewidths) {
    function(n) {
      if (length(palette) < n) {
        stop(glue::glue("Linewidth palette only has {length(palette)} values but {n} were requested."))
      }
      palette[1:n]
    }
}



# Color lists -------------------------------------------------------------

#' List of primary colors used in the BSVR
#'
#' @export
fsr_primary_colors = c(
  `black`      = "#000000",
  `blue`       = "#236192",
  `darkred`    = "#7F3035",
  `green`      = "#006747")

#' List of secondary colors used in the BSVR
#'
#' @export
fsr_secondary_colors = c(
  `altblue`    = "#005EB8",
  `teal`       = "#008C95",
  `orange`     = "#B86125",
  `tan`        = "#B9975B")

old_fsr_colors = c(
  `lightgrey`  = "#D0D3D4",
  `lightblue`  = "#9BB8D3",
  `mediumblue` = "#236192",
  `darkblue`   = "#104E8B",
  `red`        = "#7F3035",
  `darkred`    = "#803134",
  `rust`       = "#C26E60",
  `beige`      = "#DDCBA4",
  `green`      = "#46852E",
  `lightblack` = "#333F48")

#' List of colors used in BSVR reports
#'
#' @export
bsvr_colors <- c(
  `gray`       = "#535154",
  `red`        = "#CC2529",
  `blue`       = "#396AB1",
  `orange`     = "#DA7C30",
  `green`      = "#3E9651",
  `purple`     = "#6B4C9A",
  `darkred`    = "#922428",
  `beige`      = "#948B3D",
  `black`      = "#000000")

fed_palettes <- list(
  `fsr_primary`   = fsr_primary_colors,
  `fsr_secondary` = fsr_secondary_colors,
  `bsvr`          = bsvr_colors,
  `old_fsr`       = old_fsr_colors
)


# -------------------------------------------------------------------------
#' Closure to generate a color palette picker, given an input palette
#'
#' This internal function will take as an input either a) the name of a palete (
#' one of `fsr_primary_colors`, `fsr_secondary_colors`, or `bsvr_color`)
#' or b) the palette itself as a vector of hex codes representing the colors.
#' In turn, it will return a function `f(n)` that produces a vector
#' containing the first `n` colors of the input palette.
#' Optionally, it can use the input palette to generate a color ramp, so
#' it can produce as many colors as required.
#'
#' @param palette Character name of palette in fed_palettes
#' @param use_ramp Boolean indicating whether to use a color ramp to interpolate colors (via [grDevices::colorRampPalette])
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to `colorRampPalette()`
#' @return function
#' @export
# -------------------------------------------------------------------------
fed_color_pal <- function(palette = "fsr_primary", use_ramp = FALSE, reverse = FALSE, ...) {

  # If palette is a string, select the corresponding element.
  # Otherwise, assume it already exists (e.g. from another package)
  if (is.character(palette) & length(palette) == 1) {
    pal <- fed_palettes[[palette]]
  }
  else {
    pal <- palette
  }

  if (reverse) pal <- rev(pal)
  
  if (use_ramp) {
    # Supports many colors (b/c int interpolates) but doesn't use the exact colors we need
    grDevices::colorRampPalette(pal, ...)
  }
  else {
    # Just use the exact colors we need but up to the palette size (afterwards, we get an error)
    function(n) {
      if (length(pal) < n) {
        stop(glue::glue("Color palette only has {length(pal)} values but {n} were requested."))
      }
      unname(pal[1:n])
    }
  }

}
