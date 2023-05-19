# This code handles the color palettes used in FIRE/BSVR/FSR plots

# References --------------------------------------------------------------
# - Colors based on color_scheme.R
# - https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
# - For color naming ideas see: https://www.color-name.com/hex/948B3D


# Dependencies ------------------------------------------------------------
#library(ggplot2)


# Color themes ------------------------------------------------------------

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

fsr_colors = c(
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


#' Function to extract colors as hex codes
#'
#' @param ... Color names
#'
fire_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (bsvr_colors)

  bsvr_colors[cols]
}


fire_palettes <- list(
  `bsvr`  = bsvr_colors,
  `fsr`   = fsr_colors,
  `main`  = fire_cols("black", "blue", "darkred", "green"),
  `main3` = fire_cols("black", "blue", "darkred")
)

#' Return function to interpolate color palette
#'
#' @param palette Character name of palette in fire_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
fire_pal <- function(palette = "fire", reverse = FALSE, ...) {

  # If palette is a string, select the corresponding element.
  # Otherwise, assume it already exists (e.g. from another package)
  if (is.character(palette) & length(palette) == 1) {
    pal <- fire_palettes[[palette]]
  }
  else {
    pal <- palette
  }

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}



# ggplot2 scales ----------------------------------------------------------

#' Color scale constructor for fire colors
#'
#' @param palette Character name of palette in fire_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_fire <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fire_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("fire_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colors = pal(256), ...)
  }
}

#' Fill scale constructor for fire colors
#'
#' @param palette Character name of palette in fire_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_fire <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fire_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("fire_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colors = pal(256), ...)
  }
}


