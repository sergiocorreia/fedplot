



# ggplot2 scales ----------------------------------------------------------

#' NOTE: UNUSED. Color scale constructor for fire colors
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


