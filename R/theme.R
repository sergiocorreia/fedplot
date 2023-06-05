# ggpplot2 fed theme, including advanced tweaks

# https://teunbrand.github.io/ggh4x/articles/PositionGuides.html#minor-ticks
#library(ggh4x)
# Used to wrap words in caption (element_textbox_simple)
#library(ggtext)

#source(here("code", "fedtheme_theme.R"))
#source(here("code", "fedtheme_save.R"))
#source(here("code", "fedtheme_colors.R"))
#source(here("code", "fedtheme_extensions.R"))


#' Alternative to ggplot::.pt ; using bigpoints of size 72 instead of points of size 72.27.
#'
#' @export
#' @keyword internal
.bigpt <- 72 / 25.4 # Alternative to ggplot::.pt === (72.27 / 25.4)


# -------------------------------------------------------------------------
#' Fed-style [ggplot] theme
#'
#' @param font_family base font family
#' @param font_size base font family and size
#' @param legend_position where to place the legend; default is `c(.9, .1)`
#' @param color_palette color palette to use
#' @param frequency string indicating the date frequency; autodetected by default
#'
#' @examples \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # Scatterplot -> ADD
#' Bar chart -> ADD
#' Time series -> ADD
#' }
#'
#' @export 
# -------------------------------------------------------------------------

theme_fed <- function(font_family = getOption("fedplot.font_family"),
                      font_size = getOption("fedplot.font_size"),
                      legend_position = c(.9, .1),
                      color_palette = "main",
                      frequency = "default") {
  list(
    
    # Set palettes
    ggplot2::discrete_scale("color", "fed_color_scale", fed_color_pal()),
    ggplot2::discrete_scale("fill", "fed_fill_scale", fed_color_pal()),
    ggplot2::discrete_scale("linewidth", "fed_linewidth_scale", fed_linewidth_pal()),

    # Used to add secondary ticks
    ggplot2::guides(x = ggh4x::guide_axis_minor()),

    # Add label of y-axis as title (needs to be before "ylab(NULL)")
    move_y_axis_title(),

    # Disable label of y-axis (will be placed as title)
    ggplot2::ylab(NULL),

    # No need to show label of x-axis
    ggplot2::xlab(NULL),

    # Add top tick marks (top-left and top-right)
    annotate_top_tick(),

    # Top ticks are clipped unless we disable clipping
    ggplot2::coord_cartesian(clip="off"), # , expand = FALSE

    # Add frequency to top-left corner
    annotate_frequency(label = frequency, font_family = font_family, font_size = font_size * 7L / 8L),

    # Use fed theme()
    theme_fed_minimal(font_family = font_family, font_size = font_size, legend_position = legend_position)
  )
}

