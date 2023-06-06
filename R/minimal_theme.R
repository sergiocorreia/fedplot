# References:
# https://rpubs.com/mclaire19/ggplot2-custom-themes
# https://ggplot2.tidyverse.org/reference/theme.html
# https://ggplot2.tidyverse.org/reference/element.html
# https://community.rstudio.com/t/custom-ggplot2-theme-that-capitalizes-title/39688/2


# -------------------------------------------------------------------------
#' Internal Fed theme
#'
#' The `theme_fed()` object is composed of a theme, plus a number of functions
#' that achieve features that the theme itself cannot achieve.
#' This function corresponds to the theme itself, modifying `ggplot2::theme_classic`.
#'
#' @param font_family base font family
#' @param font_size base font family and size
#' @param legend_position where to place the legend; default is `c(.9, .1)`
#'
#' @export
# -------------------------------------------------------------------------

theme_fed_minimal <- function(font_family = getOption("fedplot.font_family"),
                              font_size = getOption("fedplot.font_size"),
                              legend_position = c(.9, .1)) {

  fed_linewidth = 0.5 * (2.54 / 72 / 0.075)
  # 0.5     = pubtech linewidth in points
  # 72.27   = convert points to inches --> NOTE: Use BigPoints instead (72)
  # 2.54    = convert inches to cm (25.4 converts to mm)
  # 0.075cm = 0.75mm = ggplot unit of linewidth; see https://community.rstudio.com/t/units-of-linewidth/162885
  # Also see https://stackoverflow.com/a/53418570/3977107

  #update_geom_defaults("line", list(linewidth = fed_linewidth*(font_size/8)))
  
  medium_font_size = font_size
  small_font_size = font_size * 7L / 8L

  base_line_size = 0.5 * getOption("fedplot.linewidth_adj") # stroke weight 0.5 points for axis

  # See also:
  # - https://cmap-repos.github.io/cmapplot/reference/dot-lwd.html
  # - warning from help(grid::par):
  #   The line width, a positive number, defaulting to 1. The interpretation is device-specific, and some devices do not implement line widths less than one. (See the help on the device for details of the interpretation.)
  
  ggplot2::theme_classic(
    base_family = font_family,
    base_size = medium_font_size,
    base_line_size = base_line_size,
    base_rect_size = base_line_size
  ) %+replace%
  
  ggplot2::theme(

    # See: https://ggplot2.tidyverse.org/reference/theme.html
      
    # Title; which is actually the y-axis title (indicating the unit)
    plot.title = ggplot2::element_text(
      hjust = 1,
      vjust = 1,
      margin = ggplot2::margin(t = 0, b = grid::unit(3 - 1.2, "bigpts"))), # had to add top margin b/c of ggtext (not anymore?)

    # X axis title (indicating frequency: monthly, quarterly, etc.)
    axis.title.x = ggplot2::element_blank(),

    # Footnote
    #plot.caption = ggplot2::element_text(hjust = 0, margin=margin(t=6)),
    plot.caption = ggtext::element_textbox_simple(hjust = 0, margin = ggplot2::margin(t=6+1, r=0, b=3, l=0), lineheight=1.05),

    # Axis lines
    axis.line = ggplot2::element_line(lineend="square"),

    # Axis labels
    axis.text = ggplot2::element_text(size = medium_font_size),
    axis.text.y.left = ggplot2::element_blank(),
    axis.text.y.right = ggplot2::element_text(hjust = 1, margin = ggplot2::margin(l=6)),
    # Hack: if we don't do 6+6=12 then ggh4x leaves no margin for the years (b/c of the ticks)
    axis.text.x.bottom = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(t=6+6+0.375), debug=F),
    
    # Axis ticks
    axis.ticks = ggplot2::element_line(linewidth = 0.5 * getOption("fedplot.linewidth_adj"), lineend="round"),
    # Hack: we want to set it to zero but can't without also setting minor ticks to zero; so we set them to .06pt and then the minor ticks to 100*.06=6pt
    axis.ticks.length = grid::unit(-6, units='bigpts'),
    axis.ticks.length.x = grid::unit(-.0006, units='bigpts'),
    ggh4x.axis.ticks.length.minor = ggplot2::rel(10000), # ggh4x axis tricks to fit year labels

    # Legend
    legend.title = ggplot2::element_blank(),
    legend.position = legend_position, # c(.9, .1),
    legend.justification = c("right", "bottom"),
    legend.box.background = ggplot2::element_blank(), # fill='transparent'),
    legend.background = ggplot2::element_blank(), # fill='transparent'),
    legend.key.height = grid::unit(5.75, 'bigpts'),
    legend.key.width = grid::unit(5.75, 'bigpts'),
    legend.spacing.x = grid::unit(2, 'bigpts'),
    legend.margin = ggplot2::margin(0,0,0,0),
    legend.box.margin = ggplot2::margin(0,0,0,0),
    legend.text = ggplot2::element_text(size=small_font_size, margin = ggplot2::margin(t=2)),

    # Entire plot

    plot.margin = ggplot2::margin(.3, 0, 0, 0), # margin=1 works well on Windows but not on Fed Linux
    #plot.margin = ggplot2::margin(4, 4, 4, 4),
    #plot.margin = ggplot2::margin(6, 6, 6, 6),
    #plot.margin = ggplot2::margin(0, 0, 0, 0),

    #plot.background = ggplot2::element_rect(color='red', fill=NA),
    panel.background = ggplot2::element_blank(), #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot

    #grid elements
    #panel.grid.major = element_blank(),    #strip major gridlines
    #panel.grid.minor = element_blank(),    #strip minor gridlines
    
    #Strip background
    #This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22
    #strip.background = ggplot2::element_rect(fill="red"),
    #strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}


