# References:
# https://rpubs.com/mclaire19/ggplot2-custom-themes
# https://ggplot2.tidyverse.org/reference/theme.html
# https://ggplot2.tidyverse.org/reference/element.html
# https://community.rstudio.com/t/custom-ggplot2-theme-that-capitalizes-title/39688/2


# Main function -----------------------------------------------------------

theme_fed_minimal <- function(font_family, font_size, legend_position) {

  fed_linewidth = 0.5 * (2.54 / 72 / 0.075)
  # 0.5     = pubtech linewidth in points
  # 72.27   = convert points to inches --> NOTE: Use BigPoints instead (72)
  # 2.54    = convert inches to cm (25.4 converts to mm)
  # 0.075cm = 0.75mm = ggplot unit of linewidth; see https://community.rstudio.com/t/units-of-linewidth/162885
  # Also see https://stackoverflow.com/a/53418570/3977107

  update_geom_defaults("line", list(linewidth = fed_linewidth*(font_size/8)))
  
  medium_font_size = font_size
  small_font_size = font_size - 1

  line_size_adjustment = 25.4 / 72.27 * 96 / 72 # = ggplot2::.stroke / ggplot2::.pt * 25.4 / 72.27
  base_line_size = 0.5 * line_size_adjustment # stroke weight 0.5 points for axis

  # See also:
  # - https://cmap-repos.github.io/cmapplot/reference/dot-lwd.html
  # - warning from help(grid::par):
  #   The line width, a positive number, defaulting to 1. The interpretation is device-specific, and some devices do not implement line widths less than one. (See the help on the device for details of the interpretation.)
  # Note that:
  # - 25.4 / 72.27 * 96 / 72 = ggplot2::.stroke / ggplot2::.pt * 25.4 / 72.27
  # - There are 72.27 pts in a inch, so to convert from points to mm, just multiply by 72.27 / 25.4).
  # - ggplot measures size in mm; but we care about points
  # - 1 pt is only approx. equal to 0.35mm; # https://community.rstudio.com/t/why-does-ggplot-size-parameter-not-behave-consistently/21619/4
  # - ggplot2:::.pt === 72.27 / 25.4
  # - ggplot2:::.stroke === 96 / 25.4
  
  ggplot2::theme_classic(
    base_family = font_family,
    base_size = medium_font_size,
    base_line_size = base_line_size,
    base_rect_size = base_line_size
  ) %+replace%
  
  ggplot2::theme(
      
    # Title; which is actually the y-axis title (indicating the unit)
    plot.title = element_text(
      hjust = 1,
      vjust = 1,
      margin=margin(t=0, b=unit(3, "bigpts")   )), # had to add top margin b/c of ggtext (not anymore?)

    # X axis title (indicating frequency: monthly, quarterly, etc.)
    axis.title.x = element_blank(),

    # Footnote
    #plot.caption = element_text(hjust = 0, margin=margin(t=6)),
    plot.caption = ggtext::element_textbox_simple(hjust = 0, margin=margin(t=6+1, r=0, b=3, l=0), lineheight=1.05),


    # Axis labels
    axis.text = element_text(size = medium_font_size),
    axis.text.y.left = element_blank(),
    axis.text.y.right = element_text(hjust = 1, margin = margin(l=6)),
    # Hack: if we don't do 6+6=12 then ggh4x leaves no margin for the years (b/c of the ticks)
    axis.text.x.bottom = element_text(hjust = 0.5, margin = margin(t=6+6), debug=F),
    
    # Axis ticks
    axis.ticks = element_line(linewidth = 0.5 * line_size_adjustment),
    # Hack: we want to set it to zero but can't without also setting minor ticks to zero; so we set them to .06pt and then the minor ticks to 100*.06=6pt
    axis.ticks.length = unit(-6, units='bigpts'),
    axis.ticks.length.x = unit(-.06, units='bigpts'),
    ggh4x.axis.ticks.length.minor = rel(100), # ggh4x axis tricks to fit year labels

    # Legend
    legend.title = element_blank(),
    legend.position = legend_position, # c(.9, .1),
    legend.justification = c("right", "bottom"),
    legend.box.background = element_blank(), # fill='transparent'),
    legend.background = element_blank(), # fill='transparent'),
    legend.key.height = unit(5.75, 'bigpts'),
    legend.key.width = unit(5.75, 'bigpts'),
    legend.spacing.x = unit(2, 'bigpts'),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0),
    legend.text = element_text(size=small_font_size, margin=margin(t=2)),

    # Entire plot
    #plot.margin = margin(1, 1, 1, 1), # margin=1 works well on Windows but not on Fed Linux
    #plot.margin = margin(4, 4, 4, 4),
    plot.margin = margin(6, 6, 6, 6),

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


