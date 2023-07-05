#' fedplot
#'
#' Fed-style \pkg{ggplot2} theme and functions.
#'
#' This package contains a collection of ggplot extensions,
#' (a theme, color palettes, and geoms) required to produce
#' Fed-style charts, similar to those produced for the FSR.
#' Detailed documentation can be viewed at
#' \url{https://sergiocorreia.github.io/fedplot}.
#'
#' Please report issues and suggest improvements at
#' \url{https://github.com/sergio.correia/fedplot/issues}.
#'

#' @name fedplot
#' @docType package
#' @keywords internal
"_PACKAGE"


## usethis namespace: start
#' @importFrom dplyr select distinct filter mutate case_when
#' @importFrom ggh4x guide_axis_minor
#' @importFrom ggplot2 aes coord_cartesian discrete_scale element_blank element_rect element_text Geom geom_blank geom_line geom_text ggplotGrob ggproto ggsave guides position_nudge theme theme_classic xlab ylab
#' @importFrom ggplot2 layer StatIdentity PositionIdentity GeomCustomAnn geom_hline last_plot rel .pt
#' @importFrom ggplot2 '%+replace%'
#' @importFrom glue glue
#' @importFrom ggrepel GeomTextRepel
#' @importFrom ggtext element_textbox_simple
#' @importFrom grid unit segmentsGrob rectGrob gpar gTree gList convertUnit grobName
#' @importFrom here here
#' @importFrom lubridate round_date add_with_rollback
#' @importFrom rlang list2 '%||%'
#' @importFrom systemfonts system_fonts
#' @importFrom tibble tibble
#' @importFrom ragg agg_png
## usethis namespace: end
NULL


# ' @rdname reexports
# ' @name ggplot_add
# ' @usage ggplot_add.object(...)

#' Add custom objects to ggplot
#'
#' @importFrom ggplot2 ggplot_add
#' @name ggplot_add
#' @keywords internal
#' @export
ggplot2::ggplot_add
