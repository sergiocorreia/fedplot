
# -------------------------------------------------------------------------
#' Update internal tab-separated file listing \href{https://www.nber.org/research/data/us-business-cycle-expansions-and-contractions}{NBER business cycle dates}
#' @export
# -------------------------------------------------------------------------
update_recessions <- function() {
    # Silence notes in package check
    peak <- trough <- NULL

    url <- "http://www2.nber.org/data/cycles/business_cycle_dates.json"
    recessions <- jsonlite::fromJSON(url) |> dplyr::mutate(peak = as.Date(peak), trough = as.Date(trough))
    readr::write_tsv(recessions, "recessions.tsv")
}




# -------------------------------------------------------------------------
#' Add recession bars to time series graphs
#'
#' `geom_recessions` adds shaded recession bars to a time-series ggplot2 chart
#' (a chart with a date object in the x-axis.) These recession bars correspond to all
#' \href{https://www.nber.org/research/data/us-business-cycle-expansions-and-contractions}{NBER-dated} recessions
#' that overlap with the date range of the chart.
#' By default, each recession bar will be drawn as a light blue rectangle
#' spanning the entirety of the y-axis plus a dark blue rectangle drawn only over
#' the topmost part of the chart.
#'
#' @section Implementation details: 
#' Internally, `geom_recessions` works by adding a [fedplot::GeomRecessions]
#' [ggplot2::ggproto] object, which inherits from [ggplot2::Geom]. It
#' replaces the `setup_data` and `draw_panel` methods in order to
#' load the recession data and draw the bars respectively.
#' Further, the actual drawing is done by two [grid::rectGrob]
#' objects, for the main blue bar and the top dark blue bar.
#'
#' @section Inspiration and alternative implementations: 
#' This function is heavily inspired on `geom_recessions` from the \href{https://cmap-repos.github.io/cmapplot/reference/geom_recessions.html}{CMAPPLOT} package.
#'
#' @param fill The color of the main bar; defaults to a light blue tint.
#' @param alpha The alpha transparency of the main bar; defaults to 1.0 (totally opaque).
#' @param draw_top_bar Whether to draw the top bar or not.
#' @param top_fill The color of the top bar; defaults to a dark blue tint.
#' @param top_alpha The alpha transparency of the top blue bar; defaults to 1.0 (totally opaque).
#'
#'@export
# -------------------------------------------------------------------------

geom_recessions <- function(fill = "#BDCFDE",
                            alpha = 1.0,
                            draw_top_bar = TRUE,
                            top_fill = "#236192",
                            top_alpha = 1.0
                            )
{
    # build recessions table for use in function, but hide it in a list
    # because of ggplot's requirement that parameters be of length 1
    fn <- fs::path_package("extdata", "recessions.tsv", package = "fedplot")
    recess_table <- list(readr::read_tsv(fn, col_types = "DD"))

    # return a series of gg objects to ggplot
    list(
      ggplot2::layer(
        geom = GeomRecessions,
        mapping = NULL,
        data = NULL,
        stat = "identity",
        position = "identity",
        show.legend = FALSE,
        inherit.aes = TRUE,
        params = list(recess_table = recess_table,
                      fill = fill,
                      alpha = alpha,
                      draw_top_bar = draw_top_bar,
                      top_fill = top_fill,
                      top_alpha = top_alpha)
      )
    )
}

#' Custom ggproto classes
#'
#' The `fedplot` package contains a few custom ggproto objects. For the
#' most part, these are slightly tweaked versions of ggplot2's default proto
#' objects. For more information about these, see [ggplot2::ggproto].
#'
#' @name customproto
NULL

#' @describeIn customproto Add recession bars.
#' @format NULL
#' @usage NULL
#' @export
GeomRecessions <- ggplot2::ggproto("GeomRecessions", ggplot2::Geom,
  required_aes = c("x", "recess_table"),
  default_aes = ggplot2::aes(colour = NA,
                             fill = "#bfcbdc",
                             draw_top_bar = TRUE,
                             top_fill = "#344660",
                             alpha = 1.0,
                             top_alpha = 1.0,
                             linewidth = 0.5,
                             linetype = "solid",
                             na.rm = TRUE),

  # Replace `data` with `recessions`, filtered by `data`
  setup_data = function(data, params) {
    # Silence notes in package check
    peak <- trough <- NULL

    # Remove recess table from list
    recess_table <- params[["recess_table"]][[1]]

    # Filter recessions based on date parameters from `data`
    min <- structure(min(data$x), class="Date")
    max <- structure(max(data$x), class="Date")

    data <- recess_table |>
      dplyr::filter(min < trough & peak < max) |>
      dplyr::mutate(start = pmax(min, peak),
                    end = pmin(max, trough),
                    .keep = "none") |>
    # Set up data for GeomRect
      dplyr::mutate(xmin = start,
                    xmax = end,
                    ymin = -Inf,
                    ymax = Inf,
                    PANEL = 1,
                    group = -1,
                    .keep = "none")

    # Return
    data
  },

  draw_panel = function(self, data, panel_params, coord, draw_top_bar = TRUE, lineend = "butt", linejoin = "mitre") {
    coords <- coord$transform(data, panel_params)

    grob1 <- ggname("geom_rect", grid::rectGrob(
      x = coords$xmin,
      y = coords$ymax,
      width = coords$xmax - coords$xmin,
      height = grid::unit(coords$ymax - coords$ymin, "native"),
      default.units = "native",
      just = c("left", "top"),
      gp = grid::gpar(
        col = coords$colour,
        fill = alpha(coords$fill, coords$alpha),
        lwd = coords$linewidth * .pt,
        lty = coords$linetype,
        linejoin = linejoin,
        lineend = lineend)
      ))

    if(!draw_top_bar) {
      return(grob1)
    }

    grob2 <- ggname("geom_rect", grid::rectGrob(
      x = coords$xmin,
      y = coords$ymax,
      width = coords$xmax - coords$xmin,
      height = grid::unit(1.5, "bigpts"),
      default.units = "native",
      just = c("left", "top"),
      gp = grid::gpar(
        col = coords$colour,
        fill = alpha(coords$top_fill, coords$top_alpha),
        lwd = coords$linewidth * .pt,
        lty = coords$linetype,
        linejoin = linejoin,
        lineend = lineend)
      ))

    grid::gTree("geom_rectree", children=grid::gList(grob1, grob2))
  },

  draw_key = NULL,

  rename_size = TRUE
)
