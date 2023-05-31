#' Update internal tab-separated file listing NBER business cycle dates
#' See also: https://www.nber.org/research/data/us-business-cycle-expansions-and-contractions
#' @export
update_recessions <- function() {
    url <- "http://www2.nber.org/data/cycles/business_cycle_dates.json"
    recessions <- jsonlite::fromJSON(url) |> dplyr::mutate(peak = as.Date(peak), trough = as.Date(trough))
    readr::write_tsv(recessions, "recessions.tsv")
}


# Based on https://cmap-repos.github.io/cmapplot/reference/geom_recessions.html
# In turn, based on https://github.com/tidyverse/ggplot2/blob/HEAD/R/geom-rect.R
# See also https://ggplot2.tidyverse.org/reference/ggplot2-ggproto.html

#' @export
geom_recessions <- function(fill = "#bfcbdc",
                            alpha = 1.0,
                            draw_top_bar = FALSE,
                            top_fill = "#344660",
                            top_alpha = 1.0
                            )
{
    # build recessions table for use in function, but hide it in a list
    # because of ggplot's requirement that parameters be of length 1
    fn <- fs::path_package("extdata", "recessions.tsv", package = "fedplot")
    recess_table <- list(readr::read_tsv(fn, col_types = "DD"))

    # return a series of gg objects to ggplot
    list(
      layer(
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

    grob1 <- ggplot2:::ggname("geom_rect", grid::rectGrob(
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

    grob2 <- ggplot2:::ggname("geom_rect", grid::rectGrob(
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
