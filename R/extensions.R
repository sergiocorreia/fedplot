# -------------------------------------------------------------------------
#' Complex extensions that involve hacks to ggplot internals
# -------------------------------------------------------------------------


# Change data elements overloading ggplot_add -----------------------------
# See: https://yutani.rbind.io/post/2017-11-07-ggplot-add/

#' Move y-axis title to the top-right of the chart.
#' 
#' This function reuses the chart subtitle to move the y-axis title there.
#' Therefore, using it implies that users can't add subtitles to charts.
#' 
#' @export
move_y_axis_title <- function() {
  out <- ggplot2::geom_blank()
  class(out) <- c("move_y_axis_title", class(out))
  out
}

#' @export
ggplot_add.move_y_axis_title <- function(object, plot, object_name) {
  # Swap title and y-axis title
  #plot$labels$xyz <- "foo"
  plot$labels$subtitle <- plot$labels$y
  #plot$labels$y <- NULL # NULL
  plot
}


# -------------------------------------------------------------------------
#' Add wide ticks at the top of the chart
#'
#' This function adds ticks to the top-left and top-right corners of the charts.
#' These ticks are twice as long as normal ticks and serve mostly as as
#' a decoration.
#'
#' @export
# -------------------------------------------------------------------------

annotate_top_tick <- function() {
  tick_length <- grid::unit(12, "bigpts")   # "top tick marks: 12 points"
  tick_width <- grid::unit(0.5 * getOption("fedplot.linewidth_adj") * ggplot2::.pt, "bigpts")  # "line stroke size: 0.5 point for all axis"
  zero <- grid::unit(0, "npc")
  one <- grid::unit(1, "npc")

  # See: https://www.rdocumentation.org/packages/grid/versions/3.6.2/topics/gpar
  gp <- grid::gpar(lwd = tick_width, lineend = "round", linejoin="mitre") #, clip="off", linesquare="bevel")

  top_tick_grob <- grid::segmentsGrob(
    x0 = grid::unit.c(zero, one - tick_length),
    x1 = grid::unit.c(tick_length, one),
    y0 = c(1, 1),
    y1 = c(1, 1),
    gp=gp)

  ggplot2::layer(
    data = dummy_data(),
    stat = ggplot2::StatIdentity,
    position = ggplot2::PositionIdentity,
    geom = ggplot2::GeomCustomAnn,
    inherit.aes = FALSE,
    params = list(
      grob = top_tick_grob,
      xmin = -Inf,
      xmax =  Inf,
      ymin = -Inf,
      ymax =  Inf
    )
  )
}


# -------------------------------------------------------------------------
#' Add last date of data
#'
#' This function annotates the chart with the last date where data is available.
#' It does so by looking by first filtering out missing values in the y-axis
#' variable and then computing for these rows the maximum of the x-axis variable.
#'
#' This function formats the last date string accordingly to its frequency
#' and to publication standards. For instance, monthly data gets formatted
#' as "Jan.", "Feb.", etc. with the exception of June and September that
#' appear as "June" and "Sept." respectively.
#'
#' Note that this formatting is currently incomplete for frequencies other
#' than monthly.
#'
#' @param font_family Font family. If empty, it will default to Franklin Gothic Condensed (unless the "fedplot.font_family" option is overwritten).
#' @param font_size Font size. If empty, will default to size 7 (unless the "fedplot.font_size" option is overwritten).
#' @param color Color of the text. Defaults to `black`.
#' @param nudge_x Manually nudge the x-axis of the text. Alternative to the `repel` option.
#' @param nudge_y Manually nudge the y-axis of the text. Alternative to the `repel` option.
#' @param hjust Horizontal justification; range is 0 to 1; default is 0.5 (centered).
#' @param repel If set to `TRUE`, will try to reposition the text label to avoid overlapping it with other elements of the plot (such as the lines). It relies on the [ggrepel::geom_text_repel()] function of the [ggrepel] package.
#' @param text_aes Named list, additional aesthetics to send to the geometry.
#' @inheritParams ggrepel::geom_text_repel
#' @inheritParams ggplot2::geom_text
#' @export
# -------------------------------------------------------------------------
annotate_last_date <- function(# Common options
                               font_family = getOption("fedplot.font_family"),
                               font_size = getOption("fedplot.font_size") * 7L / 8L,
                               color = "black",
                               # Manually move location
                               nudge_x = 0,
                               nudge_y = 0,
                               hjust = 0.5,
                               # Automatically move location using ggrepel
                               repel = FALSE,
                               box.padding = 0.25,
                               point.padding = 1e-6,
                               min.segment.length = 0.5,
                               arrow = NULL,
                               force = 1,
                               force_pull = 1,
                               max.time = 0.5,
                               max.iter = 10000,
                               max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
                               xlim = c(NA, NA),
                               ylim = c(NA, NA),
                               direction = c("both","y","x"),
                               seed = 1234, # NA,
                               verbose = FALSE,
                               # Options shared with geom_text
                               mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               text_aes = NULL,
                               ...) {
  #annotate(geom = "text", ...)
  position <- ggplot2::position_nudge(nudge_x, nudge_y)

  if (repel) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomLastDateRepel,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = append(
        rlang::list2(
          family = font_family,
          color = color,
          hjust = hjust,
          size = font_size / ggplot2::.pt,
          # Same as geom-text-repel.R:
          na.rm = na.rm,
          box.padding = box.padding, # ggrepel:::to_unit(box.padding),
          point.padding = point.padding, # ggrepel:::to_unit(point.padding),
          min.segment.length = min.segment.length, # ggrepel:::to_unit(min.segment.length),
          arrow = arrow,
          force = force,
          force_pull = force_pull,
          max.time = max.time,
          max.iter = max.iter,
          max.overlaps = max.overlaps,
          nudge_x = nudge_x,
          nudge_y = nudge_y,
          xlim = xlim,
          ylim = ylim,
          direction = match.arg(direction),
          seed = seed,
          verbose = verbose,
          ...
        ),
        text_aes
      )
    )
  }
  else {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomLastDate,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = append(
        rlang::list2(
          family = font_family,
          color = color,
          hjust = hjust,
          size = font_size / ggplot2::.pt,
          na.rm = na.rm,
          ...
        ),
        text_aes
      )
    )
  }
}


# @export
setup_data_last_date = function(data, params) {
  # Silence notes in package check
  label <- x <- y <- NULL

  frequency <- get_frequency_internal(data)

  data |>
    dplyr::filter(!is.na(y)) |>
    dplyr::filter(x == max(x)) |>
    dplyr::filter(dplyr::row_number() == 1) |> # This resolves ties when there are multiple bars/series ending on max. date
    dplyr::mutate(label = date2label(x, frequency)) |>
    dplyr::mutate(label = dplyr::case_when(
      label == "May." ~ "May",
      label == "Jun." ~ "June",
      label == "Jul." ~ "July",
      label == "Sep." ~ "Sept.",
      .default = label)
    )
}

#' ggproto class used to place the text by annotate_last_date, based on GeomText
#'
#' @seealso ggproto
#' @usage NULL
#' @export
GeomLastDate <- ggplot2::ggproto("GeomLastDate", ggplot2::GeomText,
  required_aes = c("x", "y"),
  setup_data = setup_data_last_date
  #setup_params = function(data, params) { params }
)


#' ggproto class used to place the text by annotate_last_date, based on GeomTextRepel
#'
#' @seealso ggproto
#' @usage NULL
#' @export
GeomLastDateRepel <- ggplot2::ggproto("GeomLastDateRepel", ggrepel::GeomTextRepel,
  required_aes = c("x", "y"),
  setup_data = setup_data_last_date
)


get_frequency_internal <- function(data) {
  dates <- dplyr::distinct(dplyr::select(data, "x"))[[1]]
  dates <- structure(dates, class="Date")

  if (all(lubridate::round_date(dates, unit="year")==dates)) {
    "Annual"
  } else if (all(lubridate::round_date(dates, unit="quarter")==dates)) {
    "Quarterly"
  } else if (all(lubridate::round_date(dates, unit="month")==dates)) {
    "Monthly"
  } else if (all(lubridate::round_date(dates, unit="week")==dates)) {
    "Weekly"
  } else {
    "Daily"
  }
}


date2label <- function(x, frequency) {
  if (frequency == "Monthly") {
    strftime(structure(x, class="Date"), "%b.")
  } else if (frequency == "Quarterly") {
    paste0("Q", lubridate::quarter(structure(x, class="Date")))
  }
  else {
    structure(x, class="Date") 
  }
}




# -------------------------------------------------------------------------
#' Add frequency label
#'
#' This function annotates the chart with the frequency used in the x-axis,
#' placing a text label on the top-left of the plot.
#' It automatically detects and labels daily, weekly, monthly, quarterly
#' and annual frequencies.
#' For other frequencies or custom text strings, you can use the `label` argument.

#' @param label Text to be added. If equal to "" or "default", it will be inferred based on the x-axis variable.
#' @param font_family Font family. If empty, it will default to Franklin Gothic Condensed (unless the "fedplot.font_family" option is overwritten).
#' @param font_size Font size. If empty, will default to size 7 (unless the "fedplot.font_size" option is overwritten).
#' @return None
#' @export
# -------------------------------------------------------------------------
annotate_frequency <- function(label = "default",
                               font_family = getOption("fedplot.font_family"),
                               font_size = getOption("fedplot.font_size") * 7L / 8L) {
  gp <- grid::gpar(fontfamily = font_family, fontsize = font_size)
  x = grid::unit(12, "bigpts")
  y = grid::unit(1, "npc") - grid::unit(3 + 0.4, "bigpts")
  text1_grob <- grid::textGrob(label, x=x, y=y, hjust=0, vjust=1, gp = gp)

  out <- ggplot2::layer(
    data = dummy_data(),
    stat = ggplot2::StatIdentity,
    position = ggplot2::PositionIdentity,
    geom = ggplot2::GeomCustomAnn,
    inherit.aes = FALSE,
    params = list(
      grob = text1_grob,
      xmin = -Inf,
      xmax =  Inf,
      ymin = -Inf,
      ymax =  Inf
    )
  )
  class(out) <- c("annotate_frequency", class(out))
  out
}


#' @export
ggplot_add.annotate_frequency <- function(object, plot, object_name) {

  # Infer the frequency label if not provided by user
  frequency_label <- object$geom_params$grob$label
  if (frequency_label == "default") {
    object$geom_params$grob$label <- get_frequency(plot)
  }
  plot$layers <- append(plot$layers, object)
  plot
}


get_frequency <- function(plot) {
  dates = dplyr::distinct(dplyr::select(plot$data, !!plot$mapping$x))[[1]]

  if (all(lubridate::round_date(dates, unit="year")==dates)) {
    "Annual"
  } else if (all(lubridate::round_date(dates, unit="quarter")==dates)) {
    "Quarterly"
  } else if (all(lubridate::round_date(dates, unit="month")==dates)) {
    "Monthly"
  } else if (all(lubridate::round_date(dates, unit="week")==dates)) {
    "Weekly"
  } else {
    "Daily"
  }
}


# -------------------------------------------------------------------------
#' Square legends
#'
#' By \href{https://github.com/tidyverse/ggplot2/issues/3669}{default}, legends of bar plots and area plots use the `draw_key_polygon` function
#' to draw a rectangle as high as the text height. `fedplot` instead uses square legends, achieved by passing the argument `key_glyph=draw_key_square`
#' to the required geometries (`geom_col`, etc.).
#' See this \href{https://stackoverflow.com/questions/65812949/set-standard-legend-key-size-with-long-label-names-ggplot}{stack overflow thread} for more details,
#' and \href{https://www.emilhvitfeldt.com/post/changing-glyph-in-ggplot2/}{this page} for examples of other legend key functions.
#'

#' @return A grid grob.
#' @export
#' @param data A single row data frame containing the scaled aesthetics to
#'   display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
#' @examples
#' library(ggplot2)
#' p <- ggplot(FSR_4_1, aes(date, value, fill=type))
#' # key glyphs can be specified by their name
#' p + geom_col(key_glyph = "square")
#'
#' # key glyphs can be specified via their drawing function
#' p + geom_col(key_glyph = draw_key_square)
# -------------------------------------------------------------------------
draw_key_square <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }

  # BUGBUG: can/should we get rid of this hack?
  data$size <- getOption("fedplot.linewidth_adj") * 0.25

  lwd <- min(data$size, min(size) / 4)

  grid::rectGrob(
    x = unit(.5, "npc"),
    y = unit(.5, "npc"),
    width = grid::unit(1, "snpc") - grid::unit(lwd, "mm"),
    height = grid::unit(1, "snpc") - grid::unit(lwd, "mm"),
    gp = grid::gpar(
      col = data$colour %||% NA,
      fill = scales::alpha(data$fill %||% "grey20", data$alpha),
      lty = data$linetype %||% 1,
      lwd = lwd * .pt,
      linejoin = params$linejoin %||% "mitre",
      lineend = params$lineend %||% "butt"
  ))
}


# -------------------------------------------------------------------------
#' Set plot size
#'
#' This function sets the size of the plot, in big points.
#' By default, it will create a figure with width equal to 192bp and
#' panel height (not total figure height!) equal to 99bp.
#' It supports other present sizes as well as custom sizes.

#' @return A `ggh4x::forcedsize` S3 object that can be added to a plot.
#' @param size dimensions of the figure. For instance, 'narrow' creates default figures of 192x99 points. Valid values are c('narrow', 'wide', 'box_narrow', 'box_wide', 'slides', 'custom'). Note that 'slides' will produce a 192x161 figure, ideal for two-pane Powerpoint slides.
#' @param plot_height height of the figure in points (technically; bigpoints); useful when size='custom'.
#' @param plot_width height of the figure in points (technically; bigpoints); useful when size='custom'.
#' @param panel_height height of the panel in points (technically; bigpoints); useful when size='custom'.
#' @param panel_width height of the panel in points (technically; bigpoints); useful when size='custom'.
#' @export
# -------------------------------------------------------------------------
set_plot_size <- function(size = c('narrow', 'wide', 'box_narrow', 'box_wide', 'slides', 'custom'),
                          plot_height = NULL,
                          plot_width = NULL,
                          panel_height = NULL,
                          panel_width = NULL) {

  size <- match.arg(size)

  # Define height and width based on plot size
  # By default, we want the plot width to be fixed
  # But only the panel height is fixed (so the plot can be taller if we use long titles or footnotes)
  if (is.null(plot_width) && is.null(panel_width)) {
    plot_width <- switch(size,
                         narrow = 192,
                         wide = 420,
                         box_narrow = 180,
                         box_wide = 396,
                         slides = 192)
  }
  if (is.null(plot_height) && is.null(panel_height)) {
    panel_height <- switch(size,
                           slides = 161,
                           99)
  }

  # If both panel and plot heights are set, we prefer the panel height
  if (!is.null(panel_height)) {
    panel_height <- grid::unit(panel_height, "bigpts")
    plot_height <- NULL
  }

  if (!is.null(plot_height)) {
    plot_height <- grid::unit(plot_height, "bigpts")
    plot_height <- grid::convertUnit(plot_height, "points")
  }

  # If both panel and plot widths are set, we prefer the plot width
  if (!is.null(plot_width)) {
    plot_width <- grid::unit(plot_width, "bigpts")
    plot_width  <- grid::convertUnit(plot_width, "points")
    panel_width <- NULL
  }

  if (!is.null(panel_width)) {
    panel_width <- grid::unit(panel_width, "bigpts")
  }

  ggh4x::force_panelsizes(rows = panel_height,
                          cols = panel_width,
                          total_height = plot_height,
                          total_width = plot_width)
}
