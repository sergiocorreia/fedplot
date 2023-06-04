# -------------------------------------------------------------------------
#' Complex extensions that involve hacks to ggplot internals
# -------------------------------------------------------------------------


# Change data elements overloading ggplot_add -----------------------------
# See: https://yutani.rbind.io/post/2017-11-07-ggplot-add/

#' @export
move_y_axis_title <- function() {
  out <- ggplot2::geom_blank()
  class(out) <- c("move_y_axis_title", class(out))
  out
}

#' @export
ggplot__add.move_y_axis_title <- function(object, plot, object_name) {
  # Swap title and y-axis title
  #plot$labels$xyz <- "foo"
  plot$labels$title <- plot$labels$y
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
  tick_width <- grid::unit(0.5 * 96 / 72, "bigpts")  # "line stroke size: 0.5 point for all axis"
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
#' @param repel If set to `TRUE`, will try to reposition the text label to avoid overlapping it with other elements of the plot (such as the lines). It relies on the [ggrepel::geom_text_repel()] function of the [ggrepel] package.
#' @param text_aes Named list, additional aesthetics to send to the geometry.
#' @inheritParams ggrepel::geom_text_repel
#' @inheritParams ggplot2::geom_text
#' @export
# -------------------------------------------------------------------------
annotate_last_date <- function(# Common options
                               font_family = getOption("fedplot.font_family"),
                               font_size = 7,
                               color = "black",
                               # Manually move location
                               nudge_x = 0,
                               nudge_y = 0,
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
          hjust = 1,
          size = font_size / .pt,
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
          hjust = 1,
          size = font_size / .pt,
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
  x <- y <- NULL

  frequency <- get_frequency_internal(data)

  data |>
    dplyr::filter(!is.na(y)) |>
    dplyr::filter(x == max(x)) |>
    dplyr::mutate(label = date2label(x, frequency)) |>
    dplyr::mutate(label = dplyr::case_when(
      label == "Jun." ~ "June",
      label == "Sep." ~ "Sept.")
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
annotate_frequency <- function(label = "",
                               font_family = getOption("fedplot.font_family"),
                               font_size = getOption("fedplot.font_size") * 7L / 8L) {
  gp <- grid::gpar(fontfamily = font_family, fontsize = font_size)
  x = grid::unit(12, "bigpts")
  y = grid::unit(1, "npc") - grid::unit(3, "bigpts")
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
ggplot__add.annotate_frequency <- function(object, plot, object_name) {

  # Infer the frequency label if not provided by user
  frequency_label <- object$geom_params$grob$label
  if (frequency_label == "default" || frequency_label == "") {
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
