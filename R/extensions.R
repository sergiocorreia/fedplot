# Install font (Linux Only) -----------------------------------------------
# - Note that showtext is not useful as it does not embed the fonts
# - Similarly, extrafont was difficult to use on Linux

#' @export
install_fed_font <- function() {
  df <- systemfonts::system_fonts()
  font_missing <- !any(df$family == "ITCFranklinGothic LT BookCn")
  if (font_missing) {
    print('Installing font file...')
    font_file <- here("code", "ITC Franklin Gothic LT Book Condensed.ttf")
    system("mkdir -p ~/.fonts")
    system(glue::glue('cp "{font_file}" ~/.fonts'))
    system("fc-cache -f -v")
  }
}


# Change data elements overloading ggplot_add -----------------------------
# See: https://yutani.rbind.io/post/2017-11-07-ggplot-add/

#' @export
null_function <- function() {
  out <- ggplot2::geom_blank()
  class(out) <- c("null_function", class(out))
  out
}

#' @export
ggplot_add.null_function <- function(object, plot, object_name) {
  # Swap title and y-axis title
  #plot$labels$xyz <- "foo"
  plot$labels$title <- plot$labels$y
  #plot$labels$y <- NULL # NULL
  plot
}


# Add top tick marks ------------------------------------------------------

annotate_top_tick <- function() {
  tick_width <- unit(0.5 * 96 / 72, "bigpts") # pkg.env$line_size_adjustment # "line stroke size: 0.5 point for all axis"
  tick_margin <- 0.19 # add so the lines appear connected; unsure why 0.19
  tick_length <- 12 - tick_margin # "top tick marks: 12 points"

  zero <- unit(0, "npc")
  one <- unit(1, "npc")
  margin <- unit(tick_margin, "bigpts")
  length <- unit(tick_length, "bigpts")

  gp <- grid::gpar(lwd = tick_width, lineend = "butt", clip="off", linesquare="bevel")

  x0_left <- grid::convertWidth( -margin, "npc")
  x1_left <- grid::convertWidth(zero + length, "npc")
  x0_right <- grid::convertWidth(one - length, "npc")
  x1_right <- grid::convertWidth(one + margin, "npc")

  top_tick_grob <- grid::segmentsGrob(
    #x0 = c(x0_left, x0_right),
    x0 = grid::unit.c(-margin, one - length),
    #x1 = grid:unit.c(margin, x1_right), c("points", "npc")),
    x1 = grid::unit.c(length, one + margin),
    y0 = c(1, 1),
    y1 = c(1, 1),
    gp=gp)

  layer(
    data = ggplot2:::dummy_data(),
    stat = StatIdentity,
    position = PositionIdentity,
    geom = GeomCustomAnn,
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

# Add last date of data ---------------------------------------------------

#' @export
annotate_last_date <- function(# Common options
                               font_family = "ITCFranklinGothic LT BookCn",
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
    layer(
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
    layer(
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


#' @export
setup_data_last_date = function(data, params) {
  frequency <- get_frequency_internal(data)

  data |>
    dplyr::filter(!is.na(y)) |>
    dplyr::filter(x == max(x)) |>
    dplyr::mutate(label = date2label(x, frequency)) |>
    dplyr::mutate(label = case_when(
      label == "Jun." ~ "June",
      label == "Sep." ~ "Sept.")
    )
}


#' @export
GeomLastDate <- ggproto("GeomLastDate", GeomText,
  required_aes = c("x", "y"),
  setup_data = setup_data_last_date
  #setup_params = function(data, params) { params }
)

#' @export
GeomLastDateRepel <- ggproto("GeomLastDateRepel", ggrepel::GeomTextRepel,
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




# Add frequency label -----------------------------------------------------

#' @export
annotate_frequency <- function(label, font_family, font_size) {
  gp <- grid::gpar(fontfamily = font_family, fontsize = font_size)
  x = unit(12, "bigpts")
  y = unit(1, "npc") - unit(3, "bigpts")
  text1_grob <- grid::textGrob(label, x=x, y=y, hjust=0, vjust=1, gp = gp)

  out <- layer(
    data = ggplot2:::dummy_data(),
    stat = StatIdentity,
    position = PositionIdentity,
    geom = GeomCustomAnn,
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



# Use round caps by default -----------------------------------------------
# http://sape.inf.usi.ch/quick-reference/ggplot2/lineend
#
#scale_lineend_manual <- function(..., values, breaks = waiver(), na.value = NA) {
#  ggplot2:::manual_scale("lineend", values, breaks, ..., na.value = na.value)
#}
