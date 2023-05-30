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
  tick_width <- 0.5 # "line stroke size: 0.5 point for all axis"
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



# Add frequency label -----------------------------------------------------

#' @export
annotate_frequency <- function(label, font_family, font_size){
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

  if (all(round_date(dates, unit="year")==dates)) {
    "Annual"
  } else if (all(round_date(dates, unit="quarter")==dates)) {
    "Quarterly"
  } else if (all(round_date(dates, unit="month")==dates)) {
    "Monthly"
  } else if (all(round_date(dates, unit="week")==dates)) {
    "Weekly"
  } else {
    "Daily"
  }
}
