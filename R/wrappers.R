# -------------------------------------------------------------------------
#' Convenience functions that wrap existing ggplot2 code.
#' This file shouldn't contain new functionality, just code
#' that can be replicated with longer calls to standard ggplot2 functions.
# -------------------------------------------------------------------------



# -------------------------------------------------------------------------
#' Draw horizontal line at the origin (y=0)
#'
#' Equivalent to `geom_hline(yintercept = 0, linewidth = getOption("fedplot.linewidth_adj"), lineend = "round")`
#'
#' @param ... Additional arguments passed to [ggplot2::geom_hline]
#' @export
# -------------------------------------------------------------------------
geom_hline_zero <- function(...) {
  ggplot2::geom_hline(yintercept = 0,
                      linewidth = 0.5 * getOption("fedplot.linewidth_adj"),
                      lineend = "round",
                      ...)
}



# -------------------------------------------------------------------------
#' Draw lines with Fed-style aesthetics
#' 
#' `geom_line_fed` is a wrapper around `ggplot2::geom_line()`
#' that adds the required line colors and line widths.
#' It also sets the `linejoin` and `lineend` attributes to their required values.
#' 
#' It is equivalent to:
#' 
#' ```
#' geom_line(aes(color=<VAR>, linewidth=<VAR>),
#'           na.rm = TRUE,
#'           linejoin = "mitre",
#'           lineend = "round")
#' ```
#' 
#' @param mapping Additional `aes()` arguments passed to [ggplot2::geom_line]
#' @param ... Additional arguments passed to [ggplot2::geom_line]
#' @export
# -------------------------------------------------------------------------

geom_line_fed <- function(mapping = NULL, ...) {
  group <- NULL  # silence notes in package check
  extra_mapping <- aes(color=ggplot2::after_scale((group)) , linewidth=ggplot2::after_scale((group)) )
  # https://stackoverflow.com/a/65733829/3977107
  mapping <- if (is.null(mapping)) extra_mapping else utils::modifyList(mapping, extra_mapping)
  
  out <- ggplot2::geom_line(mapping=mapping, na.rm = TRUE, linejoin = "round", lineend = "round", ...)
  class(out) <- c("geom_line_fed", class(out))
  out
}

#' @export
ggplot_add.geom_line_fed <- function(object, plot, object_name) {
  object$mapping$colour <- plot$mapping$group
  object$mapping$linewidth <- plot$mapping$group
  
  # Call ggplot_add again, which will call ggplot_add.Layer
  # (which in turn adds the layer to the data, etc.)
  class(object) <- class(object)[-1]
  ggplot2::ggplot_add(object, plot, object_name)
}







# -------------------------------------------------------------------------
#' Position annual scales for date data (INCOMPLETE!)
#' 
#' `scale_x_year` is a wrapper around `ggplot2::scale_x_date()`
#' that automates some of its boilerplate.
#'
#' In particular, the function will:
#'
#' 1) Set the span of the x-axis ticks to include the entire range of the data
#' 2) Position the ticks between years
#' 3) Set the start, end, and by years of the x-axis labels, indicating only years and not entire dates.
#' 4) Position the x-axis labels in the midpoint of each year.
#' 
#' It is equivalent to e.g.:
#'
#' ```
#' scale_x_date(minor_breaks=seq(from=as.Date("2003-01-01"),
#'                               to=as.Date("2023-01-01"),
#'                               by="1 years"),
#'              breaks=seq(from=as.Date("2004-06-30"),
#'                         to=as.Date("2023-06-30"),
#'                         by="3 years"),
#'              date_labels="%Y",
#'              expand=expansion(mult=.05))
#' ```
#' 
#' @param from The first year to show in the x-axis labels (breaks).
#' @param to The last year to show in the x-axis labels (breaks).
#' @param by The spacing between labels, in years.
#' @param n The suggested number of levels, to be passed to [scales::breaks_pretty()] in case from/to/by are not set.
#' @param ... other arguments passed on to [pretty()]
#' @param expand Increase the size of the x-axis by this fraction.
#' @export
# -------------------------------------------------------------------------
scale_x_year <- function(from = ggplot2::waiver(),
                         to = ggplot2::waiver(),
                         by = ggplot2::waiver(),
                         n = 5,
                         expand = 0,
                         ...) {

  if (!inherits(from, "waiver")) {
    NULL # breaks = seq(from = from, to = to, by = glue::glue("{by} years"))
  }
  else if (!inherits(by, "waiver")) {
    NULL # breaks = breaks_width(glue::glue("{by} years"))(c(as.Date("2007-05-01"), as.Date("2010-05-01")))
  }
  else{
    NULL
  }

  #minor_breaks = seq(from = tick_from, to = tick_to, by = glue::glue("1 years"))

  #out <- ggplot2::scale_x_date(minor_breaks = minor_breaks,
  #                      breaks = breaks,
  #                      date_labels = "%Y",
  #                      expand = expansion(mult = expand))

  out <- ggplot2::scale_x_date(expand = ggplot2::expansion(mult = expand))  # date_labels = "%Y", 
  class(out) <- c("scale_x_year", class(out))
  out

  #scale_x_date(minor_breaks=seq(from=as.Date("2003-01-01"), to=as.Date("2023-01-01"), by="1 years"),
  #             breaks=seq(from=as.Date("2004-06-30"), to=as.Date("2023-06-30"), by="3 years"),
  #             date_labels="%Y",
  #             expand=expansion(mult=.05)) +
}


#' @export
ggplot_add.scale_x_year <- function(object, plot, object_name) {
  #var <- quo_name(plot$mapping$x)
  var <- plot$mapping$x
  print(range(plot$data[[var]] ))

  print("<<<")
  print(attributes(object))
  print(rlang::fn_fmls(object))
  print(">>>")
  
  # Call ggplot_add again, which will call ggplot_add.Layer
  # (which in turn adds the layer to the data, etc.)
  class(object) <- class(object)[-1]
  ggplot2::ggplot_add(object, plot, object_name)
}


