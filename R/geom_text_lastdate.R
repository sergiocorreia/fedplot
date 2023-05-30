#' Add a text with the last available date
#'
#' This functions relies heavily on the `cmapplot::geom_text()` function
#' from <code><a href="https://cmap-repos.github.io/cmapplot/reference/geom_text_lastonly.html">CMAP</a></code>,
#' which in turn is based on code{ggplot2::geom_text()} and `ggplot2::geom_point()`.
#'
#' TODO: add detailed description

#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_text_lastonly
#'
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#'   Cannot be jointy specified with `position`.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function. Cannot be jointy specified with
#'   `nudge_x` or `nudge_y`.
#' @param check_overlap If `TRUE`, text that overlaps previous text in the
#'   same layer will not be plotted. `check_overlap` happens at draw time
#'   and in the order of the data. Therefore data should be arranged by the
#'   label column before calling `geom_text_lastonly()`.
#' @param add_points If `TRUE`, points will be added to the plot (for the
#'   labeled data only). Default size=2, color will match line color.
#' @param text_aes,point_aes Named list, additional aesthetics to send to the
#'   text and point geoms, respectively.
#' @param ... Additional aesthetics to send to geom_text_lastonly,
#'   which will also send them to BOTH the point and text geoms.
#'   Note that if `add_points = FALSE`, additional parameters can be passed
#'   to the text geom here, rather than in `text_aes`, without breaking.
#'
#' @examples
#' df <- data.frame(year=2010:2020, value=runif(22), var=c(rep("A", 11), rep("B", 11)))
#'
#' # Without points, label formatting or x-axis expansion
#' ggplot(df, aes(x=year, y=value, color=var)) +
#'   geom_line() +
#'   labs(title="Random lines") +
#'   scale_y_continuous("Percentage of absolutely nothing") +
#'   scale_x_continuous("Year") +
#'   geom_text_lastdate()
#'
#' # With points, label formatting and x-axis expansion
#' ggplot(df, aes(x=year, y=value, color=var, label=sprintf("%.1f%%", 100*value))) +
#'   geom_line() +
#'   labs(title="Random lines") +
#'   scale_y_continuous("Percentage of absolutely nothing", labels=scales::percent) +
#'   scale_x_continuous("Year", expand=expansion(mult=c(0.05, 0.10))) +
#'   geom_text_lastdate(add_points=TRUE, text_aes=list(fontface="bold"), point_aes=list(size=2.5))
#'
#' @export
geom_text_lastdate <- function(
    font_family = "ITCFranklinGothic LT BookCn",
    font_size = 7,
    nudge_x = 0.25,
    nudge_y = 0,
    ...
  )
{
  # Compute date
  # TODO/BUG: need to do as in ggplot_add.annotate_frequency
  # TODO: Pass font from main text
  #freq <- get_frequency(plot)
  #print(freq)
  #print('>>>>>>>>')
  #if (freq == "Monthly") {
  #  f <- function(x) strftime(x, "%b")
  #}
  #else if (freq == "Quarterly") {
  #  f <- function(x) paste0("Q", as.character(ceiling(strtoi(strftime(x, "%m"), base=10) / 3)))
  #}
  #else {
  #  f <- function(x) x
  #}
  #y <- date # f(date)


  # Return geometry
  geom_text_lastonly(
    mapping = ggplot2::aes(label = strftime(Date, "%b")),
    text_aes = list(family = font_family,
                    color = "black",
                    hjust = 1,
                    size = font_size / .pt),
    nudge_y = nudge_y,
    nudge_x = nudge_x,
    ...
  )
}
