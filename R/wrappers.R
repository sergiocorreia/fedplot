#' Convenience functions that wrap existing ggplot2 code.
#' This file shouldn't contain new functionality, just code
#' that can be replicated directly with ggplot2 standard functions.

#' Draw horizontal line at the origin (y=0)
#' Equivalent to geom_hline(yintercept = 0, linewidth = fedplot_constants$linewidth, lineend = "round") 
#' @export
geom_hline_zero <- function(...) {
	geom_hline(yintercept = 0,
		linewidth = fedplot_constants$linewidth,
		lineend = "round",
		...)
}

#' Wrapper around geom_line()
#' Equivalent to ggplot(data, aes(..., group=VARNAME)) +
#' geom_line(aes(color=VARNAME), linewidth=VARNAME)), na.rm = T, linejoin = "mitre", lineend = "round") +
#' @export
geom_line_fedx <- function(mapping = NULL, ...) {
	extra_mapping <- aes(color=after_stat(factor(group)) , linewidth=after_stat(factor(group)) )

	if (!is.null(mapping)) {
		# https://stackoverflow.com/a/65733829/3977107
		mapping <- modifyList(mapping, extra_mapping)
	}
	else {
		mapping <- extra_mapping
	}

	geom_line(mapping=mapping, na.rm = T, linejoin = "mitre", lineend = "round", ...)
}


#' @export
geom_line_fed <- function(mapping = NULL, ...) {
	#structure(list(mapping=mapping, ..., ...), class = "geom_line_fed")
	#structure(list(mapping=mapping), class = "geom_line_fed")
	#structure(list(), class = "geom_line_fed")
	out <- ggplot2::geom_line(na.rm = T, linejoin = "mitre", lineend = "round", ...)
	class(out) <- c("geom_line_fed", "geom_line", class(out))
	out
}


#' @export
ggplot_add.geom_line_fed <- function(object, plot, object_name) {
  #attributes(plot)
  browser()
  stop(123)
  View(data)
  #new_data <- dplyr::filter(plot$data, !! object$expr)
  #new_layer <- geom_point(data = new_data,
  #                        mapping = plot$mapping,
  #                        colour = alpha("red", 0.5),
  #                        size = 5)
  #plot$layers <- append(plot$layers, new_layer)
  plot
  NULL
}


#	extra_mapping <- aes(color=after_stat(factor(group)) , linewidth=after_stat(factor(group)) )
#
#	if (!is.null(mapping)) {
#		# https://stackoverflow.com/a/65733829/3977107
#		mapping <- modifyList(mapping, extra_mapping)
#	}
#	else {
#		mapping <- extra_mapping
#	}
#
#	geom_line(mapping=mapping, na.rm = T, linejoin = "mitre", lineend = "round", ...)
#}
