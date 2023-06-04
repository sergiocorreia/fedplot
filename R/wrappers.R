# -------------------------------------------------------------------------
#' Convenience functions that wrap existing ggplot2 code.
#' This file shouldn't contain new functionality, just code
#' that can be replicated directly with ggplot2 standard functions.
# -------------------------------------------------------------------------


#' Draw horizontal line at the origin (y=0)
#' Equivalent to geom_hline(yintercept = 0, linewidth = getOption("fedplot.linewidth_adj"), lineend = "round") 
#' @param ... Additional arguments to pass to [ggplot2::geom_hline]
#' @export
geom_hline_zero <- function(...) {
	ggplot2::geom_hline(yintercept = 0,
		linewidth = 0.5 * getOption("fedplot.linewidth_adj"),
		lineend = "round",
		...)
}

# ' Wrapper around geom_line()
# ' Equivalent to ggplot(data, aes(..., group=VARNAME)) +
# ' geom_line(aes(color=VARNAME), linewidth=VARNAME)), na.rm = T, linejoin = "mitre", lineend = "round") +
# ' @export
#geom_line_fedx <- function(mapping = NULL, ...) {
#	# Silence notes in package check
#	group <- NULL
#
#	extra_mapping <- aes(color=ggplot2::after_stat(factor(group)) , linewidth=ggplot2::after_stat(factor(group)) )
#
#	if (!is.null(mapping)) {
#		# https://stackoverflow.com/a/65733829/3977107
#		mapping <- utils::modifyList(mapping, extra_mapping)
#	}
#	else {
#		mapping <- extra_mapping
#	}
#
#	ggplot2::geom_line(mapping=mapping, na.rm = T, linejoin = "mitre", lineend = "round", ...)
#}
