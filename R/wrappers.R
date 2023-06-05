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
# ' Equivalent to `r ggplot(data, aes(..., group=VARNAME)) +
# ' geom_line(aes(color=VARNAME), linewidth=VARNAME)), na.rm = T, linejoin = "mitre", lineend = "round")`
#' 
#' @param mapping Additional `aes()` arguments passed to [ggplot2::geom_line]
#' @param ... Additional arguments passed to [ggplot2::geom_line]
#' @export
# -------------------------------------------------------------------------
geom_line_fed <- function(mapping = NULL, ...) {
	# Silence notes in package check
	group <- NULL

	extra_mapping <- aes(color=ggplot2::after_stat(factor(group)) , linewidth=ggplot2::after_stat(factor(group)) )

	if (!is.null(mapping)) {
		# https://stackoverflow.com/a/65733829/3977107
		mapping <- utils::modifyList(mapping, extra_mapping)
	}
	else {
		mapping <- extra_mapping
	}

	ggplot2::geom_line(mapping=mapping, na.rm = T, linejoin = "mitre", lineend = "round", ...)
}
