% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme.R
\name{theme_fed}
\alias{theme_fed}
\title{Fed-style \link{ggplot} theme}
\usage{
theme_fed(
  font_family = getOption("fedplot.font_family"),
  font_size = getOption("fedplot.font_size"),
  legend_position = c(0.9, 0.1),
  color_palette = "fsr_primary",
  fill_palette = "fsr_primary",
  linewidth_palette = "fsr_linewidths",
  frequency = "default"
)
}
\arguments{
\item{font_family}{base font family}

\item{font_size}{base font family and size}

\item{legend_position}{where to place the legend; default is \code{c(.9, .1)}}

\item{color_palette}{color palette to use (either a string with the name or a vector)}

\item{fill_palette}{fill palette to use (either a string with the name or a vector)}

\item{linewidth_palette}{linewidth palette to use (either a string with the name or a vector)}

\item{frequency}{string indicating the date frequency; autodetected by default}
}
\description{
Fed-style \link{ggplot} theme
}
\examples{
\dontrun{
library(ggplot2)
library(dplyr)

# Scatterplot -> ADD
Bar chart -> ADD
Time series -> ADD
}

}
