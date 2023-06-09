% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrappers.R
\name{scale_x_year}
\alias{scale_x_year}
\title{Position annual scales for date data (INCOMPLETE!)}
\usage{
scale_x_year(
  from = ggplot2::waiver(),
  to = ggplot2::waiver(),
  by = ggplot2::waiver(),
  n = 5,
  expand = 0,
  ...
)
}
\arguments{
\item{from}{The first year to show in the x-axis labels (breaks).}

\item{to}{The last year to show in the x-axis labels (breaks).}

\item{by}{The spacing between labels, in years.}

\item{n}{The suggested number of levels, to be passed to \code{\link[scales:breaks_pretty]{scales::breaks_pretty()}} in case from/to/by are not set.}

\item{expand}{Increase the size of the x-axis by this fraction.}

\item{...}{other arguments passed on to \code{\link[=pretty]{pretty()}}}
}
\description{
\code{scale_x_year} is a wrapper around \code{ggplot2::scale_x_date()}
that automates some of its boilerplate.
}
\details{
In particular, the function will:
\enumerate{
\item Set the span of the x-axis ticks to include the entire range of the data
\item Position the ticks between years
\item Set the start, end, and by years of the x-axis labels, indicating only years and not entire dates.
\item Position the x-axis labels in the midpoint of each year.
}

It is equivalent to e.g.:

\if{html}{\out{<div class="sourceCode">}}\preformatted{scale_x_date(minor_breaks=seq(from=as.Date("2003-01-01"),
                              to=as.Date("2023-01-01"),
                              by="1 years"),
             breaks=seq(from=as.Date("2004-06-30"),
                        to=as.Date("2023-06-30"),
                        by="3 years"),
             date_labels="\%Y",
             expand=expansion(mult=.05))
}\if{html}{\out{</div>}}
}
