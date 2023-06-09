% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{FSR_2_4}
\alias{FSR_2_4}
\title{Net issuance of risky debt}
\format{
A data frame with 148 rows and 3 columns, showing the net issuance
of risky debt across time, by type of debt.
\describe{
\item{date}{Quarter}
\item{risky_debt_type}{Type of risky debt (two categories: institutional leveraged loans, or high-yield and unrated bonds.)}
\item{value}{Net issuance of a given type of risky debt, in $bn}
}
}
\source{
\url{https://www.federalreserve.gov/publications/files/financial-stability-report-20221104.pdf}
}
\usage{
FSR_2_4
}
\description{
Sample dataset from Figure 2.4 of the November 2022 Financial Stability Report,
used to illustrate the use of the \code{fedplot} package.
This file can be used to illustrate bar plots.
}
\keyword{datasets}
