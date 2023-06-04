# #' @importFrom tibble tibble
# NULL

#' Change in house prices
#'
#' Sample dataset from Figure 1.17 of the November 2022 Financial Stability Report,
#' used to illustrate the use of the \code{fedplot} package.
#' This file can be used to illustrate line plots.
#'
#' @format A data frame with 711 rows and 3 columns, showing the 12-month change
#' in the house price index from three different sources.
#' \describe{
#'   \item{date}{Month}
#'   \item{source}{Data provider}
#'   \item{value}{12-month percent change in the house price index}
#' }
#' @source <https://www.federalreserve.gov/publications/files/financial-stability-report-20221104.pdf>
"FSR_1_17"

#' Net issuance of risky debt
#'
#' Sample dataset from Figure 2.4 of the November 2022 Financial Stability Report,
#' used to illustrate the use of the \code{fedplot} package.
#' This file can be used to illustrate bar plots.
#'
#' @format A data frame with 148 rows and 3 columns, showing the net issuance
#' of risky debt across time, by type of debt.
#' \describe{
#'   \item{date}{Quarter}
#'   \item{risky_debt_type}{Type of risky debt (two categories: institutional leveraged loans, or high-yield and unrated bonds.)}
#'   \item{value}{Net issuance of a given type of risky debt, in $bn}
#' }
#' @source <https://www.federalreserve.gov/publications/files/financial-stability-report-20221104.pdf>
"FSR_2_4"

#' Runnable money-like liabilities (% of GDP)
#'
#' Sample dataset from Figure 4.1 of the November 2022 Financial Stability Report,
#' used to illustrate the use of the \code{fedplot} package.
#' This file can be used to illustrate area plots.
#'
#' @format A data frame with 83 rows and 3 columns, showing the 
#' amount of runnable money-like liabilities across time,
#' as a share of GDP, by type of liability.
#' \describe{
#'   \item{date}{Quarter}
#'   \item{type}{Type of runnable money-like liability (six categories:
#'   Uninsured deposits, Repurchase agreements, Domestic money market funds, Commercial paper, Securities lending, and Other.)}
#'   \item{value}{Net issuance of a given type of risky debt, in $bn}
#' }
#' @source <https://www.federalreserve.gov/publications/files/financial-stability-report-20221104.pdf>
"FSR_4_1"
