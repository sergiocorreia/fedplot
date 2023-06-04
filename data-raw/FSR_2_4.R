#' Code used to prepare the `FSR_2_4` dataset

#Load and format data
fn <- here::here("data-raw", "FSR_2_4.csv")
FSR_2_4 <- readr::read_csv(fn) |>
        dplyr::mutate(date = zoo::as.Date(zoo::as.yearqtr(Date)) ) |>
        dplyr::select(-Date) |>
        tidyr::pivot_longer(!date, names_to = "risky_debt_type")

usethis::use_data(FSR_2_4, overwrite = TRUE)

