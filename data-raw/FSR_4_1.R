#' Code used to prepare the `FSR_4_1` dataset

#Load and format data
fn <- here::here("data-raw", "FSR_4_1.csv")
FSR_4_1 <- readr::read_csv(fn) |>
        dplyr::mutate(date = zoo::as.Date(zoo::as.yearqtr(Date)) ) |>
        dplyr::select(-Date) |>
        tidyr::pivot_longer(!date, names_to = "type")

usethis::use_data(FSR_4_1, overwrite = TRUE)

