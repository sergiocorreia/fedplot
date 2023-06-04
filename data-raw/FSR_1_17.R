#' Code used to prepare the `FSR_1_17` dataset

#Load and format data
fn <- here::here("data-raw", "FSR_1_17.csv")
FSR_1_17 <- readr::read_csv(fn, col_types = list(Date = readr::col_date(format="%b-%y"))) |>
        dplyr::rename(date=Date) |>
        tidyr::pivot_longer(!date, names_to = "source") #pivot for ggplot formatting

usethis::use_data(FSR_1_17, overwrite = TRUE)
