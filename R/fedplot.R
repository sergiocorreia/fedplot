#' fedplot
#'
#' Fed-style \pkg{ggplot2} theme and functions.
#'
#' This package contains a collection of ggplot extensions,
#' (a theme, color palettes, and geoms) required to produce
#' Fed-style charts, similar to those produced for the FSR.
#' Detailed documentation can be viewed at
#' <TODO>.
#'
#' Please report issues and suggest improvements at
#' <https://github.com/sergio.correia/fedplot/issues>.
#'
#' @name fedplot
#' @docType package
#' @import ggplot2
#' @importFrom dplyr select distinct filter mutate
#' @importFrom rlang list2
#' @importFrom lubridate round_date
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom systemfonts system_fonts
#' @keywords internal
"_PACKAGE"


# Global options
# See: https://stackoverflow.com/questions/12598242/global-variables-in-packages-in-r
fedplot_constants <- new.env(parent = emptyenv())
assign('line_size_adjustment', 25.4 / 72.27 * 96 / 72, fedplot_constants)

#' ## Font support
#'


#' ### Installing fonts
#'
#' Installing fonts is a tricky and system-dependent process.
#' Currently, only Linux font install is supported.
#' 
#' Notes:
#' 
#' - Do not use `showtext`; it is not useful as it does not embed the fonts into the pdf/eps
#' - `extrafont` had some issues on Linux
#' - TODO: maybe we can just load from `extdata` without installing into `~/.fonts`?

#' @export
install_fed_font <- function() {
  font <- "ITCFranklinGothic LT BookCn"
  df <- systemfonts::system_fonts()

  # Early exit to simplify code
  font_is_installed <- any(df$family == font)
  if (font_is_installed) return()

  os <- .Platform$OS.type

  if(os == "unix") {
    message('Installing font file...')
    font_file <- fs::path_package("extdata", "ITC Franklin Gothic LT Book Condensed.ttf", package = "fedplot")
    system("mkdir -p ~/.fonts")
    system(glue::glue('cp "{font_file}" ~/.fonts'))
    system("fc-cache -f -v")

    # Import the new fonts into R; untested
    extrafont::font_import(paths = "~/.fonts")
  }
  else {
    message(glue::glue("cannot install file as OS {os} is not supported; please install manually"))
    # extrafont::font_import as well??
    # systemfonts::register_font(name, plain, bold, ...)
  }
}


#' ### Loading fonts
#'
#' @export
load_fed_font <- function() {
  font <- "ITCFranklinGothic LT BookCn"
  df <- systemfonts::system_fonts()
  font_is_installed <- any(df$family == font)
  if (!font_is_installed) {
    warning(glue::glue("Cannot load font '{font}'; not installed"), immediate. = TRUE)
    return()
  }

  os <- .Platform$OS.type

  if (os == "unix") {
    warning(glue::glue("load_fed_font() not yet implemented for os {os}"))
  }
  else if (os == "windows") {
    extrafont::loadfonts(device = "win", quiet = TRUE)
  }
  else {
    warning(glue::glue("load_fed_font() not yet implemented for os {os}"))
  }
}


#' Update fonts based on system -- *must* be done with .onLoad()
#'
#' @noRd
#' @import rstudioapi
.onLoad <- function(...) {
  load_fed_font()
}
