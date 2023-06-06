# -------------------------------------------------------------------------
#' Load fonts and global parameters
#' @name font_title 
#' @keywords internal
# -------------------------------------------------------------------------
NULL

#' @noRd
.onLoad <- function(...) {

	# Set package options
	# See: https://r-pkgs.org/Code.html#sec-code-onLoad-onAttach
  op <- options()
  op.fedplot <- list(

  	# Default font family is Franklin Gothic Condensed Medium (by ITC)
    fedplot.font_family = "ITCFranklinGothic LT BookCn",

    # Default font size is 8pt
    fedplot.font_size = 8L,

    # To achieve desired line widths (in bigpoints), we need to multiply by this constant.
    # This is b/c we define widths as bigpoints but ggplot2 expects milimeters.
    # Notes:
    # - 96		: pixels in inch
    # - 72		: bigpoints in an inch
    # - 25.4	: milimeters in an inch
    # - 72.27	: points in an inch
    # Steps:
    # 1) First, mult. by 96/72 to make the unit of linewidth 1mm instead of 0.75mm
    #   "Due to a historical error, the unit of linewidth is roughly 0.75 mm"
    #    https://community.rstudio.com/t/units-of-linewidth/162885
    # 2) Then we convert from points to mm
    #    Note also that ggplot2::.pt === 72.27 / 25.4
    # BUGBUG / QUESTION: should the last division be "72" instead of "72.27"???
    fedplot.linewidth_adj = 96 / 72 * 25.4 / 72.27

  )
  toset <- !(names(op.fedplot) %in% names(op))
  if (any(toset)) options(op.fedplot[toset])

  load_fed_font()

  invisible()
}

# -------------------------------------------------------------------------
#' Internal ggplot2 functions that we have to copy (CRAN doesn't support :::)
# -------------------------------------------------------------------------

#' @keywords internal
dummy_data <- function() data_frame0(x = NA, .size = 1)

#' @keywords internal
data_frame0 <- function(...) tibble::tibble(..., .name_repair = "minimal") # Replaced deprecated "data_frame" with "tibble"

#' @keywords internal
#' @importFrom grid grobName
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}


# -------------------------------------------------------------------------
#' Font support
#'
#' `fedplot` was designed with a specific set of fonts in mind.
#' You can use the [fedplot::install_fed_font()] and
#' [fedplot::load_fed_font()] to install a free version of these fonts.
#'
#' Note that these functions are currently only partly implemented across
#' operating systems. For instance, `install_fed_font` is only implemented
#' for Linux, and `load_fed_font` for Windows.
#'
#' Lastly, note you can also override the font used through [options()], such as in:
#' `options(fedplot.font_family = "Times New Roman")`
#' @name fontsupport
# -------------------------------------------------------------------------
NULL


# -------------------------------------------------------------------------
#' Installing fonts
#'
#' Installing fonts is a tricky and system-dependent process.
#' Currently, only Linux font install is supported and Windows fonts should
#' installed manually.
#'
#' Notes:
#'
#' - Do not use `showtext`; it is not useful as it does not embed the fonts into the pdf/eps.
#' - `extrafont` had some issues on Linux.
#' - TODO: maybe we can just load from `extdata` without installing into `~/.fonts`?
#'
#' @export
#' @return None
# -------------------------------------------------------------------------

install_fed_font <- function() {
  font <- getOption("fedplot.font_family")
  df <- systemfonts::system_fonts()

  # Early exit to simplify code
  font_is_installed <- any(df$family == font)
  if (font_is_installed) return()

  os <- .Platform$OS.type

  if(os == "unix") {
    message('Installing font file...')
    font_file <- gsub(" ", "-", font)
    font_file <- fs::path_package("extdata", glue::glue("{font_file}.ttf"), package = "fedplot")
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

  invisible()
}


# -------------------------------------------------------------------------
#' Loading fonts
#'
#' Load an already-installed font into R
#'
#' Currently only available for Windows, in which case it uses `extrafont::loadfonts()`.
#'
#' @export
#' @return None
# -------------------------------------------------------------------------

load_fed_font <- function() {
  font <- getOption("fedplot.font_family")
  df <- systemfonts::system_fonts()
  font_is_installed <- any(df$family == font)
  if (!font_is_installed) {
    warning(glue::glue("Cannot load font '{font}'; not installed"), immediate. = TRUE)
    return()
  }

  os <- .Platform$OS.type

  if (os == "unix") {
    NULL # It seems we don't need to do anything as long as the font was installed on ~/.fonts
  }
  else if (os == "windows") {
    extrafont::loadfonts(device = "win", quiet = TRUE)
  }
  else {
    warning(glue::glue("load_fed_font() not yet implemented for os {os}"))
  }

  invisible()
}
