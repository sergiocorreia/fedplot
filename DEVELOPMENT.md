# Update steps

Follow [these](https://r-pkgs.org/Code.html#constant-health-checks) steps:

1. Run `library(devtools)` (optional?)
1. Edit one or more files below `R/`.
1. `devtools::document()`. Required if you’ve made any changes that impact help files, NAMESPACE, or collate order. Note that `check()` won't update NAMESPACE so we need to run this. Also, it seems that sometimes you have to manually delete the NAMESPACE file so it will get recreated without errors.
1. `devtools::load_all()`
1. Run some examples interactively.
1. `devtools::test()` (or `test_active_file()`)
1. `devtools::check()`
1. `pkgdown::build_site()`. If you get the error "No package named fedplot" then you need to restart R beforehand (`ctrl+shift+F10`)


To see files available in extdata:

```R
system.file("extdata", package = "fedplot") |> list.files()

```





# General knowledge required to work on this package:

- Documentation is written as comments using [`roxygen2`](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html):
	- https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
	- https://roxygen2.r-lib.org/articles/rd.html
	- https://stuff.mit.edu/afs/athena/software/r/current/RStudio/resources/roxygen_help.html



See also:

- https://arrow.apache.org/docs/dev/r/articles/developers/workflow.html