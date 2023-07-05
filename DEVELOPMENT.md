# Update steps

Follow [these](https://r-pkgs.org/Code.html#constant-health-checks) steps:

1. Remove the package if it's already installed: `remove.packages("fedplot")` (optional?)
1. Run `library(devtools)` (optional?)
1. Edit one or more files below `R/`.
1. `devtools::document()`. Some important notes:
	- Required if you’ve made any changes that impact help files, NAMESPACE, or collate order.
	- Note that `check()` won't update NAMESPACE so we need to run this.
	- Also, it seems that sometimes you have to manually delete the NAMESPACE file so it will get recreated without errors.
	- **Note that if you add/modify S3 methods, you need to run `document()` or the code will fail to run even after `load_all()`!!!**
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

- [gginnards](https://docs.r4photobiology.info/gginnards/reference/index.html): useful to debug geometries, layers, etc.
- https://arrow.apache.org/docs/dev/r/articles/developers/workflow.html
- https://www.andrewheiss.com/blog/2022/06/23/long-labels-ggplot/