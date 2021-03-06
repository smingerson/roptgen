
# roptgen

<!-- badges: start -->
<!-- badges: end -->

The goal of roptgen is to simplify handling of options in R packages, for both users and developers.

As a user, packages with many options or some options with a wide variety of values can be frustrating.
Where are they documented, what are their defaults? Both renv (which inspired this package) and knitr do 
a great job documenting options, but not all packages are so clear.

As a developer, writing code around options is tedious, with the need to have a default and validate
inputs. If there are only a few options, this is not too onerous, but if you want to offer a lot of
customization it gets old. And then you have to document them. Of coure, writing what they do is 
inescapable, but you shouldn't have to worry about the formatting...

roptgen solves this by taking a succinct yaml file (using an R list on the way) and generating all
the code and Roxygen documentation (not quite there yet) for you. It creates an exported object
which offers option accessors usable within your own code, and available for users to inspect
possible options and their defaults. The package offers many customizations available as options.
Unsurprisingly, the options boilerplate and documentation is generated by roptgen.

## Installation

roptgen needs much more work before maybe one day making it to CRAN, so for now, you can install from
Github with 

``` r
remotes::install_github("smingerson/roptgen")
```

## Getting started

For now, look at `inst/options.yaml`, which generates `R/options.R`. You can achieve the same 
using `roptgen::roptgen()` from within your package (or project). See `?roptgen::roptgen_options`
on how to configure this to your liking. Many things not yet implemented may be mentioned, although
I've tried to mark them as not yet implemented.

## Code of Conduct

Please note that the roptgen project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
