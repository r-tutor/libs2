
[![Travis-CI Build Status](https://travis-ci.org/hrbrmstr/htmltidy.svg?branch=master)](https://travis-ci.org/hrbrmstr/htmltidy)

<!-- README.md is generated from README.Rmd. Please edit that file -->
`htmltidy` — Clean up gnarly HTML/XML

Inspired by [this SO question](http://stackoverflow.com/questions/37061873/identify-a-weblink-in-bold-in-r) and because there's a great deal of cruddy HTML out there that needs fixing to use properly when scraping data.

NOTE: Requires [`libtidy`](http://www.html-tidy.org/) and presently is super-basic (no way to set options and pretty much only does HTML)

You'll need to first do a `brew install tidy-html5` on MacOS or `apt-get install libtidy-dev` on Ubuntu/Debian to get this to work. NOTE that the linux libraries may be older and return slightly different (but no less tidy) HTML.

**SEEKING COLLABORATORS**

This works enough for me to use in a pinch. It should be straightforward (but tedious) to:

-   enable passing options in a `list`
-   bundle `libtidy` *with the package* and get it to work on Windows, linux & MacOS as the library compiles on all three with the necessary tools.

The following functions are implemented:

-   `tidy` : Clean up gnarly HTML/XML

### Installation

``` r
devtools::install_github("hrbrmstr/htmltidy")
```

### Usage

``` r
library(htmltidy)

# current verison
packageVersion("htmltidy")
#> [1] '0.1.0.9000'

cat(tidy("<b><p><a href='http://google.com'>google &gt</a></p></b>"))
#> <!DOCTYPE html>
#> <html xmlns="http://www.w3.org/1999/xhtml">
#> <head>
#> <meta name="generator" content=
#> "HTML Tidy for HTML5 for Mac OS X version 5.2.0" />
#> <title></title>
#> </head>
#> <body>
#> <p><b><a href='http://google.com'>google &gt;</a></b></p>
#> </body>
#> </html>
```

### Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
