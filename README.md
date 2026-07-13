# Omop Bundles

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/SAFEHR-data/omop-bundles/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SAFEHR-data/omop-bundles/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Catalogue of omop concepts grouped into useful bundles to help researchers select concepts. 

## Overview

1. [Installation](#installation)
1. [Quickstart](#quickstart)
1. [Development](#development)
    - [Set up](#set-up)
    - [Updating the `renv` lockfile](#updating-the-renv-lockfile)
    - [Design](#design)
    - [Coding style](#coding-style)
1. [Deployment](./deploy/README.md)

## Installation

You can install the development version of data-catalogue from within R:

```r
# install.packages("pak")
pak::pak("SAFEHR-data/omop-bundles")
```

## Quickstart

Load the package and inspect the available bundles:

```r
library(omopbundles)

bundles <- list_bundles()
```

To retrieve concepts, create a vocabulary connection from the OMOP `concept`
and `concept_ancestor` tables. These can either be data frames or lazily loaded parquet files. 
At UCLH we have [preprocessed vocabularies available](https://github.com/SAFEHR-data/omop-vocabs-processed)
for this. 

```r
vocab_connection <- create_vocab_connection(
  connection = list(
    concept = concept,
    concept_ancestor = concept_ancestor
  ),
  connection_type = "OMOP"
)
```

Use a `bundle_id` returned by `list_bundles()` to retrieve its OMOP concepts:

```r
concepts <- get_bundle_concepts(
  bundle_id = "smoking",
  vocab_connection = vocab_connection
)
```

By default, child bundles are included and each bundle's descendant settings
are respected. These behaviours can be overridden:

```r
concept_ids <- get_bundle_concepts(
  bundle_id = c("smoking", "HBA1c"),
  vocab_connection = vocab_connection,
  resolve_descendants = FALSE,
  expand_bundle_hierarchy = FALSE,
  return_metadata = FALSE
)
```

Export a bundle as an OHDSI Atlas concept set, either as a JSON string or
directly to a file:

```r
atlas_json <- export_bundle_json(
  bundle_id = "smoking",
  vocab_connection = vocab_connection,
  resolve_descendants = TRUE
)

export_bundle_json(
  bundle_id = "smoking",
  vocab_connection = vocab_connection,
  resolve_descendants = TRUE,
  file_path = "smoking-concept-set.json"
)
```

Full documentation is available within R. e.g. `?list_bundles()`

## Development

### Set up

Make sure you have a [recent version of R](https://cloud.r-project.org/) (>= 4.4.0) installed.
Though not required, [RStudio](https://www.rstudio.com/products/rstudio/download/) is recommended as an IDE,
as it has good support for R package development and Shiny.

1. Clone this repository

    - Either with `git clone git@github.com:SAFEHR-data/omop-bundles.git`
    - Or by creating [a new project in RStudio from version control](https://docs.posit.co/ide/user/ide/guide/tools/version-control.html#creating-a-new-project-based-on-a-remote-git-or-subversion-repository)

2. Install [`{renv}`](https://rstudio.github.io/renv/index.html) and restore the project library by running the following from an R console in the project directory:

    ```r
    install.packages("renv")
    renv::restore()
    ```

### Updating the `renv` lockfile

Make sure to regularly run `renv::status(dev = TRUE)` to check if your local library and the lockfile
are up to date.

When adding a new dependency, install it in the `renv` library with

```r
renv::install("package_name")
```

and then use it in your code as usual.
`renv` will pick up the new package if it's installed and used in the project.

To update the lockfile, run

```r
renv::snapshot(dev = TRUE)
```

The `dev = TRUE` argument ensures that development dependencies (e.g. those recorded under
`Suggests` in the `DESCRIPTION` file) are also included in the lockfile.

### Coding style

We are following the [tidyverse style guide](https://style.tidyverse.org/).
The [`{styler}`](https://styler.r-lib.org/index.html) package can be used to automatically format R code to this style,
by regularly running

```r
styler::style_pkg()
```

within the project directory.
It's also recommended to install [`{lintr}`](https://github.com/r-lib/lintr) and regularly run

```r
lintr::lint_package()
```

(or have it [run automatically in your IDE](https://lintr.r-lib.org/articles/editors.html)).
