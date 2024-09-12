library(dplyr)
library(omopbundles)
library(testthat)
library(stringr)
library(purrr)



extract_bundle_details <- function(path) {
  filename <- stringr::str_remove(basename(path), "\\.csv$")

  # get the domain
  parts <- stringr::str_split(path, "/")[[1]]
  domain <- parts[length(parts) - 2]

  list(id = filename, domain = domain)
}

assert_bundle_has_name <- function(bundle) {
  bundle_name_file <- omopbundles:::get_raw_dir(bundle$domain, "bundle_names.csv")
  bundle_names <- read_csv(bundle_name_file, show_col_types = FALSE)

  expect_true(bundle$id %in% bundle_names$id,
              glue::glue("{bundle$id} should at least one name in: {bundle$domain}/bundle_names.csv"))
}

test_that("All raw bundles have at least one name", {
  raw_dir <- omopbundles:::get_raw_dir()

  concept_files <- Sys.glob(file.path(raw_dir, "*", "bundles", "*.csv"))
  bundle_ids <- purrr::map(concept_files, extract_bundle_details)
  purrr::walk(bundle_ids, assert_bundle_has_name)
})



test_that("All raw bundle names map to a bundle file that has at least one concept", {
  bundles <- omopbundles:::raw_bundles()

  # Ensure bundles dataframe is not empty
  expect_true(nrow(bundles) > 0)

  apply(bundles, 1, function(bundle) {
    concepts <- omopbundles:::raw_concept_by_bundle(bundle["domain"], bundle["id"])

    # Check that at least one concept
    expect_true(nrow(concepts) > 0)
  })

})
