library(arrow)

test_that("create_vocab_connection works with data frames", {
  concept_df <- data.frame(
    concept_id = c(4092281, 4222303),
    concept_name = c("Current smoker", "Former smoker"),
    domain_id = c("Observation", "Observation"),
    vocabulary_id = c("SNOMED", "SNOMED"),
    concept_code = c("449868002", "8517006"),
    standard_concept = c("S", "S"),
    invalid_reason = c(NA_character_, NA_character_),
    valid_start_date = as.Date(c("2002-01-31", "2002-01-31")),
    valid_end_date = as.Date(c("2099-12-31", "2099-12-31")),
    stringsAsFactors = FALSE
  )

  ancestor_df <- data.frame(
    ancestor_concept_id = integer(0),
    descendant_concept_id = integer(0),
    min_levels_of_separation = integer(0),
    max_levels_of_separation = integer(0),
    stringsAsFactors = FALSE
  )


  cdm <- list(
    concept = concept_df,
    concept_ancestor = ancestor_df
  )

  vocab_conn <- create_vocab_connection(
    connection = cdm,
    connection_type = "OMOP"
  )

  expect_s3_class(vocab_conn, "vocab_connection")
  expect_equal(vocab_conn$connection_type, "OMOP")
})

test_that("create_vocab_connection works with Arrow datasets", {

  cdm <- list(
    concept = read_parquet(testthat::test_path("testdata/vocab-parquet/concept.parquet")),
    concept_ancestor = read_parquet(testthat::test_path("testdata/vocab-parquet/concept_ancestor.parquet"))
  )

  vocab_conn <- create_vocab_connection(
    connection = cdm,
    connection_type = "OMOP"
  )

  pulse_rate_id <- 4301868

  metadata <- get_concept_metadata(vocab_conn, pulse_rate_id)

  expect_equal(metadata$concept_name, "Pulse rate")
})

test_that("create_vocab_connection rejects SNOMED", {
  expect_error(
    create_vocab_connection(connection = list(), connection_type = "SNOMED"),
    "SNOMED connection type is not yet implemented"
  )
})
