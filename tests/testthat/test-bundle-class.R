test_that("Bundle constructor and validator work as expected", {
  expect_length(bundle(c(1, 2), "bundle_1", "domain"), 2)

  expect_error(bundle(c(123, NA), id = "bundle_id", domain = "domain"), "All concept IDs must be non-missing integers")
  expect_error(bundle(c(123, 234), id = "", domain = "domain"), "The bundle ID must be a single non-empty string")
  expect_error(bundle(c(123, 234), id = "bundle_id", domain = ""), "The bundle domain must be a single non-empty string")
  expect_error(bundle(c(123, 234), id = "bundle_id", domain = "domain", version = c(1, 2)), "The bundle version must be a single non-empty string")
})

test_that("Concatenating bundles should only work when id and domain match", {
  bundle1 <- bundle(c(1, 2), id = "bundle_id1", domain = "domain")
  bundle1_extra <- bundle(c(11, 22), id = "bundle_id1", domain = "domain")
  bundle2 <- bundle(c(8, 9), id = "bundle_id2", domain = "domain")

  expect_length(c(bundle1, bundle1_extra), length(bundle1) + length(bundle1_extra))
  expect_error(c(bundle1, bundle2))
})
