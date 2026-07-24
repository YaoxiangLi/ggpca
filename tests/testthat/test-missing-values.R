test_that("missing-value processing filters and imputes features", {
  data <- data.frame(
    group = c("A", "A", "B", "B"),
    keep = c(2, NA, 6, 8),
    drop = c(NA, NA, NA, 1),
    all_missing = rep(NA_real_, 4)
  )

  result <- process_missing_value(
    data,
    missing_threshold = 50,
    metadata_cols = "group"
  )

  expect_named(result, c("group", "keep"))
  expect_equal(result$keep[[2]], 1)
})

test_that("missing-value arguments are validated", {
  data <- data.frame(group = c("A", "B"), value = c(1, NA))

  expect_error(
    process_missing_value(data, missing_threshold = 101),
    "0 to 100"
  )
  expect_error(
    process_missing_value(data, metadata_cols = "missing"),
    "metadata_cols not found"
  )
})
