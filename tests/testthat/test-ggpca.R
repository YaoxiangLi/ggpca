example_pca_data <- function() {
  data.frame(
    sample = paste0("s", 1:8),
    group = rep(c("A", "B"), each = 4),
    feature1 = c(1, 2, 3, 4, 6, 7, 8, 9),
    feature2 = c(9, 8, 7, 6, 4, 3, 2, 1),
    feature3 = c(2, 3, 5, 7, 11, 13, 17, 19)
  )
}

test_that("PCA returns a publication-ready ggplot", {
  plot <- ggpca(
    example_pca_data(),
    metadata_cols = c("sample", "group"),
    mode = "pca",
    color_var = "group",
    ellipse = FALSE
  )

  expect_s3_class(plot, "ggplot")
  expect_match(plot$labels$x, "PC1")
  expect_match(plot$labels$y, "PC2")
})

test_that("numeric metadata indices are supported", {
  plot <- ggpca(
    example_pca_data(),
    metadata_cols = 1:2,
    mode = "pca",
    ellipse = FALSE
  )
  expect_s3_class(plot, "ggplot")
})

test_that("invalid feature inputs fail clearly", {
  data <- example_pca_data()

  expect_error(
    ggpca(data, metadata_cols = "missing", ellipse = FALSE),
    "metadata_cols not found"
  )
  expect_error(
    ggpca(data, metadata_cols = 1:4, ellipse = FALSE),
    "at least two numeric"
  )

  data$feature1[[1]] <- NA_real_
  expect_error(
    ggpca(data, metadata_cols = 1:2, ellipse = FALSE),
    "process_missing_value"
  )

  data <- example_pca_data()
  data$feature1 <- 1
  expect_error(
    ggpca(data, metadata_cols = 1:2, ellipse = FALSE),
    "constant feature"
  )
})

test_that("requested dimensions must exist", {
  expect_error(
    ggpca(
      example_pca_data(),
      metadata_cols = 1:2,
      x_pc = "PC99",
      ellipse = FALSE
    ),
    "available dimensions"
  )
})

test_that("the Shiny example dataset is installed with the package", {
  example_path <- system.file("extdata", "example.csv", package = "ggpca")

  expect_true(nzchar(example_path))
  expect_true(file.exists(example_path))
})
