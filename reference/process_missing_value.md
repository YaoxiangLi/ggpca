# Process Missing Values in a Data Frame

This function filters columns in a data frame based on a specified
threshold for missing values and performs imputation on remaining
non-metadata columns using half of the minimum value found in each
column. Metadata columns are specified by the user and are exempt from
filtering and imputation.

## Usage

``` r
process_missing_value(data, missing_threshold = 25, metadata_cols = NULL)
```

## Arguments

- data:

  A data frame containing the data to be processed.

- missing_threshold:

  A numeric value representing the percentage threshold of missing
  values which should lead to the removal of a column. Default is 25.

- metadata_cols:

  A vector of either column names or indices that should be treated as
  metadata and thus exempt from missing value filtering and imputation.
  If NULL, no columns are treated as metadata.

## Value

A data frame with filtered and imputed columns as necessary.

## Examples

``` r
# \donttest{
data <- data.frame(
  A = c(1, 2, NA, 4),
  B = c(NA, NA, NA, 4),
  C = c(1, 2, 3, 4)
)
# Process missing values while ignoring column 'C' as metadata
processed_data <- process_missing_value(data, missing_threshold = 50, metadata_cols = "C")
# }
```
