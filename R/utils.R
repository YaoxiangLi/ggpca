#' Create publication-ready PCA, t-SNE, or UMAP plots
#'
#' This function generates dimensionality reduction plots (PCA, t-SNE, UMAP)
#' with options for custom labels, titles, density plots, and faceting.
#' It allows users to visualize high-dimensional data using various
#' dimensionality reduction techniques.
#'
#' @param data A data frame containing the data to be plotted. Must include both feature columns (numeric) and metadata columns (categorical).
#' @param metadata_cols A character vector of column names or a numeric vector of column indices for the metadata columns. These columns are used for grouping and faceting.
#' @param mode The dimensionality reduction method to use. One of \code{"pca"} (Principal Component Analysis), \code{"tsne"} (t-Distributed Stochastic Neighbor Embedding), or \code{"umap"} (Uniform Manifold Approximation and Projection).
#' @param scale Logical indicating whether to scale features (default: \code{TRUE} for PCA). Not used for \code{"tsne"} or \code{"umap"}.
#' @param x_pc Name of the principal component or dimension to plot on the x-axis (default: \code{"PC1"} for PCA).
#' @param y_pc Name of the principal component or dimension to plot on the y-axis (default: \code{"PC2"} for PCA).
#' @param color_var (Optional) Name of the column used to color points in the plot. If \code{NULL}, no color is applied. Supports both discrete and continuous variables. Default: \code{NULL}.
#' @param ellipse Logical indicating whether to add confidence ellipses for groups (only supported for PCA and only if \code{color_var} is discrete; default: \code{TRUE}).
#' @param ellipse_level Confidence level for ellipses (default: \code{0.9}).
#' @param ellipse_type Type of ellipse to plot, e.g., "norm" for normal distribution (default: \code{"norm"}).
#' @param ellipse_alpha Transparency level for ellipses, where 0 is fully transparent and 1 is fully opaque (default: \code{0.9}).
#' @param point_size Size of the points in the plot (default: \code{3}).
#' @param point_alpha Transparency level for the points, where 0 is fully transparent and 1 is fully opaque (default: \code{0.6}).
#' @param facet_var Formula for faceting the plot (e.g., \code{Category ~ .}), allowing users to split the plot by different groups.
#' @param tsne_perplexity Perplexity parameter for t-SNE, which balances local and global aspects of the data (default: \code{30}).
#' @param umap_n_neighbors Number of neighbors for UMAP, which determines the local structure (default: \code{15}).
#' @param density_plot Controls whether to add density plots for the x, y, or both axes. Accepts one of \code{"none"}, \code{"x"}, \code{"y"}, or \code{"both"} (default: \code{"none"}).
#' @param color_palette Name of the color palette (used for discrete variables) to use for the plot. Supports \code{"Set1"}, \code{"Set2"}, etc. from \code{RColorBrewer} (default: \code{"Set1"}).
#' @param xlab Custom x-axis label (default: \code{NULL}, will be auto-generated based on the data).
#' @param ylab Custom y-axis label (default: \code{NULL}, will be auto-generated based on the data).
#' @param title Plot title (default: \code{NULL}).
#' @param subtitle Plot subtitle (default: \code{NULL}).
#' @param caption Plot caption (default: \code{NULL}).
#'
#' @return A \code{ggplot2} object representing the dimensionality reduction plot, including scatter plots, optional density plots, and faceting options. The plot can be further customized using \code{ggplot2} functions.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point theme_bw labs theme element_blank
#' @importFrom ggplot2 element_text margin scale_color_brewer stat_ellipse
#' @importFrom ggplot2 facet_grid geom_density coord_flip scale_color_gradient scale_color_gradient2
#' @importFrom dplyr bind_cols select_if
#' @importFrom cowplot insert_xaxis_grob insert_yaxis_grob ggdraw
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @importFrom rlang .data
#'
#' @author Yaoxiang Li
#'
#' @examples
#' \donttest{
#' # Load dataset
#' pca_data <- read.csv(system.file("extdata", "example.csv", package = "ggpca"))
#'
#' # PCA example
#' p_pca_y_group <- ggpca(
#'   pca_data,
#'   metadata_cols = c(1:6),
#'   mode = "pca",
#'   color_var = "group",
#'   ellipse = TRUE,
#'   density_plot = "y",
#'   title = "PCA with Y-axis Density Plot",
#'   subtitle = "Example dataset, colored by group",
#'   caption = "Data source: Example dataset"
#' )
#' print(p_pca_y_group)
#'
#' # t-SNE example
#' p_tsne_time <- ggpca(
#'   pca_data,
#'   metadata_cols = c(1:6),
#'   mode = "tsne",
#'   color_var = "time",
#'   tsne_perplexity = 30,
#'   title = "t-SNE Plot of Example Dataset",
#'   subtitle = "Colored by time",
#'   caption = "Data source: Example dataset"
#' )
#' print(p_tsne_time)
#' }

ggpca <- function(data,
                  metadata_cols,
                  mode = c("pca", "tsne", "umap"),
                  scale = TRUE,
                  x_pc = "PC1",
                  y_pc = "PC2",
                  color_var = NULL,
                  ellipse = TRUE,
                  ellipse_level = 0.9,
                  ellipse_type = "norm",
                  ellipse_alpha = 0.9,
                  point_size = 3,
                  point_alpha = 0.6,
                  facet_var = NULL,
                  tsne_perplexity = 30,
                  umap_n_neighbors = 15,
                  density_plot = "none",
                  color_palette = "Set1",
                  # Customization options
                  xlab = NULL,
                  ylab = NULL,
                  title = NULL,
                  subtitle = NULL,
                  caption = NULL) {
  # Validate mode input
  mode <- match.arg(mode)
  density_plot <- match.arg(density_plot, choices = c("x", "y", "both", "none"))

  # Determine if metadata_cols is numeric or character and subset accordingly
  if (is.numeric(metadata_cols)) {
    metadata <- data[, metadata_cols, drop = FALSE]
    features <- data[, -metadata_cols, drop = FALSE]
  } else if (is.character(metadata_cols)) {
    metadata <- data[, metadata_cols, drop = FALSE]
    features <- data[, !names(data) %in% metadata_cols, drop = FALSE]
  } else {
    stop("metadata_cols should be either a numeric vector or a character vector.")
  }

  # Ensure features are numeric for PCA, t-SNE, or UMAP
  features <- dplyr::select_if(features, is.numeric)

  # Perform dimensionality reduction
  explained_variance <- NULL
  if (mode == "pca") {
    pca <- stats::prcomp(features, scale. = scale)
    scores <- as.data.frame(pca$x)
    colnames(scores) <- paste0("PC", seq_len(ncol(scores)))

    explained_variance <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
    xlab <- if (is.null(xlab)) paste0(x_pc, " (", explained_variance[1], "% variance)") else xlab
    ylab <- if (is.null(ylab)) paste0(y_pc, " (", explained_variance[2], "% variance)") else ylab

  } else if (mode == "tsne") {
    tsne_result <- Rtsne::Rtsne(as.matrix(features), perplexity = tsne_perplexity, check_duplicates = FALSE)
    scores <- as.data.frame(tsne_result$Y)
    colnames(scores) <- c("Dim1", "Dim2")
    x_pc <- "Dim1"
    y_pc <- "Dim2"
    xlab <- if (is.null(xlab)) x_pc else xlab
    ylab <- if (is.null(ylab)) y_pc else ylab

  } else if (mode == "umap") {
    umap_result <- umap::umap(as.matrix(features), n_neighbors = umap_n_neighbors)
    scores <- as.data.frame(umap_result$layout)
    colnames(scores) <- c("UMAP1", "UMAP2")
    x_pc <- "UMAP1"
    y_pc <- "UMAP2"
    xlab <- if (is.null(xlab)) x_pc else xlab
    ylab <- if (is.null(ylab)) y_pc else ylab
  }

  # Combine metadata with dimensionality reduction results
  plot_data <- dplyr::bind_cols(metadata, scores)

  # Set up base ggplot
  # If color_var is specified, we map color; otherwise, we map only x and y.
  if (!is.null(color_var)) {
    if (!color_var %in% colnames(plot_data)) {
      stop("`color_var` not found in the provided data. Check column names.")
    }
    aes_params <- ggplot2::aes(
      x = .data[[x_pc]],
      y = .data[[y_pc]],
      color = .data[[color_var]]
    )
  } else {
    aes_params <- ggplot2::aes(
      x = .data[[x_pc]],
      y = .data[[y_pc]]
    )
  }

  p <- ggplot2::ggplot(plot_data, aes_params) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = xlab,
      y = ylab
    ) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5), size = 10),
      axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5), size = 10),
      axis.text.x = ggplot2::element_text(size = 8, face = "bold", color = "black"),
      axis.text.y = ggplot2::element_text(size = 8, face = "bold", color = "black")
    )

  # Handle discrete vs. continuous color scales (if color_var is provided)
  if (!is.null(color_var)) {
    if (is.numeric(plot_data[[color_var]])) {
      # Continuous color scale
      p <- p + ggplot2::scale_color_gradient(low = "blue", high = "red")
    } else {
      # Discrete color scale
      p <- p + ggplot2::scale_color_brewer(palette = color_palette)
    }
  }

  # Add ellipses if requested and if color_var is discrete (PCA only)
  if (ellipse && mode == "pca") {
    if (!is.null(color_var) && !is.numeric(plot_data[[color_var]])) {
      p <- p + ggplot2::stat_ellipse(
        ggplot2::aes(group = .data[[color_var]]),
        level = ellipse_level,
        type = ellipse_type,
        alpha = ellipse_alpha
      )
    } else if (is.null(color_var)) {
      # If there's no color_var, we can draw one ellipse for all points
      # or choose to skip. Below draws a single ellipse over entire data:
      p <- p + ggplot2::stat_ellipse(
        level = ellipse_level,
        type = ellipse_type,
        alpha = ellipse_alpha
      )
    } else {
      # color_var is numeric; skip ellipses because grouping doesn't make sense
      message("Ellipses are only supported for discrete color variables. Skipping ellipses.")
    }
  }

  # Add faceting based on the specified formula
  if (!is.null(facet_var)) {
    if (inherits(facet_var, "formula")) {
      p <- p + ggplot2::facet_grid(facet_var)
    } else {
      stop("facet_var should be a formula (e.g., x ~ y, x ~ ., or . ~ y).")
    }
  }

  # Add density plots based on the specified option
  # If color_var is NULL, we use 'fill = "gray"' or a single color
  fill_mapping <- if (!is.null(color_var)) ggplot2::aes(fill = .data[[color_var]]) else ggplot2::aes()

  if (density_plot %in% c("x", "both")) {
    xdens <- ggplot2::ggplot(plot_data, fill_mapping) +
      ggplot2::geom_density(alpha = 0.7, linewidth = 0.2) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none") +
      {
        # If color_var is discrete vs numeric
        if (!is.null(color_var) && is.numeric(plot_data[[color_var]])) {
          ggplot2::scale_fill_gradient(low = "blue", high = "red")
        } else if (!is.null(color_var)) {
          ggplot2::scale_fill_brewer(palette = color_palette)
        } else {
          # No color_var
          ggplot2::scale_fill_manual(values = c("gray50"))
        }
      } +
      ggplot2::aes(x = .data[[x_pc]])

    p <- cowplot::insert_xaxis_grob(p, xdens, grid::unit(0.2, "null"), position = "top")
  }

  if (density_plot %in% c("y", "both")) {
    ydens <- ggplot2::ggplot(plot_data, fill_mapping) +
      ggplot2::geom_density(alpha = 0.7, linewidth = 0.2) +
      ggplot2::theme_classic() +
      ggplot2::coord_flip() +
      ggplot2::theme(legend.position = "none") +
      {
        # If color_var is discrete vs numeric
        if (!is.null(color_var) && is.numeric(plot_data[[color_var]])) {
          ggplot2::scale_fill_gradient(low = "blue", high = "red")
        } else if (!is.null(color_var)) {
          ggplot2::scale_fill_brewer(palette = color_palette)
        } else {
          # No color_var
          ggplot2::scale_fill_manual(values = c("gray50"))
        }
      } +
      ggplot2::aes(x = .data[[y_pc]])

    p <- cowplot::insert_yaxis_grob(p, ydens, grid::unit(0.2, "null"), position = "right")
  }

  # Draw the combined plot if density plots are used
  if (density_plot != "none") {
    p <- cowplot::ggdraw(p)
  }

  return(p)
}





#' Process Missing Values in a Data Frame
#'
#' This function filters columns in a data frame based on a specified threshold for missing values and performs imputation on remaining non-metadata columns using half of the minimum value found in each column.
#' Metadata columns are specified by the user and are exempt from filtering and imputation.
#'
#' @param data A data frame containing the data to be processed.
#' @param missing_threshold A numeric value representing the percentage threshold of missing values which should lead to the removal of a column. Default is 25.
#' @param metadata_cols A vector of either column names or indices that should be treated as metadata and thus exempt from missing value filtering and imputation. If NULL, no columns are treated as metadata.
#' @return A data frame with filtered and imputed columns as necessary.
#' @examples
#' data <- data.frame(
#'   A = c(1, 2, NA, 4),
#'   B = c(NA, NA, NA, 4),
#'   C = c(1, 2, 3, 4)
#' )
#' # Process missing values while ignoring column 'C' as metadata
#' processed_data <- process_missing_value(data, missing_threshold = 50, metadata_cols = "C")
#' @export
process_missing_value <- function(data, missing_threshold = 25, metadata_cols = NULL) {
  if (!is.null(metadata_cols) && is.numeric(metadata_cols)) {
    metadata_cols <- names(data)[metadata_cols]
  }

  valid_cols <- sapply(names(data), function(col_name) {
    missing_percent <- sum(is.na(data[[col_name]])) / nrow(data) * 100
    missing_percent <= missing_threshold || col_name %in% metadata_cols
  })

  data <- data[, valid_cols]

  non_metadata_cols <- names(data)[!names(data) %in% metadata_cols]
  for (col in non_metadata_cols) {
    if (any(is.na(data[[col]]))) {
      min_val <- min(data[[col]], na.rm = TRUE)
      data[[col]][is.na(data[[col]])] <- min_val / 2
    }
  }

  return(data)
}

