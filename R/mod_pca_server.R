#' PCA plot module server
#'
#' @param id Module id.
#'
#' @noRd
mod_pca_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    `%||%` <- function(x, y) {
      if (is.null(x)) y else x
    }

    blank_to_null <- function(x) {
      if (is.null(x) || !nzchar(trimws(x))) {
        return(NULL)
      }
      x
    }

    default_x_dimension <- function(mode) {
      if (identical(mode, "tsne")) {
        "Dim1"
      } else if (identical(mode, "umap")) {
        "UMAP1"
      } else {
        "PC1"
      }
    }

    default_y_dimension <- function(mode) {
      if (identical(mode, "tsne")) {
        "Dim2"
      } else if (identical(mode, "umap")) {
        "UMAP2"
      } else {
        "PC2"
      }
    }

    facet_term <- function(x) {
      if (identical(x, ".")) {
        return(".")
      }

      if (make.names(x) == x) {
        return(x)
      }

      paste0("`", gsub("`", "\\\\`", x), "`")
    }

    facet_formula <- function(row, col) {
      row <- if (is.null(row) || !nzchar(row)) "." else row
      col <- if (is.null(col) || !nzchar(col)) "." else col

      if (identical(row, ".") && identical(col, ".")) {
        return(NULL)
      }

      stats::as.formula(paste(facet_term(row), "~", facet_term(col)))
    }

    selected_metadata_cols <- function(data, selected) {
      selected <- selected %||% character(0)
      intersect(selected, names(data))
    }

    default_metadata_cols <- function(data) {
      cols <- names(data)
      feature_like <- grepl("^f[0-9]+$", cols)
      if (any(feature_like)) {
        return(cols[!feature_like])
      }

      cols[!vapply(data, is.numeric, logical(1))]
    }

    raw_data <- reactive({
      if (identical(input$data_source, "test") || is.null(input$data_file)) {
        example_path <- system.file("extdata", "example.csv", package = "ggpca")
        validate(need(nzchar(example_path), "Bundled example data could not be found."))
        return(utils::read.csv(example_path, check.names = TRUE))
      }

      utils::read.csv(
        input$data_file$datapath,
        header = isTRUE(input$header),
        sep = input$separator,
        quote = input$quote,
        check.names = TRUE
      )
    })

    output$column_controls <- renderUI({
      data <- raw_data()
      cols <- names(data)
      metadata_cols <- default_metadata_cols(data)
      numeric_cols <- cols[vapply(data, is.numeric, logical(1))]
      feature_cols <- setdiff(numeric_cols, metadata_cols)
      ns <- session$ns

      tagList(
        selectizeInput(
          ns("metadata_cols"),
          "Metadata columns",
          choices = cols,
          selected = metadata_cols,
          multiple = TRUE
        ),
        selectizeInput(
          ns("feature_cols"),
          "Feature columns",
          choices = numeric_cols,
          selected = feature_cols,
          multiple = TRUE
        )
      )
    })

    observeEvent(list(raw_data(), input$metadata_cols), {
      data <- raw_data()
      metadata_cols <- selected_metadata_cols(data, input$metadata_cols)
      choices <- c("None" = "", metadata_cols)

      updateSelectInput(session, "color_var", choices = choices)
      updateSelectInput(session, "facet_row", choices = c("None" = ".", metadata_cols))
      updateSelectInput(session, "facet_col", choices = c("None" = ".", metadata_cols))
    }, ignoreInit = FALSE)

    observeEvent(input$mode, {
      updateTextInput(session, "x_pc", value = default_x_dimension(input$mode))
      updateTextInput(session, "y_pc", value = default_y_dimension(input$mode))
    }, ignoreInit = TRUE)

    plot_data <- reactive({
      data <- raw_data()
      metadata_cols <- selected_metadata_cols(data, input$metadata_cols)
      feature_cols <- intersect(input$feature_cols %||% character(0), names(data))

      validate(
        need(length(feature_cols) >= 2, "Select at least two numeric feature columns."),
        need(all(vapply(data[feature_cols], is.numeric, logical(1))), "Feature columns must be numeric.")
      )

      data <- data[, unique(c(metadata_cols, feature_cols)), drop = FALSE]

      if (isTRUE(input$process_missing)) {
        data <- process_missing_value(
          data = data,
          missing_threshold = input$missing_threshold,
          metadata_cols = metadata_cols
        )
      }

      data
    })

    plot_object <- reactive({
      data <- plot_data()
      metadata_cols <- selected_metadata_cols(data, input$metadata_cols)

      validate(
        need(length(setdiff(names(data), metadata_cols)) >= 2, "At least two numeric feature columns are required.")
      )

      if (identical(input$mode, "tsne")) {
        max_perplexity <- max(1, floor((nrow(data) - 1) / 3))
        validate(need(input$tsne_perplexity <= max_perplexity, paste0(
          "For this data set, t-SNE perplexity must be <= ",
          max_perplexity,
          "."
        )))
      }

      ggpca(
        data = data,
        metadata_cols = metadata_cols,
        mode = input$mode,
        scale = isTRUE(input$scale),
        x_pc = blank_to_null(input$x_pc) %||% default_x_dimension(input$mode),
        y_pc = blank_to_null(input$y_pc) %||% default_y_dimension(input$mode),
        color_var = blank_to_null(input$color_var),
        ellipse = isTRUE(input$ellipse),
        ellipse_level = input$ellipse_level,
        ellipse_type = input$ellipse_type,
        ellipse_alpha = input$ellipse_alpha,
        point_size = input$point_size,
        point_alpha = input$point_alpha,
        facet_var = facet_formula(input$facet_row, input$facet_col),
        tsne_perplexity = input$tsne_perplexity,
        umap_n_neighbors = input$umap_n_neighbors,
        density_plot = input$density_plot,
        color_palette = input$color_palette,
        xlab = blank_to_null(input$xlab),
        ylab = blank_to_null(input$ylab),
        title = blank_to_null(input$plot_title),
        subtitle = blank_to_null(input$plot_subtitle),
        caption = blank_to_null(input$plot_caption)
      )
    })

    output$plot <- renderPlot({
      print(plot_object())
    })

    output$data_summary <- renderPrint({
      data <- plot_data()
      metadata_cols <- selected_metadata_cols(data, input$metadata_cols)
      cat("Data source:", if (identical(input$data_source, "upload")) "uploaded CSV" else "bundled test data", "\n")
      cat("Rows:", nrow(data), "\n")
      cat("Columns:", ncol(data), "\n")
      cat("Metadata columns:", paste(metadata_cols, collapse = ", "), "\n")
      cat("Feature columns:", paste(setdiff(names(data), metadata_cols), collapse = ", "), "\n")
    })

    output$data_preview <- renderTable({
      utils::head(plot_data(), 10)
    })

    output$download_plot <- downloadHandler(
      filename = function() {
        format <- input$download_format %||% "png"
        paste0("ggpca-", input$mode, "-", Sys.Date(), ".", format)
      },
      content = function(file) {
        format <- input$download_format %||% "png"
        plot <- plot_object()

        if (identical(format, "pdf")) {
          grDevices::pdf(file, width = input$download_width, height = input$download_height)
          on.exit(grDevices::dev.off(), add = TRUE)
          print(plot)
        } else if (identical(format, "svg")) {
          grDevices::svg(file, width = input$download_width, height = input$download_height)
          on.exit(grDevices::dev.off(), add = TRUE)
          print(plot)
        } else {
          ggplot2::ggsave(
            filename = file,
            plot = plot,
            width = input$download_width,
            height = input$download_height,
            dpi = input$download_dpi,
            units = "in"
          )
        }
      }
    )
  })
}
