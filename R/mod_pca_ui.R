#' PCA plot module UI
#'
#' @param id Module id.
#' @import shiny
#' @noRd
mod_pca_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    titlePanel("ggpca"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        tags$h4("Data"),
        radioButtons(
          ns("data_source"),
          "Data source",
          choices = c("Bundled test data" = "test", "Upload CSV" = "upload"),
          selected = "test"
        ),
        fileInput(ns("data_file"), "CSV file", accept = c(".csv", "text/csv")),
        checkboxInput(ns("header"), "Header", value = TRUE),
        selectInput(
          ns("separator"),
          "Separator",
          choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
          selected = ","
        ),
        selectInput(
          ns("quote"),
          "Quote",
          choices = c(Double = "\"", Single = "'", None = ""),
          selected = "\""
        ),
        checkboxInput(ns("process_missing"), "Filter and impute missing values", value = FALSE),
        sliderInput(ns("missing_threshold"), "Missing value threshold (%)", min = 0, max = 100, value = 25),
        uiOutput(ns("column_controls")),
        tags$hr(),
        tags$h4("Method"),
        selectInput(
          ns("mode"),
          "Dimensionality reduction",
          choices = c("pca", "tsne", "umap"),
          selected = "pca"
        ),
        checkboxInput(ns("scale"), "Scale features for PCA", value = TRUE),
        textInput(ns("x_pc"), "X dimension", value = "PC1"),
        textInput(ns("y_pc"), "Y dimension", value = "PC2"),
        numericInput(ns("tsne_perplexity"), "t-SNE perplexity", value = 30, min = 1, step = 1),
        numericInput(ns("umap_n_neighbors"), "UMAP neighbors", value = 15, min = 2, step = 1),
        tags$hr(),
        tags$h4("Plot"),
        selectInput(ns("color_var"), "Color variable", choices = c("None" = "")),
        checkboxInput(ns("ellipse"), "PCA confidence ellipse", value = TRUE),
        sliderInput(ns("ellipse_level"), "Ellipse level", min = 0.5, max = 0.99, value = 0.9, step = 0.01),
        selectInput(
          ns("ellipse_type"),
          "Ellipse type",
          choices = c("norm", "t", "euclid"),
          selected = "norm"
        ),
        sliderInput(ns("ellipse_alpha"), "Ellipse alpha", min = 0, max = 1, value = 0.9, step = 0.05),
        sliderInput(ns("point_size"), "Point size", min = 0.5, max = 10, value = 3, step = 0.5),
        sliderInput(ns("point_alpha"), "Point alpha", min = 0, max = 1, value = 0.6, step = 0.05),
        selectInput(
          ns("density_plot"),
          "Density plots",
          choices = c("none", "x", "y", "both"),
          selected = "none"
        ),
        selectInput(
          ns("color_palette"),
          "Discrete palette",
          choices = c("Set1", "Set2", "Set3", "Dark2", "Paired", "Accent"),
          selected = "Set1"
        ),
        tags$hr(),
        tags$h4("Facets and Labels"),
        selectInput(ns("facet_row"), "Facet rows", choices = c("None" = ".")),
        selectInput(ns("facet_col"), "Facet columns", choices = c("None" = ".")),
        textInput(ns("plot_title"), "Title", value = ""),
        textInput(ns("plot_subtitle"), "Subtitle", value = ""),
        textInput(ns("plot_caption"), "Caption", value = ""),
        textInput(ns("xlab"), "X label override", value = ""),
        textInput(ns("ylab"), "Y label override", value = ""),
        tags$hr(),
        tags$h4("Export"),
        selectInput(
          ns("download_format"),
          "Format",
          choices = c("PNG" = "png", "PDF vector" = "pdf", "SVG vector" = "svg"),
          selected = "png"
        ),
        numericInput(ns("download_width"), "Width", value = 8, min = 1, step = 0.5),
        numericInput(ns("download_height"), "Height", value = 6, min = 1, step = 0.5),
        numericInput(ns("download_dpi"), "DPI", value = 300, min = 72, step = 25),
        downloadButton(ns("download_plot"), "Download Plot")
      ),
      mainPanel(
        width = 8,
        tabsetPanel(
          tabPanel(
            "Plot",
            br(),
            plotOutput(ns("plot"), height = "680px")
          ),
          tabPanel(
            "Data",
            br(),
            verbatimTextOutput(ns("data_summary")),
            tableOutput(ns("data_preview"))
          )
        )
      )
    )
  )
}
