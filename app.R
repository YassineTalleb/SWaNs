required_packages <- c("shiny", "bslib", "readxl", "htmltools")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste(
      "The following R packages are required but not installed:",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

options(shiny.maxRequestSize = 30 * 1024^2)

source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (path in sort(source_files)) {
  source(path, local = FALSE, encoding = "UTF-8")
}

shiny::shinyApp(ui = app_ui(), server = app_server)
