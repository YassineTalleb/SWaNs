metric_card_output <- function(title, output_id, caption) {
  shiny::div(
    class = "metric-card",
    shiny::div(class = "metric-title", title),
    shiny::uiOutput(output_id),
    shiny::div(class = "metric-caption", caption)
  )
}

content_card <- function(title, ..., class = NULL) {
  shiny::div(
    class = paste("section-card", class),
    shiny::h3(class = "section-title", title),
    ...
  )
}

bullet_list <- function(items) {
  shiny::tags$ul(
    class = "scientific-list",
    lapply(items, function(item) shiny::tags$li(item))
  )
}

reading_guide <- function(title, items) {
  shiny::div(
    class = "reading-guide",
    shiny::div(class = "reading-guide-title", title),
    shiny::tags$ul(
      class = "reading-guide-list",
      lapply(items, function(item) shiny::tags$li(item))
    )
  )
}

strategy_card <- function(strategy_id, strategy_info) {
  shiny::div(
    class = "strategy-card",
    shiny::div(class = "strategy-label", strategy_info$label),
    if (!is.null(strategy_info$full_name) && nzchar(strategy_info$full_name)) {
      shiny::p(class = "strategy-full-name", strategy_info$full_name)
    },
    shiny::p(class = "strategy-short", strategy_info$short),
    shiny::p(class = "strategy-detail", strategy_info$detail)
  )
}

reference_entry <- function(authors, year = NULL, title, source = NULL, note = NULL, link = NULL, status = NULL) {
  title_node <- if (!is.null(link) && nzchar(link)) {
    shiny::tags$a(href = link, target = "_blank", rel = "noopener noreferrer", title)
  } else {
    title
  }

  shiny::div(
    class = "reference-entry",
    if (!is.null(status) && nzchar(status)) {
      shiny::div(class = "reference-status", status)
    },
    shiny::div(
      class = "reference-citation",
      shiny::span(class = "reference-authors", authors),
      if (!is.null(year) && nzchar(year)) {
        shiny::span(class = "reference-year", paste0(" (", year, ")."))
      },
      shiny::span(class = "reference-title", title_node),
      if (!is.null(source) && nzchar(source)) {
        shiny::span(class = "reference-source", paste0(" ", source, "."))
      }
    ),
    if (!is.null(note) && nzchar(note)) {
      shiny::div(class = "reference-note", note)
    }
  )
}

strategy_label_choices <- function(strategy_info) {
  labels <- vapply(strategy_info, `[[`, character(1), "label")
  stats::setNames(unname(labels), unname(labels))
}

plot_download_ui <- function(
  download_id,
  label = "Download PNG",
  note = NULL,
  legend_toggle_id = NULL,
  legend_label = "Show legend",
  title_toggle_id = NULL,
  title_label = "Show title",
  extra_toggle_id = NULL,
  extra_toggle_label = NULL,
  extra_toggle_value = FALSE
) {
  shiny::div(
    class = "plot-meta-row",
    if (!is.null(note) && nzchar(note)) {
      shiny::div(class = "plot-meta-note", note)
    },
    shiny::div(
      class = "download-row",
      if (!is.null(extra_toggle_id) && nzchar(extra_toggle_id)) {
        shiny::div(
          class = "legend-toggle",
          shiny::checkboxInput(
            extra_toggle_id,
            extra_toggle_label %||% extra_toggle_id,
            value = isTRUE(extra_toggle_value)
          )
        )
      },
      if (!is.null(legend_toggle_id) && nzchar(legend_toggle_id)) {
        shiny::div(
          class = "legend-toggle",
          shiny::checkboxInput(legend_toggle_id, legend_label, value = TRUE)
        )
      },
      if (!is.null(title_toggle_id) && nzchar(title_toggle_id)) {
        shiny::div(
          class = "legend-toggle",
          shiny::checkboxInput(title_toggle_id, title_label, value = TRUE)
        )
      },
      shiny::downloadButton(download_id, label)
    )
  )
}

app_ui <- function() {
  copy <- app_copy()
  strategy_info <- copy$strategy_notes
  strategy_choices <- strategy_label_choices(strategy_info)
  figure_guides <- copy$figure_guides
  references <- copy$references

  shiny::fluidPage(
    theme = bslib::bs_theme(
      version = 5,
      bg = "#f4f7f3",
      fg = "#203238",
      primary = "#468181",
      secondary = "#9abbb4"
    ),
    shiny::tags$head(
      shiny::tags$title("SWaNs"),
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      shiny::includeCSS("www/styles.css")
    ),
    shiny::div(
      class = "ambient-layer",
      shiny::div(class = "orb orb-a"),
      shiny::div(class = "orb orb-b"),
      shiny::div(class = "orb orb-c")
    ),
    shiny::div(
      class = "page-shell",
      shiny::div(
        class = "hero-panel",
        shiny::div(class = "hero-eyebrow", copy$hero$eyebrow),
        shiny::h1(class = "hero-title", copy$hero$title),
        shiny::p(class = "hero-subtitle", copy$hero$subtitle),
        shiny::div(
          class = "hero-grid",
          metric_card_output("Validated nodes", "hero_nodes", "All nodes in the rooted tree"),
          metric_card_output("Feasible sources", "hero_sources", "Nodes with positive prior weight"),
          metric_card_output("Outlet root", "hero_root", "Current root after validation"),
          metric_card_output(
            "Recommended range of the number of samplers",
            "hero_testers",
            "Derived from marginal gain in the number of sampling cycles per additional expected sample"
          )
        )
      ),
      shiny::tabsetPanel(
        id = "main_tabs",
        type = "tabs",
        shiny::tabPanel(
          "Overview",
          shiny::div(
            class = "tab-shell",
            shiny::fluidRow(
              shiny::column(
                width = 7,
                content_card(
                  copy$overview$title,
                  lapply(copy$overview$paragraphs, shiny::p),
                  shiny::div(class = "section-divider"),
                  bullet_list(copy$overview$workflow)
                )
              ),
              shiny::column(
                width = 5,
                content_card(
                  copy$uploads$title,
                  shiny::h4("Sewer network file"),
                  bullet_list(copy$uploads$network),
                  shiny::h4("Weight file"),
                  bullet_list(copy$uploads$weights),
                  shiny::div(
                    class = "download-row",
                    shiny::downloadButton("download_adjacency_template", "100-node adjacency example"),
                    shiny::downloadButton("download_weight_template", "100-node weight example")
                  )
                )
              )
            ),
            content_card(
              copy$comparison$title,
              lapply(copy$comparison$paragraphs, shiny::p),
              shiny::div(
                class = "strategy-grid",
                lapply(names(strategy_info), function(id) {
                  strategy_card(id, strategy_info[[id]])
                })
              )
            )
          )
        ),
        shiny::tabPanel(
          "Data & Validation",
          shiny::div(
            class = "tab-shell",
            shiny::fluidRow(
              class = "comparison-top-row",
              shiny::column(
                width = 4,
                content_card(
                  "Data source",
                  shiny::radioButtons(
                    "data_mode",
                    label = NULL,
                    choices = c(
                      "Built-in 100-node sewer network example (adjacency matrix)" = "demo",
                      "Upload my own files" = "upload"
                    ),
                    selected = "demo"
                  ),
                  shiny::conditionalPanel(
                    "input.data_mode === 'upload'",
                    shiny::fileInput(
                      "network_file",
                      "Sewer network adjacency matrix",
                      accept = c(".csv", ".xlsx", ".xls", ".txt")
                    ),
                    shiny::p(
                      class = "input-hint",
                      "Convention: a value of 1 in row A and column B means that wastewater flows from A to B."
                    ),
                    shiny::fileInput(
                      "weights_file",
                      "Weights",
                      accept = c(".csv", ".xlsx", ".xls", ".txt")
                    )
                  ),
                  shiny::actionButton("load_data", "Validate and load data", class = "primary-action")
                ),
                content_card(
                  "Validation status",
                  shiny::uiOutput("validation_ui")
                )
              ),
              shiny::column(
                width = 8,
                content_card(
                  "Sewer network overview",
                  shiny::plotOutput("network_plot", height = "580px"),
                  reading_guide(
                    figure_guides$network_overview$title,
                    figure_guides$network_overview$items
                  ),
                  plot_download_ui(
                    "download_network_plot",
                    title_toggle_id = "show_network_plot_title"
                  )
                ),
                shiny::fluidRow(
                  shiny::column(
                    width = 6,
                    content_card(
                      "Adjacency preview",
                      shiny::uiOutput("network_preview")
                    )
                  ),
                  shiny::column(
                    width = 6,
                    content_card(
                      "Weight preview",
                      shiny::uiOutput("weights_preview")
                    )
                  )
                )
              )
            )
          )
        ),
        shiny::tabPanel(
          "Simulation",
          shiny::div(
            class = "tab-shell",
            shiny::fluidRow(
              shiny::column(
                width = 4,
                content_card(
                  "Sequential Sampling Guidance",
                  shiny::selectInput(
                    "guide_strategy",
                    "Strategy for next-node recommendations",
                    choices = strategy_choices,
                    selected = strategy_info$nary_split$label
                  ),
                  shiny::numericInput(
                    "guide_testers",
                    "Number of samplers",
                    value = 3,
                    min = 1,
                    step = 1
                  ),
                  shiny::uiOutput("guided_status_ui"),
                  shiny::uiOutput("guided_recommendation_ui"),
                  shiny::uiOutput("guided_positive_ui"),
                  shiny::div(
                    class = "download-row",
                    shiny::actionButton("guided_apply", "Apply cycle result", class = "primary-action"),
                    shiny::actionButton("guided_reset", "Restart guided session")
                  )
                )
              ),
              shiny::column(
                width = 8,
                content_card(
                  "Sewer Network Guidance",
                  shiny::plotOutput("guided_network_plot", height = "560px"),
                  reading_guide(
                    figure_guides$guided_network$title,
                    figure_guides$guided_network$items
                  ),
                  plot_download_ui("download_guided_network_plot"),
                  shiny::uiOutput("guided_click_info"),
                  shiny::uiOutput("guided_history_table")
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 4,
                content_card(
                  "Single-source run",
                  shiny::selectInput(
                    "sim_strategy",
                    "Strategy",
                    choices = strategy_choices,
                    selected = strategy_info$nary_split$label
                  ),
                  shiny::numericInput("sim_testers", "Number of samplers", value = 3, min = 1, step = 1),
                  shiny::selectInput("source_node", "Source node", choices = NULL),
                  shiny::uiOutput("single_run_cards")
                )
              ),
              shiny::column(
                width = 8,
                content_card(
                  "Search history",
                  shiny::plotOutput("single_run_history_plot", height = "320px"),
                  plot_download_ui("download_single_run_history_plot"),
                  shiny::uiOutput("single_run_table")
                )
              )
            )
          )
        ),
        shiny::tabPanel(
          "Strategy Comparison",
          shiny::div(
            class = "tab-shell",
            shiny::fluidRow(
              class = "comparison-top-row",
              shiny::column(
                width = 4,
                class = "comparison-left-column",
                content_card(
                  "Comparison controls",
                  shiny::checkboxGroupInput(
                    "compare_strategies",
                    "Strategies",
                    choices = strategy_choices,
                    selected = unname(strategy_choices)
                  ),
                  shiny::sliderInput(
                    "tester_range",
                    "Range of the number of samplers",
                    min = 1,
                    max = 10,
                    value = c(1, 3),
                    step = 1
                  ),
                  shiny::numericInput(
                    "cdf_testers",
                    "CDF view for the number of samplers",
                    value = 3,
                    min = 1,
                    step = 1
                  ),
                  shiny::actionButton("run_comparison", "Refresh comparison", class = "primary-action"),
                  shiny::uiOutput("comparison_note")
                ),
                content_card(
                  copy$parallel_recommendation$title,
                  lapply(copy$parallel_recommendation$paragraphs, shiny::p),
                  shiny::uiOutput("parallel_recommendation_ui"),
                  shiny::uiOutput("parallel_efficiency_table")
                ),
                content_card(
                  figure_guides$comparison$title,
                  bullet_list(figure_guides$comparison$items),
                  class = "figure-guide-card"
                )
              ),
              shiny::column(
                width = 8,
                class = "comparison-right-column",
                content_card(
                  "Probability-weighted expected number of sampling cycles",
                  shiny::plotOutput("cycles_plot", height = "280px"),
                  plot_download_ui(
                    "download_cycles_plot",
                    legend_toggle_id = "show_cycles_plot_legend",
                    title_toggle_id = "show_cycles_plot_title"
                  )
                ),
                content_card(
                  "Probability-weighted expected number of samples",
                  shiny::plotOutput("tests_plot", height = "280px"),
                  plot_download_ui(
                    "download_tests_plot",
                    legend_toggle_id = "show_tests_plot_legend",
                    title_toggle_id = "show_tests_plot_title"
                  )
                ),
                content_card(
                  "Unweighted boxplots of the number of sampling cycles",
                  shiny::plotOutput("cycles_boxplot", height = "350px"),
                  plot_download_ui(
                    "download_cycles_boxplot",
                    note = "Each box summarises one deterministic run per feasible source node.",
                    legend_toggle_id = "show_cycles_boxplot_legend",
                    title_toggle_id = "show_cycles_boxplot_title"
                  )
                ),
                content_card(
                  "Unweighted boxplots of the number of samples",
                  shiny::plotOutput("tests_boxplot", height = "350px"),
                  plot_download_ui(
                    "download_tests_boxplot",
                    note = "Each value is the total number of samples actually required until the source is identified.",
                    legend_toggle_id = "show_tests_boxplot_legend",
                    title_toggle_id = "show_tests_boxplot_title"
                  )
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                content_card(
                  "Distribution of the number of sampling cycles",
                  shiny::plotOutput("cdf_plot", height = "340px"),
                  plot_download_ui(
                    "download_cdf_plot",
                    legend_toggle_id = "show_cdf_plot_legend",
                    title_toggle_id = "show_cdf_plot_title"
                  )
                )
              ),
              shiny::column(
                width = 6,
                content_card(
                  "Time-resource trade-off",
                  shiny::plotOutput("pareto_plot", height = "340px"),
                  plot_download_ui(
                    "download_pareto_plot",
                    legend_toggle_id = "show_pareto_plot_legend",
                    title_toggle_id = "show_pareto_plot_title",
                    extra_toggle_id = "show_all_pareto_sampler_labels",
                    extra_toggle_label = "Show all sampler labels"
                  )
                )
              )
            ),
            content_card(
              "Summary table",
              shiny::uiOutput("comparison_table"),
              shiny::div(
                class = "download-row",
                shiny::downloadButton("download_comparison_table", "Download Excel")
              )
            )
          )
        ),
        shiny::tabPanel(
          "Assumptions & Limitations",
          shiny::div(
            class = "tab-shell",
            shiny::fluidRow(
              shiny::column(
                width = 6,
                content_card(
                  copy$assumptions$title,
                  bullet_list(copy$assumptions$items)
                )
              ),
              shiny::column(
                width = 6,
                content_card(
                  copy$limitations$title,
                  bullet_list(copy$limitations$items)
                )
              )
            )
          )
        ),
        shiny::tabPanel(
          "References",
          shiny::div(
            class = "tab-shell",
            shiny::fluidRow(
              shiny::column(
                width = 7,
                content_card(
                  references$title,
                  shiny::p(class = "references-intro", references$intro),
                  shiny::div(
                    class = "reference-block",
                    shiny::div(class = "reference-block-label", references$manuscript$label),
                    reference_entry(
                      authors = references$manuscript$authors,
                      title = references$manuscript$title,
                      note = references$manuscript$note,
                      status = references$manuscript$status
                    )
                  ),
                  shiny::div(class = "section-divider"),
                  shiny::div(class = "reference-block-label", "Software environment"),
                  shiny::div(
                    class = "reference-list",
                    lapply(references$software, function(ref) {
                      reference_entry(
                        authors = ref$authors,
                        year = ref$year,
                        title = ref$title,
                        source = ref$source,
                        link = ref$link
                      )
                    })
                  )
                )
              ),
              shiny::column(
                width = 5,
                content_card(
                  references$correspondence$title,
                  shiny::div(
                    class = "correspondence-card",
                    shiny::div(class = "correspondence-name", references$correspondence$person),
                    shiny::div(class = "correspondence-role", references$correspondence$role),
                    shiny::div(
                      class = "correspondence-email",
                      shiny::tags$a(
                        href = paste0("mailto:", references$correspondence$email),
                        references$correspondence$email
                      )
                    ),
                    shiny::p(class = "correspondence-note", references$correspondence$note)
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
