`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    y
  } else {
    x
  }
}

guided_none_value <- "__none__"

label_to_strategy_id <- function(label) {
  info <- strategy_catalog()
  if (label %in% names(info)) {
    return(label)
  }

  matches <- names(info)[vapply(
    info,
    function(x) {
      identical(x$label, label) || identical(x$full_name %||% NULL, label)
    },
    logical(1)
  )]

  if (!length(matches)) {
    stop("Unknown strategy: ", label, call. = FALSE)
  }

  matches[[1]]
}

format_number_ui <- function(value, digits = 0, suffix = NULL) {
  formatted <- if (is.numeric(value)) {
    format(round(value, digits), nsmall = digits, trim = TRUE)
  } else {
    as.character(value)
  }

  shiny::div(
    class = "metric-value",
    paste0(formatted, suffix %||% "")
  )
}

format_probability_value <- function(value, digits = 4) {
  if (length(value) > 1L) {
    return(vapply(value, function(single_value) format_probability_value(single_value, digits), character(1)))
  }

  if (is.na(value)) {
    return(NA_character_)
  }

  if (!is.finite(value) || identical(value, 0)) {
    return(as.character(value))
  }

  threshold <- 10^(-digits)
  if (abs(value) < threshold) {
    return(paste0("<", formatC(threshold, format = "f", digits = digits)))
  }

  formatC(value, format = "f", digits = digits)
}

describe_upload_source <- function(file_input = NULL, fallback_path = NULL, role = "File") {
  label <- NULL

  if (is.list(file_input) && !is.null(file_input$name) && nzchar(file_input$name)) {
    label <- file_input$name
  }

  if (is.null(label) && !is.null(fallback_path) && nzchar(fallback_path)) {
    label <- basename(fallback_path)
  }

  if (is.null(label) || !nzchar(label)) {
    return(role)
  }

  paste0(role, " `", label, "`")
}

cleanup_paths <- function(paths) {
  paths <- unique(stats::na.omit(as.character(paths)))
  paths <- paths[nzchar(paths)]
  if (!length(paths)) {
    return(invisible(NULL))
  }

  existing <- file.exists(paths)
  if (any(existing)) {
    unlink(paths[existing], recursive = TRUE, force = TRUE)
  }

  invisible(NULL)
}

file_input_signature <- function(file_input) {
  if (!is.list(file_input) || is.null(file_input$datapath) || !nzchar(file_input$datapath)) {
    return(NULL)
  }

  list(
    name = file_input$name %||% "",
    size = file_input$size %||% NA_real_,
    datapath = file_input$datapath
  )
}

stage_uploaded_file <- function(file_input, target_dir, prefix = "upload") {
  if (!is.list(file_input) || is.null(file_input$datapath) || !nzchar(file_input$datapath)) {
    stop("No uploaded file is available for staging.", call. = FALSE)
  }

  if (!file.exists(file_input$datapath)) {
    stop("The uploaded file is no longer available. Please upload it again.", call. = FALSE)
  }

  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  extension <- tools::file_ext(file_input$name %||% "")
  suffix <- if (nzchar(extension)) paste0(".", extension) else ""
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%OS3")
  timestamp <- gsub("[^0-9]", "", timestamp)
  staged_path <- file.path(
    target_dir,
    paste0(prefix, "_", timestamp, "_", sprintf("%06d", sample.int(999999, 1)), suffix)
  )

  copied <- file.copy(file_input$datapath, staged_path, overwrite = TRUE, copy.mode = TRUE)
  if (!isTRUE(copied) || !file.exists(staged_path)) {
    stop("The uploaded file could not be copied into temporary session storage.", call. = FALSE)
  }

  cleanup_paths(file_input$datapath)
  normalizePath(staged_path, winslash = "/", mustWork = TRUE)
}

make_validation_error <- function(
  summary,
  details = NULL,
  hints = character(0),
  can_transpose_network = FALSE
) {
  list(
    summary = summary,
    details = details,
    hints = hints[nzchar(hints)],
    can_transpose_network = isTRUE(can_transpose_network)
  )
}

build_validation_error <- function(
  message,
  network_file = NULL,
  weights_file = NULL
) {
  lower_message <- tolower(message)
  network_label <- describe_upload_source(network_file, role = "Sewer network file")
  weights_label <- describe_upload_source(weights_file, role = "Weight file")

  if (grepl("invalid utf-8", lower_message, fixed = TRUE)) {
    return(make_validation_error(
      summary = "One of the uploaded files contains text that could not be rendered safely.",
      details = "The file appears to use a legacy text encoding or damaged special characters.",
      hints = c(
        "Save text-based sewer network files as UTF-8 CSV when possible.",
        "If the file comes from Excel, re-save it as a new CSV or XLSX file before uploading again."
      )
    ))
  }

  if (grepl("unsupported file type", lower_message, fixed = TRUE)) {
    return(make_validation_error(
      summary = "One of the uploaded files uses an unsupported file type.",
      details = message,
      hints = c(
        "Supported file types are CSV, TXT, XLSX, and XLS.",
        "Prefer CSV or XLSX for the most robust upload behavior."
      )
    ))
  }

  if (grepl("supported delimiter", lower_message, fixed = TRUE)) {
    return(make_validation_error(
      summary = paste(network_label, "could not be parsed as a delimited text table."),
      details = "The file could not be read with comma, semicolon, tab, or pipe separators.",
      hints = c(
        "Check whether the file is a plain-text export with a consistent delimiter.",
        "If possible, re-save the sewer network file as CSV or upload it as XLSX."
      )
    ))
  }

  if (grepl("weight file", lower_message, fixed = TRUE) ||
      grepl("positive weight", lower_message, fixed = TRUE) ||
      grepl("weights must be non-negative", lower_message, fixed = TRUE)) {
    return(make_validation_error(
      summary = paste(weights_label, "could not be interpreted as a valid weight table."),
      details = message,
      hints = c(
        "Use node identifiers in the first column and non-negative numeric weights in the second column.",
        "Accepted header names include `node`, `EZG`, or `EZG_full` for nodes and `weight`, `pop`, or `population` for weights.",
        "Remove notes, empty rows, or text values from the numeric weight column."
      )
    ))
  }

  if (grepl("could not be interpreted as either an adjacency matrix or an edge list", lower_message, fixed = TRUE)) {
    return(make_validation_error(
      summary = paste(network_label, "does not match the expected adjacency-matrix structure."),
      details = message,
      hints = c(
        "Check whether the uploaded file really is an adjacency matrix with row and column labels.",
        "For text files, remove title rows and keep the matrix table starting in the first row.",
        "If necessary, re-save the sewer network file as a clean CSV or XLSX export."
      )
    ))
  }

  if (grepl("appears to use the reverse direction", lower_message, fixed = TRUE)) {
    return(make_validation_error(
      summary = paste(network_label, "appears to encode the flow direction in reverse."),
      details = "In this app, a value of 1 in row A and column B must mean that wastewater flows from A to B.",
      hints = c(
        "Rows represent the source of flow and columns represent the target of flow.",
        "If your current matrix uses the opposite convention, transpose or re-export it before uploading again."
      ),
      can_transpose_network = TRUE
    ))
  }

  if (grepl("adjacency matrix", lower_message, fixed = TRUE) ||
      grepl("row and column labels", lower_message, fixed = TRUE)) {
    return(make_validation_error(
      summary = paste(network_label, "could not be interpreted as an adjacency matrix."),
      details = message,
      hints = c(
        "Place node labels in the first row and first column of the matrix.",
        "Ensure the matrix body contains only numeric edge indicators such as 0 and 1.",
        "Use the convention row = flow source and column = flow target."
      )
    ))
  }

  if (grepl("multiple parents", lower_message, fixed = TRUE) ||
      grepl("exactly one root", lower_message, fixed = TRUE) ||
      grepl("directed cycle", lower_message, fixed = TRUE) ||
      grepl("disconnected from the root", lower_message, fixed = TRUE) ||
      grepl("self-loops", lower_message, fixed = TRUE) ||
      grepl("rooted tree", lower_message, fixed = TRUE)) {
    return(make_validation_error(
      summary = paste(network_label, "does not form a valid rooted tree."),
      details = message,
      hints = c(
        "Each node may have at most one downstream parent.",
        "Exactly one outlet root must remain after validation.",
        "The sewer network must be connected and acyclic."
      )
    ))
  }

  make_validation_error(
    summary = "The uploaded files could not be validated.",
    details = message,
    hints = c(
      "Check whether the sewer network file and the weight file describe the same node set.",
      "Use the convention row = flow source and column = flow target.",
      "Prefer CSV or XLSX exports without extra title rows or comments."
    )
  )
}

guided_positive_choices <- function(recommended_nodes) {
  c(
    "None (all sampled nodes were negative)" = guided_none_value,
    format_identifier_choices(recommended_nodes)
  )
}

normalize_guided_positive_selection <- function(selection, previous_selection, recommended_nodes) {
  selection <- unique(selection %||% character(0))
  previous_selection <- unique(previous_selection %||% character(0))
  allowed_values <- c(recommended_nodes, guided_none_value)
  selection <- intersect(selection, allowed_values)

  if (!(guided_none_value %in% selection)) {
    return(sort(selection))
  }

  selected_nodes <- setdiff(selection, guided_none_value)
  if (!length(selected_nodes)) {
    return(guided_none_value)
  }

  if (guided_none_value %in% previous_selection) {
    sort(selected_nodes)
  } else {
    guided_none_value
  }
}

build_table_ui <- function(df, numeric_columns = NULL) {
  if (is.null(df)) {
    return(NULL)
  }

  if (is.null(numeric_columns)) {
    numeric_columns <- names(df)[vapply(df, is.numeric, logical(1))]
  }

  numeric_columns <- intersect(numeric_columns, names(df))

  format_table_header_label <- function(label) {
    formatted <- label
    formatted <- gsub(" of the number of ", " of\u00A0the\u00A0number\u00A0of ", formatted, fixed = TRUE)
    formatted <- gsub(" of the ", " of\u00A0the ", formatted, fixed = TRUE)
    formatted <- gsub(" number of ", " number\u00A0of ", formatted, fixed = TRUE)
    formatted
  }

  shiny::div(
    class = "table-responsive",
    shiny::tags$table(
      class = "table table-striped table-hover data-table",
      shiny::tags$thead(
        shiny::tags$tr(
          lapply(names(df), function(column_name) {
            shiny::tags$th(
              class = if (column_name %in% numeric_columns) "table-number" else NULL,
              format_table_header_label(column_name)
            )
          })
        )
      ),
      shiny::tags$tbody(
        lapply(seq_len(nrow(df)), function(row_index) {
          shiny::tags$tr(
            lapply(names(df), function(column_name) {
              cell_value <- df[[column_name]][[row_index]]
              shiny::tags$td(
                class = if (column_name %in% numeric_columns) "table-number" else NULL,
                if (is.na(cell_value)) "" else as.character(cell_value)
              )
            })
          )
        })
      )
    )
  )
}

build_adjacency_preview_df <- function(tree, max_nodes = 7L) {
  preview_nodes <- unique(c(tree$root, utils::head(setdiff(tree$nodes, tree$root), max_nodes - 1L)))
  edge_lookup <- paste(tree$edges$parent, tree$edges$child, sep = "->")
  matrix_values <- matrix(
    0L,
    nrow = length(preview_nodes),
    ncol = length(preview_nodes),
    dimnames = list(preview_nodes, preview_nodes)
  )

  for (parent_node in preview_nodes) {
    for (child_node in preview_nodes) {
      if (paste(parent_node, child_node, sep = "->") %in% edge_lookup) {
        matrix_values[[parent_node, child_node]] <- 1L
      }
    }
  }

  preview_df <- data.frame(
    Node = format_identifier_label(rownames(matrix_values)),
    as.data.frame(matrix_values, check.names = FALSE),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(preview_df)[-1] <- format_identifier_label(colnames(matrix_values))
  preview_df
}

app_server <- function(input, output, session) {
  demo_files <- swans_demo_template_filenames()
  session_temp_dir <- file.path(tempdir(), paste0("swans_session_", session$token))
  guided_values <- shiny::reactiveValues(
    state = NULL,
    pending_positive = character(0)
  )
  staged_uploads <- shiny::reactiveValues(
    network_path = NULL,
    weights_path = NULL,
    network_signature = NULL,
    weights_signature = NULL
  )
  network_transpose_requested <- shiny::reactiveVal(FALSE)
  model_load_token <- shiny::reactiveVal(0L)

  cleanup_staged_uploads <- function(slots = c("network", "weights")) {
    slots <- match.arg(slots, choices = c("network", "weights"), several.ok = TRUE)

    if ("network" %in% slots) {
      cleanup_paths(staged_uploads$network_path)
      staged_uploads$network_path <- NULL
      staged_uploads$network_signature <- NULL
    }

    if ("weights" %in% slots) {
      cleanup_paths(staged_uploads$weights_path)
      staged_uploads$weights_path <- NULL
      staged_uploads$weights_signature <- NULL
    }

    invisible(NULL)
  }

  ensure_staged_upload <- function(file_input, slot = c("network", "weights")) {
    slot <- match.arg(slot)
    signature <- file_input_signature(file_input)

    if (is.null(signature)) {
      stop("No uploaded file is available for processing.", call. = FALSE)
    }

    path_field <- paste0(slot, "_path")
    signature_field <- paste0(slot, "_signature")
    current_path <- staged_uploads[[path_field]]
    current_signature <- staged_uploads[[signature_field]]

    if (identical(current_signature, signature) && !is.null(current_path) && file.exists(current_path)) {
      return(current_path)
    }

    cleanup_staged_uploads(slot)
    staged_path <- stage_uploaded_file(file_input, session_temp_dir, prefix = slot)
    staged_uploads[[path_field]] <- staged_path
    staged_uploads[[signature_field]] <- signature
    staged_path
  }

  session$onSessionEnded(function() {
    cleanup_staged_uploads(c("network", "weights"))
    cleanup_paths(session_temp_dir)
  })

  safe_model_build <- function(
    expr,
    network_file = NULL,
    weights_file = NULL
  ) {
    tryCatch(
      list(model = expr, error = NULL),
      error = function(e) {
        list(
          model = NULL,
          error = build_validation_error(
            message = conditionMessage(e),
            network_file = network_file,
            weights_file = weights_file
          )
        )
      }
    )
  }

  observeEvent(list(input$network_file, input$data_mode), {
    network_transpose_requested(FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$network_file, {
    cleanup_staged_uploads("network")
  }, ignoreInit = TRUE)

  observeEvent(input$weights_file, {
    cleanup_staged_uploads("weights")
  }, ignoreInit = TRUE)

  observeEvent(input$data_mode, {
    if (identical(input$data_mode, "demo")) {
      cleanup_staged_uploads(c("network", "weights"))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$load_data, {
    model_load_token(model_load_token() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$transpose_network_matrix, {
    network_transpose_requested(TRUE)
    model_load_token(model_load_token() + 1L)
  }, ignoreInit = TRUE)

  current_bundle <- shiny::eventReactive(model_load_token(), ignoreNULL = FALSE, {
    shiny::withProgress(message = "Preparing the sampling model", value = 0.1, {
      if (identical(input$data_mode, "demo")) {
        safe_model_build(
          build_demo_model(),
          network_file = list(name = basename(demo_files$network)),
          weights_file = list(name = basename(demo_files$weights))
        )
      } else {
        shiny::req(input$network_file, input$weights_file)
        staged_network_path <- ensure_staged_upload(input$network_file, "network")
        staged_weights_path <- ensure_staged_upload(input$weights_file, "weights")
        safe_model_build(
          build_model_from_files(
            network_path = staged_network_path,
            weights_path = staged_weights_path,
            transpose_network = network_transpose_requested()
          ),
          network_file = input$network_file,
          weights_file = input$weights_file
        )
      }
    })
  })

  initialize_guided_session <- function() {
    bundle <- current_bundle()
    if (is.null(bundle$model)) {
      guided_values$state <- NULL
      guided_values$pending_positive <- character(0)
      return(invisible(NULL))
    }

    guided_values$state <- initialize_guided_state(
      tree = bundle$model,
      strategy_id = label_to_strategy_id(input$guide_strategy),
      testers = input$guide_testers
    )
    guided_values$pending_positive <- character(0)
    invisible(NULL)
  }

  guided_config_changed <- shiny::reactive({
    state <- guided_values$state
    if (is.null(state)) {
      return(FALSE)
    }

    !identical(state$strategy_id, label_to_strategy_id(input$guide_strategy)) ||
      !identical(state$testers, max(1L, as.integer(input$guide_testers)))
  })

  observeEvent(current_bundle(), {
    bundle <- current_bundle()
    if (is.null(bundle$model)) {
      guided_values$state <- NULL
      return()
    }

    shiny::updateSelectInput(
      session,
      "source_node",
      choices = format_identifier_choices(bundle$model$source_nodes),
      selected = bundle$model$source_nodes[[1]]
    )

    initialize_guided_session()
  }, ignoreNULL = FALSE)

  observeEvent(input$guided_reset, {
    initialize_guided_session()
  })

  observeEvent(list(input$guide_strategy, input$guide_testers), {
    state <- guided_values$state
    if (is.null(state) || state$cycle > 0) {
      return()
    }

    initialize_guided_session()
  }, ignoreNULL = FALSE)

  observeEvent(input$guided_positive_nodes, {
    state <- guided_values$state
    recommended_nodes <- if (is.null(state)) character(0) else state$recommended_nodes
    normalized <- normalize_guided_positive_selection(
      selection = input$guided_positive_nodes,
      previous_selection = guided_values$pending_positive,
      recommended_nodes = recommended_nodes
    )

    guided_values$pending_positive <- normalized

    input_selection <- sort(unique(input$guided_positive_nodes %||% character(0)))
    if (!identical(sort(normalized), input_selection)) {
      shiny::updateCheckboxGroupInput(
        session,
        "guided_positive_nodes",
        choices = guided_positive_choices(recommended_nodes),
        selected = normalized
      )
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$guided_apply, {
    state <- guided_values$state
    bundle <- current_bundle()
    shiny::req(state, bundle$model)

    if (guided_config_changed() && state$cycle > 0) {
      shiny::showNotification(
        "The strategy or the number of samplers changed. Restart the guided session to apply them.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    if (guided_config_changed()) {
      initialize_guided_session()
      shiny::showNotification(
        "Guided session refreshed to the new settings. Please review the updated recommendations before applying results.",
        type = "message"
      )
      return(invisible(NULL))
    }

    guided_values$state <- advance_guided_state(
      tree = bundle$model,
      state = state,
      positive_nodes = setdiff(guided_values$pending_positive, guided_none_value)
    )
    guided_values$pending_positive <- character(0)
  })

  comparison_bundle <- shiny::eventReactive(input$run_comparison, ignoreNULL = FALSE, {
    bundle <- current_bundle()
    shiny::req(bundle$model)

    selected_labels <- input$compare_strategies
    if (!length(selected_labels)) {
      stop("Please select at least one strategy.", call. = FALSE)
    }

    selected_ids <- vapply(selected_labels, label_to_strategy_id, character(1))
    tester_values <- seq.int(input$tester_range[[1]], input$tester_range[[2]])
    last_progress <- 0

    shiny::withProgress(message = "Running deterministic comparisons", value = 0, {
      compare_strategies(
        tree = bundle$model,
        testers_range = tester_values,
        strategies = selected_ids,
        progress = function(value, detail) {
          shiny::incProgress(amount = value - last_progress, detail = detail)
          last_progress <<- value
        }
      )
    })
  })

  comparison_summary_df <- shiny::reactive({
    result <- comparison_bundle()
    shiny::req(result)

    table_df <- add_marginal_efficiency(result$summary)
    table_df$strategy <- vapply(
      table_df$strategy,
      function(id) strategy_catalog()[[id]]$label,
      character(1)
    )

    table_df$expected_cycles <- round(table_df$expected_cycles, 3)
    table_df$mean_cycles <- round(table_df$mean_cycles, 3)
    table_df$min_cycles <- round(table_df$min_cycles, 3)
    table_df$lower_quartile_cycles <- round(table_df$lower_quartile_cycles, 3)
    table_df$median_cycles <- round(table_df$median_cycles, 3)
    table_df$upper_quartile_cycles <- round(table_df$upper_quartile_cycles, 3)
    table_df$expected_total_tests <- round(table_df$expected_total_tests, 3)
    table_df$mean_total_tests <- round(table_df$mean_total_tests, 3)
    table_df$min_total_tests <- round(table_df$min_total_tests, 3)
    table_df$lower_quartile_total_tests <- round(table_df$lower_quartile_total_tests, 3)
    table_df$median_total_tests <- round(table_df$median_total_tests, 3)
    table_df$upper_quartile_total_tests <- round(table_df$upper_quartile_total_tests, 3)
    table_df$marginal_efficiency <- vapply(
      table_df$marginal_efficiency,
      function(value) {
        if (is.na(value)) {
          return("")
        }
        if (is.infinite(value)) {
          return("dominant")
        }
        format(round(value, 3), nsmall = 3, trim = TRUE)
      },
      character(1)
    )

    names(table_df) <- c(
      "Strategy",
      "Number of samplers",
      "Expected number of sampling cycles",
      "Mean number of sampling cycles",
      "Minimum number of sampling cycles",
      "Lower quartile of the number of sampling cycles",
      "Median number of sampling cycles",
      "Upper quartile of the number of sampling cycles",
      "Maximum number of sampling cycles",
      "Expected number of samples",
      "Mean number of samples",
      "Minimum number of samples",
      "Lower quartile of the number of samples",
      "Median number of samples",
      "Upper quartile of the number of samples",
      "Maximum number of samples",
      "Marginal efficiency"
    )

    table_df
  })

  parallel_recommendation <- shiny::reactive({
    result <- comparison_bundle()
    shiny::req(result)
    estimate_parallel_recommendation(result$summary)
  })

  observe({
    min_tester <- input$tester_range[[1]]
    max_tester <- input$tester_range[[2]]
    current <- input$cdf_testers %||% min_tester
    current <- min(max(current, min_tester), max_tester)
    shiny::updateNumericInput(
      session,
      "cdf_testers",
      value = current,
      min = min_tester,
      max = max_tester
    )
  })

  single_run <- shiny::reactive({
    bundle <- current_bundle()
    shiny::req(bundle$model, input$source_node)

    simulate_search(
      tree = bundle$model,
      source_node = input$source_node,
      strategy_id = label_to_strategy_id(input$sim_strategy),
      testers = input$sim_testers,
      keep_history = TRUE
    )
  })

  register_plot_download <- function(output_id, filename, width = 1600, height = 900, plotting_code) {
    output[[output_id]] <- shiny::downloadHandler(
      filename = function() filename,
      content = function(file) {
        grDevices::png(file, width = width, height = height, res = 220, pointsize = 14)
        on.exit(grDevices::dev.off(), add = TRUE)
        plotting_code()
      }
    )
  }

  output$hero_nodes <- shiny::renderUI({
    bundle <- current_bundle()
    if (is.null(bundle$model)) {
      return(format_number_ui("--"))
    }
    format_number_ui(bundle$model$validation$summary$node_count)
  })

  output$hero_sources <- shiny::renderUI({
    bundle <- current_bundle()
    if (is.null(bundle$model)) {
      return(format_number_ui("--"))
    }
    format_number_ui(bundle$model$validation$summary$source_count)
  })

  output$hero_root <- shiny::renderUI({
    bundle <- current_bundle()
    if (is.null(bundle$model)) {
      return(format_number_ui("--"))
    }
    shiny::div(class = "metric-value metric-code", format_identifier_label(bundle$model$root))
  })

  output$hero_testers <- shiny::renderUI({
    recommendation <- try(parallel_recommendation(), silent = TRUE)
    if (inherits(recommendation, "try-error") || is.null(recommendation) || !isTRUE(recommendation$available)) {
      return(format_number_ui("--"))
    }

    shiny::div(
      class = "metric-value",
      recommendation$range_label
    )
  })

  output$validation_ui <- shiny::renderUI({
    bundle <- current_bundle()

    if (!is.null(bundle$error)) {
      error_info <- bundle$error
      if (is.character(error_info)) {
        error_info <- make_validation_error(summary = error_info)
      }

      return(
        shiny::div(
          class = "status-block status-error",
          shiny::strong("Validation failed."),
          shiny::p(error_info$summary),
          if (!is.null(error_info$details) && nzchar(error_info$details)) {
            shiny::p(
              shiny::strong("Details: "),
              error_info$details
            )
          },
          if (length(error_info$hints)) {
            shiny::tagList(
              shiny::strong("Please check:"),
              bullet_list(error_info$hints)
            )
          },
          if (isTRUE(error_info$can_transpose_network) && identical(input$data_mode, "upload")) {
            shiny::div(
              class = "validation-action-row",
              shiny::actionButton(
                "transpose_network_matrix",
                "Transpose matrix and validate again",
                class = "secondary-action"
              )
            )
          }
        )
      )
    }

    model <- bundle$model
    summary <- model$validation$summary
    warning_items <- model$validation$warnings
    if (isTRUE(model$network_meta$transposed_input)) {
      warning_items <- c(
        "The uploaded adjacency matrix was transposed by the app before validation because the original upload appeared to encode the flow direction in reverse.",
        warning_items
      )
    }

    shiny::tagList(
      shiny::div(
        class = "status-block status-success",
        shiny::strong("Sewer network validated."),
        shiny::p(
          paste(
            "Outlet root:", format_identifier_label(summary$root), "| Nodes:", summary$node_count,
            "| Edges:", summary$edge_count, "| Feasible sources:", summary$source_count
          )
        )
      ),
      if (length(warning_items)) {
        shiny::div(
          class = "status-block status-warning",
          shiny::strong("Validation notes"),
          bullet_list(warning_items)
        )
      }
    )
  })

  output$network_plot <- shiny::renderPlot({
    bundle <- current_bundle()
    shiny::req(bundle$model)
    plot_network_snapshot(
      bundle$model,
      show_title = isTRUE(input$show_network_plot_title %||% TRUE)
    )
  }, res = 96)

  output$network_preview <- shiny::renderUI({
    bundle <- current_bundle()
    shiny::req(bundle$model)

    preview_df <- build_adjacency_preview_df(bundle$model)
    build_table_ui(
      preview_df,
      numeric_columns = names(preview_df)[-1]
    )
  })

  output$weights_preview <- shiny::renderUI({
    bundle <- current_bundle()
    shiny::req(bundle$model)

    df <- data.frame(
      node = names(bundle$model$raw_weights),
      weight = unname(bundle$model$raw_weights),
      probability = unname(bundle$model$probabilities),
      stringsAsFactors = FALSE
    )
    df <- df[df[["weight"]] > 0, , drop = FALSE]
    df$node <- format_identifier_label(df$node)
    df$probability <- format_probability_value(df$probability)
    names(df) <- c("Node", "Weight", "Probability")
    build_table_ui(
      utils::head(df, 8),
      numeric_columns = c("Weight", "Probability")
    )
  })

  output$single_run_cards <- shiny::renderUI({
    run <- single_run()

    shiny::div(
      class = "mini-metrics",
      shiny::div(class = "mini-metric", shiny::span("Number of sampling cycles"), shiny::strong(run$cycles)),
      shiny::div(class = "mini-metric", shiny::span("Number of samples"), shiny::strong(run$total_tests)),
      shiny::div(class = "mini-metric", shiny::span("Identified"), shiny::strong(format_identifier_label(run$identified_node))),
      shiny::div(class = "mini-metric", shiny::span("Status"), shiny::strong(if (run$success) "resolved" else "review"))
    )
  })

  output$single_run_history_plot <- shiny::renderPlot({
    plot_search_history(single_run())
  }, res = 96)

  output$single_run_table <- shiny::renderUI({
    history <- single_run()$history
    if (!nrow(history)) {
      return(build_table_ui(data.frame(Note = "No additional sampling cycles were required.")))
    }

    history$remaining_probability <- format_probability_value(history$remaining_probability)
    history$tested_nodes <- format_identifier_sequence(history$tested_nodes)
    history$positive_nodes <- format_identifier_sequence(history$positive_nodes)
    history$focus_node <- format_identifier_label(history$focus_node)
    names(history) <- c(
      "Sampling cycle",
      "Sampled nodes",
      "Positive nodes",
      "Outcome",
      "Focus node",
      "Remaining candidates",
      "Remaining probability"
    )
    build_table_ui(
      history,
      numeric_columns = c("Sampling cycle", "Remaining candidates", "Remaining probability")
    )
  })

  output$guided_status_ui <- shiny::renderUI({
    state <- guided_values$state
    bundle <- current_bundle()

    if (!is.null(bundle$error)) {
      return(
        shiny::div(
          class = "status-block status-error",
          shiny::strong("Guided mode is unavailable until the data validate successfully.")
        )
      )
    }

    shiny::req(state)

    strategy_label <- strategy_catalog()[[state$strategy_id]]$label
    note_block <- NULL

    if (guided_config_changed() && state$cycle > 0) {
        note_block <- shiny::div(
          class = "status-block status-warning",
          shiny::strong("Pending configuration change"),
          shiny::p("Click 'Restart guided session' to apply the newly selected strategy or number of samplers.")
        )
      }

    shiny::tagList(
      shiny::div(
        class = "mini-metrics",
        shiny::div(class = "mini-metric", shiny::span("Number of sampling cycles"), shiny::strong(state$cycle)),
        shiny::div(class = "mini-metric", shiny::span("Strategy"), shiny::strong(strategy_label)),
        shiny::div(class = "mini-metric", shiny::span("Number of samplers"), shiny::strong(state$testers)),
        shiny::div(class = "mini-metric", shiny::span("Remaining sources"), shiny::strong(length(state$remaining_sources)))
      ),
      note_block
    )
  })

  output$guided_recommendation_ui <- shiny::renderUI({
    state <- guided_values$state
    shiny::req(state)

    if (isTRUE(state$inconsistent)) {
      return(
        shiny::div(
          class = "status-block status-error",
          shiny::strong("No weighted source remains."),
          shiny::p("The current combination of sample outcomes excludes every weighted source. Please check the uploaded weights or the reported sample results.")
        )
      )
    }

    if (isTRUE(state$done)) {
      return(
        shiny::div(
          class = "status-block status-success",
          shiny::strong("Source identified."),
          shiny::p(
            paste(
              "The only remaining weighted source candidate is",
              format_identifier_label(state$identified_source),
              "."
            )
          ),
          shiny::div(
            class = "recommendation-pills",
            shiny::span(
              class = "recommendation-pill is-final",
              format_identifier_label(state$identified_source)
            )
          )
        )
      )
    }

    shiny::div(
      class = "status-block status-info",
      shiny::strong("Recommended nodes for the next sampling cycle"),
      shiny::p("Collect samples at the highlighted nodes below. Use the checklist to mark positive results for this cycle. Every unselected sampled node is treated as negative and its full upstream subtree is excluded. If all sampled nodes are negative, select 'None'."),
      shiny::div(
        class = "recommendation-pills",
        lapply(state$recommended_nodes, function(node) {
          shiny::span(class = "recommendation-pill", format_identifier_label(node))
        })
      )
    )
  })

  output$guided_positive_ui <- shiny::renderUI({
    state <- guided_values$state
    shiny::req(state)

    if (isTRUE(state$done) || isTRUE(state$inconsistent) || !length(state$recommended_nodes)) {
      return(NULL)
    }

    shiny::checkboxGroupInput(
      "guided_positive_nodes",
      "Which of these sampled nodes were positive?",
      choices = guided_positive_choices(state$recommended_nodes),
      selected = guided_values$pending_positive
    )
  })

  output$guided_network_plot <- shiny::renderPlot({
    state <- guided_values$state
    bundle <- current_bundle()
    shiny::req(state, bundle$model)

    plot_guided_network(
      tree = bundle$model,
      state = state,
      pending_positive = guided_values$pending_positive
    )
  }, res = 96)

  output$guided_click_info <- shiny::renderUI({
    state <- guided_values$state
    shiny::req(state)

    shiny::div(
      class = "status-block status-info",
      shiny::strong("How to record positive samples"),
      shiny::p("Use the checklist below the recommendations to mark the sampled nodes that returned a positive result in the current sampling cycle."),
      shiny::p("If no sampled node was positive, select 'None'. All sampled nodes that are not selected are treated as negative.")
    )
  })

  output$guided_history_table <- shiny::renderUI({
    state <- guided_values$state
    shiny::req(state)

    if (!nrow(state$history)) {
      return(build_table_ui(data.frame(Note = "No guided sampling cycle has been applied yet.")))
    }

    history_df <- state$history
    history_df$tested_nodes <- format_identifier_sequence(history_df$tested_nodes)
    history_df$positive_nodes <- format_identifier_sequence(history_df$positive_nodes)
    history_df$focus_node <- format_identifier_label(history_df$focus_node)
    history_df$identified_source <- format_identifier_label(history_df$identified_source)
    names(history_df) <- c(
      "Sampling cycle",
      "Sampled nodes",
      "Positive nodes",
      "Outcome",
      "Focus node",
      "Remaining source count",
      "Identified source"
    )
    build_table_ui(
      history_df,
      numeric_columns = c("Sampling cycle", "Remaining source count")
    )
  })

  output$comparison_note <- shiny::renderUI({
    result <- comparison_bundle()
    shiny::req(result)

    summary_df <- result$summary
    best_idx <- which.min(summary_df$expected_cycles)
    best_row <- summary_df[best_idx, , drop = FALSE]
    sampler_text <- if (identical(best_row$testers, 1L) || identical(best_row$testers, 1)) {
      "1 sampler"
    } else {
      paste(best_row$testers, "samplers")
    }

    shiny::div(
      class = "status-block status-info",
      shiny::strong("Configuration with the minimum expected number of sampling cycles"),
      shiny::p(
        paste(
          strategy_catalog()[[best_row$strategy]]$label,
          "using", sampler_text, "requires an expected number of",
          round(best_row$expected_cycles, 2), "sampling cycles."
        )
      ),
      shiny::p(
        "Expected-value plots are probability-weighted by the uploaded node weights. The boxplots of the number of sampling cycles and the number of samples show unweighted spreads across feasible source nodes; for samples, each value is the total number of samples actually required until the source is identified."
      )
    )
  })

  output$parallel_recommendation_ui <- shiny::renderUI({
    recommendation <- try(parallel_recommendation(), silent = TRUE)
    if (inherits(recommendation, "try-error") || is.null(recommendation)) {
      return(
        shiny::div(
          class = "status-block status-info",
          shiny::strong("Recommendation unavailable."),
          shiny::p("Run a strategy comparison across at least two values of the number of samplers to estimate a recommended range.")
        )
      )
    }

    strategy_label <- if (!is.na(recommendation$strategy) && recommendation$strategy %in% names(strategy_catalog())) {
      strategy_catalog()[[recommendation$strategy]]$label
    } else {
      "the evaluated reference strategy"
    }

    formula_text <- "marginal efficiency(k) = (E[C_(k-1)] - E[C_k]) / (E[S_k] - E[S_(k-1)])"

    if (!isTRUE(recommendation$available)) {
      return(
        shiny::div(
          class = "status-block status-info",
          shiny::strong("Recommendation unavailable."),
          shiny::p("At least two values of the number of samplers must be evaluated for the reference strategy before a recommendation can be estimated."),
          shiny::tags$p(shiny::tags$code(formula_text))
        )
      )
    }

    if (identical(recommendation$reason, "dominant")) {
      return(
        shiny::div(
          class = "status-block status-success",
          shiny::strong(paste("Recommended range of the number of samplers:", recommendation$range_label)),
          shiny::p(
            paste(
              "The recommendation is based on", strategy_label,
              "and includes values of the number of samplers that strictly dominate the previous evaluated value by reducing both the expected number of sampling cycles and the expected number of samples."
            )
          ),
          shiny::tags$p(shiny::tags$code(formula_text))
        )
      )
    }

    if (identical(recommendation$reason, "no_positive_gain")) {
      return(
        shiny::div(
          class = "status-block status-warning",
          shiny::strong(paste("Recommended number of samplers:", recommendation$range_label)),
          shiny::p(
            paste(
              "Within the evaluated range for", strategy_label,
              "no increase in the number of samplers produced a positive marginal gain in the number of sampling cycles per additional expected sample."
            )
          ),
          shiny::tags$p(shiny::tags$code(formula_text))
        )
      )
    }

    shiny::div(
      class = "status-block status-info",
      shiny::strong(paste("Recommended range of the number of samplers:", recommendation$range_label)),
      shiny::p(
        paste(
          "The recommendation is based on", strategy_label,
          "and includes values of the number of samplers whose marginal efficiency reaches at least",
          paste0(round(100 * recommendation$threshold), "%"),
          "of the peak positive value in the evaluated range."
        )
      ),
      shiny::tags$p(shiny::tags$code(formula_text))
    )
  })

  output$parallel_efficiency_table <- shiny::renderUI({
    recommendation <- try(parallel_recommendation(), silent = TRUE)
    if (inherits(recommendation, "try-error") || is.null(recommendation)) {
      return(NULL)
    }

    efficiency_df <- recommendation$efficiency_table
    if (is.null(efficiency_df) || nrow(efficiency_df) < 2L) {
      return(NULL)
    }

    display_df <- efficiency_df[!is.na(efficiency_df$previous_testers), c(
      "previous_testers",
      "testers",
      "marginal_cycle_gain",
      "marginal_test_increase",
      "marginal_efficiency"
    )]

    if (!nrow(display_df)) {
      return(NULL)
    }

    names(display_df) <- c(
      "Previous number of samplers",
      "Current number of samplers",
      "Delta expected number of sampling cycles",
      "Delta expected number of samples",
      "Marginal efficiency"
    )

    display_df[["Delta expected number of sampling cycles"]] <- round(display_df[["Delta expected number of sampling cycles"]], 3)
    display_df[["Delta expected number of samples"]] <- round(display_df[["Delta expected number of samples"]], 3)
    display_df[["Marginal efficiency"]] <- vapply(
      display_df[["Marginal efficiency"]],
      function(value) {
        if (is.infinite(value)) {
          return("dominant")
        }
        format(round(value, 3), nsmall = 3, trim = TRUE)
      },
      character(1)
    )

    build_table_ui(
      display_df,
      numeric_columns = names(display_df)
    )
  })

  output$cycles_plot <- shiny::renderPlot({
    result <- comparison_bundle()
    shiny::req(result)
    plot_metric_lines(
      result$summary,
      "expected_cycles",
      show_legend = isTRUE(input$show_cycles_plot_legend),
      show_title = isTRUE(input$show_cycles_plot_title)
    )
  }, res = 96)

  output$tests_plot <- shiny::renderPlot({
    result <- comparison_bundle()
    shiny::req(result)
    plot_metric_lines(
      result$summary,
      "expected_total_tests",
      show_legend = isTRUE(input$show_tests_plot_legend),
      show_title = isTRUE(input$show_tests_plot_title)
    )
  }, res = 96)

  output$cycles_boxplot <- shiny::renderPlot({
    result <- comparison_bundle()
    shiny::req(result)
    plot_metric_boxplots(
      result,
      "cycles",
      show_legend = isTRUE(input$show_cycles_boxplot_legend),
      show_title = isTRUE(input$show_cycles_boxplot_title)
    )
  }, res = 96)

  output$tests_boxplot <- shiny::renderPlot({
    result <- comparison_bundle()
    shiny::req(result)
    plot_metric_boxplots(
      result,
      "total_tests",
      show_legend = isTRUE(input$show_tests_boxplot_legend),
      show_title = isTRUE(input$show_tests_boxplot_title)
    )
  }, res = 96)

  output$pareto_plot <- shiny::renderPlot({
    result <- comparison_bundle()
    shiny::req(result)
    plot_pareto_front(
      result$summary,
      show_legend = isTRUE(input$show_pareto_plot_legend),
      show_title = isTRUE(input$show_pareto_plot_title),
      show_all_sampler_labels = isTRUE(input$show_all_pareto_sampler_labels)
    )
  }, res = 96)

  output$cdf_plot <- shiny::renderPlot({
    result <- comparison_bundle()
    shiny::req(result)
    plot_cycle_cdf(
      result,
      input$cdf_testers,
      show_legend = isTRUE(input$show_cdf_plot_legend),
      show_title = isTRUE(input$show_cdf_plot_title)
    )
  }, res = 96)

  output$comparison_table <- shiny::renderUI({
    build_table_ui(comparison_summary_df())
  })

  output$download_adjacency_template <- shiny::downloadHandler(
    filename = function() basename(demo_files$network),
    content = function(file) {
      write_demo_adjacency_csv(file)
    }
  )

  register_plot_download(
    output_id = "download_network_plot",
    filename = "network_overview.png",
    width = 1800,
    height = 1200,
    plotting_code = function() {
      bundle <- current_bundle()
      shiny::req(bundle$model)
      plot_network_snapshot(
        bundle$model,
        show_title = isTRUE(input$show_network_plot_title %||% TRUE)
      )
    }
  )

  register_plot_download(
    output_id = "download_single_run_history_plot",
    filename = "single_run_history.png",
    width = 1800,
    height = 1100,
    plotting_code = function() {
      plot_search_history(single_run())
    }
  )

  register_plot_download(
    output_id = "download_guided_network_plot",
    filename = "guided_network.png",
    width = 1800,
    height = 1400,
    plotting_code = function() {
      state <- guided_values$state
      bundle <- current_bundle()
      shiny::req(state, bundle$model)
      plot_guided_network(
        tree = bundle$model,
        state = state,
        pending_positive = guided_values$pending_positive
      )
    }
  )

  register_plot_download(
    output_id = "download_cycles_plot",
    filename = "expected_cycles.png",
    width = 1800,
    height = 1100,
    plotting_code = function() {
      result <- comparison_bundle()
      shiny::req(result)
      plot_metric_lines(
        result$summary,
        "expected_cycles",
        show_legend = isTRUE(input$show_cycles_plot_legend),
        show_title = isTRUE(input$show_cycles_plot_title)
      )
    }
  )

  register_plot_download(
    output_id = "download_tests_plot",
    filename = "expected_number_of_samples.png",
    width = 1800,
    height = 1100,
    plotting_code = function() {
      result <- comparison_bundle()
      shiny::req(result)
      plot_metric_lines(
        result$summary,
        "expected_total_tests",
        show_legend = isTRUE(input$show_tests_plot_legend),
        show_title = isTRUE(input$show_tests_plot_title)
      )
    }
  )

  register_plot_download(
    output_id = "download_cycles_boxplot",
    filename = "cycles_boxplots.png",
    width = 1800,
    height = 1200,
    plotting_code = function() {
      result <- comparison_bundle()
      shiny::req(result)
      plot_metric_boxplots(
        result,
        "cycles",
        show_legend = isTRUE(input$show_cycles_boxplot_legend),
        show_title = isTRUE(input$show_cycles_boxplot_title)
      )
    }
  )

  register_plot_download(
    output_id = "download_tests_boxplot",
    filename = "samples_boxplots.png",
    width = 1800,
    height = 1200,
    plotting_code = function() {
      result <- comparison_bundle()
      shiny::req(result)
      plot_metric_boxplots(
        result,
        "total_tests",
        show_legend = isTRUE(input$show_tests_boxplot_legend),
        show_title = isTRUE(input$show_tests_boxplot_title)
      )
    }
  )

  register_plot_download(
    output_id = "download_cdf_plot",
    filename = "cycle_cdf.png",
    width = 1800,
    height = 1200,
    plotting_code = function() {
      result <- comparison_bundle()
      shiny::req(result)
      plot_cycle_cdf(
        result,
        input$cdf_testers,
        show_legend = isTRUE(input$show_cdf_plot_legend),
        show_title = isTRUE(input$show_cdf_plot_title)
      )
    }
  )

  register_plot_download(
    output_id = "download_pareto_plot",
    filename = "pareto_tradeoff.png",
    width = 1800,
    height = 1200,
    plotting_code = function() {
      result <- comparison_bundle()
      shiny::req(result)
      plot_pareto_front(
        result$summary,
        show_legend = isTRUE(input$show_pareto_plot_legend),
        show_title = isTRUE(input$show_pareto_plot_title),
        show_all_sampler_labels = isTRUE(input$show_all_pareto_sampler_labels)
      )
    }
  )

  output$download_comparison_table <- shiny::downloadHandler(
    filename = function() {
      paste0("strategy_comparison_summary_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
    },
    content = function(file) {
      write_simple_xlsx(
        path = file,
        sheet_name = "Summary table",
        data = comparison_summary_df()
      )
    }
  )

  output$download_weight_template <- shiny::downloadHandler(
    filename = function() basename(demo_files$weights),
    content = function(file) {
      write_demo_weight_csv(file)
    }
  )
}
