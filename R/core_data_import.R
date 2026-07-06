coerce_utf8_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- enc2utf8(x)
  sub("^\ufeff", "", x)
}

coerce_utf8_df <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- coerce_utf8_text(names(df))

  for (column_name in names(df)) {
    if (is.character(df[[column_name]]) || is.factor(df[[column_name]])) {
      df[[column_name]] <- coerce_utf8_text(df[[column_name]])
    }
  }

  df
}

normalize_node_ids <- function(x) {
  x <- trimws(coerce_utf8_text(x))
  x[is.na(x)] <- ""
  x
}

normalize_header_token <- function(x) {
  x <- tolower(normalize_node_ids(x))
  gsub("[^a-z0-9]+", "", x)
}

probe_delimited_text <- function(path, delimiter, nrows = 30L) {
  file_encoding <- detect_text_encoding(path)

  tryCatch(
    utils::read.table(
      path,
      sep = delimiter,
      header = FALSE,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fill = TRUE,
      quote = "\"",
      comment.char = "",
      fileEncoding = file_encoding,
      nrows = nrows
    ),
    error = function(e) NULL
  )
}

# Heuristic encoding detection for text uploads. The goal is not to identify
# every legacy encoding perfectly, but to robustly recover common spreadsheet
# exports without requiring users to re-save files first.
detect_text_encoding <- function(path, nlines = 40L) {
  raw_lines <- tryCatch(
    readLines(path, n = nlines, warn = FALSE, encoding = "bytes"),
    error = function(e) character(0)
  )

  if (!length(raw_lines)) {
    return("UTF-8")
  }

  candidates <- c("UTF-8", "Windows-1252", "latin1")
  scores <- lapply(seq_along(candidates), function(i) {
    encoding <- candidates[[i]]
    converted <- suppressWarnings(iconv(raw_lines, from = encoding, to = "UTF-8", sub = NA))
    data.frame(
      encoding = encoding,
      valid_lines = sum(!is.na(converted)),
      priority = i,
      stringsAsFactors = FALSE
    )
  })

  score_df <- do.call(rbind, scores)
  score_df <- score_df[order(-score_df$valid_lines, score_df$priority), , drop = FALSE]
  score_df$encoding[[1]]
}

# Probe a small prefix of the file under several common delimiters and choose
# the one that yields the most stable multi-column structure.
detect_text_delimiter <- function(path) {
  candidates <- c(",", ";", "\t", "|")
  scores <- lapply(seq_along(candidates), function(i) {
    delimiter <- candidates[[i]]
    parsed <- probe_delimited_text(path, delimiter)

    if (is.null(parsed) || !nrow(parsed) || !ncol(parsed)) {
      return(data.frame(
        delimiter = delimiter,
        median_fields = 0,
        max_fields = 0,
        consistent_rows = 0,
        priority = i,
        stringsAsFactors = FALSE
      ))
    }

    non_empty_counts <- apply(parsed, 1, function(row) {
      sum(nzchar(trimws(as.character(row))))
    })
    best_width <- max(non_empty_counts)

    data.frame(
      delimiter = delimiter,
      median_fields = stats::median(non_empty_counts),
      max_fields = best_width,
      consistent_rows = sum(non_empty_counts == best_width),
      priority = i,
      stringsAsFactors = FALSE
    )
  })

  score_df <- do.call(rbind, scores)
  viable <- score_df[score_df$max_fields > 1, , drop = FALSE]

  if (!nrow(viable)) {
    stop(
      "The text file could not be parsed with any supported delimiter (comma, semicolon, tab, pipe).",
      call. = FALSE
    )
  }

  viable <- viable[order(
    -viable$median_fields,
    -viable$max_fields,
    -viable$consistent_rows,
    viable$priority
  ), , drop = FALSE]

  viable$delimiter[[1]]
}

# Unified reader for CSV/TXT/XLS/XLSX uploads. All downstream validators work
# on plain data frames so that import and error handling remain format-agnostic.
read_tabular_file <- function(path) {
  extension <- tolower(tools::file_ext(path))

  if (extension %in% c("xlsx", "xls")) {
    return(
      coerce_utf8_df(
        readxl::read_excel(
          path,
          col_names = FALSE,
          .name_repair = "minimal"
        )
      )
    )
  }

  if (extension %in% c("csv", "txt")) {
    delimiter <- detect_text_delimiter(path)
    file_encoding <- detect_text_encoding(path)
    return(
      coerce_utf8_df(utils::read.table(
        path,
        sep = delimiter,
        header = FALSE,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        fill = TRUE,
        quote = "\"",
        comment.char = "",
        fileEncoding = file_encoding
      ))
    )
  }

  stop("Unsupported file type: ", extension, call. = FALSE)
}

coerce_numeric_matrix <- function(df) {
  values <- suppressWarnings(as.numeric(as.matrix(df)))
  matrix(values, nrow = nrow(df), ncol = ncol(df))
}

adjacency_degree_profile <- function(row_ids, col_ids, value_matrix) {
  node_ids <- sort(unique(c(row_ids, col_ids)))
  edge_flag <- value_matrix != 0

  out_degree <- setNames(numeric(length(node_ids)), node_ids)
  in_degree <- setNames(numeric(length(node_ids)), node_ids)

  out_degree[row_ids] <- rowSums(edge_flag)
  in_degree[col_ids] <- colSums(edge_flag)

  list(
    nodes = node_ids,
    out_degree = out_degree,
    in_degree = in_degree
  )
}

looks_like_flow_direction_matrix <- function(profile) {
  out_degree <- profile$out_degree
  sum(out_degree == 0) == 1L && all(out_degree[out_degree > 0] == 1)
}

looks_like_reversed_flow_matrix <- function(profile) {
  in_degree <- profile$in_degree
  sum(in_degree == 0) == 1L && all(in_degree[in_degree > 0] == 1)
}

parse_weights_table <- function(raw_df) {
  df <- coerce_utf8_df(raw_df)

  if (ncol(df) < 2 || nrow(df) < 1) {
    stop("The weight file must contain at least two columns.", call. = FALSE)
  }

  first_row <- normalize_header_token(unlist(df[1, 1:2], use.names = FALSE))
  # Accept a small family of common header names so that example files and
  # operational exports do not need to follow one rigid template.
  looks_like_header <- identical(length(first_row), 2L) &&
    first_row[1] %in% c(
      "node", "id", "ezg", "ezgfull", "name", "label",
      "nodelabel", "nodename", "nodeid", "catchment", "catchmentid"
    ) &&
    first_row[2] %in% c(
      "weight", "weights", "pop", "population", "rpop",
      "probability", "prob", "inhabitants"
    )

  if (looks_like_header) {
    df <- df[-1, , drop = FALSE]
  }

  nodes <- normalize_node_ids(df[[1]])
  weights <- suppressWarnings(as.numeric(df[[2]]))

  keep <- nzchar(nodes)
  nodes <- nodes[keep]
  weights <- weights[keep]

  if (!length(nodes)) {
    stop("No node identifiers could be read from the weight file.", call. = FALSE)
  }

  if (any(is.na(weights))) {
    stop("The weight file contains missing or non-numeric weights.", call. = FALSE)
  }

  if (any(weights < 0)) {
    stop("Weights must be non-negative.", call. = FALSE)
  }

  aggregated <- stats::aggregate(weights, by = list(node = nodes), FUN = sum)
  names(aggregated)[2] <- "weight"

  total_weight <- sum(aggregated$weight)
  if (total_weight <= 0) {
    stop("The sum of all weights must be positive.", call. = FALSE)
  }

  aggregated$probability <- aggregated$weight / total_weight

  list(
    data = aggregated,
    meta = list(
      rows = nrow(aggregated),
      total_weight = total_weight,
      duplicated_nodes_collapsed = any(duplicated(nodes))
    )
  )
}

# Support both spreadsheet-style matrices with embedded labels in the first
# row/column and plain tables whose column names already contain node labels.
extract_adjacency_matrix_components <- function(raw_df) {
  df <- coerce_utf8_df(raw_df)
  if (ncol(df) < 2 || nrow(df) < 2) {
    stop("The adjacency matrix must include row and column labels.", call. = FALSE)
  }

  top_row <- normalize_node_ids(unlist(df[1, -1, drop = TRUE], use.names = FALSE))
  left_col <- normalize_node_ids(unlist(df[-1, 1, drop = TRUE], use.names = FALSE))

  use_embedded_labels <- length(top_row) == length(left_col) &&
    all(nzchar(top_row)) &&
    all(nzchar(left_col)) &&
    setequal(top_row, left_col)

  if (use_embedded_labels) {
    row_ids <- left_col
    col_ids <- top_row
    values <- df[-1, -1, drop = FALSE]
  } else {
    row_ids <- normalize_node_ids(df[[1]])
    col_ids <- normalize_node_ids(names(df)[-1])
    values <- df[, -1, drop = FALSE]
  }

  if (!all(nzchar(row_ids)) || !all(nzchar(col_ids))) {
    stop(
      "Adjacency matrices must provide non-empty row and column node labels.",
      call. = FALSE
    )
  }

  if (!setequal(row_ids, col_ids)) {
    stop(
      "Row and column labels in the adjacency matrix must describe the same node set.",
      call. = FALSE
    )
  }

  value_matrix <- coerce_numeric_matrix(values)
  if (any(is.na(value_matrix))) {
    stop(
      "Adjacency matrices may only contain numeric edge indicators in the body.",
      call. = FALSE
    )
  }

  list(
    row_ids = row_ids,
    col_ids = col_ids,
    value_matrix = value_matrix
  )
}

transpose_adjacency_network_df <- function(raw_df) {
  components <- extract_adjacency_matrix_components(raw_df)
  transposed_matrix <- t(components$value_matrix)

  transposed_df <- data.frame(
    Node = components$col_ids,
    as.data.frame(transposed_matrix, check.names = FALSE),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  names(transposed_df) <- c("", components$row_ids)
  transposed_df
}

# Parse the user-facing flow matrix. Internally, edges are stored as
# downstream-parent -> upstream-child so that the outlet remains the unique root.
parse_adjacency_network <- function(raw_df) {
  components <- extract_adjacency_matrix_components(raw_df)
  row_ids <- components$row_ids
  col_ids <- components$col_ids
  value_matrix <- components$value_matrix

  edge_positions <- which(value_matrix != 0, arr.ind = TRUE)
  if (!nrow(edge_positions)) {
    stop("The adjacency matrix does not contain any edges.", call. = FALSE)
  }

  raw_edges <- data.frame(
    from = row_ids[edge_positions[, "row"]],
    to = col_ids[edge_positions[, "col"]],
    stringsAsFactors = FALSE
  )

  profile <- adjacency_degree_profile(row_ids, col_ids, value_matrix)
  # Detect the common mistake that rows and columns were supplied in the
  # opposite hydraulic direction before building the rooted tree.
  if (!looks_like_flow_direction_matrix(profile) && looks_like_reversed_flow_matrix(profile)) {
    stop(
      paste(
        "The adjacency matrix appears to use the reverse direction.",
        "In this app, a value of 1 in row A and column B must mean that wastewater flows from A to B."
      ),
      call. = FALSE
    )
  }

  edges <- data.frame(
    parent = raw_edges$to,
    child = raw_edges$from,
    stringsAsFactors = FALSE
  )
  reverse_edges <- data.frame(
    parent = raw_edges$from,
    child = raw_edges$to,
    stringsAsFactors = FALSE
  )

  list(
    edges = unique(edges),
    reverse_edges = unique(reverse_edges),
    meta = list(
      format = "adjacency",
      convention = "row_to_column_flow",
      raw_edge_count = nrow(raw_edges),
      out_degree = profile$out_degree,
      in_degree = profile$in_degree
    )
  )
}

# End-to-end model builder for uploaded files. If the requested orientation
# fails but the reversed orientation forms a valid rooted tree, the app reports
# that the matrix most likely encodes the flow direction backwards.
build_model_from_files <- function(network_path, weights_path, transpose_network = FALSE) {
  raw_network <- read_tabular_file(network_path)
  raw_weights <- read_tabular_file(weights_path)

  if (isTRUE(transpose_network)) {
    raw_network <- transpose_adjacency_network_df(raw_network)
  }

  parsed_network <- parse_adjacency_network(
    raw_df = raw_network
  )
  parsed_weights <- parse_weights_table(raw_weights)

  if (isTRUE(transpose_network)) {
    parsed_network$meta <- modifyList(parsed_network$meta, list(
      transposed_input = TRUE
    ))
  }

  fixed_model <- tryCatch(
    build_tree_model(
      edges = parsed_network$edges,
      weights = parsed_weights$data,
      network_meta = parsed_network$meta,
      weight_meta = parsed_weights$meta
    ),
    error = function(e) e
  )

  if (!inherits(fixed_model, "error")) {
    root_out_degree <- parsed_network$meta$out_degree[[fixed_model$root]]
    if (is.finite(root_out_degree) && root_out_degree > 0) {
      stop(
        paste(
          "The adjacency matrix appears to use the reverse direction.",
          "In this app, a value of 1 in row A and column B must mean that wastewater flows from A to B."
        ),
        call. = FALSE
      )
    }

    return(fixed_model)
  }

  reverse_model <- tryCatch(
    build_tree_model(
      edges = parsed_network$reverse_edges,
      weights = parsed_weights$data,
      network_meta = modifyList(parsed_network$meta, list(
        convention = "reverse_row_to_column_flow"
      )),
      weight_meta = parsed_weights$meta
    ),
    error = function(e) e
  )

  if (!inherits(reverse_model, "error")) {
    stop(
      paste(
        "The adjacency matrix appears to use the reverse direction.",
        "In this app, a value of 1 in row A and column B must mean that wastewater flows from A to B."
      ),
      call. = FALSE
    )
  }

  stop(conditionMessage(fixed_model), call. = FALSE)
}
