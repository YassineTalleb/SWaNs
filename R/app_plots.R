strategy_palette <- function() {
  c(
    nary_split = "#20656a",
    max_rpop = "#c77c48",
    max_cum_rpop = "#4e6aa0",
    skipping_cum_rpop = "#8b5d88"
  )
}

plot_text_style <- function(scale = 1) {
  list(
    axis = 1.22 * scale,
    label = 1.28 * scale,
    main = 1.42 * scale,
    sub = 1.08 * scale,
    legend = 0.9 * scale,
    annotation = 1.04 * scale,
    node_label = 1.08 * scale,
    point_label = 0.96 * scale
  )
}

set_plot_par <- function(mar, bg = NA, family = "sans", xpd = FALSE, mgp = NULL, scale = 1) {
  text_style <- plot_text_style(scale)
  par_args <- list(
    mar = mar,
    bg = bg,
    family = family,
    xpd = xpd,
    cex.axis = text_style$axis,
    cex.lab = text_style$label,
    font.axis = 2,
    font.lab = 2
  )

  if (!is.null(mgp)) {
    par_args$mgp <- mgp
  }

  do.call(graphics::par, par_args)
  invisible(text_style)
}

draw_strategy_legend <- function(
  strategies,
  palette,
  text_style,
  type = c("line", "fill"),
  location = "top"
) {
  type <- match.arg(type)
  labels <- vapply(strategies, function(id) strategy_catalog()[[id]]$label, character(1))

  legend_args <- list(
    location,
    legend = labels,
    horiz = TRUE,
    bty = "n",
    cex = text_style$legend,
    text.font = 2
  )

  if (identical(type, "line")) {
    legend_args <- c(
      legend_args,
      list(
        col = palette[strategies],
        pch = 19,
        lwd = 2.8
      )
    )
  } else {
    legend_args <- c(
      legend_args,
      list(
        fill = grDevices::adjustcolor(palette[strategies], alpha.f = 0.72),
        border = palette[strategies]
      )
    )
  }

  do.call(graphics::legend, legend_args)
}

compute_node_cex <- function(weight_values, node_count) {
  if (!length(weight_values)) {
    return(numeric(0))
  }

  max_weight <- max(weight_values, na.rm = TRUE)
  if (!is.finite(max_weight) || max_weight <= 0) {
    return(rep(0.8, length(weight_values)))
  }

  crowding_factor <- min(1, 6 / sqrt(max(1, node_count)))
  min_cex <- 0.45 + 0.35 * crowding_factor
  max_cex <- 1.2 + 2.6 * crowding_factor
  scaled_weight <- sqrt(pmax(weight_values, 0) / max_weight)

  min_cex + (max_cex - min_cex) * scaled_weight
}

compute_layout_limits <- function(node_df, point_cex, top_label_offset = 0.45) {
  if (!nrow(node_df)) {
    return(list(xlim = c(-1, 1), ylim = c(-1, 1)))
  }

  x_range <- range(node_df$x, na.rm = TRUE)
  y_range <- range(node_df$y, na.rm = TRUE)
  x_span <- diff(x_range)
  y_span <- diff(y_range)

  if (!is.finite(x_span) || x_span <= 0) {
    x_span <- 1
  }

  if (!is.finite(y_span) || y_span <= 0) {
    y_span <- 1
  }

  max_cex <- if (length(point_cex)) max(point_cex, na.rm = TRUE) else 1
  x_pad <- max(0.9, 0.05 * x_span, 0.22 * max_cex)
  y_pad_bottom <- max(0.7, 0.08 * y_span, 0.24 * max_cex)
  y_pad_top <- max(1.0 + top_label_offset, 0.1 * y_span + top_label_offset, 0.26 * max_cex + top_label_offset)

  list(
    xlim = c(x_range[[1]] - x_pad, x_range[[2]] + x_pad),
    ylim = c(y_range[[1]] - y_pad_bottom, y_range[[2]] + y_pad_top)
  )
}

rect_bounds <- function(rect) {
  list(
    xmin = rect$left,
    xmax = rect$left + rect$w,
    ymin = rect$top - rect$h,
    ymax = rect$top
  )
}

point_in_bounds <- function(x, y, bounds, eps = 1e-9) {
  x >= (bounds$xmin - eps) &&
    x <= (bounds$xmax + eps) &&
    y >= (bounds$ymin - eps) &&
    y <= (bounds$ymax + eps)
}

segment_orientation <- function(ax, ay, bx, by, cx, cy) {
  (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
}

point_on_segment <- function(px, py, ax, ay, bx, by, eps = 1e-9) {
  px >= min(ax, bx) - eps &&
    px <= max(ax, bx) + eps &&
    py >= min(ay, by) - eps &&
    py <= max(ay, by) + eps
}

segments_intersect <- function(ax, ay, bx, by, cx, cy, dx, dy, eps = 1e-9) {
  o1 <- segment_orientation(ax, ay, bx, by, cx, cy)
  o2 <- segment_orientation(ax, ay, bx, by, dx, dy)
  o3 <- segment_orientation(cx, cy, dx, dy, ax, ay)
  o4 <- segment_orientation(cx, cy, dx, dy, bx, by)

  if ((o1 > eps && o2 < -eps || o1 < -eps && o2 > eps) &&
      (o3 > eps && o4 < -eps || o3 < -eps && o4 > eps)) {
    return(TRUE)
  }

  if (abs(o1) <= eps && point_on_segment(cx, cy, ax, ay, bx, by, eps)) {
    return(TRUE)
  }

  if (abs(o2) <= eps && point_on_segment(dx, dy, ax, ay, bx, by, eps)) {
    return(TRUE)
  }

  if (abs(o3) <= eps && point_on_segment(ax, ay, cx, cy, dx, dy, eps)) {
    return(TRUE)
  }

  if (abs(o4) <= eps && point_on_segment(bx, by, cx, cy, dx, dy, eps)) {
    return(TRUE)
  }

  FALSE
}

segment_intersects_bounds <- function(x0, y0, x1, y1, bounds) {
  if (max(x0, x1) < bounds$xmin || min(x0, x1) > bounds$xmax ||
      max(y0, y1) < bounds$ymin || min(y0, y1) > bounds$ymax) {
    return(FALSE)
  }

  if (point_in_bounds(x0, y0, bounds) || point_in_bounds(x1, y1, bounds)) {
    return(TRUE)
  }

  edges <- list(
    c(bounds$xmin, bounds$ymin, bounds$xmin, bounds$ymax),
    c(bounds$xmin, bounds$ymax, bounds$xmax, bounds$ymax),
    c(bounds$xmax, bounds$ymax, bounds$xmax, bounds$ymin),
    c(bounds$xmax, bounds$ymin, bounds$xmin, bounds$ymin)
  )

  any(vapply(
    edges,
    function(edge) {
      segments_intersect(
        x0, y0, x1, y1,
        edge[[1]], edge[[2]], edge[[3]], edge[[4]]
      )
    },
    logical(1)
  ))
}

legend_overlap_score <- function(bounds, node_df, edge_df) {
  node_hits <- sum(
    node_df$x >= bounds$xmin &
      node_df$x <= bounds$xmax &
      node_df$y >= bounds$ymin &
      node_df$y <= bounds$ymax
  )

  edge_hits <- if (nrow(edge_df)) {
    sum(mapply(
      segment_intersects_bounds,
      edge_df$x,
      edge_df$y,
      edge_df$xend,
      edge_df$yend,
      MoreArgs = list(bounds = bounds)
    ))
  } else {
    0L
  }

  100 * node_hits + 10 * edge_hits
}

draw_auto_legend <- function(node_df, edge_df, plot_limits, legend_args) {
  x_span <- diff(plot_limits$xlim)
  y_span <- diff(plot_limits$ylim)
  x_inset <- 0.02 * x_span
  y_inset <- 0.03 * y_span

  candidates <- list(
    list(name = "top_left", x = plot_limits$xlim[[1]] + x_inset, y = plot_limits$ylim[[2]] - y_inset, xjust = 0, yjust = 1, priority = 1),
    list(name = "top_right", x = plot_limits$xlim[[2]] - x_inset, y = plot_limits$ylim[[2]] - y_inset, xjust = 1, yjust = 1, priority = 2),
    list(name = "bottom_left", x = plot_limits$xlim[[1]] + x_inset, y = plot_limits$ylim[[1]] + y_inset, xjust = 0, yjust = 0, priority = 3),
    list(name = "bottom_right", x = plot_limits$xlim[[2]] - x_inset, y = plot_limits$ylim[[1]] + y_inset, xjust = 1, yjust = 0, priority = 4),
    list(name = "left_center", x = plot_limits$xlim[[1]] + x_inset, y = mean(plot_limits$ylim), xjust = 0, yjust = 0.5, priority = 5),
    list(name = "right_center", x = plot_limits$xlim[[2]] - x_inset, y = mean(plot_limits$ylim), xjust = 1, yjust = 0.5, priority = 6),
    list(name = "top_center", x = mean(plot_limits$xlim), y = plot_limits$ylim[[2]] - y_inset, xjust = 0.5, yjust = 1, priority = 7),
    list(name = "bottom_center", x = mean(plot_limits$xlim), y = plot_limits$ylim[[1]] + y_inset, xjust = 0.5, yjust = 0, priority = 8)
  )

  scored_candidates <- lapply(candidates, function(candidate) {
    candidate_box <- do.call(
      graphics::legend,
      c(
        list(
          x = candidate$x,
          y = candidate$y,
          xjust = candidate$xjust,
          yjust = candidate$yjust,
          plot = FALSE
        ),
        legend_args
      )
    )
    bounds <- rect_bounds(candidate_box$rect)
    candidate$score <- legend_overlap_score(bounds, node_df, edge_df)
    candidate$bounds <- bounds
    candidate
  })

  best_index <- which.min(vapply(
    scored_candidates,
    function(candidate) candidate$score + candidate$priority / 100,
    numeric(1)
  ))
  best_candidate <- scored_candidates[[best_index]]

  do.call(
    graphics::legend,
    c(
      list(
        x = best_candidate$x,
        y = best_candidate$y,
        xjust = best_candidate$xjust,
        yjust = best_candidate$yjust
      ),
      legend_args
    )
  )

  invisible(best_candidate)
}

build_tree_layout <- function(snapshot) {
  x_position <- setNames(rep(NA_real_, length(snapshot$nodes)), snapshot$nodes)
  next_leaf_x <- 1

  place_node <- function(node) {
    children <- snapshot$active_children[[node]]

    if (!length(children)) {
      x_position[[node]] <<- next_leaf_x
      next_leaf_x <<- next_leaf_x + 1
      return(invisible(NULL))
    }

    for (child in children) {
      place_node(child)
    }

    x_position[[node]] <<- mean(x_position[children])
  }

  place_node(snapshot$current_root)

  node_df <- data.frame(
    node = snapshot$nodes,
    x = as.numeric(x_position[snapshot$nodes]),
    y = -as.numeric(snapshot$depth[snapshot$nodes]),
    cum_weight = as.numeric(snapshot$cum_weight[snapshot$nodes]),
    raw_weight = as.numeric(snapshot$raw_weight[snapshot$nodes]),
    is_root = snapshot$nodes == snapshot$current_root,
    stringsAsFactors = FALSE
  )

  edge_rows <- vector("list", 0)
  for (parent_node in snapshot$nodes) {
    for (child_node in snapshot$active_children[[parent_node]]) {
      edge_rows[[length(edge_rows) + 1L]] <- data.frame(
        parent = parent_node,
        child = child_node,
        x = x_position[[parent_node]],
        y = -snapshot$depth[[parent_node]],
        xend = x_position[[child_node]],
        yend = -snapshot$depth[[child_node]],
        stringsAsFactors = FALSE
      )
    }
  }

  edge_df <- if (length(edge_rows)) {
    do.call(rbind, edge_rows)
  } else {
    data.frame(
      parent = character(0),
      child = character(0),
      x = numeric(0),
      y = numeric(0),
      xend = numeric(0),
      yend = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  list(nodes = node_df, edges = edge_df)
}

plot_network_snapshot <- function(
  tree,
  current_root = tree$root,
  active_nodes = tree$nodes,
  show_title = TRUE
) {
  snapshot <- collect_active_subtree(tree, current_root, active_nodes)
  layout <- build_tree_layout(snapshot)
  node_df <- layout$nodes
  edge_df <- layout$edges

  palette_fill <- grDevices::colorRampPalette(c("#d9efe8", "#3e817f"))(100)
  fill_index <- pmax(1, pmin(100, ceiling(node_df$cum_weight * 99) + 1))
  point_bg <- palette_fill[fill_index]
  point_cex <- compute_node_cex(node_df$raw_weight, nrow(node_df))
  plot_limits <- compute_layout_limits(node_df, point_cex, top_label_offset = 0.55)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  text_style <- set_plot_par(
    mar = if (isTRUE(show_title)) c(1.5, 1.5, 4.4, 1.5) else c(1.5, 1.5, 1.5, 1.5),
    bg = NA,
    family = "sans",
    xpd = NA
  )
  graphics::plot(
    node_df$x,
    node_df$y,
    type = "n",
    axes = FALSE,
    ann = FALSE,
    xlim = plot_limits$xlim,
    ylim = plot_limits$ylim,
    xaxs = "i",
    yaxs = "i"
  )

  if (nrow(edge_df) > 0) {
    graphics::segments(
      x0 = edge_df$x,
      y0 = edge_df$y,
      x1 = edge_df$xend,
      y1 = edge_df$yend,
      col = grDevices::adjustcolor("#9bb8b1", alpha.f = 0.8),
      lwd = 1.8
    )
  }

  graphics::points(
    node_df$x,
    node_df$y,
    pch = 21,
    cex = point_cex,
    bg = point_bg,
    col = "#17323b",
    lwd = 1.3
  )

  root_row <- node_df[node_df$is_root, , drop = FALSE]
  graphics::text(
    x = root_row$x,
    y = root_row$y + 0.45,
    labels = format_identifier_label(root_row$node),
    family = "serif",
    cex = text_style$node_label,
    col = "#17323b",
    font = 2
  )

  graphics::title(
    main = if (isTRUE(show_title)) "Current rooted sewer network" else "",
    sub = if (isTRUE(show_title)) {
      paste(
        "The outlet is shown at the top.",
        "Node size shows direct prior probability; darker blue-green shading shows higher cumulative upstream probability."
      )
    } else {
      ""
    },
    family = "serif",
    cex.main = text_style$main,
    cex.sub = text_style$sub,
    font.main = 2,
    font.sub = 2,
    col.sub = "#4d666d"
  )
}

plot_metric_lines <- function(summary_df, metric, show_legend = TRUE, show_title = TRUE) {
  palette <- strategy_palette()
  strategies <- unique(summary_df$strategy)
  metric_label <- switch(
    metric,
    expected_cycles = "Probability-weighted expected number of sampling cycles",
    expected_total_tests = "Probability-weighted expected number of samples",
    metric
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  text_style <- set_plot_par(mar = c(4.9, 5.4, 4.1, 1.6), bg = NA, family = "sans")

  x_range <- range(summary_df$testers)
  y_range <- range(summary_df[[metric]])
  if (diff(y_range) <= 0) {
    y_range <- y_range + c(-0.5, 0.5)
  }
  y_padding <- 0.12 * diff(y_range)
  y_limits <- c(y_range[[1]], y_range[[2]] + y_padding)

  graphics::plot(
    x = x_range,
    y = y_limits,
    type = "n",
    axes = FALSE,
    xaxt = "n",
    xlab = "Number of samplers",
    ylab = metric_label,
    main = if (isTRUE(show_title)) metric_label else "",
    sub = if (isTRUE(show_title)) "Node weights are normalised to source probabilities before averaging." else "",
    family = "serif",
    cex.main = text_style$main,
    cex.sub = text_style$sub,
    font.main = 2,
    font.sub = 2,
    col.sub = "#4d666d"
  )
  graphics::grid(col = grDevices::adjustcolor("#95b6af", alpha.f = 0.25))
  graphics::axis(1, at = sort(unique(summary_df$testers)), lwd = 1.4, lwd.ticks = 1.2)
  graphics::axis(2, lwd = 1.4, lwd.ticks = 1.2)

  for (strategy_id in strategies) {
    df <- summary_df[summary_df$strategy == strategy_id, , drop = FALSE]
    df <- df[order(df$testers), , drop = FALSE]
    graphics::lines(
      df$testers,
      df[[metric]],
      type = "b",
      pch = 19,
      lwd = 3.1,
      cex = 1.15,
      col = palette[[strategy_id]]
    )
  }

  if (isTRUE(show_legend)) {
    draw_strategy_legend(strategies, palette, text_style, type = "line")
  }
}

plot_metric_boxplots <- function(comparison_result, metric, show_legend = TRUE, show_title = TRUE) {
  evaluation_rows <- lapply(comparison_result$evaluations, function(evaluation) {
    data.frame(
      strategy = evaluation$strategy,
      testers = evaluation$testers,
      cycles = evaluation$per_source$cycles,
      total_tests = evaluation$per_source$total_tests,
      stringsAsFactors = FALSE
    )
  })

  boxplot_df <- do.call(rbind, evaluation_rows)
  if (is.null(boxplot_df) || !nrow(boxplot_df)) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No deterministic comparison results are available.", cex = 1.1)
    return(invisible(NULL))
  }

  metric_label <- switch(
    metric,
    cycles = "Number of sampling cycles",
    total_tests = "Number of samples",
    metric
  )

  title_text <- switch(
    metric,
    cycles = "Unweighted boxplots of the number of sampling cycles",
    total_tests = "Unweighted boxplots of the number of samples",
    metric_label
  )

  palette <- strategy_palette()
  strategies <- unique(comparison_result$summary$strategy)
  testers <- sort(unique(boxplot_df$testers))
  group_span <- min(0.72, 0.18 * max(1, length(strategies)))
  offsets <- if (length(strategies) == 1L) {
    0
  } else {
    seq(-group_span / 2, group_span / 2, length.out = length(strategies))
  }

  box_data <- list()
  box_positions <- numeric(0)
  box_colors <- character(0)

  for (tester_value in testers) {
    tester_df <- boxplot_df[boxplot_df$testers == tester_value, , drop = FALSE]

    for (strategy_index in seq_along(strategies)) {
      strategy_id <- strategies[[strategy_index]]
      value_vector <- tester_df[tester_df$strategy == strategy_id, metric, drop = TRUE]
      if (!length(value_vector)) {
        next
      }

      box_data[[length(box_data) + 1L]] <- value_vector
      box_positions <- c(box_positions, tester_value + offsets[[strategy_index]])
      box_colors <- c(
        box_colors,
        grDevices::adjustcolor(palette[[strategy_id]], alpha.f = 0.72)
      )
    }
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  text_style <- set_plot_par(
    mar = c(5.8, 4.8, 4.2, 1.6),
    mgp = c(3.4, 1, 0),
    bg = NA,
    family = "sans"
  )

  y_range <- range(unlist(box_data), na.rm = TRUE)
  if (!all(is.finite(y_range))) {
    y_range <- c(0, 1)
  }
  if (diff(y_range) <= 0) {
    y_range <- y_range + c(-0.5, 0.5)
  }
  y_padding <- 0.08 * diff(y_range)
  top_padding <- if (isTRUE(show_legend)) 0.16 * diff(y_range) else y_padding
  y_limits <- c(y_range[[1]] - y_padding, y_range[[2]] + top_padding)

  graphics::plot(
    x = range(testers) + c(-0.8, 0.8),
    y = y_limits,
    type = "n",
    axes = FALSE,
    xaxt = "n",
    xlab = "Number of samplers",
    ylab = metric_label,
    main = if (isTRUE(show_title)) title_text else "",
    family = "serif",
    cex.main = text_style$main,
    cex.sub = text_style$sub,
    font.main = 2,
    font.sub = 2,
    col.sub = "#4d666d"
  )
  graphics::grid(col = grDevices::adjustcolor("#95b6af", alpha.f = 0.25))

  graphics::boxplot(
    box_data,
    at = box_positions,
    add = TRUE,
    axes = FALSE,
    xaxt = "n",
    yaxt = "n",
    range = 0,
    outline = TRUE,
    border = box_colors,
    col = box_colors,
    lwd = 2,
    boxwex = 0.14
  )

  graphics::axis(1, at = testers, labels = testers, lwd = 1.4, lwd.ticks = 1.2)
  graphics::axis(2, lwd = 1.4, lwd.ticks = 1.2)

  if (isTRUE(show_legend)) {
    draw_strategy_legend(strategies, palette, text_style, type = "fill")
  }
}

plot_pareto_front <- function(
  summary_df,
  show_legend = TRUE,
  show_title = TRUE,
  show_all_sampler_labels = FALSE
) {
  palette <- strategy_palette()
  strategies <- unique(summary_df$strategy)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  text_style <- set_plot_par(mar = c(4.9, 5.4, 4.2, 1.6), bg = NA, family = "sans")

  x_range <- range(summary_df$expected_total_tests)
  y_range <- range(summary_df$expected_cycles)
  if (diff(x_range) <= 0) {
    x_range <- x_range + c(-0.5, 0.5)
  }
  if (diff(y_range) <= 0) {
    y_range <- y_range + c(-0.5, 0.5)
  }
  x_padding <- 0.06 * diff(x_range)
  y_padding <- 0.08 * diff(y_range)
  top_padding <- if (isTRUE(show_legend)) 0.16 * diff(y_range) else y_padding

  graphics::plot(
    x_range + c(-x_padding, x_padding),
    y_range + c(-y_padding, top_padding),
    type = "n",
    axes = FALSE,
    xaxt = "n",
    xlab = "Expected number of samples",
    ylab = "Expected number of sampling cycles",
    main = if (isTRUE(show_title)) "Time-resource trade-off" else "",
    family = "serif",
    cex.main = text_style$main,
    font.main = 2
  )
  graphics::grid(col = grDevices::adjustcolor("#95b6af", alpha.f = 0.25))
  graphics::axis(1, lwd = 1.4, lwd.ticks = 1.2)
  graphics::axis(2, lwd = 1.4, lwd.ticks = 1.2)

  for (strategy_id in strategies) {
    df <- summary_df[summary_df$strategy == strategy_id, , drop = FALSE]
    df <- df[order(df$testers), , drop = FALSE]
    label_df <- if (isTRUE(show_all_sampler_labels)) {
      df
    } else {
      df[df$testers %in% c(1L, max(df$testers)), , drop = FALSE]
    }

    graphics::lines(
      df$expected_total_tests,
      df$expected_cycles,
      col = grDevices::adjustcolor(palette[[strategy_id]], alpha.f = 0.55),
      lwd = 2.4
    )
    graphics::points(
      df$expected_total_tests,
      df$expected_cycles,
      pch = 19,
      cex = 1.35,
      col = palette[[strategy_id]]
    )
    graphics::text(
      label_df$expected_total_tests,
      label_df$expected_cycles,
      labels = label_df$testers,
      pos = 3,
      cex = text_style$point_label,
      col = palette[[strategy_id]],
      font = 2
    )
  }

  if (isTRUE(show_legend)) {
    draw_strategy_legend(strategies, palette, text_style, type = "line")
  }
}

plot_cycle_cdf <- function(comparison_result, testers, show_legend = TRUE, show_title = TRUE) {
  keys <- names(comparison_result$evaluations)
  picked <- keys[grepl(paste0("::", testers, "$"), keys)]
  sampler_text <- if (identical(testers, 1L) || identical(testers, 1)) {
    "1 sampler"
  } else {
    paste(testers, "samplers")
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  text_style <- set_plot_par(mar = c(4.9, 5.4, 4.1, 1.6), bg = NA, family = "sans")

  if (!length(picked)) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No results available for this number of samplers.", cex = text_style$annotation, font = 2)
    return(invisible(NULL))
  }

  plot_rows <- lapply(picked, function(key) comparison_result$evaluations[[key]])
  x_range <- range(unlist(lapply(plot_rows, function(x) x$distributions$cycles)))
  if (diff(x_range) <= 0) {
    x_range <- x_range + c(-0.5, 0.5)
  }

  graphics::plot(
    x = x_range,
    y = if (isTRUE(show_legend)) c(0, 1.08) else c(0, 1),
    type = "n",
    axes = FALSE,
    xaxt = "n",
    xlab = "Number of sampling cycles",
    ylab = "Prior-weighted cumulative probability",
    main = if (isTRUE(show_title)) paste("Distribution of the number of sampling cycles at", sampler_text) else "",
    family = "serif",
    cex.main = text_style$main,
    font.main = 2
  )
  graphics::grid(col = grDevices::adjustcolor("#95b6af", alpha.f = 0.25))
  graphics::axis(1, lwd = 1.4, lwd.ticks = 1.2)
  graphics::axis(2, at = pretty(c(0, 1)), lwd = 1.4, lwd.ticks = 1.2)

  palette <- strategy_palette()
  legends <- character(0)
  legend_cols <- character(0)

  for (evaluation in plot_rows) {
    df <- evaluation$distributions
    graphics::lines(df$cycles, df$cdf, type = "s", lwd = 3, col = palette[[evaluation$strategy]])
    graphics::points(df$cycles, df$cdf, pch = 19, cex = 1.15, col = palette[[evaluation$strategy]])
    legends <- c(legends, strategy_catalog()[[evaluation$strategy]]$label)
    legend_cols <- c(legend_cols, palette[[evaluation$strategy]])
  }

  if (isTRUE(show_legend)) {
    graphics::legend(
      "top",
      legend = legends,
      col = legend_cols,
      pch = 19,
      lwd = 2.8,
      horiz = TRUE,
      bty = "n",
      cex = text_style$legend,
      text.font = 2
    )
  }
}

plot_search_history <- function(run_result) {
  history <- run_result$history

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  text_style <- set_plot_par(mar = c(4.9, 5.4, 4.1, 1.6), bg = NA, family = "sans")

  if (!nrow(history)) {
    graphics::plot.new()
    graphics::text(
      0.5,
      0.5,
      "The source is already identified without additional samples.",
      family = "serif",
      cex = text_style$annotation,
      col = "#3b5960",
      font = 2
    )
    return(invisible(NULL))
  }

  x_values <- history$cycle
  y_values <- history$remaining_probability

  graphics::plot(
    x = range(x_values),
    y = c(0, 1),
    type = "n",
    xlab = "Sampling cycle",
    ylab = "Remaining prior probability",
    main = "Search trace",
    sub = paste("Identified source:", format_identifier_label(run_result$identified_node)),
    family = "serif",
    cex.main = text_style$main,
    cex.sub = text_style$sub,
    font.main = 2,
    font.sub = 2,
    col.sub = "#4d666d"
  )
  graphics::grid(col = grDevices::adjustcolor("#95b6af", alpha.f = 0.25))

  graphics::polygon(
    x = c(min(x_values), x_values, max(x_values)),
    y = c(0, y_values, 0),
    col = grDevices::adjustcolor("#d7ece6", alpha.f = 0.9),
    border = NA
  )
  graphics::lines(x_values, y_values, lwd = 3, col = "#256e73")

  point_cols <- ifelse(history$outcome == "hit", "#20656a", "#c77c48")
  graphics::points(x_values, y_values, pch = 19, cex = 1.35, col = point_cols)

  graphics::legend(
    "topright",
    legend = c("Hit", "Miss"),
    col = c("#20656a", "#c77c48"),
    pch = 19,
    bty = "n",
    cex = text_style$legend,
    text.font = 2
  )
}

plot_guided_network <- function(tree, state, pending_positive = character(0)) {
  full_layout <- build_tree_layout(collect_active_subtree(tree, tree$root, tree$nodes))
  node_df <- full_layout$nodes
  edge_df <- full_layout$edges

  remaining_sources <- intersect(state$active_nodes, tree$source_nodes)
  recommended_nodes <- state$recommended_nodes

  node_fill <- rep("#d56a6a", nrow(node_df))
  node_border <- rep("#7f2f35", nrow(node_df))
  node_lwd <- rep(1, nrow(node_df))

  weight_scale <- tree$probabilities[node_df$node]
  point_cex <- compute_node_cex(weight_scale, nrow(node_df))
  plot_limits <- compute_layout_limits(node_df, point_cex, top_label_offset = 0.75)

  in_remaining <- node_df$node %in% remaining_sources
  if (any(in_remaining)) {
    source_palette <- grDevices::colorRampPalette(c("#d7eee8", "#3f827f"))(100)
    source_index <- pmax(1, pmin(100, ceiling(tree$probabilities[node_df$node[in_remaining]] * 99) + 1))
    node_fill[in_remaining] <- source_palette[source_index]
    node_border[in_remaining] <- "#24545b"
  }

  in_recommended <- node_df$node %in% recommended_nodes
  node_fill[in_recommended] <- "#f1c56d"
  node_border[in_recommended] <- "#9e6a16"
  node_lwd[in_recommended] <- 2.2

  in_pending <- node_df$node %in% pending_positive
  node_fill[in_pending] <- "#1f6f73"
  node_border[in_pending] <- "#103e41"
  node_lwd[in_pending] <- 2.8

  is_focus_root <- node_df$node == state$current_root
  node_border[is_focus_root] <- "#17323b"
  node_lwd[is_focus_root] <- 3

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  text_style <- set_plot_par(mar = c(1.5, 1.5, 5.1, 1.5), bg = NA, family = "sans", xpd = NA)
  graphics::plot(
    node_df$x,
    node_df$y,
    type = "n",
    axes = FALSE,
    ann = FALSE,
    xlim = plot_limits$xlim,
    ylim = plot_limits$ylim,
    xaxs = "i",
    yaxs = "i"
  )

  if (nrow(edge_df) > 0) {
    edge_col <- ifelse(
      edge_df$child %in% remaining_sources | edge_df$parent %in% remaining_sources,
      grDevices::adjustcolor("#8cb4ab", alpha.f = 0.7),
      grDevices::adjustcolor("#c78686", alpha.f = 0.35)
    )

    graphics::segments(
      x0 = edge_df$x,
      y0 = edge_df$y,
      x1 = edge_df$xend,
      y1 = edge_df$yend,
      col = edge_col,
      lwd = 1.8
    )
  }

  graphics::points(
    node_df$x,
    node_df$y,
    pch = 21,
    cex = point_cex,
    bg = node_fill,
    col = node_border,
    lwd = pmax(node_lwd, 1.5)
  )

  labeled_nodes <- unique(c(state$current_root, recommended_nodes, pending_positive))
  labeled_nodes <- labeled_nodes[nzchar(labeled_nodes)]
  if (length(labeled_nodes) > 0) {
    label_df <- node_df[node_df$node %in% labeled_nodes, , drop = FALSE]
    graphics::text(
      x = label_df$x,
      y = label_df$y + 0.55,
      labels = format_identifier_label(label_df$node),
      family = "serif",
      cex = text_style$point_label,
      col = "#17323b",
      font = 2
    )
  }

  graphics::title(
    main = "Sewer network guidance status",
    sub = paste(
      "Node size shows direct prior probability.",
      "Color encodes search status; the thick dark border marks the current focus node."
    ),
    family = "serif",
    cex.main = text_style$main,
    cex.sub = text_style$sub,
    font.main = 2,
    font.sub = 2,
    col.sub = "#4d666d"
  )

  draw_auto_legend(
    node_df = node_df,
    edge_df = edge_df,
    plot_limits = plot_limits,
    legend_args = list(
      legend = c(
        "Remaining source candidate",
        "Recommended for sampling",
        "Selected as positive",
        "Excluded as source"
      ),
      pt.bg = c("#5f9b96", "#f1c56d", "#1f6f73", "#d56a6a"),
      col = c("#24545b", "#9e6a16", "#103e41", "#7f2f35"),
      pch = 21,
      pt.cex = 1.9,
      pt.lwd = c(1.2, 2.2, 2.8, 1.2),
      bty = "o",
      cex = text_style$legend,
      text.font = 2,
      bg = grDevices::adjustcolor("#ffffff", alpha.f = 0.9),
      box.col = grDevices::adjustcolor("#d6dfdb", alpha.f = 0.9)
    )
  )

  invisible(full_layout)
}
