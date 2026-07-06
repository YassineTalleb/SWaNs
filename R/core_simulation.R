weighted_distribution <- function(values, weights, value_name = "cycles") {
  aggregated <- stats::aggregate(weights, by = list(value = values), FUN = sum)
  aggregated <- aggregated[order(aggregated$value), , drop = FALSE]
  names(aggregated) <- c(value_name, "probability")
  aggregated
}

# Match the boxplot whiskers used in the app by explicitly returning the
# five-number summary with min and max as whisker endpoints.
boxplot_summary_stats <- function(values) {
  stats_values <- grDevices::boxplot.stats(
    values,
    coef = 0,
    do.conf = FALSE,
    do.out = FALSE
  )$stats

  list(
    min = stats_values[[1]],
    lower_quartile = stats_values[[2]],
    median = stats_values[[3]],
    upper_quartile = stats_values[[4]],
    max = stats_values[[5]]
  )
}

# Deterministic single-source execution of one strategy. Once source node,
# sampler count, and strategy are fixed, the full search path is reproducible.
simulate_search <- function(tree, source_node, strategy_id, testers, keep_history = FALSE) {
  if (!source_node %in% tree$source_nodes) {
    stop("Source node must have a positive weight in the model.", call. = FALSE)
  }

  active_nodes <- tree$nodes
  current_root <- tree$root
  cycle_count <- 0L
  total_tests <- 0L
  history_rows <- list()

  repeat {
    snapshot <- collect_active_subtree(tree, current_root, active_nodes)
    candidates <- setdiff(snapshot$nodes, snapshot$current_root)

    if (!length(candidates)) {
      break
    }

    tested_nodes <- select_strategy_nodes(snapshot, strategy_id, testers)
    if (!length(tested_nodes)) {
      break
    }

    cycle_count <- cycle_count + 1L
    total_tests <- total_tests + length(tested_nodes)

    # Under the deterministic observation model, a tested node is positive iff
    # the true source lies inside its upstream subtree.
    positive_mask <- vapply(
      tested_nodes,
      function(node) source_node %in% snapshot$subtree_nodes[[node]],
      logical(1)
    )
    positive_nodes <- tested_nodes[positive_mask]
    update <- apply_cycle_results(
      snapshot = snapshot,
      active_nodes = active_nodes,
      current_root = current_root,
      tested_nodes = tested_nodes,
      positive_nodes = positive_nodes
    )
    active_nodes <- update$active_nodes
    current_root <- update$current_root
    focus_node <- update$focus_node
    positive_nodes <- update$positive_nodes
    outcome <- if (isTRUE(update$has_positive)) "hit" else "miss"

    if (keep_history) {
      history_rows[[length(history_rows) + 1L]] <- data.frame(
        cycle = cycle_count,
        tested_nodes = paste(tested_nodes, collapse = ", "),
        positive_nodes = if (length(positive_nodes)) {
          paste(positive_nodes, collapse = ", ")
        } else {
          "none"
        },
        outcome = outcome,
        focus_node = focus_node,
        remaining_candidates = length(active_nodes),
        remaining_probability = sum(tree$probabilities[active_nodes]),
        stringsAsFactors = FALSE
      )
    }
  }

  history <- if (length(history_rows)) {
    do.call(rbind, history_rows)
  } else {
    data.frame(
      cycle = integer(0),
      tested_nodes = character(0),
      positive_nodes = character(0),
      outcome = character(0),
      focus_node = character(0),
      remaining_candidates = integer(0),
      remaining_probability = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  list(
    source_node = source_node,
    cycles = cycle_count,
    total_tests = total_tests,
    identified_node = current_root,
    success = identical(current_root, source_node) && length(active_nodes) == 1L,
    history = history
  )
}

# Exhaustively evaluate one strategy for every feasible source node and return
# both prior-weighted expectations and unweighted per-source summaries.
evaluate_strategy <- function(tree, strategy_id, testers) {
  source_probs <- tree$probabilities[tree$source_nodes]
  per_source <- vector("list", length(tree$source_nodes))

  for (i in seq_along(tree$source_nodes)) {
    source_node <- tree$source_nodes[[i]]
    run <- simulate_search(
      tree = tree,
      source_node = source_node,
      strategy_id = strategy_id,
      testers = testers,
      keep_history = FALSE
    )

    per_source[[i]] <- data.frame(
      source_node = source_node,
      probability = source_probs[[source_node]],
      cycles = run$cycles,
      total_tests = run$total_tests,
      success = run$success,
      stringsAsFactors = FALSE
    )
  }

  per_source <- do.call(rbind, per_source)
  cycle_distribution <- weighted_distribution(per_source$cycles, per_source$probability, "cycles")
  cycle_distribution$cdf <- cumsum(cycle_distribution$probability)
  cycle_stats <- boxplot_summary_stats(per_source$cycles)
  total_test_stats <- boxplot_summary_stats(per_source$total_tests)

  list(
    strategy = strategy_id,
    testers = testers,
    per_source = per_source,
    distributions = cycle_distribution,
    summary = data.frame(
      strategy = strategy_id,
      testers = testers,
      expected_cycles = sum(per_source$cycles * per_source$probability),
      mean_cycles = mean(per_source$cycles),
      min_cycles = cycle_stats$min,
      lower_quartile_cycles = cycle_stats$lower_quartile,
      median_cycles = cycle_stats$median,
      upper_quartile_cycles = cycle_stats$upper_quartile,
      max_cycles = cycle_stats$max,
      expected_total_tests = sum(per_source$total_tests * per_source$probability),
      mean_total_tests = mean(per_source$total_tests),
      min_total_tests = total_test_stats$min,
      lower_quartile_total_tests = total_test_stats$lower_quartile,
      median_total_tests = total_test_stats$median,
      upper_quartile_total_tests = total_test_stats$upper_quartile,
      max_total_tests = total_test_stats$max,
      stringsAsFactors = FALSE
    )
  )
}

# Compare all requested strategy/sampler combinations exactly rather than by
# Monte Carlo simulation; this is feasible because every run is deterministic.
compare_strategies <- function(tree, testers_range, strategies, progress = NULL) {
  testers_range <- sort(unique(as.integer(testers_range)))
  strategies <- unique(strategies)

  total_jobs <- length(testers_range) * length(strategies)
  job_index <- 0L
  evaluations <- list()
  summary_rows <- list()

  for (strategy_id in strategies) {
    for (testers in testers_range) {
      job_index <- job_index + 1L
      if (is.function(progress)) {
        progress(
          job_index / total_jobs,
          paste(
            "Evaluating",
            strategy_catalog()[[strategy_id]]$label,
            "with",
            testers,
            "sampler(s)"
          )
        )
      }

      evaluation <- evaluate_strategy(tree, strategy_id, testers)
      key <- paste(strategy_id, testers, sep = "::")
      evaluations[[key]] <- evaluation
      summary_rows[[key]] <- evaluation$summary
    }
  }

  summary_df <- do.call(rbind, summary_rows)
  rownames(summary_df) <- NULL

  list(summary = summary_df, evaluations = evaluations)
}

identify_parallel_strategy <- function(summary_df, preferred_strategy = "nary_split") {
  if (!nrow(summary_df)) {
    return(NA_character_)
  }

  if (preferred_strategy %in% summary_df$strategy) {
    preferred_strategy
  } else {
    unique(summary_df$strategy)[1]
  }
}

format_tester_selection <- function(testers) {
  testers <- sort(unique(as.integer(testers)))

  if (!length(testers)) {
    return(NA_character_)
  }

  if (length(testers) == 1L) {
    return(as.character(testers))
  }

  if (all(diff(testers) == 1L)) {
    return(paste(range(testers), collapse = " to "))
  }

  paste(testers, collapse = ", ")
}

compute_parallel_efficiency <- function(summary_df, preferred_strategy = "nary_split") {
  target_strategy <- identify_parallel_strategy(summary_df, preferred_strategy)
  if (is.na(target_strategy)) {
    return(NULL)
  }

  strat_df <- summary_df[summary_df$strategy == target_strategy, , drop = FALSE]
  strat_df <- strat_df[order(strat_df$testers), , drop = FALSE]
  rownames(strat_df) <- NULL

  if (!nrow(strat_df)) {
    return(NULL)
  }

  previous_cycles <- c(NA_real_, head(strat_df$expected_cycles, -1L))
  previous_tests <- c(NA_real_, head(strat_df$expected_total_tests, -1L))
  previous_testers <- c(NA_integer_, head(strat_df$testers, -1L))

  strat_df$previous_testers <- previous_testers
  strat_df$marginal_cycle_gain <- previous_cycles - strat_df$expected_cycles
  strat_df$marginal_test_increase <- strat_df$expected_total_tests - previous_tests

  # Efficiency is defined as cycles saved per additional expected sample when
  # moving from k-1 to k samplers for the same strategy.
  strat_df$marginal_efficiency <- vapply(
    seq_len(nrow(strat_df)),
    function(i) {
      gain <- strat_df$marginal_cycle_gain[[i]]
      test_delta <- strat_df$marginal_test_increase[[i]]

      if (is.na(gain) || is.na(test_delta)) {
        return(NA_real_)
      }

      # When an additional sampler reduces both cycles and expected samples, the
      # higher sampler count strictly dominates the previous one.
      if (test_delta <= 0) {
        if (gain > 0) {
          return(Inf)
        }
        return(0)
      }

      gain / test_delta
    },
    numeric(1)
  )

  strat_df
}

# Add the same marginal-efficiency quantity to the full summary table, computed
# separately within each strategy after ordering rows by the number of samplers.
add_marginal_efficiency <- function(summary_df) {
  if (!nrow(summary_df)) {
    summary_df$marginal_efficiency <- numeric(0)
    return(summary_df)
  }

  strategy_order <- unique(summary_df$strategy)
  rows <- lapply(strategy_order, function(strategy_id) {
    strategy_df <- summary_df[summary_df$strategy == strategy_id, , drop = FALSE]
    strategy_df <- strategy_df[order(strategy_df$testers), , drop = FALSE]

    previous_cycles <- c(NA_real_, head(strategy_df$expected_cycles, -1L))
    previous_samples <- c(NA_real_, head(strategy_df$expected_total_tests, -1L))
    cycle_gain <- previous_cycles - strategy_df$expected_cycles
    sample_delta <- strategy_df$expected_total_tests - previous_samples

    strategy_df$marginal_efficiency <- vapply(
      seq_len(nrow(strategy_df)),
      function(i) {
        gain <- cycle_gain[[i]]
        delta <- sample_delta[[i]]

        if (is.na(gain) || is.na(delta)) {
          return(NA_real_)
        }

        if (delta <= 0) {
          if (gain > 0) {
            return(Inf)
          }
          return(0)
        }

        gain / delta
      },
      numeric(1)
    )

    strategy_df
  })

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

estimate_parallel_recommendation <- function(
  summary_df,
  preferred_strategy = "nary_split",
  efficiency_threshold = 0.75
) {
  efficiency_df <- compute_parallel_efficiency(summary_df, preferred_strategy)

  if (is.null(efficiency_df) || nrow(efficiency_df) < 2L) {
    return(list(
      available = FALSE,
      reason = "insufficient_data",
      strategy = identify_parallel_strategy(summary_df, preferred_strategy),
      recommended_testers = integer(0),
      range_label = NA_character_,
      threshold = efficiency_threshold,
      peak_efficiency = NA_real_,
      efficiency_table = efficiency_df
    ))
  }

  positive_rows <- efficiency_df[
    is.finite(efficiency_df$marginal_efficiency) &
      efficiency_df$marginal_efficiency > 0 &
      !is.na(efficiency_df$previous_testers),
    ,
    drop = FALSE
  ]

  dominating_rows <- efficiency_df[
    is.infinite(efficiency_df$marginal_efficiency) &
      !is.na(efficiency_df$previous_testers),
    ,
    drop = FALSE
  ]

  if (nrow(dominating_rows)) {
    recommended <- dominating_rows$testers
    return(list(
      available = TRUE,
      reason = "dominant",
      strategy = efficiency_df$strategy[[1]],
      recommended_testers = recommended,
      range_label = format_tester_selection(recommended),
      threshold = efficiency_threshold,
      peak_efficiency = Inf,
      efficiency_table = efficiency_df
    ))
  }

  if (!nrow(positive_rows)) {
    baseline <- min(efficiency_df$testers)
    return(list(
      available = TRUE,
      reason = "no_positive_gain",
      strategy = efficiency_df$strategy[[1]],
      recommended_testers = baseline,
      range_label = as.character(baseline),
      threshold = efficiency_threshold,
      peak_efficiency = 0,
      efficiency_table = efficiency_df
    ))
  }

  peak_efficiency <- max(positive_rows$marginal_efficiency)
  recommended <- positive_rows$testers[
    positive_rows$marginal_efficiency >= efficiency_threshold * peak_efficiency
  ]

  list(
    available = TRUE,
    reason = "estimated",
    strategy = efficiency_df$strategy[[1]],
    recommended_testers = recommended,
    range_label = format_tester_selection(recommended),
    threshold = efficiency_threshold,
    peak_efficiency = peak_efficiency,
    efficiency_table = efficiency_df
  )
}

find_recommended_parallel_window <- function(
  summary_df,
  preferred_strategy = "nary_split",
  efficiency_threshold = 0.75
) {
  recommendation <- estimate_parallel_recommendation(
    summary_df = summary_df,
    preferred_strategy = preferred_strategy,
    efficiency_threshold = efficiency_threshold
  )

  if (!isTRUE(recommendation$available)) {
    return(NA_character_)
  }

  recommendation$range_label
}
