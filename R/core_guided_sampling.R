empty_guided_history <- function() {
  data.frame(
    cycle = integer(0),
    tested_nodes = character(0),
    positive_nodes = character(0),
    outcome = character(0),
    focus_node = character(0),
    remaining_source_count = integer(0),
    identified_source = character(0),
    stringsAsFactors = FALSE
  )
}

# Recompute the guided-session state after every configuration change or cycle.
# The remaining feasible sources are always the active nodes with positive prior
# weight under the current deterministic search constraints.
refresh_guided_state <- function(tree, state) {
  snapshot <- collect_active_subtree(tree, state$current_root, state$active_nodes)
  remaining_sources <- intersect(state$active_nodes, tree$source_nodes)
  state$remaining_sources <- remaining_sources
  state$excluded_nodes <- setdiff(tree$nodes, remaining_sources)
  state$snapshot <- snapshot

  if (!length(remaining_sources)) {
    state$recommended_nodes <- character(0)
    state$identified_source <- NA_character_
    state$done <- FALSE
    state$inconsistent <- TRUE
    return(state)
  }

  if (length(remaining_sources) == 1L) {
    state$recommended_nodes <- character(0)
    state$identified_source <- remaining_sources[[1]]
    state$done <- TRUE
    state$inconsistent <- FALSE
    return(state)
  }

  state$recommended_nodes <- select_strategy_nodes(
    snapshot = snapshot,
    strategy_id = state$strategy_id,
    testers = state$testers
  )
  state$identified_source <- NA_character_
  state$done <- length(state$recommended_nodes) == 0L
  state$inconsistent <- FALSE
  state
}

# Initialise guided sampling on the full validated sewer network.
initialize_guided_state <- function(tree, strategy_id, testers) {
  refresh_guided_state(
    tree = tree,
    state = list(
      strategy_id = strategy_id,
      testers = max(1L, as.integer(testers)),
      active_nodes = tree$nodes,
      current_root = tree$root,
      cycle = 0L,
      history = empty_guided_history(),
      last_tested = character(0),
      last_positive = character(0),
      identified_source = NA_character_,
      done = FALSE,
      inconsistent = FALSE
    )
  )
}

# Advance one guided cycle by applying the user-reported positive samples. All
# recommended but unselected nodes are treated as negative in that cycle.
advance_guided_state <- function(tree, state, positive_nodes = character(0)) {
  if (isTRUE(state$done) || isTRUE(state$inconsistent)) {
    return(refresh_guided_state(tree, state))
  }

  tested_nodes <- state$recommended_nodes
  if (!length(tested_nodes)) {
    return(refresh_guided_state(tree, state))
  }

  snapshot <- state$snapshot
  update <- apply_cycle_results(
    snapshot = snapshot,
    active_nodes = state$active_nodes,
    current_root = state$current_root,
    tested_nodes = tested_nodes,
    positive_nodes = positive_nodes
  )
  state$active_nodes <- update$active_nodes
  state$current_root <- update$current_root
  focus_node <- update$focus_node
  positive_nodes <- update$positive_nodes
  outcome <- if (isTRUE(update$has_positive)) "positive" else "negative"

  state$cycle <- state$cycle + 1L
  state$last_tested <- tested_nodes
  state$last_positive <- positive_nodes
  state <- refresh_guided_state(tree, state)

  # Store a compact audit trail so that reviewers and users can reconstruct
  # every guided decision made within the current session.
  history_row <- data.frame(
    cycle = state$cycle,
    tested_nodes = paste(tested_nodes, collapse = ", "),
    positive_nodes = if (length(positive_nodes) > 0) {
      paste(positive_nodes, collapse = ", ")
    } else {
      "none"
    },
    outcome = outcome,
    focus_node = focus_node,
    remaining_source_count = length(state$remaining_sources),
    identified_source = if (is.na(state$identified_source)) "" else state$identified_source,
    stringsAsFactors = FALSE
  )

  state$history <- rbind(state$history, history_row)
  state
}
