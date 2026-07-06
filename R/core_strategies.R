strategy_catalog <- function() {
  list(
    nary_split = list(
      label = "kGBS",
      full_name = "k-Batch Generalized Binary Search",
      short = "Partitions remaining probability mass as evenly as possible.",
      detail = paste(
        "k-Batch Generalized Binary Search chooses sampling nodes that divide the",
        "remaining prior mass into similarly sized sections. This usually improves",
        "the expected number of sampling cycles."
      )
    ),
    max_rpop = list(
      label = "MRP",
      full_name = "Maximum relative population algorithm",
      short = "Prioritises nodes with the largest direct node weight.",
      detail = paste(
        "The Maximum relative population algorithm focuses on nodes that are most",
        "likely to be the source themselves. This can create fast early hits but",
        "usually narrows the search space less evenly."
      )
    ),
    max_cum_rpop = list(
      label = "MCRP",
      full_name = "Maximum cumulative relative population algorithm",
      short = "Prioritises nodes with the largest subtree probability mass.",
      detail = paste(
        "The Maximum cumulative relative population algorithm chooses nodes whose",
        "full upstream subtree contains the most remaining probability. This often",
        "mirrors a greedy coverage strategy."
      )
    ),
    skipping_cum_rpop = list(
      label = "SMCRP",
      full_name = "Skipping maximum cumulative relative population algorithm",
      short = "Like MCRP, but avoids direct parent-child pairs within one cycle.",
      detail = paste(
        "The Skipping maximum cumulative relative population algorithm keeps samples",
        "from stacking directly on top of one another in the same cycle unless there",
        "are not enough independent alternatives left."
      )
    )
  )
}

# Shared tie-breaking helper used by all greedy strategies:
# prefer larger primary score, then larger secondary score, then a stable
# lexicographic node order so that runs remain deterministic.
rank_nodes <- function(nodes, primary, secondary = NULL) {
  if (!length(nodes)) {
    return(character(0))
  }

  secondary <- if (is.null(secondary)) rep(0, length(nodes)) else secondary
  order_idx <- order(-primary, -secondary, nodes)
  nodes[order_idx]
}

# MCRP ranks by cumulative upstream mass and uses direct node mass only to
# resolve ties between equally informative subtrees.
select_max_cum_rpop <- function(snapshot, testers) {
  candidates <- setdiff(snapshot$nodes, snapshot$current_root)
  ranked <- rank_nodes(
    nodes = candidates,
    primary = snapshot$cum_weight[candidates],
    secondary = snapshot$raw_weight[candidates]
  )
  head(ranked, testers)
}

# MRP ranks by direct node probability; cumulative upstream mass is used only
# as a deterministic tie-break and is not the optimisation target itself.
select_max_rpop <- function(snapshot, testers) {
  candidates <- setdiff(snapshot$nodes, snapshot$current_root)
  ranked <- rank_nodes(
    nodes = candidates,
    primary = snapshot$raw_weight[candidates],
    secondary = snapshot$cum_weight[candidates]
  )
  head(ranked, testers)
}

# SMCRP starts from the MCRP ranking but temporarily blocks immediate parent
# and child nodes so that multiple samplers are spread across non-adjacent
# sub-sewersheds whenever enough alternatives are available.
select_skipping_cum_rpop <- function(snapshot, testers) {
  candidates <- setdiff(snapshot$nodes, snapshot$current_root)
  ranked <- rank_nodes(
    nodes = candidates,
    primary = snapshot$cum_weight[candidates],
    secondary = snapshot$raw_weight[candidates]
  )

  selected <- character(0)
  blocked <- character(0)

  for (node in ranked) {
    if (length(selected) >= testers) {
      break
    }

    if (node %in% blocked) {
      next
    }

    selected <- c(selected, node)
    parent_node <- snapshot$parent[[node]]
    child_nodes <- snapshot$active_children[[node]]
    blocked <- unique(c(blocked, parent_node, child_nodes))
  }

  if (length(selected) < testers) {
    fallback <- setdiff(ranked, selected)
    selected <- c(selected, head(fallback, testers - length(selected)))
  }

  selected
}

# kGBS greedily approximates a (k+1)-way balanced partition of the remaining
# prior mass. Each selected node claims one upstream block, and the unresolved
# complement forms the final block.
select_nary_split <- function(snapshot, testers) {
  candidates <- setdiff(snapshot$nodes, snapshot$current_root)
  max_picks <- min(length(candidates), testers)

  if (max_picks == 0L) {
    return(character(0))
  }

  adjusted_mass <- snapshot$cum_weight
  excluded <- setNames(rep(FALSE, length(snapshot$nodes)), snapshot$nodes)
  selected <- character(0)
  target <- 1 / (testers + 1)

  for (i in seq_len(max_picks)) {
    pool <- setdiff(candidates[!excluded[candidates]], selected)

    if (length(pool) > 0) {
      # Choose the subtree whose remaining cumulative mass is closest to the
      # ideal block size. Ties are resolved in favour of larger masses, then
      # larger direct node probabilities, then node order.
      distances <- abs(adjusted_mass[pool] - target)
      ord <- order(distances, -adjusted_mass[pool], -snapshot$raw_weight[pool], pool)
      chosen <- pool[[ord[[1]]]]

      selected <- c(selected, chosen)
      mass <- adjusted_mass[[chosen]]
      ancestor <- snapshot$parent[[chosen]]

      # Once a subtree has been assigned to one sampler, remove its mass from
      # all active downstream ancestors so that later picks partition only the
      # still-unresolved remainder.
      while (!is.na(ancestor) && ancestor %in% snapshot$nodes) {
        adjusted_mass[[ancestor]] <- adjusted_mass[[ancestor]] - mass
        ancestor <- snapshot$parent[[ancestor]]
      }

      # Prevent nested selections inside the same claimed upstream block.
      excluded[snapshot$subtree_nodes[[chosen]]] <- TRUE
    } else {
      fallback <- setdiff(candidates, selected)
      if (!length(fallback)) {
        break
      }

      # Fallback preserves deterministic behaviour when topology prevents a
      # further non-overlapping balanced split.
      ord <- order(
        -snapshot$raw_weight[fallback],
        -snapshot$cum_weight[fallback],
        fallback
      )
      chosen <- fallback[[ord[[1]]]]
      selected <- c(selected, chosen)
    }
  }

  selected
}

# Dispatch to the requested strategy after normalising the sampler count to at
# least one active sampling location.
select_strategy_nodes <- function(snapshot, strategy_id, testers) {
  testers <- max(1L, as.integer(testers))

  switch(
    strategy_id,
    nary_split = select_nary_split(snapshot, testers),
    max_rpop = select_max_rpop(snapshot, testers),
    max_cum_rpop = select_max_cum_rpop(snapshot, testers),
    skipping_cum_rpop = select_skipping_cum_rpop(snapshot, testers),
    stop("Unknown strategy: ", strategy_id, call. = FALSE)
  )
}
