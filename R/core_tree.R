make_children_map <- function(nodes, edges) {
  children <- setNames(vector("list", length(nodes)), nodes)
  for (node in nodes) {
    children[[node]] <- character(0)
  }

  if (nrow(edges) > 0) {
    split_children <- split(edges$child, edges$parent)
    for (node in names(split_children)) {
      children[[node]] <- unique(split_children[[node]])
    }
  }

  children
}

# Kahn-style topological traversal used only for validation: if not all nodes
# are visited, the uploaded directed structure contains a cycle.
topological_order <- function(nodes, edges, children_map) {
  indegree <- setNames(integer(length(nodes)), nodes)
  if (nrow(edges) > 0) {
    child_counts <- table(edges$child)
    indegree[names(child_counts)] <- as.integer(child_counts)
  }

  queue <- nodes[indegree == 0]
  queue <- sort(queue)
  visited <- character(0)

  while (length(queue) > 0) {
    node <- queue[[1]]
    queue <- queue[-1]
    visited <- c(visited, node)

    for (child in children_map[[node]]) {
      indegree[child] <- indegree[child] - 1L
      if (indegree[child] == 0L) {
        queue <- c(queue, child)
        queue <- sort(unique(queue))
      }
    }
  }

  list(order = visited, acyclic = length(visited) == length(nodes))
}

# Reachability check from the outlet root. A valid sampling tree must consist
# of exactly one connected component downstream of that root.
reachable_nodes <- function(root, children_map) {
  visited <- character(0)
  stack <- root

  while (length(stack) > 0) {
    node <- stack[[1]]
    stack <- stack[-1]

    if (node %in% visited) {
      next
    }

    visited <- c(visited, node)
    stack <- c(children_map[[node]], stack)
  }

  visited
}

# Build the internal rooted-tree representation after import. Inputs are
# already parsed into downstream parent -> upstream child edges and normalised
# node probabilities.
build_tree_model <- function(edges, weights, network_meta = list(), weight_meta = list()) {
  edges <- unique(as.data.frame(edges, stringsAsFactors = FALSE))
  if (!all(c("parent", "child") %in% names(edges))) {
    stop("Edges must contain 'parent' and 'child' columns.", call. = FALSE)
  }

  weights <- as.data.frame(weights, stringsAsFactors = FALSE)
  if (!all(c("node", "weight", "probability") %in% names(weights))) {
    stop("Weights must contain 'node', 'weight', and 'probability' columns.", call. = FALSE)
  }

  nodes <- unique(c(edges$parent, edges$child, weights$node))
  nodes <- nodes[nzchar(nodes)]
  nodes <- sort(nodes)

  if (!length(nodes)) {
    stop("No nodes were found after parsing the network and weight files.", call. = FALSE)
  }

  if (any(edges$parent == edges$child)) {
    stop("Self-loops are not allowed in the network graph.", call. = FALSE)
  }

  parent <- setNames(rep(NA_character_, length(nodes)), nodes)
  if (nrow(edges) > 0) {
    duplicated_children <- unique(edges$child[duplicated(edges$child)])
    if (length(duplicated_children) > 0) {
      stop(
        paste(
          "The network is not a rooted tree because some nodes have multiple parents:",
          paste(duplicated_children, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    parent[edges$child] <- edges$parent
  }

  roots <- nodes[is.na(parent)]
  if (length(roots) != 1L) {
    stop(
      paste(
        "Exactly one root is required for the sampling tree, but",
        length(roots),
        "roots were found."
      ),
      call. = FALSE
    )
  }

  children <- make_children_map(nodes, edges)
  topo <- topological_order(nodes, edges, children)
  if (!isTRUE(topo$acyclic)) {
    stop("The uploaded network contains a directed cycle.", call. = FALSE)
  }

  root <- roots[[1]]
  reached <- reachable_nodes(root, children)
  if (length(reached) != length(nodes)) {
    missing_nodes <- setdiff(nodes, reached)
    stop(
      paste(
        "The network is disconnected from the root. Unreachable nodes:",
        paste(missing_nodes, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  raw_weights <- setNames(rep(0, length(nodes)), nodes)
  raw_weights[weights$node] <- weights$weight
  probabilities <- setNames(rep(0, length(nodes)), nodes)
  probabilities[weights$node] <- weights$probability

  source_nodes <- names(probabilities)[probabilities > 0]
  if (!length(source_nodes)) {
    stop("At least one node must have a positive weight.", call. = FALSE)
  }

  warnings <- character(0)
  if (probabilities[root] > 0) {
    warnings <- c(
      warnings,
      "The root has a positive weight. This is allowed, but it means the outlet itself can be the source."
    )
  }

  zero_weight_nodes <- names(probabilities)[probabilities == 0]
  validation <- list(
    valid = TRUE,
    warnings = warnings,
    summary = list(
      node_count = length(nodes),
      edge_count = nrow(edges),
      source_count = length(source_nodes),
      root = root,
      zero_weight_count = length(zero_weight_nodes),
      total_weight = sum(raw_weights)
    )
  )

  structure(
    list(
      nodes = nodes,
      edges = edges,
      root = root,
      parent = parent,
      children = children,
      raw_weights = raw_weights,
      probabilities = probabilities,
      source_nodes = source_nodes,
      validation = validation,
      network_meta = network_meta,
      weight_meta = weight_meta
    ),
    class = "sampling_tree_model"
  )
}

# Restrict computations to the currently active part of the sewer network and
# recompute cumulative upstream mass there. These values drive all strategies.
collect_active_subtree <- function(tree, current_root, active_nodes = tree$nodes) {
  active_flag <- setNames(rep(FALSE, length(tree$nodes)), tree$nodes)
  active_flag[active_nodes] <- TRUE

  nodes <- character(0)
  depth <- setNames(integer(0), character(0))
  active_children <- setNames(vector("list", 0), character(0))
  queue <- current_root
  depth[current_root] <- 0L

  while (length(queue) > 0) {
    node <- queue[[1]]
    queue <- queue[-1]
    nodes <- c(nodes, node)

    node_children <- tree$children[[node]]
    node_children <- node_children[active_flag[node_children]]
    active_children[[node]] <- node_children

    if (length(node_children) > 0) {
      new_depth <- depth[[node]] + 1L
      depth[node_children] <- new_depth
      queue <- c(queue, node_children)
    }
  }

  subtree_nodes <- setNames(vector("list", length(nodes)), nodes)
  subtree_mass <- setNames(numeric(length(nodes)), nodes)

  # Process nodes bottom-up so that every upstream subtree mass is the direct
  # node probability plus the masses of its active upstream children.
  for (node in rev(nodes)) {
    node_children <- active_children[[node]]
    child_nodes <- if (length(node_children) > 0) {
      unlist(subtree_nodes[node_children], use.names = FALSE)
    } else {
      character(0)
    }

    subtree_nodes[[node]] <- c(node, child_nodes)
    subtree_mass[[node]] <- tree$probabilities[[node]] +
      sum(subtree_mass[node_children])
  }

  total_mass <- subtree_mass[[current_root]]
  cum_weight <- subtree_mass
  if (total_mass > 0) {
    cum_weight <- subtree_mass / total_mass
  }

  list(
    current_root = current_root,
    nodes = nodes,
    active_children = active_children,
    depth = depth[nodes],
    subtree_nodes = subtree_nodes,
    subtree_mass = subtree_mass[nodes],
    cum_weight = cum_weight[nodes],
    raw_weight = tree$probabilities[nodes],
    parent = tree$parent,
    total_mass = total_mass
  )
}

# Apply one full sampling cycle under the deterministic observation model.
# Positive samples restrict the source to the intersection of sampled upstream
# subtrees; negative samples remove their upstream subtrees completely.
apply_cycle_results <- function(snapshot, active_nodes, current_root, tested_nodes, positive_nodes = character(0)) {
  tested_nodes <- intersect(unique(tested_nodes), snapshot$nodes)
  positive_nodes <- intersect(unique(positive_nodes), tested_nodes)
  negative_nodes <- setdiff(tested_nodes, positive_nodes)

  next_active_nodes <- active_nodes
  next_root <- current_root
  focus_node <- current_root
  has_positive <- length(positive_nodes) > 0

  if (has_positive) {
    # A valid source must lie inside every positive sampled subtree.
    next_active_nodes <- Reduce(
      intersect,
      c(list(next_active_nodes), unname(snapshot$subtree_nodes[positive_nodes]))
    )

    depths <- snapshot$depth[positive_nodes]
    # Move the search root to the deepest positive node so that subsequent
    # strategy choices operate inside the most downstream confirmed region.
    ord <- order(-depths, positive_nodes)
    focus_node <- positive_nodes[[ord[[1]]]]
    next_root <- focus_node
  }

  if (length(negative_nodes) > 0) {
    # A negative sample excludes its entire upstream sub-sewershed.
    removed_nodes <- unique(unlist(snapshot$subtree_nodes[negative_nodes], use.names = FALSE))
    next_active_nodes <- setdiff(next_active_nodes, removed_nodes)
  }

  list(
    active_nodes = next_active_nodes,
    current_root = next_root,
    focus_node = focus_node,
    positive_nodes = positive_nodes,
    negative_nodes = negative_nodes,
    has_positive = has_positive
  )
}
