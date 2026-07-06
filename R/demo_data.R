swans_demo_template_filenames <- function() {
  list(
    network = "demo_network_100_adjacency.csv",
    weights = "demo_weights_100.csv"
  )
}

swans_demo_node_ids <- function() {
  c(sprintf("N%03d", seq_len(99)), "Outlet")
}

swans_demo_flow_edges <- function() {
  data.frame(
    from = c(
      "N006", "N020", "N039", "N040", "N041", "N042", "N062", "N063", "N064", "N079",
      "N080", "N081", "N082", "N092", "N093", "N007", "N008", "N024", "N009", "N010",
      "N026", "N011", "N012", "N013", "N031", "N014", "N015", "N016", "N034", "N017",
      "N018", "N019", "N038", "N021", "N022", "N023", "N025", "N027", "N028", "N029",
      "N030", "N032", "N033", "N035", "N036", "N037", "N043", "N044", "N045", "N046",
      "N049", "N050", "N051", "N054", "N055", "N059", "N060", "N061", "N047", "N048",
      "N052", "N053", "N056", "N057", "N058", "N065", "N066", "N067", "N068", "N069",
      "N070", "N071", "N076", "N077", "N072", "N073", "N074", "N075", "N078", "N083",
      "N087", "N088", "N089", "N091", "N084", "N085", "N086", "N090", "N094", "N098",
      "N099", "N095", "N096", "N097", "N001", "N002", "N003", "N004", "N005"
    ),
    to = c(
      "N001", "N001", "N002", "N002", "N002", "N002", "N003", "N003", "N003", "N004",
      "N004", "N004", "N004", "N005", "N005", "N006", "N007", "N007", "N008", "N009",
      "N009", "N010", "N011", "N012", "N012", "N013", "N014", "N015", "N015", "N016",
      "N017", "N018", "N018", "N020", "N021", "N022", "N024", "N026", "N027", "N028",
      "N029", "N031", "N032", "N034", "N035", "N036", "N039", "N039", "N039", "N039",
      "N040", "N040", "N040", "N041", "N041", "N042", "N042", "N042", "N044", "N044",
      "N049", "N051", "N055", "N056", "N057", "N062", "N062", "N062", "N062", "N062",
      "N062", "N063", "N064", "N064", "N071", "N072", "N073", "N074", "N076", "N079",
      "N080", "N080", "N081", "N082", "N083", "N084", "N085", "N089", "N092", "N093",
      "N093", "N094", "N095", "N096", "Outlet", "Outlet", "Outlet", "Outlet", "Outlet"
    ),
    stringsAsFactors = FALSE
  )
}

swans_demo_weight_input <- function() {
  data.frame(
    node = sprintf("N%03d", seq_len(99)),
    weight = c(
      18L, 11L, 9L, 7L, 15L, 8L, 6L, 18L, 11L, 9L,
      7L, 15L, 8L, 6L, 18L, 11L, 9L, 7L, 15L, 8L,
      6L, 18L, 11L, 9L, 7L, 15L, 8L, 6L, 18L, 11L,
      9L, 7L, 15L, 8L, 11L, 9L, 7L, 6L, 10L, 11L,
      9L, 7L, 6L, 10L, 11L, 9L, 7L, 6L, 10L, 11L,
      9L, 7L, 6L, 10L, 11L, 9L, 7L, 6L, 15L, 7L,
      4L, 11L, 5L, 3L, 15L, 7L, 4L, 11L, 5L, 3L,
      15L, 7L, 4L, 11L, 5L, 3L, 12L, 6L, 4L, 8L,
      12L, 6L, 4L, 8L, 12L, 6L, 4L, 8L, 12L, 6L,
      14L, 5L, 10L, 14L, 5L, 10L, 14L, 5L, 10L
    ),
    stringsAsFactors = FALSE
  )
}

swans_demo_adjacency_df <- function() {
  nodes <- swans_demo_node_ids()
  edge_matrix <- matrix(
    0L,
    nrow = length(nodes),
    ncol = length(nodes),
    dimnames = list(nodes, nodes)
  )

  flow_edges <- swans_demo_flow_edges()
  edge_positions <- cbind(
    match(flow_edges$from, nodes),
    match(flow_edges$to, nodes)
  )
  edge_matrix[edge_positions] <- 1L

  data.frame(
    Node = rownames(edge_matrix),
    as.data.frame(edge_matrix, check.names = FALSE),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

build_demo_model <- function() {
  flow_edges <- swans_demo_flow_edges()
  parsed_weights <- parse_weights_table(swans_demo_weight_input())

  build_tree_model(
    edges = data.frame(
      parent = flow_edges$to,
      child = flow_edges$from,
      stringsAsFactors = FALSE
    ),
    weights = parsed_weights$data,
    network_meta = list(
      format = "adjacency",
      convention = "row_to_column_flow",
      raw_edge_count = nrow(flow_edges),
      built_in_example = TRUE
    ),
    weight_meta = parsed_weights$meta
  )
}

write_demo_adjacency_csv <- function(path) {
  utils::write.csv(
    swans_demo_adjacency_df(),
    file = path,
    row.names = FALSE,
    quote = FALSE
  )
}

write_demo_weight_csv <- function(path) {
  utils::write.csv(
    swans_demo_weight_input(),
    file = path,
    row.names = FALSE,
    quote = FALSE
  )
}
