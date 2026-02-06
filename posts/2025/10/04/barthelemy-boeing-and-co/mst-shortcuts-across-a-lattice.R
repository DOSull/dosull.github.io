library(igraph)
library(sfnetworks) # makes visualizing graphs easier
library(sf)
library(dplyr)
library(tidyr)
library(DescTools)  # convenient all pairs and Gini functions
library(ggplot2)
library(cols4all)
library(patchwork)

# function to give edges as data frame including from/to vertices
get_edge_df <- function(G) {
  df <- G |>
    as_edgelist() |>
    as.data.frame() |>
    rename(from = V1, to = V2)
  # if there are edge attributes also include those
  if (length(edge_attr_names(G)) > 0) {
    df <- df |> bind_cols(G |> edge.attributes() |> as.data.frame())
  }
  df
}

# collapse all current 2-degree nodes by joining the pair of
# vertices they sit between and removing existing links
# this is done by compiling lists of edges to remove and add
# and then adding and removing all of them
# this is to avoid complications of modifying the structure
# while iterating over it
collapse_deg_2_nodes <- function(G) {
  # access vertices by name
  vs <- vertex_attr(G, "name")
  k2 <- V(G)[which(degree(G) == 2)]
  # empty lists for edges we will add and remove
  new_edges <- c()
  old_edges <- c()
  nodes_handled <- c()
  for (v in k2) {
    nodes_handled <- c(nodes_handled)
    # get this vertex's neighbourhood
    edges <- neighborhood(G, order = 1, nodes = V(G)[v]) |>
      unlist() |>
      as.vector()
    # split neighbourhood into this, and the other nodes
    this_node <- vs[v]
    other_nodes <- vs[edges[edges != v]]
    old_edges <- c(old_edges, paste(other_nodes, this_node, sep = "|"))
    # only add a new edge if neither neighbour has already been handled 
    shared_nodes <- other_nodes %in% vs[nodes_handled] # vs[k2]
    if (!any(shared_nodes)) {
      new_edges <- c(new_edges, other_nodes)
    }
  }
  # add and remove all edges and do some cleanup
  G |> 
    add_edges(new_edges) |>
    delete_edges(old_edges) |>
    largest_component() |>
    simplify()
}

# remove redundant nodes, i.e., waypoints along a sequence
# of edges with no branching
remove_redundant_nodes <- function(G) {
  while (sum(degree(G) == 2) > 0) {
    G <- collapse_deg_2_nodes(G)
  }
  G
}

# code starts here


lims <- -10:10          # coordinate range in x and y directions
extent <- length(lims)  # extent of that range
size <- extent * extent # how many lattice sites there are
n_mst <- 10             # number of nodes in min spanning tree
prop_retained <- 0.65    # proportion of lattice edges to retain

# make a lattice
L <- igraph::make_lattice(c(extent, extent), dim = 2) |>
  set_vertex_attr("name", value = 1:size |> as.character()) |>
  set_vertex_attr("x", value = rep(lims, extent)) |>
  set_vertex_attr("y", value = rep(lims, each = extent))

# make the lattice circular by imposing a radius limit
disc <- sqrt(vertex_attr(L, "x") ^ 2 + vertex_attr(L, "y") ^ 2) <= max(lims)
L <- L |> subgraph(vids = V(L)$name[disc])

# get the lattice edge and vertex dataframes
Lv <- L |>
  vertex.attributes() |>
  as.data.frame()

# remove some proportion of edges
Le <- L |>
  get_edge_df() |>
  slice_sample(prop = prop_retained)

# and make a new lattice - removing redundant nodes
L2 <- Le |> 
  graph_from_data_frame(directed = FALSE, vertices = Lv) |>
  largest_component()

|>
  remove_redundant_nodes()

# get the remaining vertices...
L2v <- L2 |> 
  vertex.attributes() |> 
  as.data.frame()

# ... and edges and add weight (i.e. edge length) information
L2e <- L2 |> 
  get_edge_df() |>
  left_join(L2v, by = join_by(from == name)) |>
  rename(x1 = x, y1 = y) |>
  left_join(L2v, by = join_by(to == name)) |>
  rename(x2 = x, y2 = y) |>
  mutate(weight = sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)) |>
  select(from, to, weight)

# make a minimum spanning tree from a random subset of nodes
mst_v <- L2v |>
  slice_sample(n = n_mst)
  
# first create all to all edges
mst_all_e <- CombPairs(mst_v$name) |>
  mutate(from = as.character(X1), to = as.character(X2)) |>
  select(-X1, -X2) |>
  left_join(L2v, by = join_by(from == name)) |>
  rename(x1 = x, y1 = y) |>
  left_join(L2v, by = join_by(to == name)) |>
  rename(x2 = x, y2 = y) |>
  mutate(weight = sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)) |>
  select(from, to, weight)

# generate the MST from the all-to-all network
min_ST <- mst_all_e |>
  graph_from_data_frame(directed = FALSE, vertices = mst_v) %>%
  mst()

# merge the new edges into the existing
combined_edges <- L2e |>
  bind_rows(min_ST |> get_edge_df())

# make up combined graph and analyse
G <- graph_from_data_frame(combined_edges, directed = FALSE, vertices = L2v)
eb <- G |> edge_betweenness(directed = FALSE)
G <- G |> set_edge_attr(name = "betweenness", value = eb)

G_sf <- sfnetwork(nodes = G |> vertex.attributes() |> 
                    as.data.frame() |> 
                    st_as_sf(coords = c("x", "y")),
                  edges = G |> get_edge_df(),
                  directed = FALSE,
                  edges_as_lines = TRUE)
edges <- G_sf |> 
  activate("edges") |> 
  st_as_sf()

plot1 <- ggplot() + 
  geom_sf(data = edges, aes(colour = betweenness, linewidth = betweenness)) +
  scale_color_binned_c4a_seq(palette = "tableau.classic_red") +
  scale_linewidth(range = c(0.1, 3)) +
  guides(colour = "none", linewidth = "none") +
  theme_void()

G2 <- graph_from_data_frame(L2e, directed = FALSE, vertices = L2v)
eb2 <- G2 |> edge_betweenness(directed = FALSE)
G2 <- G2 |> set_edge_attr(name = "betweenness", value = eb2)

G2_sf <- sfnetwork(nodes = G2 |> vertex.attributes() |>
                    as.data.frame() |>
                    st_as_sf(coords = c("x", "y")),
                  edges = G2 |> get_edge_df(),
                  directed = FALSE,
                  edges_as_lines = TRUE)
edges2 <- G2_sf |> activate("edges") |> st_as_sf()

plot2 <- ggplot() +
  geom_sf(data = edges2, aes(colour = betweenness, linewidth = betweenness)) +
  scale_color_binned_c4a_seq(palette = "tableau.classic_red") +
  scale_linewidth(range = c(0.1, 3)) +
  guides(colour = "none", linewidth = "none") +
  theme_void()

plot1 | plot2