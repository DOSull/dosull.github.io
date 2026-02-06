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

get_chains <- function(G) {
  chains <- list()
  k2 <- which(degree(G) == 2) |> names()
  for (v in k2) {
    n2 <- neighbors(G, v) |> names()
    n2d2 <- n2[which(n2 %in% k2)] 
    if (length(n2d2 > 0)) {
      existing_chain <- FALSE
      i <- 1
      n <- length(chains)
      while (!existing_chain & i <= n) {
        if (any(n2d2 %in% chains[[i]])) {
          chains[[i]] <- c(chains[[i]], n2d2) |> unique()
          existing_chain <- TRUE
        }
        i <- i + 1
      }
      if (!existing_chain) {
        chains[[length(chains) + 1]] <- c(v, n2d2)
      }
    }
  }
  chains
}

collapse_chains <- function(G) {
  chains <- G |> get_chains()
  nodes_to_delete <- c()
  edges_to_add <- c()
  for (chain in chains) {
    ext_chain <- 
      neighborhood(graph = G, order = 1, nodes = chain) |>
      unlist() |>
      names() |>
      unique()
    degrees <- degree(G, ext_chain)
    nodes_to_delete <- c(nodes_to_delete, ext_chain[which(degrees == 2)])
    if (sum(degrees != 2) == 2) {
      edges_to_add <- c(edges_to_add, ext_chain[which(degrees != 2)])
    }
  }
  G |> 
    delete_vertices(nodes_to_delete) |>
    add_edges(edges_to_add) |>
    largest_component() |>
    simplify()
}

# code starts here


lims <- -50:50          # coordinate range in x and y directions
extent <- length(lims)  # extent of that range
size <- extent * extent # how many lattice sites there are
n_mst <- 10             # number of nodes in min spanning tree
prop_retained <- 0.593  # proportion of lattice edges to retain

set.seed(1)

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
  collapse_chains()

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