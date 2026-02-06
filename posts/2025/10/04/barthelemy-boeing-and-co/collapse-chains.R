# collapse_k2_vertices <- function(G) {
#   k2 <- which(degree(G) == 2) |> names()
#   for (v in k2) {
#     n2 <- neighbors(G, v) |> names()
#     G |> 
#       delete_vertices(v) |>
#       add_edges(n2)
#   }
#   G |> largest_component() |> simplify()
# }

collapse_chains <- function(G) {
  chains <- G |> 
    subgraph(V(G)[which(degree(G) == 2)]) |> 
    components()
  vs_to_delete <- c()
  es_to_add <- c()
  for (i in 1:chains$no) {
    vs <- names(chains$membership)[which(chains$membership == i)]
    vs_to_delete <- c(vs_to_delete, vs)
    ext_chain <- neighborhood(G, order = 1, nodes = vs) |>
      unlist() |> names() |> unique()
    end_vs <- ext_chain[which(!(ext_chain %in% vs))]
    if (length(end_vs) == 2) {
      es_to_add <- c(es_to_add, end_vs)
    }
  }
  G |> add_edges(es_to_add) |>
    delete_vertices(vs_to_delete) |>
    largest_component() |>
    simplify()
}

collapse_all_chains <- function(G) {
  while (degree_distribution(G)[3] > 0) {
    G <- G |> collapse_chains()
  }
  G
}

par(mfrow = c(1, 2))
L2 |> plot.igraph(vertex.label = NA, vertex.size = 5, vertex.color = "white",
                  edge.color = "black", edge.width = 2)
L2 |> 
  collapse_all_chains() |>
  plot.igraph(vertex.label = NA, vertex.size = 5, vertex.color = "white",
              edge.color = "black", edge.width = 2)

L2 |> 
  collapse_all_chains() |>
  degree_distribution()
