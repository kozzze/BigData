#1
library(igraph)

N <- 5

set.seed(123) 
G_size <- sample((N + 10):((N/10 + 5)^2 + 5*N), 1)
G_size

g <- graph.ring(G_size)

vcount(g)
ecount(g)

plot(g, main="Кольцевой граф", vertex.label=NA)

g[]

#2

g1 <- graph.empty() + vertices(1:G_size, color="yellow")

set.seed(123)
edges1 <- sample(V(g1), N * 8 * 2, replace=TRUE)  
g1 <- g1 + edges(edges1, color="red")

plot(g1, main="g1 с красными ребрами", edge.color=E(g1)$color, vertex.label=NA)
g1[]

edges2 <- sample(V(g1), N * 10 * 2, replace=TRUE)
g1 <- g1 + edges(edges2, color="blue")

plot(g1, main="g1 с красными и синими ребрами", edge.color=E(g1)$color, vertex.label=NA)
g1[]

#3

edges_black <- list(
  c(2*N + 23, 2*N + 20),
  c(2*N + 12, N + 15),
  c(2*N - 1, N + 8),
  c(2*N, 2*N + 1),
  c(N + 7, N + 13)
)

for (pair in edges_black) {
  if (all(pair %in% V(g1))) {
    g1 <- g1 + edge(pair[1], pair[2], color="black")
  }
}

plot(g1, main="g1 с черными ребрами", edge.color=E(g1)$color, vertex.label=NA)

neighbors(g1, N)

incident(g1, N)

are.connected(g1, N + 10, N + 12)

g1[]


#4

deg <- degree(g1)
max_deg_index <- which.max(deg)

g1 <- g1 + vertex("NEW")
g1 <- g1 + edge("NEW", max_deg_index, color="purple")

all_names <- c(LETTERS, letters)
vertex_names <- all_names[1:vcount(g1)]
V(g1)$name <- vertex_names

plot(g1, main="граф с новой вершиной", vertex.label=V(g1)$name, edge.color=E(g1)$color)

g1[]

which(deg > 2 & deg < 5)

#5

coords_circle <- layout_(g1, in_circle())
plot(g1, layout=coords_circle, main="layout: in_circle", edge.color=E(g1)$color, vertex.label=V(g1)$name)

coords_tree <- layout_(g1, as_tree())
plot(g1, layout=coords_tree, main="layout: as_tree", edge.color=E(g1)$color, vertex.label=V(g1)$name)

coords_lattice <- layout_with_lgl(g1)
plot(g1, layout=coords_lattice, main="layout: lattice style", edge.color=E(g1)$color, vertex.label=V(g1)$name)

#6

diameter(g1)

paths <- all_shortest_paths(g1, from=V(g1)[1], to=V(g1), mode="all")
paths$res

deg <- degree(g1)
plot(g1, vertex.size=deg * 3, main="граф со степенями вершин", edge.color=E(g1)$color, vertex.label=V(g1)$name)


# Задание 2 (3. ВИП гости)

conflicts <- matrix(c(
  1, 2,
  1, 3,
  4, 5
), byrow=TRUE, ncol=2)

g_vip <- graph_from_edgelist(conflicts, directed=FALSE)

is_bipartite <- bipartite.mapping(g_vip)

if (is_bipartite$res) {
  cat("YES\n")
  cat("Стол 1:", which(is_bipartite$type), "\n")
  cat("Стол 2:", which(!is_bipartite$type), "\n")
} else {
  cat("NO\n")
}
















