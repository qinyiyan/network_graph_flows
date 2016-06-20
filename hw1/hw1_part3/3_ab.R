#Problem 3
#(a)

library(igraph)

num <- 1000
g <- aging.prefatt.game(num, pa.exp = 1, aging.exp = -1, aging.bin = 100, directed = FALSE)
dg <- degree(g)
h <- hist(dg, breaks = seq(0, to = max(dg) + 1, by = 1), freq = FALSE, main = "Histogram of degree distribution
	 with 1000 nodes", xlab = "Degree")
p <- data.frame(x = h$mids, y = h$density)
plot(p, type = "o", main = "Degree Distribution with 1000 nodes", xlab = "Degree", ylab = "Density")

#(b)
com <- fastgreedy.community(g)
mod <- modularity(com)
plot(com, g)
mod