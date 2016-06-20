library(igraph)

iteration = 500
num = 1000
deg1 = numeric()
deg2 = numeric()
Diameter = numeric(0)
path = numeric ()
#Q(a)
for (i in 1:iteration) {
  g = forest.fire.game(num, fw.prob = 0.37, bw.factor = 0.32/0.37, directed = TRUE)
  deg1 = c(deg1, degree(g, mode = "in"))
  deg2 = c(deg1, degree(g, mode = "out"))
  Diameter = c (Diameter, diameter(g))
  path = c (path, get.diameter(g, directed = TRUE, weights = NULL))
}

h1 = hist(deg1, breaks = seq(0, to = max(deg1) + 1, by = 1), freq = FALSE, main = "Histogram of in degree distribution with 1000 nodes", 
          xlab = "Degree")
p1 = data.frame(x = h1$mids, y = h1$density)
plot(p1, type = "o", main = "In Degree Distribution with 1000 nodes", xlab = "Degree", ylab = "Density")
plot(p1, type = "o", log = "xy", main = "In Degree Distribution with 1000 nodes(log)", xlab = "Degree", ylab = "Density")

h2 = hist(deg2, breaks = seq(0, to = max(deg2) + 1, by = 1), freq = FALSE, main = "Histogram of out degree distribution with 1000 nodes", 
          xlab = "Degree")
p2 = data.frame(x = h2$mids, y = h2$density)
plot(p2, type = "o", main = "Out Degree Distribution with 1000 nodes", xlab = "Degree", ylab = "Density")
plot(p2, type = "o", log = "xy", main = "Out Degree Distribution with 1000 nodes(log)", xlab = "Degree",ylab = "Density")

#Q(b)
is.connected(g)
D_avg = mean(Diameter)
D_avg

#Q(c)
property<-edge.betweenness.community (g,directed = TRUE, edge.betweenness = TRUE, merges = TRUE, bridges = TRUE, modularity = TRUE, membership = TRUE)
wc = walktrap.community(g)
sizes(wc)
sizes(property)
mod = modularity(wc)
mod
modularity_betweenness = modularity(property)
modularity_betweenness

plot(wc$membership, main="membership plot (walktrap)")
plot(wc$modularity, main="modularity plot (walktrap)")
plot(property$membership,main="Membership Plot")
plot(property$modularity,main="Modularity Plot")

plot(wc,g)
plot(property,g)
