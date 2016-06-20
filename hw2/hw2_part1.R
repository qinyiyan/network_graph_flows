library(igraph)
library(netrw)
#1_a
g <- erdos.renyi.game(1000, p = 0.01, direct = FALSE)

#1_b
avg = numeric()
std = numeric()

for (i in 1:100) {
  print(i)
  dists = numeric()
  rw <- netrw(g, walker.num = 500, damping = 1, T = i, output.walk.path = TRUE)
  for (j in 1:500) {
    dist = shortest.paths(g, v = rw$walk.path[1, j], to = rw$walk.path[i, j])
    dists = c(dists, dist)
  }
  avg = c(avg, mean(dists))
  std = c(std, sd(dists))
}
plot(avg, main = "average distance_1000 nodes", xlab = "step size", ylab = "avg")
plot(std, main = "standard deviation_1000 nodes", xlab = "step size", ylab = "std")



#1_d
#node = 100
avg1 = numeric()
std1 = numeric()
g1 <- erdos.renyi.game(100, p = 0.01, direct = FALSE)
for (i in 1:100) {
  print(i)
  dists = numeric()
  rw <- netrw(g1, walker.num = 100, damping = 1, T = i, output.walk.path = TRUE)
  for (j in 1:100) {
    dist = shortest.paths(g1, v = rw$walk.path[1, j], to = rw$walk.path[i, j])
    if (dist == Inf) 
      dist = 0
    dists = c(dists, dist)
  }
  avg1 = c(avg1, mean(dists))
  std1 = c(std1, sd(dists))
}
plot(avg1, main = "average distance_100 nodes", xlab = "step size", ylab = "avg")
plot(std1, main = "standard deviation_100 nodes", xlab = "step size", ylab = "std")

#node = 10000
avg2 = numeric()
std2 = numeric()
g2 <- erdos.renyi.game(10000, p = 0.01, direct = FALSE)
for (i in 1:100) {
  print(i)
  dists = numeric()
  rw <- netrw(g2, walker.num = 1000, damping = 1, T = i, output.walk.path = TRUE)
  for (j in 1:1000) {
    dist = shortest.paths(g2, v = rw$walk.path[1, j], to = rw$walk.path[i, j])
    dists = c(dists, dist)
  }
  avg2 = c(avg, mean(dists))
  std2 = c(std, sd(dists))
  
}
plot(avg2, main = "average distance_10000 nodes", xlab = "step size", ylab = "avg")
plot(std2, main = "standard deviation_10000 nodes", xlab = "step size", ylab = "std")

d0 = diameter(g)
d1 = diameter(g1)
d2 = diameter(g2)


#1_e
d<-degree.distribution(g)
plot(d,xlab="Degree",ylab="Density",main="Degree Distribution of Graph of 1000 Nodes Network")
rw <- netrw(g, walker.num = 1000, damping = 1, T = 100, output.walk.path = TRUE)
nodes = rw$walk.path[100,]
dgr<-degree(g,nodes)
dd = hist(dgr,breaks=seq(from=0,to=max(dgr), by=1))
plot(dd$count,xlab="Degree",ylab="Density",main="Degree Distribution of the Nodes Reached at the End of the Random Walk")






