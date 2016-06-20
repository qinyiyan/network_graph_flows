library(igraph)
library(netrw)

#4.a
g_a<-random.graph.game(1000, 0.01, directed=TRUE)
net1<-netrw(g_a, walker.num=1000,start.node=1:vcount(g_a),damping=0.85,T=1000, output.walk.path=TRUE)
plot(net1$ave.visit.prob,xlab="Nodes",ylab="Visit Probablity",main="PageRank - Random Walk ",pch=1)

#4.b
g <- random.graph.game(1000, p = 0.01, direct = FALSE)
rw_ = netrw(g, walker.num = 1000, damping = 0.85, T = 100, output.visit.prob = TRUE, output.nodes = (1:1000), 
            teleport.prob = pagerank)
pk = rw_$ave.visit.prob
plot(pk, main = "personalized PageRank")

#4.c