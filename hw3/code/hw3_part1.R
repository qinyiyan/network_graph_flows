#Code in R for EE232E Homework 3
#In this dataset sorted_directed_net.txt, there are 10501 vertices and 427486 edges
#The format of the graph(directed) in the .txt file is (v1 v2 w12)
library("igraph")
#Part 1:
#judging whether the graph is connected
#read the .txt file to construct the graph, before it, you have to check out where your dataset is
#file = scan("sorted_directed_net.txt", what=list(0,0,0))
file = scan("sorted_directed_net.txt", what=list(0,0,0))
fromlink <- file[[1]] + 1
tolink <- file[[2]] + 1
edgelist = cbind(fromlink, tolink)

g <- graph.edgelist(el=edgelist, directed=TRUE);
E(g)$weight <- file[[3]];

is.connected(g)
is.directed(g)
is.weighted(g)

#finding out the giant connected component
cluster_1<- clusters(g, mode="strong")
gccIndex = which.max(cluster_1$csize)
non_gcc_nodes = (1:vcount(g))[cluster_1$membership != gccIndex]
gcc = delete.vertices(g,non_gcc_nodes)