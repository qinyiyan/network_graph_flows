edgelistFile <- "/Users/dijiafan/Desktop/facebook_combined.txt"
g <- read.graph(edgelistFile , format = "ncol" , directed=FALSE)
core_Nodes <- which(neighborhood.size(g, 1, nodes=V(g)) > 200)
core <- core_Nodes[1]
subgraph_coreNodes <- neighborhood(g, 1, nodes=V(g)[core])
subgraph_coreNodes <- subgraph_coreNodes[[1]]
nonsubgraph_coreNodes <- which( !( (1:vcount(g)) %in% subgraph_coreNodes) )
subgraph_core <- delete.vertices(g , nonsubgraph_coreNodes)
vertexSizeVector = rep(3,vcount(subgraph_core))
vertexSizeVector[V(subgraph_core)$name==V(g)[core]$name] = 5

nonsubgraph_coreNodes1<-c(nonsubgraph_coreNodes,V(g)[core])
subgraph_core1 <- delete.vertices(g, nonsubgraph_coreNodes1)
plot(subgraph_core1, vertex.label=NA, vertex.size=5, main="Community structure after removing core node 1")

# find communities
community_1 <- fastgreedy.community(subgraph_core1)
modularity(subgraph_core,membership(community_1))
plot(community_1, subgraph_core1, vertex.label=NA, vertex.size=vertexSizeVector, main="Community structure after removing core node 1 - Fast-Greedy")

community_2 <- edge.betweenness.community(subgraph_core1)
modularity(subgraph_core,membership(community_2))
plot(community_2, subgraph_core1, vertex.label=NA, vertex.size=3, main="Community structure after removing core node 1 - Edge-Betweenness")

community_3 <- infomap.community(subgraph_core1)
modularity(subgraph_core,membership(community_3))

plot(community_3, subgraph_core1, vertex.label=NA, vertex.size=3, main="Community structure after removing core node 1 - Infomap community")

hist(community_1$membership, col="red", main="Community distribution of Fast-Greedy Algorithm", xlab="community number", ylab="number of nodes")
hist(community_2$membership, col="yellow", main="Community distribution of Edge-Betweenness Algorithm", xlab="community number", ylab="number of nodes")
hist(community_3$membership,col="blue",main="Community distribution of Infomap Algorithm",xlab="community number",ylab="number of nodes")