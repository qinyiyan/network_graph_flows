library("igraph")
#library("netrw")
# generate the graph
g = read.graph("/Users/qinyiyan/Google Drive/EE232E/HW3/sorted_directed_net.txt", format = "ncol", directed = TRUE)
print(is.connected(g))
cl=clusters(g)
#the connected components with the most nodes
gcc <- induced.subgraph(g, which(cl$membership == which.max(cl$csize)))
#generate a simple undirected network
#one undirected edge is created for each pair of verices, no multiple edges
gcc_undirected_simple = as.undirected(gcc, mode = "collapse", edge.attr.comb = list(weight = "prod"))
E(gcc_undirected_simple)$weight = sqrt(E(gcc_undirected_simple)$weight)
fastgreedy_simple = fastgreedy.community(gcc_undirected_simple)
lpc_simple = label.propagation.community(gcc_undirected_simple)
threshold = 0.3
count = 0

for (i in 1:vcount(gcc_undirected_simple)){
  personalized_vect = rep(0,vcount(gcc_undirected_simple))
  personalized_vect[i] = 1
  probability = page.rank(gcc_undirected_simple,personalized = personalized_vect)
  names(probability$vector) = membership(fastgreedy_simple)

  largest_thirsty = tail(sort(probability$vector),30)
  largest_thirsty = largest_thirsty/sum(largest_thirsty)
  M = rep(0,max(membership(fastgreedy_simple)))
 
  for (j in 1:30){
    M[as.numeric(names(largest_thirsty[j]))] = M[as.numeric(names(largest_thirsty[j]))]+largest_thirsty[j]
  }
  if (length(which(M>threshold))>=2){
    print(i);
    print("this node belongs to multiple communiteis!")
    print(which(M>threshold))
    count = count+1
  }
}

#label.propagation.community
threshold = 0.1
for (i in 1:vcount(gcc_undirected_simple)){
  personalized_vect = rep(0,vcount(gcc_undirected_simple))
  personalized_vect[i] = 1
  probability = page.rank(gcc_undirected_simple,personalized = personalized_vect)
  names(probability$vector) = membership(lpc_simple)
  
  largest_thirsty = tail(sort(probability$vector),30)
  largest_thirsty = largest_thirsty/sum(largest_thirsty)
  M = rep(0,max(membership(lpc_simple)))
  
  for (j in 1:30){
    M[as.numeric(names(largest_thirsty[j]))] = M[as.numeric(names(largest_thirsty[j]))]+largest_thirsty[j]
  }
  if (length(which(M>threshold))>=2){
    print(i);
    print("this node belongs to multiple communiteis!")
    print(which(M>threshold))
    count = count+1
  }
}
