library('igraph')

g <- read.graph("/Users/qinyiyan/Google Drive/EE232E/proj1/facebook_combined.txt")

# core nodes : neighbourhood count > 200
core_nodes = c()
for (i in 1:length(V(g)))
{
  if(neighborhood.size(g,1,i) > 200)
  {
    core_nodes <- c(core_nodes,i)
  }
}

# personal networks of these core nodes
for (i in 1:length(core_nodes))
{
  personal_network <- induced.subgraph(g,as.numeric(unlist(neighborhood(g,1,core_nodes[i]))))
  
  # community structure of this personal network
  comm_FG <- fastgreedy.community(as.undirected(personal_network))
  
  # calculate features for each community
  member_lists <- c()
  modularity_index <- c()
  clustering_coeff <- c()
  density <- c()
  community_size <- c()
  clique_number <- c()
  max_eccentricity <- c()
  average_degree <- c()
  
  for (j in 1:max(comm_FG$membership))
  {
    member_lists[j] <- list(which(comm_FG$membership == j))
    temp_sub_graph <- induced.subgraph(personal_network, as.numeric(unlist(member_lists[j])))
    
    # Modularity Index
    # modularity_index[j] <- modularity(temp_sub_graph, membership = rep(j, length(member_lists[[j]])))
    modularity_index[j] <-  mean(comm_FG$modularity[comm_FG$membership == j])
    
    # Clustering Coefficient
    # clustering_coeff[j] <- clusters(temp_sub_graph)$no
    clustering_coeff[j] <- transitivity(temp_sub_graph)
    
    # Density
    density[j] <- graph.density(temp_sub_graph)
    
    # Community size
    community_size[j] <- length(member_lists[[j]])
    
    # Clique Number
    clique_number[j] <- clique.number(temp_sub_graph)
    
    # Maximum eccentricity
    max_eccentricity[j] <-  max(eccentricity(temp_sub_graph))
    # Average degree 
    average_degree[j] <- mean(degree(temp_sub_graph))
    
  }
  
  cat("Personal Network ", i, "\n")
  cat("Modularity Indices ", modularity_index, "\n")
  cat("Clustering Coefficients ", clustering_coeff, "\n")
  cat("Densities ", density, "\n")
  cat("Community sizes ", community_size, "\n")
  cat("Sizes of largest cliques ", clique_number, "\n")
  cat("Maximun Eccentricity ", max_eccentricity, "\n")
  cat("Average degree", average_degree, "\n")
  cat("community size:")
  for (size in community_size){
    if(size < 100)
      type=1
    else
      type=2
    cat (type, " ")
  }
  cat("\n\n\n")
}

