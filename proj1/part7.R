library(igraph)
setwd("/Users/qinyiyan/Google Drive/EE232E/proj1/gplus/")
filesPath <- "/Users/qinyiyan/Google Drive/EE232E/proj1/gplus/"
file_list = list.files(filesPath)

# total number of files = 792
# number of files/user = 6
# total number of users = 792/6 = 132
# files for: 1)circles 2)edges 3)egofeat 4)feat 5)featnames 6)followers
f = 1
while (f <= length(file_list))
{
  ego_node_index = as.character(unlist(strsplit(file_list[f], "[.]"))[1])
  
  # read circles
  Circles = read.csv(file_list[f], header=FALSE, sep="\t", colClasses = "character")
  #print (Circles)
  # check whether this user has more than 2 circles
  #print(nrow(Circles))
  if (nrow(Circles) > 2)
  {
    f = f + 1
    
    # read edges and get the personal network
    personal_network <- read.graph(file_list[f], format="ncol", directed=TRUE, names=TRUE)
    ego_edges_list <- c()
    for (v in 1:length(V(personal_network)))
    {
      ego_edges_list <- c(ego_edges_list, V(personal_network)[v]$name, ego_node_index)
    }
    
    personal_network <- add.vertices(personal_network, 1, name = ego_node_index)
    personal_network <- add.edges(personal_network, ego_edges_list)
    personal_undirected <-as.undirected(personal_network)
    # community structure using Walktrap
    comm_WT <- walktrap.community(personal_network)
    plot(comm_WT,personal_undirected,vertex.label=NA,vertex.size=7,edge.arrow.size=0.2,main=paste("Community Structure for WALK TRAP",ego_node_index))
  
    # community structure using Infomap 
    comm_IM<- infomap.community(personal_network)
    plot(comm_IM,personal_undirected,vertex.label=NA,vertex.size=7,edge.arrow.size=0.2,main=paste("Community Structure for Info-map",ego_node_index))
    
    # show how communities from comm_WT overlap with the user's circles
    write(paste("node: ", ego_node_index,"\n"),file="/Users/qinyiyan/Google Drive/EE232E/proj1/part7_output.txt", append=TRUE)
    write(paste("Walktrap\n"),file="/Users/qinyiyan/Google Drive/EE232E/proj1/part7_output.txt", append=TRUE)
    for (r in 1:nrow(Circles))
    {
      write(paste("circle:",r,"\n"),file="/Users/qinyiyan/Google Drive/EE232E/proj1/part7_output.txt", append=TRUE)
      nodes_list_from_circles <- c(Circles[r,])
      #paste(sprintf("%s \t", nodes_list_from_circles[1]))
      nodes_list_from_circles <- nodes_list_from_circles[-1]
      #print(nodes_list_from_circles)
      for (c in 1:max(comm_WT$membership))
      {
        member_nodes<-vector()
        for (i in 1:length(comm_WT$membership))
          if(comm_WT$membership[i]==c){
            member_nodes <- c(member_nodes,(comm_WT$name[i]))
          }
        #member_nodes <- as.character(list(V(personal_network)[which(comm_WT$membership == c)]$name))
        #print(member_nodes)
        write(paste(length(intersect(nodes_list_from_circles, member_nodes))/length(member_nodes), "\t"),file="/Users/qinyiyan/Google Drive/EE232E/proj1/part7_output.txt", append=TRUE)
       # print (nodes_list_from_circles)
      }
      #print (member_nodes)
      write(paste("~\n\n"),file="/Users/qinyiyan/Google Drive/EE232E/proj1/part7_output.txt", append=TRUE)
    }
    # show how communities from comm_IM overlap with the user's circles
    write(paste("info-map"),file="/Users/qinyiyan/Google Drive/EE232E/proj1/part7_output.txt", append=TRUE)
    for (r in 1:nrow(Circles))
    {
      write(paste("circle:",r,"\n"),file="/Users/qinyiyan/Google Drive/EE232E/proj1/part7_output.txt", append=TRUE)
      nodes_list_from_circles <- c(Circles[r,])
      #paste(sprintf("%s \t", nodes_list_from_circles[1]))
      nodes_list_from_circles <- nodes_list_from_circles[-1]
      #print(nodes_list_from_circles)
      for (c in 1:max(comm_IM$membership))
      {
        member_nodes<-vector()
        for (i in 1:length(comm_IM$membership))
          if(comm_IM$membership[i]==c){
            member_nodes <- c(member_nodes,(comm_IM$name[i]))
          }
        #member_nodes <- as.character(list(V(personal_network)[which(comm_WT$membership == c)]$name))
        #print(member_nodes)
        write(paste(length(intersect(nodes_list_from_circles, member_nodes))/length(member_nodes), "\t"),file="/Users/qinyiyan/Google Drive/EE232E/proj1/part7_output.txt", append=TRUE)
        # print (nodes_list_from_circles)
      }
      #print (member_nodes)
      write(paste("~\n"),file="/Users/qinyiyan/Google Drive/EE232E/proj1/part7_output.txt", append=TRUE)
    }
    write(paste("\n\n\n"),file="/Users/qinyiyan/Google Drive/EE232E/proj1/part7_output.txt", append=TRUE)
    
    
    f = f + 5
  }
  else
  {
    f = f + 6
  }
}

