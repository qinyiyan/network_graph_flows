library('hash')
################part 7####################
movie2index = readLines("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/movie2index.txt")
#split data using \t\t as seperator
movie2index = strsplit(movie2index, split="\t")
movies = sapply(movie2index, "[", 1)
movieIndex = as.numeric(sapply(movie2index, "[", 2))

vertex_movie_index = readLines("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/vertex_movie_index.txt")

df_movie = read.table("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/movie_graph_notebook.txt")
names(df_movie) = c("from", "to", "weight")
g1 = graph.data.frame(df_movie, directed  = FALSE, vertices = vertex_movie_index)
fc = fastgreedy.community(g1)
sizes(fc)
membership(fc)['318106']

wc = walktrap.community(g1, weights = E(g1)$weight, steps = 4, merges =TRUE, modularity = TRUE, membership = TRUE)



# movie2neighborrating
movie2rating = readLines("/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/movie2rating.txt")
movie2rating = strsplit(movie2rating, split="\t")
ratedMovie = unlist(lapply(movie2rating, function(x) x[1]))
rate = unlist(lapply(movie2rating, function(x) as.numeric(x[2])))
movie2rating_hash = hash(ratedMovie, rate)

index1 = '825676'
index2 = '458218'
index3 = '318106'


community2movie_hash <-hash()
names = fc$names
name_v <- c(names[1][[1]])
name_v
names[1]
if (has.key(name_v, community2movie_hash)){
  print ("y")
}

community2movie_hash["a"] = "aa"
community2movie_hash["a"]

membership(fc)[names[1]][[1]]
#movie2rating_hash

#####try hash:community2movie_hash
h<-list()
names(h)
for (i in 1:length(names)){
  community <-c( membership(fc)[names[i]][[1]])
  community[1]
  if (toString(community[1]) %in% names(h) ){
    h[[toString(community[1])]]<-c(h[[toString(community[1])]],names[i])
    
  }
  else{
    h[[toString(community[1])]]<-names[i]
  }
}
names(h)
h[["37"]]
length(h[["37"]])
h[[toString(community[1])]][1]
has.key(h[[toString(community[1])]][1], movie2rating_hash)
movie2rating_hash[h[[toString(community[1])]][1]]
values(movie2rating_hash, h[[toString(community[1])]][1])[[1]]
##### mean_community_rating  ########
mean_community_rating <- function(movie){
  rating = 0
  community = membership(fc)[movie]
  community[1]
  member_count = 0
  if  (toString(community[1]) %in% names(h) ){
    members = h[[toString(community[1])]]
    #member_count = length(members)
    for (i in 1:length(members)){
      if (has.key(members[i], movie2rating_hash)){
        #print(rating)
        #print(movie2rating_hash[members[i]])
        member_count = member_count+1
        rating = rating +values(movie2rating_hash, members[i])[[1]]
      }
    }
  }
  rating = 1.0*rating /member_count
  return(rating)
}
mean_community_rating("771211")
mean_community_rating("458218")
mean_community_rating("318106")


neighbors = neighborhood(graph = g1, order = 1, nodes = "318106")
length(neighbors[[1]])
neighbors[[1]][1]
mean_neighbor_rating <- function(movie){
  rating = 0
  nei_count = 0
  neighbors = neighborhood(graph = g1, order = 1, nodes = movie)
  #print (neighbors[[1]])
  #print (length(neighbors[[1]]))
  if (length(neighbors[[1]])>1){
    for (i in 1:length(neighbors[[1]])){
      if (has.key(toString(neighbors[[1]][i]), movie2rating_hash)){
        rating = rating + values(movie2rating_hash, toString(neighbors[[1]][i]))[[1]]
        nei_count = nei_count+1
        #print ("a")
      }
    }
  }
  #print (rating)
  rating = 1.0*rating/nei_count
  return(rating)
}
mean_neighbor_rating("771211")
mean_neighbor_rating("458218")
mean_neighbor_rating("318106")
#################part 8####################

#make movie2neighborrating & movie2fcrating
movie2neighborrating = {}
names = fc$names
names[1]
for (i in (1:length(names))){
  temp <- c(names[i], mean_neighbor_rating(names[i]))
  movie2neighborrating <- c(movie2neighborrating, temp)
}
######fc2rating hash
fc_rating = {}
community = names(h)
community
length(community)
for (j in (1:length(community))){
  members = h[[community[j]]]
  member_count = 0
  rating = 0
  for (i in 1:length(members)){
    if (has.key(members[i], movie2rating_hash)){
      #print(rating)
      #print(movie2rating_hash[members[i]])
      member_count = member_count+1
      rating = rating +values(movie2rating_hash, members[i])[[1]]
    }
  }
  rating = 1.0*rating/member_count
  fc_rating[[community[j]]] <- rating
}
community = membership(fc)[["1890"]]
community
fc_rating[[1]]

movie2fcrating = {}
communities = names(fc_rating)
for (i in (1:length(names))){
  community = membership(fc)[[names[i]]]
  temp <- c(names[i],fc_rating[[community]])
  movie2fcrating<-c(movie2fcrating, temp )
}

movie2fc_dir <- "/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/R_movie2fc_rating.txt"
movie2fc_output <- file (movie2fc_dir)
writeLines(movie2fcrating,movie2fc_output)

movie2neighbor_dir <- "/Users/qinyiyan/Google Drive/EE232E/proj2/project_2_data/R_movie2neighbor_rating.txt"
movie2neighbor_output <- file (movie2neighbor_dir)
writeLines(movie2neighborrating,movie2neighbor_output)
