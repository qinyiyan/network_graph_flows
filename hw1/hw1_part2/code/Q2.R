library("igraph")

#solution 2 (a)
graph<- barabasi.game(1000,directed=FALSE)
d<-degree.distribution(graph)
plot(d, main='Degree Distribution',xlab="degree",ylab="density")


connectedness=diameter_=numeric(0)
for (i in 1:100){
  g = barabasi.game(1000,directed=FALSE)
  connectedness = c(connectedness, is.connected(g))
  diameter_ = c(diameter_, diameter(g))
}

Dia_avg = mean(diameter_)
cat("the average diameter of network is",Dia_avg,'\n')

#solution 2 (b)
con_avg = mean(connectedness)
cat("it has probability ",con_avg ,"the network is connected.",'\n')

cl = clusters(graph)
gccIndex = which.max(cl$csize)
nonGccNodes = (1:vcount(graph))[cl$membership != gccIndex]
gcc = delete.vertices(graph,nonGccNodes)
struct = fastgreedy.community(gcc)
mod = modularity(struct)
cat("the modularity of this 1000 nodes network is ",mod,'\n')

#solution 2 (c)
graph2<- barabasi.game(10000,directed=FALSE)
cl2 = clusters(graph2)
gccIndex2 = which.max(cl2$csize)
nonGccNodes2 = (1:vcount(graph2))[cl2$membership != gccIndex2]
gcc2 = delete.vertices(graph2,nonGccNodes2)
struct2 = fastgreedy.community(gcc2)
mod2 = modularity(struct2)
cat("the modularity of this 10000 nodes network is ",mod2,'\n')

#solution 2(d)
graph<- barabasi.game(1000,directed=FALSE)
degree_=numeric(0)
for(i in 1:1000){
  rand = sample(1000,1)
  neighbor_ = neighbors(graph,rand)
  picked = sample(neighbor_, 1)
  degree_ = c(degree_,degree(graph,picked))
}

plot(density(degree_),main='Degree distribution', xlab='degree', ylab='density')