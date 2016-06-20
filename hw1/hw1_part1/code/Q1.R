#solution 1(a)
library(igraph)
p <- c(0.01,0.05,0.1)
sig <- c(1,1,1)
col <-c("red","green","blue")

for(i in 1:3){
  for(j in 1:100){
    graph<-erdos.renyi.game(1000,p[i])
    if(sig[i]){
      d<-degree.distribution(graph)
      sig[i] = 0
    }else{
      d<- d + degree.distribution(graph)
    }
  }
  d = d/100
  plot(d, type="o", col=col[i],main=paste("Density function for graph",i,"(with p=",p[i],")"),xlab="degree",ylab="Probability Density")
 
}



#solution 1(b)
connect_ <- c(0,0,0)
diameter_ <- c(0,0,0)
for(i in 1:3){
  for(j in 1:100){
    graph<-erdos.renyi.game(1000,p[i])
    if(is.connected(graph)){connect_[i]=connect_[i]+1}
    diameter_[i] = diameter_[i]+ diameter(graph)
  }
  diameter_[i] =  diameter_[i]/100
  connect_[i] = connect_[i]/100
  cat("when p =", p[i],",it has probability ",connect_[i],"the network is connected. ")
  cat("Diameter for this network: ",diameter_[i],'\n')
}



#solution 1(c)
p = 0.05
p_c = 0
iter = 0.0001
for(i in 1:50){
  while(1){
    graph<-erdos.renyi.game(1000,p)
    if(is.connected(graph)){
      p = p - iter
    }else{
      p_c = p_c + p
      break
    }
  }
}
p_c = p_c /50

cat("when p < ",p_c,"the generated random networks are disconnected, and when p > ",p_c,"the generated random networks are connected")




