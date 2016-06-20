#Part3:
#cummunity detection
#option 1 using label.propagation
gcc_undirected1 = as.undirected(gcc,mode = "each")
cluster_lp = label.propagation.community(gcc_undirected1)
color_lp = cluster_lp$membership+1
png(filename="3_1.png")
plot(gcc_undirected1,vertex.label=NA,vertex.size=3,vertex.color=color_lp,main = "Community Structure Using Label Propagation")

#option 2 convert gcc to an undirected graph
sqrtweight<-function(weight){
    new_weight = sqrt(weight[1]*weight[2])
    new_weight
}
#using fastgreedy
gcc_undirected2 = as.undirected(gcc,mode = "collapse",edge.attr.comb = sqrtweight)
cluster_fg = fastgreedy.community(gcc_undirected2)
color_fg = cluster_fg$membership+1
png(filename="3_2.png")
plot(gcc_undirected2,vertex.label=NA,vertex.size=3,vertex.color=color_fg,main = "Community Structure Using Fast Greedy")

#using label.propagation
cluster_lp2 = label.propagation.community(gcc_undirected2)
color_lp2 = cluster_lp2$membership+1
png(filename="3_3.png")
plot(gcc_undirected2,vertex.label=NA,vertex.size=3,vertex.color=color_lp2,main = "Community Structure Using Label Propagation in option 2")