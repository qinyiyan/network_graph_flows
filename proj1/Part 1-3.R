#firstly you have to install the packages 'fit.models' and 'ggplot2'
library("igraph")
library("fit.models")
library("ggplot2")#attention! you have to install 'colorspace' before 'ggplot2'

#setting your file path
#getwd()
#setwd('/Users/Fiona/Google_Drive/UCLA/Courses/EE232E/Spring16/Project1/')
g <- read.graph('facebook_combined.txt' , format = "ncol" , directed=FALSE)

#Question 1
is.connected(g)#TRUE
is.directed(g)#FALSE

diameter(g, directed = FALSE)#diameter,8
mean(degree(g))#43.69101

#degree_distribution
degree_distribution=degree.distribution(g)
png(filename="1_1.png")
plot(degree_distribution, type= "h", main="Degree Distribution", xlab="degrees", ylab="distribution")
#barplot(degree_distribution,main="Bar Plot of Degree Distribution", xlab="degrees", ylab="distribution")

#fitting the curve
h1 = hist(degree(g), breaks=seq(0.0, by=1 , length.out=max(degree(g))+2))      
df1=data.frame(x=h1$mids, y=h1$density)
png(filename="1_2.png")
plot(df1,main="degree distribution of the graph", xlab="degrees", ylab="distribution",type="o")
models <- list( nls(y ~ I(1/x*a) + b*x, data = df1, start = list(a = 1, b = 1)), 
                nls(y ~ (a + b*log(x)), data=df1, start = setNames(coef(lm(y ~ log(x), data=df1)), c("a", "b"))),
                nls(y ~ I(exp(1)^(a + b * x)), data=df1, start = list(a=-3.59,b=-0.02)),
                nls(y ~ I(1/x*a)+b, data=df1, start = list(a=1,b=1)),
                lm(y~x, data = df1), 
                lm(y~I(1/x), data=df1),
                lm(y ~ log(x), data = df1)
				)
png(filename="1_3.png")
ggplot(df1, aes(x, y)) + geom_point(size = 5) +
   stat_smooth(method = "nls", formula = as.formula(models[[1]]), data=df1, start = list(a=0,b=0), size = 1, se = FALSE, colour = "blue") + 
   stat_smooth(method = "nls", formula = as.formula(models[[2]]), data=df1, start = setNames(coef(lm(y ~ log(x), data=df1)), c("a", "b")), size = 1, se = FALSE, colour = "yellow") +
   stat_smooth(method = "nls", formula = as.formula(models[[3]]), data=df1, start = list(a=0,b=0), size = 1, se = FALSE, colour = "purple") +
   stat_smooth(method = "nls", formula = as.formula(models[[4]]), data=df1, start = list(a=0,b=0), size = 1, se = FALSE, colour = "red")+
   stat_smooth(method = "lm", formula = as.formula(models[[5]]), size = 1, se = FALSE, colour = "white") + 
   stat_smooth(method = "lm", formula = as.formula(models[[6]]), size = 1, se = FALSE, colour = "green") + 
   stat_smooth(method = "lm", formula = as.formula(models[[7]]), size = 1, se = FALSE, colour = "pink") +
   ggtitle("Fitted models for Degree Distribution of the Graph")+ xlab("degrees") +ylab("distribution")
png(filename="1_4.png")
ggplot(df1, aes(x, y)) + geom_point(size = 5) + stat_smooth(method = "nls", formula = as.formula(models[[3]]), data=df1, start = list(a=0,b=0), size = 1, se = FALSE, colour = "yellow")+ggtitle("Best Fitted model for Degree Distribution of the Graph")+ xlab("degrees") +ylab("distribution")
#next, the fitted model could be displayed using summary() function
summary(nls(y ~ I(exp(1)^(a + b * x)), data=df1, start = list(a=-3.59,b=-0.02)))
#here we can get the fitted curve: y=exp^(-3.594-0.029*x)
#mean squared error, 4.016458e-07
df2=data.frame(x=h1$mids,y=exp(1)^(-3.594-0.029*h1$mids))
MSE=sum((df2$y-df1$y)^2)/max(degree(g))
#end question 1

#Question 2
#firstly constructing such a personal network for node 1
personal_network<-neighborhood(g,1,1)
personal_network<-personal_network[[1]]
length(personal_network) #348 nodes in this subgraph
non_personal_network<-which(!((1:vcount(g)) %in% personal_network))
subGraph <- delete.vertices(g , non_personal_network)
length(V(subGraph))# number of nodes,348
length(E(subGraph))# number of edges,2866

#Question 3
#firstly, we may as well see what the personal network looks like
fg = fastgreedy.community(subGraph)
color_vec = fg$membership+1
png(filename="3_1.png")
plot(subGraph,vertex.color=color_vec,vertex.label=NA,vertex.size=3,main='community structure using fastgreedy')

#finding the cores in the subGraph
cores <- which(neighborhood.size(g, 1, nodes=V(g)) > 200)
length(cores)# 41 core nodes in the subGraph
mean(degree(g)[cores]) #average degree 277.439

#we choose No.8 core node to carry out corresponding researches
core <- cores[8]
subgraph_core <- neighborhood(g, 1, nodes=V(g)[core])
subgraph_core <- subgraph_core[[1]]
length(subgraph_core)#218 nodes in the core??s personal network
nonsubgraph_core <- which(!( (1:vcount(g)) %in% subgraph_core))
subGraph_core <- delete.vertices(g , nonsubgraph_core)
png(filename="3_2.png")
plot(subGraph_core,main="Sub Network of the Core Node 8",vertex.size=3,vertex.label=NA)

#detecting community structures using the three algorithms
fastg = fastgreedy.community(subGraph_core)#two communities are detected
length(fastg)
color_vec = fastg$membership+1
modularity(subGraph_core,membership(fastg))#0.1288126
png(filename="3_3.png")
plot(fastg,subGraph_core,vertex.color=color_vec,main="Communities for Core Node 8 Using Fastgreedy",vertex.size=5,vertex.label=NA)

edgebet = edge.betweenness.community(subGraph_core) #here, it consumes too much time to run this line of code, in the end, 117 communities are detected
length(edgebet)
modularity(subGraph_core,membership(edgebet))#0.01361108
color_vec = edgebet$membership+1
png(filename="3_4.png")
plot(edgebet, subGraph_core,vertex.color=color_vec,main="Communities for Core Node 8 Using Edgebetweenness",vertex.size=5,vertex.label=NA)

infocom = infomap.community(subGraph_core) # only 1 community are detected
length(infocom)
color_vec = infocom$membership+1
modularity(subGraph_core,membership(infocom))#0 because only one community detected here
png(filename="3_5.png")
plot(infocom, subGraph_core,vertex.color=color_vec,main="Communities for Core Node 8 Using Infomap",vertex.size=5,vertex.label=NA)
#draw the nodes distribution in communities to compare the results
png(filename="3_6.png")
hist(fastg$membership,col="dark red",main="Community Distribution of the Fast Greedy Algorithm",xlab="Community Number",ylab="Number of Nodes in a Community")
png(filename="3_7.png")
hist(edgebet$membership,col="dark green",main="Community Distribution of the Edge-Betweenness Algorithm",xlab="Community Number",ylab="Number of Nodes in a Community")
png(filename="3_8.png")
hist(infocom$membership,col="dark blue",main="Community Distribution of the Infomap Algorithm",xlab="Community Number",ylab="Number of Nodes in a Community")
