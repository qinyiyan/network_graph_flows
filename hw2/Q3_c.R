library("igraph")
library("netrw")

numNode = 1000
numWalk = 1000
p = 0.01
g = random.graph.game(numNode, p, directed = FALSE)
rw = netrw(g, walker.num = numWalk, damping = 0.85, T = 100, output.visit.prob = TRUE, output.nodes = (1:numNode))
deg = degree(g)
vst = rw$ave.visit.prob
prob =  numeric(max(deg) - min(deg) + 1)
count = numeric(max(deg) - min(deg) + 1)

for(i in 1:numNode)
{
	prob[deg[i]] = prob[deg[i]] + vst[i]
	count[deg[i]] = count[deg[i]] + 1
}

for(k in min(deg):max(deg))
{
	prob[k] = prob[k] / count[k]
}

plot(prob, main = "Relationship Between Probability and Degree", xlab = "Degree", ylab = "Probability", type = "o")

plot(vst, main = "Probability visit each node", xlab = "Node", ylab = "Probability")
relation = cor(deg, vst)
relation

#0.9224223