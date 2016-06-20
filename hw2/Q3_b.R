library("igraph")
library("netrw")

numNode = 1000
numWalk = 1000
p = 0.01
g = random.graph.game(numNode, p, directed = TRUE)
rw = netrw(g, walker.num = numWalk, damping = 1, T = 100, output.visit.prob = TRUE, output.nodes = (1:numNode))
in_deg = degree(g, mode = "in")
out_deg = degree(g, mode = "out")
vst = rw$ave.visit.prob
prob_in = numeric(max(in_deg) - min(in_deg) + 1)
prob_out = numeric(max(out_deg) - min(out_deg) + 1)
count_in = numeric(max(in_deg) - min(in_deg) + 1)
count_out = numeric(max(out_deg) - min(out_deg) + 1)

for(i in 1:numNode)
{
	prob_in[in_deg[i]] = prob_in[in_deg[i]] + vst[i]
	count_in[in_deg[i]] = count_in[in_deg[i]] + 1
}

for(k in min(in_deg):max(in_deg))
{
	prob_in[k] = prob_in[k] / count_in[k]
}

for(i in 1:numNode)
{
	prob_out[out_deg[i]] = prob_out[out_deg[i]] + vst[i]
	count_out[out_deg[i]] = count_out[out_deg[i]] + 1
}

for(k in min(out_deg):max(out_deg))
{
	prob_out[k] = prob_out[k] / count_out[k]
}

plot(prob_in, main = "Relationship Between Probability and In-Degree", xlab = "Degree", ylab = "Probability", type = "o")
plot(prob_out, main = "Relationship Between Probability and Out-Degree", xlab = "Degree", ylab = "Probability", type = "o")
plot(vst, main = "Probability visit each node", xlab = "Node", ylab = "Probability")
relation_in = cor(in_deg, vst)
relation_out = cor(out_deg, vst)
relation_in       # 0.8703661
relation_out      # 0.02821963