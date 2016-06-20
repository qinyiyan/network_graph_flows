edgelistFile <- "/Users/dijiafan/Desktop/facebook_combined.txt"
g <- read.graph(edgelistFile , format = "ncol" , directed=FALSE)
core_nodes = {}
core_nodes<-which(neighborhood.size(g, 1) > 200)

commNeib_find <- function(u,v,g)
{
  neighborsU <- neighborhood(g,1,u)[[1]][-1]
  neighborsV <- neighborhood(g,1,v)[[1]][-1]
  intersect(neighborsU, neighborsV)
}

# embeddedness calculations
# ===========================
embd_find <- function (u,v,g)
{
  embd = length(commNeib_find(u,v,g))
  embd
}

perNet_find <- function(u, g)
{
  pnNodes <- neighborhood(g , 1 , nodes=u)[[1]]
  nonPNNodes <- which( !( (1:vcount(g)) %in% pnNodes)  )
  perNetw <- delete.vertices(g , nonPNNodes)
  perNetw$name =  sort(pnNodes)
  perNetw
}

# node ids
# ========
nodeID_find <- function(g, vertex)
{
  temp <- which(g$name == vertex)
  temp
}

# dispersion
# ==========
disp_find <- function(u,v,g) 
{
  disp <- 0
  commonUV <- commNeib_find(u, v, g)
  gNoUV <- delete.vertices(g, c(u, v))
  
  for(s in commonUV) 
  {
    for(t in commonUV) 
    {
      if(s != t && s!=u && s!=v && t!=u && t!=v) 
      {
        short_d<-get.shortest.paths(gNoUV,from=s,to=t)
        if(length(short_d$vpath[[1]])>0){
        d<-length(short_d$vpath[[1]])-1
        disp <- disp + d}
        
      }
    }
  }
  disp=disp/2
}

# ratio, emb, disp
# =====
dispEmb_find <- function(g,coreNode){
  
  dHigh = 0;
  dNode = 0;
  rHigh = 0;
  rNode = 0;
  eHigh = 0;
  eNode = 0;
  
  pnOfU <- perNet_find(coreNode,g)
  u <- nodeID_find(pnOfU, coreNode)
  
  nodes <- V(pnOfU)
  for(v in nodes){
    if(v == u)
      next
    
    dip = disp_find(u,v,g)
    embd = embd_find(u,v,g)
    
    if (embd > 0)
    {
      rt = dip/embd
      if (rt > rHigh)
      {
        rNode = v;
        rHigh=rt;
      }
    }
    
    if (dip > dHigh)
    {
      dNode = v;
      dHigh=dip;
    }
    if (embd > eHigh)
    {
      eNode = v
      eHigh=embd;
    }
    
  }
  
#figure 1  
#=============
  if (dNode > 0)
  {
    # community detection
    fc = fastgreedy.community(pnOfU); sizes(fc)
    mfc = membership(fc)
    
    sizeVet = rep(3, length(V(pnOfU)));
    sizeVet[dNode] = 8;  
    colEd = rep(8, length(E(pnOfU)));
    colEd[which(get.edgelist(pnOfU,name=F)[,1] == dNode | get.edgelist(pnOfU,name=F)[,2] == dNode)] = 3;
    E(pnOfU)$color = colEd;
    widEd = rep(1, length(E(pnOfU)));
    widEd[which(get.edgelist(pnOfU,name=F)[,1] == dNode | get.edgelist(pnOfU,name=F)[,2] == dNode)] = 3;
    dev.new ();
    plot(pnOfU, vertex.label= NA, vertex.color=mfc,vertex.size=sizeVet, edge.width = widEd,mark.groups = by(seq_along(mfc), mfc, invisible),main="Max dispersion");
    }
  
  else
  {
    print (paste(c("No high Disp node", toString(coreNode)), collapse=" "));
  }

#figure 2  
#=============  
  if (eNode > 0)
  {
    # community detection
    fc = fastgreedy.community(pnOfU); sizes(fc)
    mfc = membership(fc)
    sizeVet = rep(3, length(V(pnOfU)));
    sizeVet[eNode] = 8;  
    colEd = rep(8, length(E(pnOfU)));
    colEd[which(get.edgelist(pnOfU,name=F)[,1] == eNode | get.edgelist(pnOfU,name=F)[,2] == eNode)] = 3;
    E(pnOfU)$color = colEd;
    widEd = rep(1, length(E(pnOfU)));
    widEd[which(get.edgelist(pnOfU,name=F)[,1] == eNode | get.edgelist(pnOfU,name=F)[,2] == eNode)] = 3;
    dev.new ();
    plot(pnOfU, vertex.label= NA, vertex.color=mfc,vertex.size=sizeVet, edge.width = widEd,mark.groups = by(seq_along(mfc), mfc, invisible),main="Max embeddedness");# ,mark.groups = by(seq_along(mfc), mfc) );
    }
  else
  {
    print (paste(c("No high Emb node", toString(coreNode)), collapse=" "));
  }

#figure 3  
#=============
  if (rNode > 0)
  {

    # community detection
    fc = fastgreedy.community(pnOfU); sizes(fc)
    mfc = membership(fc)
    
    sizeVet = rep(3, length(V(pnOfU)));
    sizeVet[rNode] = 8;  
    colEd = rep(8, length(E(pnOfU)));
    colEd[which(get.edgelist(pnOfU,name=F)[,1] == rNode | get.edgelist(pnOfU,name=F)[,2] == rNode)] = 3;
    E(pnOfU)$color = colEd;
    widEd = rep(1, length(E(pnOfU)));
    widEd[which(get.edgelist(pnOfU,name=F)[,1] == rNode | get.edgelist(pnOfU,name=F)[,2] == rNode)] = 3;
    dev.new ();
    plot(pnOfU, vertex.label= NA, vertex.color=mfc,vertex.size=sizeVet, edge.width = widEd,mark.groups = by(seq_along(mfc), mfc, invisible) , main="Max dispersion/embeddedness");
    }
  else
  {
    print (paste(c("No high Disp node", toString(coreNode)), collapse=" "));
  }
  
}

# =======================

dispVec <- c();
embVec <- c();


for(coreNode in core_nodes)
{
  pnOfU <- perNet_find(coreNode,g)
  u <- nodeID_find(pnOfU, coreNode)
  
  nodes <- V(pnOfU)
  for(v in nodes){
    if(v == u)
      next
    
    embd = embd_find(u,v,g)
    dip = disp_find(u,v,g)
    embVec <- c(embVec, embd);
    dispVec <- c(dispVec, dip);
    
  }
}


hist (embVec, breaks=seq (-0.5, by=1, length.out=max(embVec) +2), main ="embd Distribution", xlab="embd");
hist (dispVec, breaks=seq (-0.5, by=1, length.out=max(dispVec) +2), main="dip Distribution", xlab="dip");

dispEmb_find(g,core_nodes[1])
dispEmb_find(g,core_nodes[9])
dispEmb_find(g,core_nodes[10])