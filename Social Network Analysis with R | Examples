# Social Network Analysis
library(igraph)
g <- graph(c(1,2,2,3,3,4,4,1),
           directed = F,
           n=7)

g1 <- graph(c("Amy", "Ram", "Ram", "Li", "Li", "Amy",
        "Amy", "Li", "Kate", "Li"),
        directed=T)

# Network measures
degree(g1, mode='all')
degree(g1, mode='in')
degree(g1, mode='out')

diameter(g1, directed=F, weights = NA)
edge_density(g1, loops = F)
ecount(g1)/(vcount(g1)*(vcount(g1)-1))
reciprocity(g1)
closeness(g1, mode='all', weights = NA)
betweenness(g1, directed=T, weights=NA)
edge_betweenness(g1, directed=T, weights=NA)

# Read data file
data <- read.csv('https://raw.githubusercontent.com/bkrai/R-files-from-YouTube/main/networkdata.csv', header=T)
y <- data.frame(data$first, data$second)

# Create network
net <- graph.data.frame(y, directed=T)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

# Histogram of node degree
hist(V(net)$degree)

# Network diagram
plot(net)

# Highlighting degrees & layouts
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.fruchterman.reingold)

# Hub and authorities
hs <- hub_score(net)$vector
as <- authority.score(net)$vector
par(mfrow=c(1,2))
set.seed(123)
plot(net,
     vertex.size=hs*30,
     main = 'Hubs',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)

plot(net,
     vertex.size=as*30,
     main = 'Authorities',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)
par(mfrow=c(1,1))

# Community detection
net <- graph.data.frame(y, directed = F)
cnet <- cluster_edge_betweenness(net)
plot(cnet)
