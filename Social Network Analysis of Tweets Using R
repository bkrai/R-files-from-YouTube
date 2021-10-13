# Read file
apple <- read.csv(file.choose(), header = T)

# Build corpus
library(tm)
corpus <- iconv(apple$text, to = "utf-8-mac")
corpus <- Corpus(VectorSource(corpus))

# Clean text
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'stocks', 
                   replacement = 'stock')
cleanset <- tm_map(cleanset, stripWhitespace)

# Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm <- tdm[rowSums(tdm)>30,]
tdm[1:10,1:10]

# Network of terms
library(igraph)
tdm[tdm>1] <- 1
termM <- tdm %*% t(tdm)
termM[1:10,1:10]
g <- graph.adjacency(termM, weighted = T, mode = 'undirected')
g
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

# Histogram of node degree
hist(V(g)$degree,
     breaks = 100,
     col = 'green',
     main = 'Histogram of Node Degree',
     ylab = 'Frequency',
     xlab = 'Degree of Vertices')

# Network diagram
set.seed(222)
plot(g)
plot(g,
     vertex.color='green',
     vertex.size = 4,
     vertex.label.dist = 1.5,
     vertex.label = NA)

# Community detection
comm <- cluster_edge_betweenness(g)
plot(comm, g)

prop <- cluster_label_prop(g)
plot(prop, g)

greed <- cluster_fast_greedy(as.undirected(g))
plot(greed, as.undirected(g))

# Hub and authorities
hs <- hub_score(g, weights = NA)$vector
as <- authority_score(g, weights=NA)$vector
par(mfrow=c(1,2))
plot(g, vertex.size=hs*50, main='Hubs',
     vertex.label=NA,
     vertex.color=rainbow(50))
plot(g, vertex.size=as*30, main='Authorities',
     vertex.label=NA,
     vertex.color=rainbow(50))
par(mfrow=c(1,1))

# Highlighting degrees
V(g)$label.cex <- 2.2*V(g)$degree / max(V(g)$degree) + 0.3
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight) + .4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
plot(g,
     vertex.color='green',
     vertex.size = V(g)$degree*.5)

# Network of tweets
tweetM <- t(tdm) %*% tdm
g <- graph.adjacency(tweetM, weighted = T, mode = 'undirected')
V(g)$degree <- degree(g)
g <- simplify(g)
hist(V(g)$degree,
     breaks = 100,
     col = 'green',
     main = 'Histogram of Degree',
     ylab = 'Freuqency',
     xlab = 'Degree')

# Set labels of vertices to tweet IDs
V(g)$label <- V(g)$name
V(g)$label.cex <- 1
V(g)$label.color <- rgb(.4, 0, 0, .7)
V(g)$size <- 2
V(g)$frame.color <- NA
plot(g, vertex.label=NA, vertex.size=6)

# Delete vertices
egam <- (log(E(g)$weight)+.2)/ max(log(E(g)$weight)+ .2)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
g2 <- delete.vertices(g, V(g)[degree(g)<40])
plot(g2,
     vertex.label.cex = .9,
     vertex.label.color = 'black')

# Delete edges
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
g3 <- delete.edges(g, E(g)$weight <- 1)
g3 <- delete.vertices(g3, V(g3)[degree(g3)<20])
plot(g3)
apple$text[c(747, 430)]
