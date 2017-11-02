library(tm)
library(igraph)

twitter_data = results_olympics
twitter_data$text = paste(substr(twitter_data$text,2,nchar(twitter_data$text))) 
# We may need to remove the first charactore 'b' from the string.

text = twitter_data$text
text_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
text_clean = gsub("@\\w+", "", text_clean)
text_clean = gsub("[[:punct:]]", "", text_clean)
text_clean = gsub("[[:digit:]]", "", text_clean)
text_clean = gsub("http\\w+", "", text_clean)

text_corpus = Corpus(VectorSource(text_clean))
text_corpus = tm_map(text_corpus, tolower)
text_corpus = tm_map(text_corpus, removeWords, c(stopwords("english"), "olympics"))
text_corpus = tm_map(text_corpus, stripWhitespace)
text_corpus = tm_map(text_corpus,removePunctuation)
#text_corpus = tm_map(text_corpus, PlainTextDocument)

tdm = TermDocumentMatrix(text_corpus)
m = as.matrix(tdm)

# remove sparse terms (word frequency > 90% percentile)
wf = rowSums(m)
m1 = m[wf>quantile(wf,probs=0.9), ]

# remove columns with all zeros
m1 = m1[,colSums(m1)!=0]

# for convenience, every matrix entry must be binary (0 or 1)
m1[m1 > 1] = 1

# change it to a Boolean matrix
#m[m>=1] <- 1
# transform into a term-term adjacency matrix
termMatrix = m1 %*% t(m1)

# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted = T, mode = "undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

# plot a graph
set.seed(3535)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)

V(g)$label.cex <- 1.2 * V(g)$degree / max(V(g)$degree) + 0.2
V(g)$label.color <- rgb(0.0, 0.0, 0.2, 0.8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight) + 0.3) / max(log(E(g)$weight) + 0.3)
E(g)$color <- rgb(0.5, 0.5, 0.0, egam)
E(g)$width <- egam
# plot the graph in layout1
plot(g, layout=layout1)