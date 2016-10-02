require(ggplot2)
require(dplyr)

#require(wordcloud)
require(RColorBrewer)

source("preprocess_text.R")
source("prepare_svd_document_model.R")
source("svd_search.R")

docs <- load_document_data("data/")
term_mat <- load_document_term_mat("data/")
unique_words <- load_unique_words("data/")
search_space <- load_search_space("data/")

# U is relation of documents to documents.
U <- search_space$u
S <- search_space$d
# transposed relation of attributes to objects rows equal the attributes
V <- search_space$v

zeros <- c(1:ncol(U)*nrow(U)) * 0
searchSI <- matrix(data=zeros, ncol=ncol(U), nrow=nrow(U))
for(j in 1:ncol(searchSI)) {
  searchSI[j,j] <- sqrt(S[j])
}
ssU <- U%*%searchSI
ssV <- V%*%searchSI


# given the SVD check the scaled contribution
# the contribution of each component to the variation in the data set 
# scaled by 1 / (sum of squares)
# f_k = s_k^2 / \sum_{i=1}^r s_i^2
# entropy = -\frac{1}{\log r} \sum_{i=1}^r f_k \log (f_k)
ss <- sum(S^2)
fK <- S^2 / ss
# the entropy explained in the data set. This defines the threshold for the significant components.
entropy <- -1/log(length(S)) * sum(fK * log(fK))
# find the first n components <= entropy
comps <- c(fK[1])
for(i in 2:length(fK)) {
  if (sum(comps) > entropy) {
    break;
  }
  comps <- c(comps, fK[i])
}

squared_error <- data.frame(comp=1:length(comps), std=comps)
ggplot(squared_error) +
  geom_bar(aes(x=comp, y=std), stat="identity") +
  ggtitle(paste("First", length(comps), "components sum to entropy", 
                sprintf("%.5f", entropy)))


# the document term space
# we need to partition documents based on the closest component of the projection.
# we have identified length(comps) components but ideally want to reduce the grouping
# subjectively we can see in this case the elbow around the 50 mark, so we will look at clustering into the first 50 components.
# so this means taking each record of U and each 
# we've chosen the first 50 components by visually inspecting the plot.
K = 50
# so we want to cluster U into K clusters.
# we will use the term x term matrix for the first 50 components to along the columns to find the highest ranking terms for
# the first 50 dimensions
# we will then create 50 term vectors and find the groups of documents that most closely correspond to each of those 50 vectors
# and label them with the cluster.

maxTerms <- function(V, unique_words, N) {
  result <- list()
  
  # columns is the component
  # so accross each component we identify the top N terms
  for(i in 1:N) {
    col <- V[,i]
    col <- sort(abs(col), decreasing=TRUE, index.return=TRUE)
    idx <- col$ix
    # the length of col corresponds to the length of terms
    terms <- unique_words[idx[1:N]]
    result[[i]] <-terms[1:N]
    }
  result
}


maxUniqueTerms <- function(V, unique_words, N) {
  result <- list()
  indices <- list()
  # columns is the component
  # so accross each component we identify the top N terms
  for(i in 1:N) {
    col <- V[,i]
    col <- sort(abs(col), decreasing=TRUE, index.return=TRUE)
    idx <- col$ix
    test <- as.vector(unlist(indices))
    common <- intersect(test, idx)
    # select the ones that are uncommon
    idx2 <- setdiff(col$ix, common)
    # the length of col corresponds to the length of terms
    terms <- unique_words[idx2[1:N]]
    indices[[i]] <- idx2[1:N]
    result[[i]] <-terms[1:N]
  }
  result
}
# these are text so we 
terms <- maxUniqueTerms(ssV, unique_words, K)


## get the term matrices of the terms in a data frame
maxTermIndexes <- function(V, unique_words, N) {
  result <- list()
  # columns is the component
  # so accross each component we identify the top N terms
  for(i in 1:N) {
    col <- V[,i]
    col <- sort(abs(col), decreasing=TRUE, index.return=TRUE)
    idx <- col$ix
    # the length of col corresponds to the length of terms
    terms <- unique_words[idx[1:N]]
    terms <- terms[1:N] 
    search_mat <- convert_terms_to_matrix(terms, unique_words)
    search_mat <- matrix(data=as.numeric(search_mat), nrow=1, ncol=length(search_mat))
    result[[i]] <- search_mat
  }
  df <- data.frame(matrix(unlist(result), nrow=N, byrow=TRUE))
  df
}


## get the term matrices of the terms in a data frame
maxUniqueTermIndexes <- function(V, unique_words, N) {
  result <- list()
  indices <- list()
  # columns is the component
  # so accross each component we identify the top N terms
  for(i in 1:N) {
    col <- V[,i]
    col <- sort(abs(col), decreasing=TRUE, index.return=TRUE)
    idx <- col$ix
    test <- as.vector(unlist(indices))
    common <- intersect(test, idx)
    # select the ones that are uncommon
    idx2 <- setdiff(col$ix, common)
    indices[[i]] <- idx2[1:N]
    # the length of col corresponds to the length of terms
    terms <- unique_words[idx2[1:N]]
    terms <- terms[1:N] 
    search_mat <- convert_terms_to_matrix(terms, unique_words)
    search_mat <- matrix(data=as.numeric(search_mat), nrow=1, ncol=length(search_mat))
    result[[i]] <- search_mat
  }
  df <- data.frame(matrix(unlist(result), nrow=N, byrow=TRUE))
  df
}

search_terms <- maxUniqueTermIndexes(ssV, unique_words, K)

M <- as.matrix(search_terms)
# we want the total set of terms that appear in terms and we want the indices
indices <- list()
for(i in 1:nrow(M)) {
  indices[[i]] <- which(M[i,] == 1)
}
indices <- unique(unlist(indices))
# this means there are length(indices) unique words in the entire system.
M2 <- M[,indices]
labels <- unique_words[indices]
require(gplots)
pallette <- colorRampPalette(c("blue", "green"))(n=299)

heatmap(M2, Rowv=NA, Colv=NA, 
          labCol=labels,
          trace="none",
          col=pallette,
          margins=c(5,5),
          main="term membership in component")

pdf("test.pdf", width=22, height=20, pointsize=12)
heatmap(t(M2), Rowv=NA, Colv=NA, 
        labRow=labels,
        trace="none",
        col=pallette,
        main="term membership in component")
dev.off()

# now we have 50 sets of queries
# for each query we need to perform clustering 
# we will use the built in pclust algorithm on the documents
# and we'll see if theres a matching characteristic
searchResults <- function(search_space, search_term_mat, select_data_cols) {
  zeros <- c(1:ncol(U)*nrow(U)) * 0
  searchSI <- matrix(data=zeros, ncol=ncol(U), nrow=nrow(U))
  for(j in 1:ncol(searchSI)) {
    searchSI[j,j] <- sqrt(S[j])
  }
  ssU <- U%*%searchSI
  ssV <- V%*%searchSI
  
  distances <- lapply(1:nrow(search_term_mat), function(i) {
    search_mat <- search_term_mat[i,]
    sM <- as.matrix(search_mat)
    # search for query1 using the projection of the term matrix into the term document matrix
    proj <- sM%*%(ssV)
    # find the closest cosine distance to U
    dist <- unlist(sapply(1:nrow(ssU), function(j) {
      u <- ssU[j,]
      cosine_dist(t(proj), u)
    }))
    dist
  })
  # convert the distances into N docs x N cols
  docs <- docs[,select_data_cols]
  df <- data.frame(matrix(unlist(distances), nrow=nrow(docs), byrow=TRUE))
  colnames(df) <- c(1:nrow(search_term_mat))
  result <- cbind(docs, df)
  result
}

search_results <- searchResults(search_space, search_terms, c("item_key", "text")) 

# now we need a way of pruning search results that are not strong
# we assign the cluster membership to the search result with the maximum cosine distance
assignMaxResults <- function(docs, search_results) {
  cluster <- list()
  for(i in 1:nrow(search_results)) {
    dist <- unlist(search_results[i,3:ncol(search_results)])
    # distance must be absolute
    maxidx <- which(dist == max(abs(dist)))
    maxidx <- maxidx[1]
    cluster[[i]] <- maxidx
  }
  cluster <- unlist(cluster)
  cluster
}
cluster <- assignMaxResults(docs, search_results)
docs$cluster <- cluster

hist(cluster, breaks=K, main="population document clusters")

plot(U[1,],U[2,],col=(cluster))

V2 <- t(V)

plot(V2[2,], V2[1,], col=(cluster))


require(scatterplot3d)
# the scatter plot shows some interesting directions along components 1 2 and 4.


scatterplot3d(U[1,],U[2,], U[3,], color=as.numeric(cluster))

docs_grouped <- docs[order(docs$cluster),]

write.csv(docs_grouped, file="data/docs_clustered.csv", row.names=FALSE)

terms_data <-  data.frame(matrix(unlist(terms), nrow=length(terms), byrow=TRUE))



counts <- count(docs_grouped, c(cluster))
colnames(counts) <- c("cluster", "count")
terms_data$count <- counts$count[1:50]
terms_data$cluster <- counts$cluster[1:50]

write.csv(terms_data, file="data/key_terms_clusters.csv", row.names=TRUE)

# TODO: count the number of occurances of a given term in terms_data and correspondance to a cluster.
# then plot in heatmap between term and cluster.

set.seed(1234)
require(wordcloud)
# requires slam package
wordcloud(words=terms_data$X1, freq=terms_data$count, min.freq=1, 
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

test <- paste(terms_data$X1, terms_data$X2, terms_data$X3, terms_data$X4, terms_data$X5)
pallette <- colorRampPalette(brewer.pal(8,"Dark2"))(50)

png("requirement_cloud.png", width=1280, height=1280)
wordcloud(words=test, freq=terms_data$cluster, 
          max.words=Inf,  random.order=TRUE, 
          rot.per=0.1,
          colors=pallette)
dev.off()

test <- data.frame(terms=terms_data$X1, cluster=terms_data$cluster)
for(i in 2:50) {
  temp <- data.frame(terms=terms_data[,i], cluster=terms_data$cluster)
  test <- rbind(test, temp)
}


png("requirement_cloud2.png", width=1280, height=1280)
wordcloud(words=test$terms, freq=test$cluster, 
          max.words=Inf,  random.order=TRUE, 
          rot.per=0.3,
          colors=pallette)
dev.off()

test2 <- paste(terms_data[,1:50])

