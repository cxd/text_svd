require(ggplot2)
require(dplyr)
source("preprocess_text.R")

file <- "input/input_data.csv"
data <- read.csv(file, header=TRUE)

colnames(data) <- c("project_id", "item_key", "text")

# convert all text into lines.
lines <- sapply(data$text, preprocess_line)
# 
id_cols <- c("project_id", "item_key")

doc_term_mat <- convert_to_matrix(data, id_cols, lines)

# get SVD of term document matrix
# A = USV'
# U represents relation of rows
# V' represents relation of attributes
# S represents square root of eigenvalues
X <- data.matrix(doc_term_mat[,3:ncol(doc_term_mat)])

# using the X matrix relates documents to terms so that would be interesting

# cluster the variance covariance matrix relating documents to each other
Xprime <- X%*%t(X)

A <- svd(Xprime)
U <- A$u
S <- A$d
# transposed relation of attributes to objects rows equal the attributes
V <- A$v

# given the SVD check the scaled contribution
# the contribution of each component to the variation in the data set 
# scaled by 1 / (sum of squares)
# f_k = s_k^2 / \sum_{i=1}^r s_i^2
# entropy = -\frac{1}{\log r} \sum_{i=1}^r f_k \log (f_k)
ss <- sum(S^2)
fK <- S^2 / ss

entropy <- -1/log(length(S)) * sum(fK * log(fK))
# we can search for the first n components within f_k that sum to the entropy
# fortunately the first 100 components sum to the entropy contained in the data set
# this is a large number of components, but given we are dealing with
# a vocabulary of words a large number of components are expected.

# in the svd(X) matrix of objects to attributes the entropy is approx = fK[1:100]
# test <- sum(fK[1:100])
# in the variance covariance matrix Xprime the entropy is approx the first 3 components
test <- sum(fK[1:3])


plot(fK)

dS <- data.frame(x=1:length(S), scale=S, root=sqrt(S), fK=fK)

ggplot(data=dS, aes(x=x)) + 
  geom_line(aes(y=scale), col="red") +
  geom_line(aes(y=root), col="blue") +
  geom_line(aes(y=fK), col="purple") +
  ggtitle("Scale of variation due to component")

# the contribution of each component to the variation in the data set 
# scaled by 1 / (sum of squares)
ggplot(data=dS, aes(x=x)) + 
  geom_line(aes(y=fK), col="purple") +
  ggtitle("Scale of variation due to component")

# first 100 components
ggplot(data=dS, aes(x=x[1:100])) + 
  geom_line(aes(y=fK[1:100]), col="purple") +
  ggtitle("Scale of variation due to component for first 100 components ")



plot(sqrt(S), type="l")

# relating documents togethor in the first 2 dimensions of U
# we need to colour code by membership to some common class label
# if we take the top 10 dimensions, we should then project membership of each document
# based on closest proximity to each dimension as voted by k-nearest neighbours.
# the top 10 dimensions would then account for the variability.
# however there is some indication of cluster membership within the scatter plot
plot(V[1,], V[2,])

# rotating 4th and 10th dimension
# this suggests there may be some similarity along these dimensions
plot(V[1,], V[3,])
require(scatterplot3d)
# the scatter plot shows some interesting directions along components 1 2 and 4.
scatterplot3d(V[1,],V[2,], V[3,])

# using the first components for example the first 10 significant 
# components from V*sqrt(S)
# determine the cosine distance of U*sqrt(S) and determine the closest
# measure

zeros <- 0 * 1:(ncol(U)*nrow(U))
SI <- matrix(data=zeros, ncol=ncol(U), nrow=nrow(U))
for(i in 1:ncol(SI)) {
  SI[i,i] <- sqrt(S[i])
}

sU <- U%*%SI
sV <- V%*%SI

# 250 components of significance
compV <- sV[1:100,]
# for xprime the top 3 components are of interest

cosine_dist <- function(a, b) {
  xx <- crossprod(a,a)
  yy <- crossprod(b,b)
  xy <- crossprod(a,b)
  xy/(xx*yy)
}

doc_term_mat$component <- 1:nrow(doc_term_mat)*0
doc_term_mat$cosine_dist <- 1:nrow(doc_term_mat)*0
for(i in 1:nrow(sU)) {
  a <- sU[i,]
  dist <- sapply(1:nrow(compV), function(k) {
    b <- compV[k,]
    cosine_dist(a, b)
  })
  dist <- unlist(dist)
  # the closest documents will have magnitudes that are large
  # the smaller the magnitude the further away it is
  max <- max(abs(dist))
  idx <- which(abs(dist) == max)
  doc_term_mat$component[i] <- idx
  doc_term_mat$cosine_dist[i] <- dist[idx]
}

# now we can plot the document projections based on matrix
P <- data.frame(c1=U[,1], 
                c2=U[,2],
                c3=U[,3],
                c4=U[,4],
                c5=U[,5],
                c6=U[,6],
                c7=U[,7],
                c8=U[,8],
                c9=U[,9],
                c10=U[,10],
                c50=U[,50],
                component=doc_term_mat$component)

plot(P[,1], P[,2], col=P$component)

# note visually when plotting the components
# those elements that are further from the origin may be of interest.

ggplot(data=P, aes(col=factor(component))) +
  geom_point(aes(x=c1, y=c2)) +
  ggtitle("scatter plot of components 1 and 2")


ggplot(data=P, aes(col=factor(component))) +
  geom_point(aes(x=c1, y=c4)) +
  ggtitle("scatter plot of components 1 and 4")


ggplot(data=P, aes(col=factor(component))) +
  geom_point(aes(x=c2, y=c4)) +
  ggtitle("scatter plot of components 2 and 4")


ggplot(data=P, aes(col=factor(component))) +
  geom_point(aes(x=c4, y=c50)) +
  ggtitle("scatter plot of components 1 and 50")

data$component <- doc_term_mat$component
data$cosine_dist <- doc_term_mat$cosine_dist


write.csv(doc_term_mat, "data/doc_term_mat.csv", row.names=FALSE)

write.csv(data, "data/labelled_data.csv", row.names=FALSE)


cnts <- count(data, c(component))

# note the distances of the same sign are closest to each other 
# the clusters are "conical" in shape, there is a positve and negative
# distance, an example of components associated with component 4
# which represents two directions of the conical shape in the space
temp <- data[data$component == 4,c("text", "cosine_dist")]
# note the closer to 0 the more perpendicular the text is
tempA <- temp[temp$cosine_dist < 0,]
tempB <- temp[temp$cosine_dist > 0,]
tempA <- tempA[order(-tempA$cosine_dist),]

tempB <- tempB[order(-tempB$cosine_dist),]

# note the cosine distances indicate those items that are closely related to each other.
# whilst identified to be in the same component eg component 4
# the cosine similarity determines how close each vector is to each other.
# therefore in presenting closely related documents
# use the closest n subset of items in proximity.

# for example in tempA the first 3 items are closely related
tempA[1:3,]
# tempA 4 is further away in the space and not as strongly related.
# hence in the clustering a radius needs to be used to select very closely related items
# this could be a cluster of k neigbours.
tempA[4,]

# for example tempB has indexes 14:20 as closely related for example
tempB[14:20,]
# those seem to be about messaging on conference

# note the similarity measure is sensitive to distance, so the radius threshold
# would need to be appropriately small, 
tempB[21:23,]

tempB[24:26,]

cnts[10:20,]


temp <- data[data$component == 23,c("text", "cosine_dist")]
temp

# when searching using a query, preprocess the input string
# convert it to a text vector which maps terms to a single
# document vector. All terms in the vocab would be presentin the vector
# with a 0 or 1 for whether the search query contains the term.
# then use the cosine similarity on the input term vector against
# the projection of VS to determine which sets of documents are closely matched.
# order in largest magnitude as the larger the magnitude the closer the angle.

query1 <- "the system will allow the user to register their voice"
# test an already known requirement
query2 <- "The database user for VocalPassword will have alter table rights for the VocalPassword database at runtime of the system"

terms <- preprocess_line(query1)
lines_list <- lines
words <- unlist(lines_list)
unique_words <- sort(unique(words))
search_mat <- convert_terms_to_matrix(terms, unique_words)
search_mat <- matrix(data=as.numeric(search_mat), nrow=1, ncol=length(search_mat))
# now we need to project the search_mat into the svd space to find the closest
# matching documents.
# for this purpose we need an SVD that relates terms documents

terms2 <- preprocess_line(query2)
search_mat2 <- convert_terms_to_matrix(terms2, unique_words)
search_mat2 <- matrix(data=as.numeric(search_mat2), nrow=1, ncol=length(search_mat2))


# calculate the term to document svd mapping
searchA <- svd(X)
searchU <- searchA$u
searchS <- searchA$d
# transposed relation of attributes to objects rows equal the attributes
searchV <- searchA$v

# test the relationship between terms terms
require(corrplot)
M <- cor(X, t(searchV))
corrplot(M, method="shade")
# note using the transpose will test the relation of documents.

zeros <- 0 * 1:(ncol(U)*nrow(U))

searchSI <- matrix(data=zeros, ncol=ncol(searchU), nrow=nrow(searchU))
for(i in 1:ncol(SI)) {
  searchSI[i,i] <- sqrt(searchS[i])
}

ssU <- searchU%*%searchSI
ssV <- searchV%*%searchSI

# search for query1 using the projection of the term matrix into the term document matrix
proj <- search_mat%*%ssV
# find the closest cosine distance to U
dist <- unlist(sapply(1:nrow(ssU), function(i) {
  u <- ssU[i,]
  cosine_dist(t(proj), u)
}))
docs <- data[,c("item_key", "text")]
docs$cosine_dist <- dist
docs <- docs[order(-abs(docs$cosine_dist)),]
## 


# search for query2 using the projection
# the result should exactly match query2 because this is an already existing requirement

# search for query1 using the projection of the term matrix into the term document matrix
proj2 <- search_mat2%*%ssV
# find the closest cosine distance to U
dist <- unlist(sapply(1:nrow(ssU), function(i) {
  u <- ssU[i,]
  cosine_dist(t(proj2), u)
}))
docs2 <- data[,c("item_key", "text")]
docs2$cosine_dist <- dist
docs2 <- docs2[order(-abs(docs2$cosine_dist)),]
## 