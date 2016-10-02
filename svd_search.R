require(dplyr)
source("preprocess_text.R")
source("prepare_svd_document_model.R")

## cosine distance between two vectors
cosine_dist <- function(a, b) {
  xx <- crossprod(a,a)
  yy <- crossprod(b,b)
  xy <- crossprod(a,b)
  xy/(xx*yy)
}

# search the prepared dataset for the query and return top matches
# with a new data set built from the select_data_cols
search_text <- function(query, select_data_cols, outputBase) {
  data <- load_document_data(outputBase)
  unique_words <- load_unique_words(outputBase)
  searchA <- load_search_space(outputBase)
  terms <- preprocess_line(query)
  search_mat <- convert_terms_to_matrix(terms, unique_words)
  search_mat <- matrix(data=as.numeric(search_mat), nrow=1, ncol=length(search_mat))
  searchU <- searchA$u
  searchS <- searchA$d
  # transposed relation of attributes to objects rows equal the attributes
  searchV <- searchA$v
  zeros <- 0 * 1:(ncol(searchU)*nrow(searchU))
  
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
  docs <- data[,select_data_cols]
  docs$cosine_dist <- dist
  docs <- docs[order(-abs(docs$cosine_dist)),]
  docs
}