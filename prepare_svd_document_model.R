## this script will generate the SVD document model
## this document model can be used to perform search operations.

require(ggplot2)
require(dplyr)
source("preprocess_text.R")

prepare_document_model <- function(inputCsv, data_cols, id_cols, outputBase, text_col="text") {
  data <- read.csv(inputCsv, header=TRUE)
  colnames(data) <- data_cols
  # convert all text into lines.
  lines <- sapply(data[,text_col], preprocess_line)
  
  results <- convert_to_matrix(data, id_cols, lines)
  term_counts <- results$terms
  doc_term_mat <- results$doc_mat
  
  startIdx <- length(id_cols)
  
  # get SVD of term document matrix
  # A = USV'
  # U represents relation of rows
  # V' represents relation of attributes
  # S represents square root of eigenvalues
  X <- data.matrix(doc_term_mat[,startIdx:ncol(doc_term_mat)])
  
  lines_list <- lines
  words <- unlist(lines_list)
  unique_words <- sort(unique(words))
  
  # Calculate the column means and sd 
  column_means <- apply(X, 2, mean)
  column_sd <- apply(X, 2, sd)
  
  # Scale the data set.
  X <- scale(X)
  
  searchSpace <- svd(X)
  
  saveRDS(data, file=paste(outputBase, "documents_data.RData", sep=""))
  saveRDS(unique_words, file=paste(outputBase, "unique_words.RData", sep=""))
  saveRDS(searchSpace, file=paste(outputBase, "search_space.RData", sep=""))
  saveRDS(doc_term_mat, file=paste(outputBase, "document_term_mat.RData", sep=""))
  saveRDS(term_counts, file=paste(outputBase, "term_counts.RData", sep=""))
  saveRDS(column_means, file=paste(outputBase, "doc_mat_column_means.RData", sep=""))
  saveRDS(column_sd, file=paste(outputBase, "doc_mat_column_sdev.RData", sep=""))
  
}

load_term_counts <- function(outputBase) {
  readRDS(paste(outputBase, "term_counts.RData", sep=""))
}

load_document_data <- function(outputBase) {
  readRDS(paste(outputBase, "documents_data.RData", sep=""))
}

load_document_term_mat <- function(outputBase) {
  readRDS(paste(outputBase, "document_term_mat.RData", sep=""))
}

load_document_term_column_means <- function(outputBase) {
  readRDS(paste(outputBase, "doc_mat_column_means.RData", sep=""))
}

load_document_term_column_sdev <- function(outputBase) {
  readRDS(paste(outputBase, "doc_mat_column_sdev.RData", sep=""))
}

load_unique_words <- function(outputBase) {
  readRDS(paste(outputBase, "unique_words.RData", sep=""))
}

load_search_space <- function(outputBase) {
  readRDS(paste(outputBase, "search_space.RData", sep=""))
}

