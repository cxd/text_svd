## this script will generate the SVD document model
## this document model can be used to perform search operations.

require(ggplot2)
require(dplyr)
source("preprocess_text.R")

prepare_document_model <- function(inputCsv, data_cols, id_cols, outputBase) {
  data <- read.csv(inputCsv, header=TRUE)
  colnames(data) <- data_cols
  # convert all text into lines.
  lines <- sapply(data$text, preprocess_line)
  
  doc_term_mat <- convert_to_matrix(data, id_cols, lines)
  # get SVD of term document matrix
  # A = USV'
  # U represents relation of rows
  # V' represents relation of attributes
  # S represents square root of eigenvalues
  X <- data.matrix(doc_term_mat[,3:ncol(doc_term_mat)])
  
  lines_list <- lines
  words <- unlist(lines_list)
  unique_words <- sort(unique(words))
  
  searchSpace <- svd(X)
  
  saveRDS(data, file=paste(outputBase, "documents_data.RData", sep=""))
  saveRDS(unique_words, file=paste(outputBase, "unique_words.RData", sep=""))
  saveRDS(searchSpace, file=paste(outputBase, "search_space.RData", sep=""))
  saveRDS(doc_term_mat, file=paste(outputBase, "document_term_mat.RData", sep=""))
}

load_document_data <- function(outputBase) {
  readRDS(paste(outputBase, "documents_data.RData", sep=""))
}

load_document_term_mat <- function(outputBase) {
  readRDS(paste(outputBase, "document_term_mat.RData", sep=""))
}

load_unique_words <- function(outputBase) {
  readRDS(paste(outputBase, "unique_words.RData", sep=""))
}

load_search_space <- function(outputBase) {
  readRDS(paste(outputBase, "search_space.RData", sep=""))
}

