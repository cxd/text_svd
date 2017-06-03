library(shiny)
require(dplyr)
source("preprocess_text.R")
source("prepare_svd_document_model.R")
source("svd_search.R")

outputBase <- "data/"
searchThreshold <- 0.01
id_cols <- c("project_id", "item_key")
data <- load_document_data(outputBase) 
unique_words <- load_unique_words(outputBase)
searchA <- load_search_space(outputBase)
doc_term_mat <- load_document_term_mat(outputBase)

projects <- levels(data[,1])