library(shiny)
require(dplyr)
source("preprocess_text.R")
source("prepare_svd_document_model.R")
source("svd_search.R")

outputBase <- "data/"
data <- load_document_data(outputBase) 
projects <- levels(data[,1])