require(dplyr)
source("preprocess_text.R")
source("prepare_svd_document_model.R")
source("svd_search.R")

file <- "input/input_data.csv"

data_cols <- c("project_id", "item_key", "text")
id_cols <- c("project_id", "item_key")

prepare_document_model(file, data_cols, id_cols, "data/")
