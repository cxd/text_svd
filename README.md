#Text SVD

This is an experiment with using an SVD and document term matrix to enable 

1. searching document examples by projecting a query term vector into the term space and using cosine similarity to rank results.
2. potentially extend the method for document clustering, or to gain further insights into common themes based on strongly related terms.


This project has an example of the first use case in the form of an R-shiny user interface.

#Usage

## Input Data

To use these scripts, first prepare a document set, which is a CSV file and include headers having the structure.

```
project_id,item_key,text
```

An example data file is in the input directory. The example data is derived from a set of defects from the apache HTTP server project space. 

## Preparing the Term Document Models

The script "run_prepare_svd.R" is used to prepare the document model which is used in the search process. The script will read the file from "input/input_data.csv" and generate a set of output RData files stored beneath the data directory.

## Provisioning the Term Document Models

To provision the term document models copy the RData files from the data/*.RData into the search directory.

```
cp data/*.RData search/data/ 
```

## Running the example search interface.

The example search interface can be run using the script "run_search.R".
By default it will listen on localhost port 7660. 

## Other Notes.

The bulk of the work done is based on a document term frequency matrix D with columns of D indicating each unique term frequency in each document row. The SVD is taken from this matrix and then operations are run in the term x doc space (V) or document x document (U) spaces. 

``` 
  X <- data.matrix(doc_term_mat[,3:ncol(doc_term_mat)])
  lines_list <- lines
  words <- unlist(lines_list)
  unique_words <- sort(unique(words))
  search_space <- svd(X)
  U <- search_space$u
  S <- search_space$d
  V <- search_space$v
```

When performing search the query is projected into a scaled version of U by multiplying the indicator matrix for the query against a scaled version of V and then using cosine distance to map back into closest documents. Unfortunately U contains all documents in the data set, so it wouldn't fit into memory for large volumes of data.

*This is a toy example to experiment with, and is not intended for large volumes of documents* 

However, it still may be suitable for reasonably number of small summary items, such as defect titles, lists of requirements, or short abstracts for collections of documents. For example the input data in this example has around 5000 or so defect titles that are available for search.  
 
``` 
search_mat <- search_term_mat[i,]
sM <- as.matrix(search_mat)
# search for query1 using the projection of the term matrix into the term document matrix
proj <- sM%*%(ssV)
# find the closest cosine distance to U
dist <- unlist(sapply(1:nrow(ssU), function(j) {
u <- ssU[j,]
cosine_dist(t(proj), u)
}))
```

Documents are assigned the distance from the query, and then ordered for maximum absolute distance (the distance can be in either direction).
 
``` 
  docs$cosine_dist <- dist
  docs <- docs[order(-abs(docs$cosine_dist)),]
```

Maybe, using an approach to select terms that correspond with the highest absolute coefficients in V it would be possible to generate a set of queries automatically for a selected top number of components in V (representing the largest variances in the diagonal S), then iteratively running the queries to calculate the proximities of each query per component against each document, the closest cosine distances then are used to label each document resulting in a group of documents clustered to each component. This way documents are labeled with their corresponding component.

The script "cluster_documents.R" makes some attempt at the above method, and outputs a set of CSV files one of which is called "docs_clustered.csv" that labels documents pertaining to the clusters assigned by this method.
