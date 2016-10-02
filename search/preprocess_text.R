require(deployrUtils)

deployrPackage("stringr")

skip_list <- read.csv("input/skip_list.csv", header=FALSE)

# note preprocessing from
# http://www.mjdenny.com/Text_Processing_In_R.html
preprocess_line <- function(line) {
  temp <- stringr::str_to_lower(line)
  temp <- stringr::str_replace_all(temp, "[^a-zA-Z0-9\\s]", " ")
  temp <- stringr::str_replace_all(temp, "[\\s]+", " ")
  temp <- stringr::str_split(temp," ")[[1]]
  indexes <- which(temp == "")
  if (length(indexes) > 0) {
    temp <- temp[-indexes]
  }
  # remove items from the skip list
  len <- nrow(skip_list)
  for(i in 1:len) {
    indexes <- which(temp == skip_list[i,1])
    if (length(indexes) > 0) {
      temp <- temp[-indexes]
    }
  }
  temp
}

# data is a data frame for the original document samples it is limited in size
# to what R can keep in memory
# id_cols is the set of id columns to transfer into the new document matrix
# lines list is a list of lists, same number of rows as data, each list represents the
# list of terms extracted from the original document
# the output matrix is a document term matrix, were the terms are counted for each appearance in the document.
# the matrix is n documents x m total unique words.
# this routine is fairly slow, extracting the term counts needs some work.
convert_to_matrix <- function(data, id_cols, lines_list) {
  words <- unlist(lines_list)
  unique_words <- sort(unique(words))
  # count the words in each column of the matrix for each document
  M <- matrix(nrow=nrow(data), ncol=length(unique_words))
  idx <- 1:nrow(data)
  # this section needs some work on speeding up
  # the following is just a brute force count for each column in unique words
  # the rows x columns has linear O(n*m) where n is rows of documents and m is number of unique words
  rows <- sapply(idx, function(i) {
    cols <- sapply(1:length(unique_words), function(j) {
      txt <- lines_list[[i]]
      word <- unique_words[j]
      indexes <- which(txt == word)
      cnt <- length(indexes[indexes > 0])
      cnt
    })
    data.frame(t(unlist(cols)))
  })
  M <- t(rows)
  df <- data.frame(data[,id_cols])
  df <- cbind(df, M)
  colnames(df) <- c(id_cols, unique_words)
  df
}

convert_terms_to_matrix <- function(terms, unique_words) {
  unique_words <- sort(unique_words)
  # count the words in each column of the matrix for each document
  M <- matrix(nrow=1, ncol=length(unique_words))
  idx <- 1:1
  # this section needs some work on speeding up
  # the following is just a brute force count for each column in unique words
  # the rows x columns has linear O(n*m) where n is rows of documents and m is number of unique words
  cols <- sapply(1:length(unique_words), function(j) {
      txt <- terms
      word <- unique_words[j]
      indexes <- which(txt == word)
      cnt <- length(indexes[indexes > 0])
      cnt
    })
  df <- data.frame(t(unlist(cols)))
  colnames(df) <- c(unique_words)
  df
}
