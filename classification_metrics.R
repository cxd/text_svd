## these methods are taken from the article "Computing classification evaluation metrics in R"
## http://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html

## Compute metrics for a balanced test result, assuming all classes are represented in equal proportions
## between training and test
## We also compute a one-vs-all when instances are not uniformly distributed over the classes.
classification_metrics <- function (predictedClasses, actualClasses) {
  results <- as.matrix(table(actual=actualClasses, predicted=predictedClasses))
  # total number of instances
  n <- sum(results)
  # number of classes
  nc <- nrow(results)
  # number of instances per class
  rowsums <- apply(results, 1, sum)
  # number of predictions per class
  colsums <- apply(results, 2, sum)
  # instances over actual classes
  p <- rowsums / n
  # instances over predicted classes
  q <- colsums / n
  # accuracy
  diag <- diag(results)
  accuracy <- sum(diag) / n
  # per class precision, recall and f-1
  precision <- diag/colsums
  recall <- diag / rowsums
  f1 <- 2*precision * recall/(precision+recall)
  uniformStats <- data.frame(labels=rownames(results), cnt=rowsums, precision=precision, recall=recall, f1=f1) 
  
  oneVsAll <- lapply(1:nc,
                     function(i) {
                       v = c(results[i,i],
                             rowsums[i] - results[i,i],
                             colsums[i] - results[i,i],
                             n-rowsums[i] - colsums[i] + results[i,i])
                       matrix(v,nrow=2, byrow=T)
                     })
  
  s <- matrix(0,nrow=2,ncol=2)
  for(i in 1:nc) { s = s + oneVsAll[[i]]}
  avgAccuracy <- sum(diag(s))/sum(s)
  
  # because the sum of the onvall matrices is a symmetric matrix, the microaveraged precision, recall and f1 
  # will be the same.
  micro_prf = (diag(s)/apply(s,1,sum))[1];
  
  
  ## Compute the majority class metrics
  mcIndex <- which(rowsums==max(rowsums))[1]
  mcAccuracy <- as.numeric(p[mcIndex])
  mcRecall <- 0*p; mcRecall[mcIndex] <- 1
  mcPrecision <- 0*p;mcPrecision[mcIndex] = p[mcIndex]
  mcF1 <- 0*p; mcF1[mcIndex] = 2*mcPrecision[mcIndex]/(mcPrecision[mcIndex])
  mcStats <- data.frame(labels=rownames(results), cnt=rowsums, precision=mcPrecision, recall=mcRecall, f1=mcF1)
  
  # Compute baseline random selection
  # predfict n/nc instances as class 'a' instances correctly classified with probability p_a
  randGuess <- (n/nc)*matrix(rep(p,nc),nc, nc, byrow=F)
  rgAccuracy <- 1/nc
  rgPrecision <- p
  rgRecall <- 0*p+1/nc
  rgF1 <- 2*p / (nc*p+1)
  randomStats <- data.frame(labels=rownames(results), cnt=rowsums, precision=rgPrecision, recall=rgRecall, f1=rgF1)
  
  # Compute kappa statustic
  expAccuracy <- sum(p*q)
  kappa <- (accuracy - expAccuracy) / (1-expAccuracy)
  
  list(
    resultTable=results,
    uniformAccuracy=accuracy,
    uniformStats=uniformStats,
    nonUniformAvgAccuracy=avgAccuracy,
    microPRF=micro_prf,
    majorityClassAccuracy=mcAccuracy,
    majorityClassStats=mcStats,
    randomAccuracy=rgAccuracy,
    randomStats=randomStats,
    kappa=kappa
  
    )
  
} 