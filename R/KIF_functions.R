




## additional function
#' @title Kendall Interaction score (KIF)
#' @description \code{KIF} function computes the KIF score of a single couple.
#' @param biSample a numeric matrix with two columns and \eqn{n} rows.
#' @param y the response variable. \code{y} must be a factor.
#' @details
#' KIF measure aims to select the relevant couples for the multi-class classification task. It is based on Kendall's Tau and ranges form 0 to 1. KIF is a model-free procedure, handles continuous, categorical or a mixture of continuous-categorical features, and is invariant under monotonic transformations. \code{KIF} function computes KIF score of a single couple.
#
#'@return \code{KIF} function returns the KIF score of the given couple
#' @importFrom ccaPP corKendall
#' @export
KIF <- function(biSample,y){

  Label <- unique(y)
  sampleK <- lapply(1:length(Label), function(cls){biSample[y==Label[cls],]})

  ncls <- sapply(1:length(Label), function(cls){sum(y==Label[cls])})
  n <- sum(ncls) ## number of observations

  ClassK <- sapply(1:length(Label), function(cls){corKendall(sampleK[[cls]][,1], sampleK[[cls]][,2])})
  Class0 <- corKendall(biSample[,1], biSample[,2])

  ClassK0 <- sapply(1:length(Label), function(cls){abs((ncls[cls]/n)*(ClassK[cls]-Class0))})

  sum(ClassK0)
}





## main function
#' @title Kendall Interaction Filter (KIF)
#' @description an interaction screening procedure based on Kendall's Tau.
#'
#' @param Sample a numeric matrix containing the data with \eqn{n} rows (observations) and \eqn{p} columns (features).
#' @param y the response variable. \code{y} must be a factor.
#' @param ncor number of cores to use for code parallelization. Default is 2.
#' @param threshold the number of relevant couples to select.
#'
#' @import parallel
#' @import mvtnorm
#' @details
#' KIF measure aims to select the relevant couples for the multi-class classification task. It is based on Kendall's Tau and ranges form 0 to 1. KIF is a model-free procedure, handles continuous, categorical or a mixture of continuous-categorical features, and is invariant under monotonic transformations. \code{KIFall} function computes KIF scores of all possible couples, then returns the "threshold" couples with highest scores.
#'
#' @return \code{KIFall} function returns the first "threshold" selected couples among all the selected couples.
#' @export
#' @examples
#'\dontrun{
#' ############### KIFall function ###############
#' library(mvtnorm)
#' set.seed(1)
#' n1 <- 100
#' n2 <- 100
#' n <- n1 +n2
#' p <- 500
#' sigma <- diag(p)
#' sigma[upper.tri(sigma)] <- 0.2
#' sigma[lower.tri(sigma)] <- 0.2
#' sigma1 <- sigma
#' sigma2 <- sigma
#' sigma1[1,2] <- 0.8
#' sigma1[2,1] <- 0.8
#' sigma1[3,4] <- 0.8
#' sigma1[4,3] <- 0.8
#' sigma2[3,4] <- -0.8
#' sigma2[4,3] <- -0.8
#' mean1 <- c(rep(0,p))
#' mean2 <- c(rep(0,p))
#' Sample <- rbind(rmvnorm(n1, mean1, sigma1), rmvnorm(n2, mean2, sigma2))
#' y <- c(rep(1,n1), rep(0,n2))
#' out <- KIFall(Sample, y, 4, 10)
#'}
KIFall <- function(Sample, y, ncor = 2, threshold){

  ## Sample must be a matrix
  ## y must be categorical
  y <- as.factor(y)
  Label <- levels(y)
  y <- as.numeric(as.vector(factor(y, levels = levels(as.factor(y)), labels = 1:length(Label))))
  ncls <- sapply(1:length(Label), function(cls){sum(y==Label[cls])}) ## number of obsevations in each class
  n <- sum(ncls) ## number of observations
  p <- dim(Sample)[2] ## number of predictors

  ## fix the number of selected couples
  Threshold <- 1:threshold

  ## define a matrix to score the KIF scores for each couple
  biIndex <- diag(p)
  diag(biIndex) <- 0

  ## needed to parallelize the code
  if(!(exists("IndexSuite"))){
    IndexSuite <<- list()
    i <- 1
    j <- 2
    for(ktk in 1:(p*(p-1)/2)){
      IndexSuite[[ktk]] <<- c(i,j)
      j <- j+1
      if(j>p){
        i <- i+1
        j <- i+1
      }
    }
  }

  ## Compute the scores and save them
  cl <- makeCluster(ncor)
  clusterExport(cl, c("KIF", "corKendall", "Sample", "y", "n"))
  vecBeindex <- parSapply(cl, IndexSuite, function(x){KIF(Sample[,x],y)})
  stopCluster(cl)
  biIndex[lower.tri(biIndex)] <- vecBeindex
  biIndex <- t(biIndex)

  ## choose the first "threshold" couples and save them to the vector kendall
  kendall <- numeric()
  for(i in Threshold){
    MaXcore <- as.vector(which(biIndex==max(biIndex), arr.ind = T))
    biIndex[MaXcore[1],MaXcore[2]] <- 0
    kendall[i] <- paste(MaXcore[1],MaXcore[2], sep = ",")
  }
  ##return the selected couples
  kendall
}



