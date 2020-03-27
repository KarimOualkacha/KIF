## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  library(devtools)
#  devtools::install_github("KarimOualkacha/KIF", build_vignettes = TRUE)
#  
#  library(KIF)
#  ## Loading required package : mvtnorm
#  ## Loading required package : ccaPP
#  ## Loading required package : parallel
#  ## Loading required package : pcaPP
#  ## Loading required package : robustbase

## ---- eval=FALSE--------------------------------------------------------------
#  library(mvtnorm)
#  set.seed(1)
#  n1 <- 100
#  n2 <- 100
#  n <- n1 +n2
#  p <- 500
#  sigma <- diag(p)
#  sigma[upper.tri(sigma)] <- 0.2
#  sigma[lower.tri(sigma)] <- 0.2
#  sigma1 <- sigma
#  sigma2 <- sigma
#  sigma1[1,2] <- 0.8
#  sigma1[2,1] <- 0.8
#  sigma1[3,4] <- 0.8
#  sigma1[4,3] <- 0.8
#  sigma2[3,4] <- -0.8
#  sigma2[4,3] <- -0.8
#  mean1 <- c(rep(0,p))
#  mean2 <- c(rep(0,p))
#  Sample <- rbind(rmvnorm(n1, mean1, sigma1), rmvnorm(n2, mean2, sigma2))
#  y <- c(rep(1,n1), rep(0,n2))

## ---- eval=FALSE--------------------------------------------------------------
#  couple12 <- Sample[,1:2]
#  couple34 <- Sample[,3:4]
#  out12 <- KIF(couple12,y)
#  out34 <- KIF(couple34,y)

## ---- eval=FALSE--------------------------------------------------------------
#  out12
#  ## [1] 0.199798
#  out34
#  ## [1] 0.5165657

## ---- eval=FALSE--------------------------------------------------------------
#  outall <- KIFall(Sample, y, 2, 10)
#  outall
#  ## [1] "3,4" "3,263" "1,2" "59,475" "42,350" "162,303" "393,483" "42,101"  "223,437" "246,366"
#  

