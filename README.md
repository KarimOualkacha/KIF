# KIF

The Kendall Interaction Filter for Variable Interaction Screening in High Dimensional Classification Problems

## Synopsis
Kendall Interaction Fliter (KIF) approach, which conducts Interaction Screening in the high dimensional data for multi-class classification outcome

## What is KIF
This is a package implementing the _Kendall Interaction Filter_ (_KIF_), an efficient interaction screening method aiming to select relevant couples to the classification task in the high dimensional data frame. The measure _KIF_ is presented in the paper "The Kendall Interaction Filter for Variable Interaction Screening in High Dimensional Classification Problems". It has several advantages:

*	It is model-free.
*	It has the ability to process both continuous and categorical features.
*	It has the sure screening property.
*	It is heredity-assumption free.
*	It is robust against heavy-tailed distributions.

The __KIF__ package implements two methods; namely __KIF.couple__ and __KIF.all__. The method __KIF.couple__ takes a couple as an input and returns its _Kendall Interaction Filter_ score as an output while the method __KIF.all__ takes as an input the complete dataset and returns the most relevant couples, to the classification task, as an output.


## Installation       
To install __KIF__ package, run:   
```r
library(devtools)
devtools::install_github("KarimOualkacha/KIF", build_vignettes = TRUE)

library(KIF)
## Loading required package : mvtnorm
## Loading required package : ccaPP
## Loading required package : parallel
## Loading required package : pcaPP
## Loading required package : robustbase
```


## Quick start example

### Toy example
We generate a toy dataset to illustrate the usage of the functions __KIF.couple__ and __KIF.all__. The dataset has 200 observations and 500 explanatory variables. It is a two class example where each class has 100 observations.     
```r
library(mvtnorm)
set.seed(1)
n1 <- 100
n2 <- 100
n <- n1 +n2
p <- 500
sigma <- diag(p)
sigma[upper.tri(sigma)] <- 0.2
sigma[lower.tri(sigma)] <- 0.2
sigma1 <- sigma
sigma2 <- sigma
sigma1[1,2] <- 0.8
sigma1[2,1] <- 0.8
sigma1[3,4] <- 0.8
sigma1[4,3] <- 0.8
sigma2[3,4] <- -0.8
sigma2[4,3] <- -0.8
mean1 <- c(rep(0,p))
mean2 <- c(rep(0,p))
Sample <- rbind(rmvnorm(n1, mean1, sigma1), rmvnorm(n2, mean2, sigma2))
y <- c(rep(1,n1), rep(0,n2))
```
The relevenant couples, to the classification task, are "1,2" and "3,4". Couple "3,4" is more relevant than "1,2".

### KIF.couple function
The __KIF.couple__ function requires as arguments a pair of explanatory variables and the labels variable and returns as an output the corresponding _Kendall Interaction Filter_ score.
```r
out12 <- KIF.couple(Sample, y, c(1,2))
out34 <- KIF.couple(Sample, y, c(3,4))
```
The result is:
```r
out12
## [1] 0.199798
out34
## [1] 0.5165657
```
_Kendall Interaction Filter_ score of couple "3,4" is higher than that of couple "1,2", as expected.

### KIF.all function
The __KIF.all__ function requires as arguments the dataset, the labels variable, the number of cores to use for parallelization and the number of pairs to select among the first selected couples. It returns as an output the couples selected as relevant ones based on thier decreasing _Kendall Interaction Filter_ scores order. 
```r
outall <- KIF.all(Sample, y, 1, 10)
outall
##     [,1] [,2]
## [1,]    3    4
## [2,]    3  263
## [3,]    1    2
## [4,]   59  475
## [5,]   42  350
## [6,]  162  303
## [7,]  393  483
## [8,]   42  101
## [9,]  223  437
##[10,]  246  366
```
Couples "1,2" and "3,4" are both among the first 10 selected couples, impliying that _Kendall Interaction Filter_ indeed has the ability to select the relevant couples.

## Authors

Youssef Anzarmou, Abdallah Mkhadri, Karim Oualkacha
