library(ggplot2)

set.seed(1)

cache_var <- TRUE


## ---- cache=cache_var----------------------------------------------------------------------------------------------------------------
start <- Sys.time()
for(i in 1:8) {
  Sys.sleep(1)
}
difftime(Sys.time(), start)


## ---- cache=cache_var----------------------------------------------------------------------------------------------------------------
start <- Sys.time()
obj <- lapply(as.list(1:8), \(i) Sys.sleep(1))
difftime(Sys.time(), start)


## ---- cache=cache_var----------------------------------------------------------------------------------------------------------------
library(parallel)
detectCores()  # How many "cores" does our system have?
cluster <- makeCluster(8)



## ---- cache=cache_var----------------------------------------------------------------------------------------------------------------
start <- Sys.time()
obj <- parLapply(cluster, as.list(1:8), \(i) Sys.sleep(1))
difftime(Sys.time(), start)

stopCluster(cluster)


## ---- size = "scriptsize",cache=cache_var--------------------------------------------------------------------------------------------
College <- read.csv("https://www.statlearning.com/s/College.csv",
                 stringsAsFactors = TRUE)[,-1]
suppressMessages(library(randomForest))

start <- Sys.time()
College.rf <- randomForest(Private ~ .,
                           data=College,
                           ntree=2000,
                           mtry = 6)
difftime(Sys.time(), start, units = "secs")


## ---- size = "scriptsize",cache=cache_var--------------------------------------------------------------------------------------------
par_ntree <- c(100, 500, 1000)
par_mtry <- c(4, 6, 8)


## ---- size = "scriptsize"------------------------------------------------------------------------------------------------------------
RF_test_misclass <- 
function(ntree, mtry, fold) {
  ran_for <- randomForest(Private ~ .,
                          data = College,
                          subset = (folds != fold),
                          ntree = ntree,
                          mtry = mtry)
  pred_resp <- predict(ran_for, newdata = College[folds == fold,])
  CV_misclass <- mean(pred_resp != College$Private[folds == fold])
  return(CV_misclass)
}


## ---- size = "small",cache=cache_var-------------------------------------------------------------------------------------------------
par_ntree <- c(100, 500, 1000)#
par_mtry <-  c(4, 6, 8)
num_folds <- 10
folds <- sample(1:num_folds, nrow(College), replace = TRUE)
RF_test_misclass(ntree = 100, mtry = 6, fold = 5)

start <- Sys.time()
for(cur_ntree in par_ntree) {
  for(cur_mtry in par_mtry)   {
    for(cur_fold in 1:num_folds) {
      err <- RF_test_misclass(ntree = cur_ntree,
                              mtry = cur_mtry,
                              fold = cur_fold)
    }
  }
}
difftime(Sys.time(), start, units = "secs")


## ---- size = "scriptsize",cache=cache_var--------------------------------------------------------------------------------------------
# expand.grid enumerates combinations of it's inputs
params <- expand.grid(ntree = par_ntree,
                      mtry = par_mtry,
                      fold = 1:num_folds)
head(params,3)
params.list <- split(params, seq(nrow(params)))
head(params.list,2)


## ---- size = "small",cache=cache_var-------------------------------------------------------------------------------------------------
start <- Sys.time()

params$errors <- 
lapply(params.list,  # each list element is a row of parameters
        \(r) RF_test_misclass(ntree = r$ntree,
                              mtry = r$mtry,
                              fold = r$fold))

difftime(Sys.time(),start,units = "secs")


## ---- size = "scriptsize",cache=cache_var--------------------------------------------------------------------------------------------
library(parallel)
clust <- makeCluster(8)
# we need to "send" data to each of the worker nodes we created in the cluster
clusterExport(clust,
              varlist = c("College","params","RF_test_misclass","folds"))
msgs <- clusterEvalQ(cl = clust, library(randomForest))

start <- Sys.time()
params$errors_2 <- 
parLapply(clust,params.list,
      \(row) RF_test_misclass(ntree = row$ntree,
                              mtry = row$mtry,
                              fold = row$fold))
difftime(Sys.time(),start,units = "secs")
stopCluster(clust)


## ---- size = "scriptsize",cache=cache_var--------------------------------------------------------------------------------------------
library(foreach)
start <- Sys.time()
obj <- 
# This passes (ntree[1], mtry[1], fold[1]), (ntree[2], mtry[2], fold[2]), etc...
foreach(ntree = params$ntree,
        mtry = params$mtry,
        fold = params$fold, .combine = rbind) %dopar%
  {
    data.frame(ntree = ntree,
               mtry = mtry,
               fold = fold,
               error = 
                 RF_test_misclass(ntree = ntree, mtry = mtry, fold = fold))
  }
difftime(Sys.time(),start,units = "secs")


## ---- size = "scriptsize",cache=cache_var--------------------------------------------------------------------------------------------
library(doParallel)
registerDoParallel(8)


## ---- size = "scriptsize",cache=cache_var--------------------------------------------------------------------------------------------
start <- Sys.time()
obj <- 
foreach(ntree = params$ntree,
        mtry = params$mtry,
        fold = params$fold, .combine = rbind) %dopar%
  {
    data.frame(ntree = ntree,
               mtry = mtry,
               fold = fold,
               error = 
                 RF_test_misclass(ntree = ntree, mtry = mtry, fold = fold))
  }
difftime(Sys.time(),start,units = "secs")


## ---- size = "scriptsize", eval=FALSE------------------------------------------------------------------------------------------------
## writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")


## ---- size = "scriptsize", eval=FALSE------------------------------------------------------------------------------------------------
## Sys.which("make")


## ---- size = "scriptsize", eval=FALSE------------------------------------------------------------------------------------------------
## install.packages("Rcpp")


## ------------------------------------------------------------------------------------------------------------------------------------
library(Rcpp)
evalCpp("1+1")
evalCpp("1+1")


## ---- size = "scriptsize"------------------------------------------------------------------------------------------------------------
MH_R <- function(n_samp) {
  P_samp = rep(0, n_samp)
  P_samp[1] = 1     # Starting value (arbitrary)
  
  for(i in 1:(n_samp-1)){
    current_tar = P_samp[i]
    # Propose a move from N(current_tar, sd = .5)
    proposed_tar = rnorm(1, mean = current_tar, sd=0.5)
    # Compute acceptance ratio
    if(abs(proposed_tar) < 3) {
      alpha = (-1*abs(proposed_tar) + 3)/
              (-1*abs(current_tar) + 3)
    } else alpha = 0
    
    u <- runif(1,min = 0, max = 1)
    
    if(u < alpha){
      P_samp[i+1] = proposed_tar
    } else {
      P_samp[i+1] = current_tar 
    }
  }
  return(P_samp)
}


## #include <Rcpp.h>

## using namespace Rcpp;

## 
## // [[Rcpp::export]]

## NumericVector MH_Rcpp(int n_samp) {

##   double current_tar, proposed_tar, alpha, u; // Must declare variables

## 

##   NumericVector P_samp(n_samp);

##   P_samp[0] = 1;

## 
##   for(int i = 0; i < (n_samp-1); i++) {

##     current_tar = P_samp[i];

##     proposed_tar = R::rnorm(current_tar, 0.5);

## 

##     if(fabs(proposed_tar) < 3)

##       alpha = (-1*fabs(proposed_tar) + 3)/

##               (-1*fabs(current_tar) + 3);

##     else alpha = 0;

## 

##     u = R::runif(0,1);

## 

##     if(u < alpha)

##       P_samp[i+1] = proposed_tar;

##     else P_samp[i+1] = current_tar;

##   }

##   return(P_samp);

## }


## ---- cache=cache_var----------------------------------------------------------------------------------------------------------------
start <- Sys.time()
R_res <- MH_R(1e7)
difftime(Sys.time(), start)

start <- Sys.time()
Rcpp_res <- MH_Rcpp(1e7)
difftime(Sys.time(), start)

