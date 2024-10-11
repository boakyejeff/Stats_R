
set.seed(1)


## ----Mean of a Random Sample--------------------------------------------------------------------------------
a_sample <- rnorm(n = 100, mean = 40, sd = 5)
mean(a_sample)


## ----Mean of 10 Random Samples------------------------------------------------------------------------------
# Using sapply() like this is a convenient way to compactly repeat a function
sapply(1:24, \(x){ rnorm(n = 100, mean = 40, sd = 5) |> mean() } )


## ----Demo of CLT, cache = TRUE------------------------------------------------------------------------------
x_bars <- sapply(1:1e6, \(x){ rnorm(n = 100, mean = 40, sd = 5) |> mean() } )
mean(x_bars)
var(x_bars)


## ----ttest demo, eval = FALSE-------------------------------------------------------------------------------
## t.test(x = dat, mu = 40, alternative = "greater")


## ----ttest of mean of random sample-------------------------------------------------------------------------
a_sample <- rnorm(n = 100, mean = 40, sd = 5)
mean(a_sample)
t.test(a_sample, mu = 40, alternative = "greater")


## ----whats a p value----------------------------------------------------------------------------------------
t_val <-     (mean(a_sample) - 40) / 
       (sd(a_sample)/sqrt(length(a_sample)))
1 - pt(t_val,df = length(a_sample) - 1)


## ----Type I error, cache = TRUE-----------------------------------------------------------------------------
p_value_RS <- function(n, mu_0, mu_test, sigma) {
  # mu_0 controls the true mean
  a_sample <- rnorm(n = n, mean = mu_0, sd = sigma)
  # mu_test controls the mu we test around.
  p_val <- t.test(a_sample, mu = mu_test, alternative = "greater")$p.value
  return(p_val)
}
# Type I error
# mean() of a logical will give the proportion of TRUE
mean(sapply(1:5000, \(x) p_value_RS(100, 40, 40, 5) < 0.05 ))
#
# Type II error: Fail to rej H0 when H0 is false
mean(sapply(1:5000, \(x) p_value_RS(100, 41, 40, 5) > 0.05 ))
mean(sapply(1:5000, \(x) p_value_RS(100, 42, 40, 5) > 0.05 ))


## ----Two Sample T test--------------------------------------------------------------------------------------
#head(sleep, 3)
ttest <- t.test(extra ~ group, data = sleep) # test extra BY group
ttest


## ----TTest_Mod_Object---------------------------------------------------------------------------------------
str(ttest)


## ----Power Funtion------------------------------------------------------------------------------------------
Power_calc <- function(mu0,   # value we are hypothesizing around
                       muA,   # TRUE value of the mean
                       sigma, # Noise in the process (assumed)
                       alpha, # significance level
                       nData, # Sample size of the experiment
                       nSim){ # Number of simuations used
  pvalues <- rep(NA, nSim)
  for(i in 1:nSim){
    ExperimentalData <- rnorm(n = nData, mean = muA, sd = sigma)
    TtestResult <- t.test(ExperimentalData, mu = mu0 ,alternative = "greater")
    pvalues[i] <- TtestResult$p.value
  }
  typeIIerrRate <- mean(pvalues > alpha)
  return(1 - typeIIerrRate)
}
Power_calc(mu0 = 25, muA = 25.1, sigma = 25, alpha = .05, nData = 100, nSim = 1000)
Power_calc(mu0 = 25, muA = 30, sigma = 25, alpha = .05, nData = 100, nSim = 1000)


## ----Sample Size effect on Power----------------------------------------------------------------------------
Power_calc(mu0 = 25, muA = 30, sigma = 25, alpha = .05, nData = 3, nSim = 100)
Power_calc(mu0 = 25, muA = 30, sigma = 25, alpha = .05, nData = 10, nSim = 100)
Power_calc(mu0 = 25, muA = 30, sigma = 25, alpha = .05, nData = 20, nSim = 100)
Power_calc(mu0 = 25, muA = 30, sigma = 25, alpha = .05, nData = 50, nSim = 100)
Power_calc(mu0 = 25, muA = 30, sigma = 25, alpha = .05, nData = 100, nSim = 100)
Power_calc(mu0 = 25, muA = 30, sigma = 25, alpha = .05, nData = 250, nSim = 100)


## ----Sample Size Power Calculator---------------------------------------------------------------------------
SampleSizePower <- function(Nvec, ShowProg = FALSE, ...){
  NumCalcs <- length(Nvec)
  PowerResults <- rep(NA, NumCalcs)
  if(ShowProg) ProgBar <- txtProgressBar(min = 0,
                                         max = NumCalcs,
                                         style = 3)
  for(i in 1:NumCalcs){
    PowerResults[i] <- Power_calc(nData = Nvec[i], ...)
    if(ShowProg) setTxtProgressBar(ProgBar, i)
  }
  if(ShowProg) close(ProgBar)
  return(PowerResults)
}


## ----Power plots, out.width="70%", cache = TRUE-------------------------------------------------------------
SampleSizes <- seq(5,200,by=5)
Powers <- SampleSizePower(Nvec = SampleSizes, mu0 = 25, muA=30,
                          sigma=25, alpha = 0.05, nSim = 1e5)
plot(x = SampleSizes, y = Powers, ylim = c(0,1), type = "b")
abline(h = 0.8, v = 155, lty = 2)


## ----lm in R------------------------------------------------------------------------------------------------
head(cars, 3)
lm_mod <- lm(dist ~ speed, data = cars)
lm_mod


## ----plotting the model, out.width="85%"--------------------------------------------------------------------
plot(y = cars$dist, x = cars$speed)
abline(lm_mod)


## ----diagnostics, out.width="80%"---------------------------------------------------------------------------
par(mfrow = c(2,2))
plot(lm_mod)


## ----summary, out.width="80%"-------------------------------------------------------------------------------
summary(lm_mod)


## ----predicting, out.width="80%"----------------------------------------------------------------------------
predict(lm_mod)


## ----predicting on new data, out.width="80%"----------------------------------------------------------------
new_df <- data.frame(speed = c(0, 5, 10, 20, 30))
predict(lm_mod, newdata = new_df)


## ----predicting on new data with intervals------------------------------------------------------------------
# see ?predict.lm
predict(lm_mod, newdata = new_df, interval = "confidence")
predict(lm_mod, newdata = new_df, interval = "prediction")


## -----------------------------------------------------------------------------------------------------------
data("marketing", package = "datarium")
model <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(model)


## ---- out.width="85%"---------------------------------------------------------------------------------------
par(mfrow = c(2,2))
plot(x = marketing$youtube, y = marketing$sales)
plot(x = marketing$facebook, y = marketing$sales)
plot(x = marketing$newspaper, y = marketing$sales)


## -----------------------------------------------------------------------------------------------------------
model2 <- lm(sales ~ poly(youtube,2) + facebook + newspaper, data = marketing)
summary(model2)


## -----------------------------------------------------------------------------------------------------------
model3 <- lm(sales ~ I(log(youtube)) + facebook + newspaper, data = marketing)
summary(model3)


## -----------------------------------------------------------------------------------------------------------
model4 <- lm(sales ~ poly(youtube,2) + facebook * newspaper, data = marketing)
coefficients(model4)
model5 <- lm(sales ~ poly(youtube,2) + facebook : newspaper, data = marketing)
coefficients(model5)


## ----Two models from earlier--------------------------------------------------------------------------------
coefficients(model2)
coefficients(model4)


## ----anova example------------------------------------------------------------------------------------------
#     larger, smaller
anova(model4, model2)


## ----drop1 example------------------------------------------------------------------------------------------
drop1(model4, test = "F")


## ----RandomForest preprocess--------------------------------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))
# Some quick preprocessing
marketing_train_rows <- sample(1:nrow(marketing), 0.9*nrow(marketing), replace = FALSE)
marketing_train <- marketing[marketing_train_rows,] 
marketing_test <- marketing[-marketing_train_rows,]
dim(marketing)
dim(marketing_train)
dim(marketing_test)
# Create seperate dataframes for X and Y for both training and test
X_train <- marketing_train |> select(youtube, facebook, newspaper)
X_test <- marketing_test |> select(youtube, facebook, newspaper)
Y_train <- marketing_train$sales
Y_test <- marketing_test$sales


## ----RandomForest fitting-----------------------------------------------------------------------------------
suppressPackageStartupMessages(library(randomForest))
RF_mod <- randomForest(x = X_train,
                       y = Y_train,
                       mtry = 2,
                       ntree = 1000)
YTestPred_RF <- predict(RF_mod, newdata = X_test)
RF_MSE <- mean((Y_test - YTestPred_RF)^2) 
RF_MSE


## ----XGBoost fitting----------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(xgboost))
XGB_mod <- xgboost(data = as.matrix(X_train), label = Y_train,
                   max.depth = 2, eta = .1, nrounds = 1000, verbose = 0)
YTestPred_XGB <- predict(XGB_mod, newdata = as.matrix(X_test))
XGB_MSE <- mean(( Y_test - YTestPred_XGB )^2) 
XGB_MSE


## ----Liner Model With Comparison----------------------------------------------------------------------------
lm_mod <- lm(sales ~ poly(youtube, 2) + facebook + newspaper,
             data = marketing_train)
YTestPred_LM <- predict(lm_mod, newdata = marketing_test)
LM_MSE <- mean(( Y_test - YTestPred_LM )^2) 
LM_MSE
# Benchmark of "null" model
Bench_MSE <- mean(( Y_test - mean(Y_train) )^2) 
# RMSE = root mean squared error
# -- on the same scale as the data
sqrt(c(LM = LM_MSE, RF = RF_MSE, XGB = XGB_MSE, Ysd = Bench_MSE))


## ----Liner Model With Comparison plot, out.width="72%"------------------------------------------------------
plot(x = Y_test, y = YTestPred_LM, ylim = c(5, 33))
points(x = Y_test, y = YTestPred_RF, pch = 2, col = 2)
points(x = Y_test, y = YTestPred_XGB, pch = 3, col = 3)
points(x = Y_test, y = rep(mean(Y_train),length(Y_test)), pch = 4, col = 4)
abline(0,1)
legend("topleft", legend = c("LM", "RF", "XGB","null"),
       pch = 1:4, col = 1:4)
