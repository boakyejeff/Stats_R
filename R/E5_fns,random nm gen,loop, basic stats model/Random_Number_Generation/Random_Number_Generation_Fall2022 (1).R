
options(width=60)

library(ggplot2)

set.seed(1)


## ----the sample function, echo=TRUE, eval=FALSE-------------
## sample(x,               # The collection we wish to sample from
##        size,            # How many draws we from x we want
##        replace = FALSE, # Should sampling be done with replacement?
##        prob = NULL)     # Individual probabilities of being drawn


## ----Simulating rolls of die, echo=TRUE, eval=TRUE----------
sample(x = 1:6, size = 1)     # Simulation of rolling a die once
# Simulation of rolling a die twice (or rolling two dice)
sample(x = 1:6, size = 2)     


## ----cars data load and subsample---------------------------
# The "cars" data give the speed of 1920's eta 
# cars and the distances taken to stop.
data(cars)
dim(cars)

training_rows <- sample(1:nrow(cars), size = .9*nrow(cars))

training_data <- cars[training_rows,]
dim(training_data)
# Test data is the data which is "held out"
test_data <- cars[-training_rows,]
dim(test_data)


## ----Cars lm() model evaluation, size = "footnotesize"------
training_model <- lm(dist ~ speed, data = training_data)
mean(residuals(training_model)^2)  # MSE = Mean Squared Error

test_predictions <- predict(training_model, newdata = test_data)
#mean(     y          -      yhat       )^2
mean((test_data$dist - test_predictions)^2)


## ----Obtaining Binomial probabilities-----------------------
dbinom(x = 1, size = 4, prob = .5)


## ----Obtaining Binomial probabilities 2---------------------
pbinom(q = 1,
       size = 4,
       prob = .5)


## ----Sum of Binomial probabilities--------------------------
dbinom(x = 0, size = 4, prob = .5) +
  dbinom(x = 1, size = 4, prob = .5)


## -----------------------------------------------------------
rbinom(n = 20, size = 1, prob = .5)


## ----Random draws from a Binomial distribution--------------
rbinom(n = 5, size = 10, prob = .5)


## ----PlotPoisson, echo = TRUE, size="tiny",fig.height=2.5,fig.width=4.5----
PoissonData <- data.frame(NumEvents = 0:20,
                          MeanIs1 = dpois(x = 0:20, lambda = 1),
                          MeanIs5 = dpois(x = 0:20, lambda = 5),
                          MeanIs9 = dpois(x = 0:20, lambda = 9))
ggplot(PoissonData, aes(x=NumEvents,y=MeanIs1)) + 
    geom_bar(stat='identity') 


## ----Poisson cumulative probability, echo = TRUE,fig.height=2.5,fig.width=4.5----
ppois(q = 2, lambda = 1)


## ----Poisson quantiles, echo = TRUE,fig.height=2.5,fig.width=4.5----
qpois(p = .38, lambda = 1)


## ----Random draws from a Poisson distribution, echo = TRUE,fig.height=2.5,fig.width=4.5----
rpois(n = 10, lambda = 1)


## ----Plotting various Normal distributions, echo = TRUE,size="tiny", out.width="75%"----
normal_dat <- data.frame(x = seq(-5, 5, length.out = 1000))
normal_dat$mean0var1 <- dnorm(x = normal_dat$x, mean = 0, sd = sqrt(1))
normal_dat$mean2var3 <- dnorm(x = normal_dat$x, mean = 2, sd = sqrt(3))
normal_dat$mean1var_p5 <- dnorm(x = normal_dat$x, mean = 1, sd = sqrt(.5))
plot(x = normal_dat$x, y = normal_dat$mean0var1, type="l", xlab = "x", ylab = "density", ylim=c(0,.6))
lines(x = normal_dat$x, y = normal_dat$mean2var3, lty=2)
lines(x = normal_dat$x, y = normal_dat$mean1var_p5, lty=3)


## ----Transforming the Normal distribution, echo = TRUE,size="tiny", out.width="75%"----
rand_norm <- rnorm(10000,mean = 10, sd = 2)
scaled_rand_norm <- (rand_norm - mean(rand_norm))/sd(rand_norm)
par(mfrow = c(1,2))
hist(rand_norm, main="Original", xlab = "", ylab = "", yaxt='n')
hist(scaled_rand_norm, main="Scaled", xlab = "", ylab = "", yaxt='n')


## ----Uniform Distribution Plot, echo = TRUE, size="tiny",fig.height=1.5,fig.width=4.5----
a <- 0
b <- 1
UniformData <- data.frame(x = seq(-1, 2, length=1000))
UniformData$fx <- ifelse(UniformData$x > a & UniformData$x < b,
                         yes = 1 / (b-a),
                         no = 0)
ggplot(UniformData, aes(x = x, y = fx)) + geom_line(stat = 'identity') + ylab('f(x)') + theme_bw()


## ----Generating random values from a uniform distribution, echo = TRUE,fig.height=1.5,fig.width=4.5----
runif(n = 5, min = 0, max = 10)


## ----Sampling via the inverse CDF technique, echo = TRUE,fig.height=1.5,fig.width=4.5----
u0 <- runif(n = 50000, min = 0, max = 1)
x0 <- qnorm(u0)  # Inverse CDF for N(0,1)


## ----Normal Inverse Animation, include=TRUE,echo = FALSE, fig.show='animate', interval=1, out.width="92%"----
x_vec <- seq(-3, 3, length.out = 1000)
pdf_vec <- dnorm(x_vec)
cdf_vec <- pnorm(x_vec)


par(mfrow=c(1,2))
par(xpd = TRUE)
nsim <- 100
all_x <- rep(NA,nsim)

for(i in 1:nsim){
  u_0 <- runif(1)
  x_0 <- qnorm(u_0)
  
  # Make first plot
  plot(x_vec, cdf_vec, type = "l",ylim = c(0,1),main = paste0("Generated u = ",round(u_0,3)),xlab = "")
  arrows(x0 = -3.5, y0 = u_0, x1 = x_0, y1 = u_0, lty = 1)
  text(x = -2.5,y = u_0 + .0125,labels = paste0("u = ",round(u_0,3)),col="red")
  arrows(x1 = x_0, y1 = -.03, x0 = x_0, y0 = u_0, lty = 1)
  text(x = x_0,y = -.07,labels = paste0("x = ",round(x_0,3)),col="red")
  
  all_x[i] <- x_0
  hist(all_x,
       xlim = c(-3,3),
       ylim = c(0,.75),
       main = paste0(sum(!is.na(all_x))," samples generated"),
       freq = FALSE,
       breaks=c((-1000):1000)/2.5,
       xlab = "")
  lines(x_vec, pdf_vec, type = "l", cex.axis = 1.5,ylim = c(0,5),xlim = c(0,8))
  arrows(x1 = x_0, y1 = 0, x0 = x_0, y0 = -.11, lty = 1)
  text(x = x_0,y = -.12,labels = paste0("x = ",round(x_0,3)),col="red")
}


## ----Plotting Samples Obtained from inverse sampling, size="scriptsize" ,echo = TRUE, out.width="85%"----
par(mfrow = c(1,2))
hist(u0, main="Original", xlab = "", ylab = "", yaxt='n')
hist(x0, main="Transformed", xlab = "", ylab = "", yaxt='n')


## ----Some Distribution without a built in r<dist>,echo = TRUE, out.width="50%"----
f_x <- function(x) 1/(2*sqrt(x)) * exp( -1*sqrt(x) )
x_vec <- seq(0.01, 5, length.out = 1000)
pdf_vec <- f_x(x_vec)
plot(x_vec, pdf_vec, type = "l", cex.axis = 1.5)


## ----Some Distributions CDF, echo = TRUE, out.width="60%"----
F_x <- function(x) 1 - exp( -1*sqrt(x) )
cdf_vec <- F_x(x_vec)
par(mfrow = c(1,2))
plot(x_vec, pdf_vec, type = "l")
plot(x_vec, cdf_vec, type = "l",ylim = c(0,1))


## ----Other Distribution Inverse Animation, include=TRUE,echo = FALSE, fig.show='animate', interval=.025, out.width="92%"----
f_x <- function(x) 1/(2*sqrt(x)) * exp( -1*sqrt(x) )
F_x <- function(x) 1 - exp( -1*sqrt(x) )
Finv_x <- function(u) {
  if( any( (u >= 1) | (u <= 0) ) ) stop("u must be between (0,1)")
  (log(1-u))^2
}

x_vec <- seq(0.001, 8, length.out = 1000)
pdf_vec <- f_x(x_vec)
cdf_vec <- F_x(x_vec)


par(mfrow=c(1,2))
par(xpd = TRUE)
nsim <- 500
all_x <- rep(NA,nsim)

for(i in 1:nsim){
  u_0 <- runif(1)
  x_0 <- Finv_x(u_0)
  
  # Make first plot
  plot(x_vec, cdf_vec, type = "l",ylim = c(0,1),main = paste0("Generated u = ",round(u_0,3)))
  arrows(x0 = -1, y0 = u_0, x1 = x_0, y1 = u_0, lty = 1)
  text(x = -.9,y = u_0 + .0125,labels = paste0("u = ",round(u_0,3)),col="red")
  arrows(x1 = x_0, y1 = 0, x0 = x_0, y0 = u_0, lty = 1)
  text(x = x_0,y = -.07,labels = paste0("x = ",round(x_0,3)),col="red")
  
  all_x[i] <- x_0
  hist(all_x,
       xlim = c(0,8),
       ylim = c(0,1),
       main = paste0(sum(!is.na(all_x))," samples generated"),
       freq = FALSE,
       breaks=c(0:100000)/2.5)
  lines(x_vec, pdf_vec, type = "l", cex.axis = 1.5,ylim = c(0,5),xlim = c(0,8))
  arrows(x1 = x_0, y1 = 0, x0 = x_0, y0 = -.11, lty = 1)
  text(x = x_0,y = -.12,labels = paste0("x = ",round(x_0,3)),col="red")
}


## ----Setting seeds to begin from a fixed point, size = "tiny"----
set.seed(1)
sample(1:1e6, size = 4)
set.seed(1)
sample(1:1e6, size = 4)
set.seed(1)
sample(1:1e6, size = 2)
sample(1:1e6, size = 2)
sample(1:1e6, size = 1)


## ----Rolling a die------------------------------------------
two_rolls <- sample(1:6, size = 2, replace = TRUE)
two_rolls
sum(two_rolls)


## ----Function for playing a game, size="scriptsize"---------
game <- function(...) { 
  two_rolls <- sample(1:6, size = 2, replace = TRUE)
  sum_of_rolls <- sum(two_rolls)
  # check if sum is 7 or 11, and store this in a logical object
  is_winning_sum <- sum_of_rolls %in% c(7,11)
  # Return amount of money the player receives after the game
  # (negative for a loss)
  if(!is_winning_sum) {
    return(-0.75)
  } else if(sum_of_rolls == 7) {
    return(3)
  } else if(sum_of_rolls == 11) {
    return(5)
  }
}
game()
sapply(1:25, game)


## ----Examples of playing the game, out.width="50%"----------
prize_plays <- sapply(1:10, game)
prize_plays
cumsum(prize_plays)
prize_longterm <- cumsum(sapply(1:10000, game))
plot(prize_longterm, type="l", xlab="Round", ylab = "$")


## ----Function for playing a game (with optimization), size="scriptsize"----
game <- function(...,
                 loss_price = -0.75,
                 win_7_prize = 3,
                 win_11_prize = 5) { 
  two_rolls <- sample(1:6, size = 2, replace = TRUE)
  sum_of_rolls <- sum(two_rolls)
  # check if sum is 7 or 11
  is_winning_sum <- sum_of_rolls %in% c(7,11)
  # Return amount of money the player receives after the game (negative for a loss)
  if(!is_winning_sum) {
    return(loss_price)
  } else if(sum_of_rolls == 7) {
    return(win_7_prize)
  } else if(sum_of_rolls == 11) {
    return(win_11_prize)
  }
}
# Create a function to compute total winnings after repeated plays
game_longterm_return <- function(num_games = 10, ...) {
  prize_plays <- sapply(1:num_games, game, ...)
  return(sum(prize_plays))
}


## ----Longterm playng of game--------------------------------
game_longterm_return(num_games = 1e4, # 10,000
                     loss_price = -0.75,
                     win_7_prize = 3,
                     win_11_prize = 5)

game_longterm_return(num_games = 1e4, # 10,000
                     loss_price = -1,
                     win_7_prize = 3,
                     win_11_prize = 5)


## ----Longterm playing of game multiple times, size="scriptsize", fig.height=2.5, fig.width=3.5, cache=TRUE----
multiple_10k_plays <- rep(NA, times = 5000)
for(r in 1:length(multiple_10k_plays)) {
  multiple_10k_plays[r] <- game_longterm_return(num_games = 1e4,
                                                loss_price = -1,
                                                win_7_prize = 3,
                                                win_11_prize = 5)
}
mean(multiple_10k_plays > 0)
hist(multiple_10k_plays, xlab = "Prize money", main="", cex.axis=.5)


## ----t-test example, size="scriptsize"----------------------
some_data <- rnorm(n = 20, mean = 30, sd = 3)
t.test(x = some_data, mu = 25, alternative = "greater")


## ----CV point estimate, size="small"------------------------
rand_unif <- runif(n = 50, min = 5, max = 10)
sd(rand_unif)/mean(rand_unif)  # this is just a point estimate


## ----Function for bootstrapping,size="small"----------------
compute_cv = function(data, indices){
 bootstrap_sample = data[indices]
 bootstrap_sample_cv = sd(bootstrap_sample)/mean(bootstrap_sample)
 return(bootstrap_sample_cv)
}
compute_cv(rand_unif, sample(1:50, size = 50, replace=T))
compute_cv(rand_unif, sample(1:50, size = 50, replace=T))


## ----Bootstrapping, size="footnotesize"---------------------
n_boot <- 5000
random_cv <- rep(NA, n_boot)
for(i in 1:n_boot) {
  random_cv[i] <- compute_cv(rand_unif,
                             sample(1:50, size = 50, replace=T))
}
mean(random_cv)
quantile(random_cv,0.025)
quantile(random_cv,0.975)


## ----Bootstrap histogram,size = "tiny", fig.height=3.5, fig.width=5.0----
hist(random_cv)


## ----Boot package, size = "tiny"----------------------------
library(boot)
boot_res <- boot(data = rand_unif, # data
                 statistic = compute_cv, # function to compute
                 R = 5000) # number of resamples
boot_res


## ----Bootstrap CI, size = "footnotesize"--------------------
c(quantile(boot_res$t[,1], .025), quantile(boot_res$t[,1], .975))


## ----Target Function, size = "tiny", fig.height=2.5, fig.width=3.5----
target = function(x){
  vec <- 
    ifelse(test = x < -3 | x > 3,
           yes = 0,
           no = -1*abs(x) + 3)
  return(vec)
}
x_vals <- seq(-4, 4, length.out = 500)
plot(x = x_vals, y = target(x_vals), type="l",ylab = "density", cex.axis=0.5)


## ----MH Core code, eval=TRUE, cache = TRUE------------------
n_samp <- 1e8
P_samp = rep(NA, n_samp)
P_samp[1] = 1     # Starting value (arbitrary)
for(i in 1:(n_samp-1)){
  current_tar = P_samp[i]
  # Propose a move from N(current_tar, sd = .5)
  proposed_tar = rnorm(1, mean = current_tar, sd=0.5)
  # Compute acceptance ratio
  alpha <- target(proposed_tar)/target(current_tar) 
  u <- runif(1,min = 0, max = 1)
  if(u < alpha){
    P_samp[i+1] = proposed_tar
  } else {
    P_samp[i+1] = current_tar 
  }
}


## ----Plot Density of MH samples, size = "footnotesize", fig.height=3.5, fig.width=4.5, cache = TRUE----
plot(density(P_samp))

