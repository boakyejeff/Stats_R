## ---- echo=FALSE, include = FALSE, eval=TRUE, out.width="70%"-----------------------------------------------
par(mar = c(0,0,0,0))


## ----Simple Plot, echo=TRUE, eval=TRUE, out.width="70%"-----------------------------------------------------
read_data <- read.csv("~/STAT8110/my_data.csv")
plot(x = read_data$x, y = read_data$y)


## ---- echo = FALSE, out.width="100%"------------------------------------------------------------------------
# initialization
par(mar=c(3,3,3,3))
num <- 0 ; 
num1 <- 0
plot(0,0 , xlim=c(0,21) , ylim=c(0.5,6.5), col="white" , yaxt="n", xaxt = "n" , ylab="" , xlab="")

#fill the graph
for (i in seq(1,20)){
  points(i,1 , pch=i , cex=3)
  points(i,2 , col=i , pch=16 , cex=3)
  points(i,3 , col="black" , pch=16 , cex=i*0.25)
  
  #lty
  if(i %in% c(seq(1,18,3))){
    num=num+1
    points(c(i,i+2), c(4,4) , col="black" , lty=num , type="l" , lwd=2)
    text(i+1.1 , 4.15 , num)
  }
  
  #type and lwd 
  if(i %in% c(seq(1,20,5))){
    num1=num1+1
    points(c(i,i+1,i+2,i+3), c(5,5,5,5) , col="black"  , type=c("p","l","b","o")[num1] , lwd=2)
    text(i+1.1 , 5.2 , c("p","l","b","o")[num1] )
    points(c(i,i+1,i+2,i+3), c(6,6,6,6) , col="black"  , type="l",  lwd=num1)
    text(i+1.1 , 6.2 , num1 )
    
  }
}

#add axis
axis(2, at = c(1,2,3,4,5,6), labels = c("pch" , "col" , "cex" , "lty", "type" , "lwd" ), 
     tick = TRUE, col = "black", las = 1, cex.axis = 0.8)

axis(1, at = 1:20, labels = 1:20, 
     tick = TRUE, col = "black", las = 1, cex.axis = 0.8)

# abline(v = 1:20,lty=3,lwd = .1)


## ----Simple Plot2, echo=TRUE, eval=TRUE, out.width="80%"----------------------------------------------------
plot(x = read_data$x, y = read_data$y,
     main = "A simple plot", xlab = "x", ylab = "y",
     pch = 8, cex = 2, col = 11)


## ---- echo=TRUE, eval=TRUE, out.width="60%"-----------------------------------------------------------------
# Passing in one vector to plot will place indecies on
# the horizontal axis
plot( 1:10, type = "o", lty = 4, pch=2,cex=5)


## ----Making Lines 1, echo=TRUE, eval=TRUE, out.width="60%"--------------------------------------------------
head(read_data, 3)
plot(x = read_data$x, y = read_data$y,type="l")


## ----Making Lines 2, echo=TRUE, eval=TRUE, out.width="50%"--------------------------------------------------
suppressPackageStartupMessages( library(tidyverse) )
read_data <- read_data |> arrange(x)
head(read_data, 3)
plot(x = read_data$x, y = read_data$y,type="l")


## ---- out.width="45%"---------------------------------------------------------------------------------------
x <- seq(from = -2*pi, to = 2*pi, length.out = 1000)
y <- sin(x)
plot(x,y, type = "l", lty = 1)
# Make a smaller set for points
x2 <- seq(from = -2*pi, to = 2*pi, length.out = 14)
y2 <- sin(x2)

# 7 = yellow(ish), 2 = red(ish), 3 = green(ish)
col_vec <- rep(7, times = length(x2))
col_vec[y2 > 0.5] <- 3 # larger y values are green(ish)
col_vec[y2 < -0.5] <- 2 # smaller y values are red(ish)
points(x = x2, y = y2, col = col_vec, pch = 16, cex=3)
abline(h = c(-0.5, 0.5), lty = 3)
text(x = c(-3*pi/2, -1*pi/2, pi/2),
     y = c(-1, 1, 0),
     labels = c("Down low", "Up high!", "Middle"), cex = 2)


## ----out.height="50%", out.width="70%"----------------------------------------------------------------------
x_vals <- seq(from = 0, to = 2*pi, length.out = 30)
library(emojifont)
plot(NULL, xlim = c( min(x_vals), max(x_vals) ), ylim = c( -1, 1 ), xlab = "", ylab = "")
for(xx in x_vals) {
  text(x = xx, y = sin(xx),
       labels=emoji('skier'), family='EmojiOne',
       cex=3, 
       srt = atan(cos(xx)) * 180/pi - 15)
}
text(x = pi/2, y = 0, labels=emoji('panda_face'), cex=10, family='EmojiOne')
text(x = 3*pi/2, y = 0, labels=emoji('panda_face'), cex=10, family='EmojiOne')


## ----out.height="70%", out.width="90%"----------------------------------------------------------------------
par(mfrow = c(1,3), cex.axis = 2)
plot(1:10)
plot(x_vals, sin(x_vals), type = "l")
plot(cars)


## ----plotting an LM object, echo=TRUE, eval=TRUE, out.width="60%"-------------------------------------------
regression <- lm(y ~ I(x^2), data = read_data)
plot(regression)


## ----ggplot, echo=TRUE, eval=TRUE, out.width="70%"----------------------------------------------------------
library(ggplot2)
ggplot(read_data) + # Specify data (as a data frame)
geom_point(aes(x = x, y = y)) # add geometry


## ----ggplot with aes specified, echo=TRUE, eval=TRUE, out.width="60%"---------------------------------------
pl <- 
  # this aes() will pass to all subsequent layers
  ggplot(read_data, aes(x = x, y = y)) + 
  geom_point() # add geometry
pl


## ----Adding to a ggplot, echo=TRUE, eval=TRUE, out.width="60%"----------------------------------------------
library(ggplot2)
pl + geom_smooth()


## ----Airquality, echo=TRUE, eval=TRUE, out.width="60%"------------------------------------------------------
data(Grunfeld, package="AER")
head(Grunfeld)


## ----Airquality Basic Plot, echo=TRUE, eval=TRUE, out.width="60%", warning=FALSE----------------------------
pl <- 
  ggplot(Grunfeld, aes(x = capital, y = value)) + 
  geom_point() # add geometry
pl


## ----Airquality Colored Plot, echo=TRUE, eval=TRUE, out.width="60%", warning=FALSE--------------------------
pl <- 
  ggplot(Grunfeld, aes(x = capital, y = value, color = firm)) + 
  geom_point() # add geometry
pl


## ---- echo=TRUE, eval=TRUE, out.width="60%", warning=FALSE--------------------------------------------------
pl <- 
  ggplot(Grunfeld, aes(x = invest, y = value)) + 
  geom_point() +# add geometry
  facet_wrap(~firm) +
  scale_y_log10() + scale_x_log10() # transform x and y axis
pl


## ---- out.width="60%"---------------------------------------------------------------------------------------
pl <- 
  ggplot(Grunfeld, aes(x = year, y = capital, color = firm, group = firm)) + 
  geom_point() +# add geometry
  facet_wrap(~firm) +
  scale_y_log10()
pl


## ---- out.width="60%"---------------------------------------------------------------------------------------
Grunfeld_longer <- 
  Grunfeld |>
  pivot_longer(cols = c("invest","value","capital"),
               names_to = "Quantity",
               values_to = "Dollars")

head(Grunfeld_longer,7)

pl <- 
  ggplot(Grunfeld_longer, aes(x = year, y = Dollars, color = Quantity)) + 
  geom_line() +# add geometry
  facet_wrap(~firm) +
  scale_y_log10()


## ---- out.width="80%"---------------------------------------------------------------------------------------
pl


## ---- out.width="80%"---------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(lubridate))
root_url <- "https://github.com/CSSEGISandData/COVID-19/raw/master/"
loc_url <- "csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
COVID19 <- read.csv( paste0(root_url, loc_url) )

COVID19_MO <- 
COVID19 |> 
  filter(Province_State == "Missouri") |> 
  # Grab county and Date information (Dates start with "X" when read in)
  select(Admin2, starts_with("X")) |> 
  # We want a long DF for plotting
  pivot_longer(cols = starts_with("X"), # more convenient tidyverse functions
               names_to = "Date",
               values_to = "Cases") |>
  # Replace X with nothing (so we have a typical date format)
  mutate(Date = sub(pattern = "X", replacement = "", x = Date),
         # mdy() can figure out how to parse the '.' separator!
         Date = mdy(Date)) |>
  rename(County = Admin2) |>
  # One final filter to keep things topical
  filter(County %in% c("St. Louis", "St. Louis City", "Boone"),
         Date > mdy( "3/16/2020" ))


## ---- out.width="80%", warning=FALSE------------------------------------------------------------------------
p <- ggplot(COVID19_MO, aes(x = Date, y = Cases, color = County)) + 
  geom_line() + scale_y_log10()
p + theme_bw()


## ---- out.width="75%", out.height="50%", fig.width=10, fig.height=6, warning=FALSE, message=FALSE-----------
COVID19_MO <- 
  COVID19_MO |> 
  # For each county...
  group_by(County) |> 
  # ... Create a variable based on sequential differences of cases
  mutate(New_Cases = c(0, diff(Cases)))
# 0 values cause issues with log transform, so set to NA so they are dropped
COVID19_MO$New_Cases[COVID19_MO$New_Cases==0] <- NA

p <- ggplot(COVID19_MO, aes(x = Date, y = New_Cases, color = County)) + 
  geom_point() + geom_smooth(span = .3) + scale_y_log10()
p + theme_bw()


## ----Writing a plot, echo=TRUE, eval=TRUE, out.width="60%", warning=FALSE-----------------------------------
pdf("~/STAT8110/my_plot.pdf") # turn on the PDF graphics device
pl # make a plot
dev.off() # Turn off the graphics device (this closes the file)
