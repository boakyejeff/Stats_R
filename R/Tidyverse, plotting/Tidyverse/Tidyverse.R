

##Join function must have the same colum 
## left join keeps record in left table and then keep match records for right table
## right join keeps record in right table and then keep match records for left table
## Inner join - keeps matching records for both left and right
## Outer joins  - keep all records in both sets of records

## ---- include=FALSE-----------------------------------------------------------------------------------------------------

install.packages("tidyverse", type='win.binary')

library(tidyverse)



## -----------------------------------------------------------------------------------------------------------------------
Table_1 <- data.frame(Name = c("John", "Joe", "Fred"),
                      X1 = c("blue", "red", "yellow"),
                      X2 = c(7, 4, 5))
Table_2 <- data.frame(Name = c("John", "Fred", "Bill"),
                      X3 = c(20, 36, 12),
                      X4 = c("A", "B", "C"))
Table_1
Table_2


## -----------------------------------------------------------------------------------------------------------------------
library(tidyverse)
Table_1 |>
  left_join(Table_2, by = "Name")

Table_1 |>
   left_join(Table_2, by = "Name")
left_join(Table_1, Table_2, by = "Name")
## -----------------------------------------------------------------------------------------------------------------------
Table_1 |>
  right_join(Table_2, by = "Name")


## -----------------------------------------------------------------------------------------------------------------------
Table_1 |>
  inner_join(Table_2, by = "Name")


## -----------------------------------------------------------------------------------------------------------------------
Table_1 |>
  full_join(Table_2, by = "Name")


## -----------------------------------------------------------------------------------------------------------------------
intall.packages()

install.packages("nycflights13")

library(nycflights13)



## -----------------------------------------------------------------------------------------------------------------------
data(weather)
head(weather,3)


## ---- warning=FALSE, message=FALSE--------------------------------------------------------------------------------------
flt_grp <- flights |> 
  group_by(origin, year, month, day) |>
  summarise(mean_delay = mean(dep_delay, na.rm=TRUE) )
head(as.data.frame(flt_grp),3)

weather_grp <- weather |>
  group_by(origin, year, month, day) |>
  summarise(total_precip = sum(precip))
head(as.data.frame(weather_grp),3)


## ---- warning=FALSE, message=FALSE--------------------------------------------------------------------------------------
weather_grp |> 
  mutate(Rained = ifelse(total_precip > 0, "yes", "no")) |>
  left_join(flt_grp,by = c('origin','year','month','day')) |>
  group_by(Rained) |>
  summarise(AverageAverageDelay = mean(mean_delay),
            count = n())


## -----------------------------------------------------------------------------------------------------------------------
LongData <- data.frame(Name = c("John","John","John","Bill","Bill","Bill"),
                       Year = rep( c("Y2018", "Y2019", "Y2020"), times = 2),
                       Commute_Time = c(20, 15, 0, 12, 10, 5))
LongData


## ----echo = TRUE, size="footnotesize"-----------------------------------------------------------------------------------
WideData <-   
  LongData |> 
    pivot_wider(id_cols = Name,
                names_from = Year,
                values_from = Commute_Time) |>
    as.data.frame()
WideData


## ----echo = TRUE, size="footnotesize"-----------------------------------------------------------------------------------
WideData |> 
  pivot_longer(cols = !Name, # transform all but "Name"
               names_to = "Year", 
               values_to = "Commute_Time") |>
  as.data.frame()


## ----echo = TRUE, size="footnotesize"-----------------------------------------------------------------------------------
install.packages("AER")

library(AER)

Grunfeld
data(Grunfeld, package="AER")
Grunfeld |> arrange(year) |> head(5)


## ----echo = TRUE, size="footnotesize"-----------------------------------------------------------------------------------
Grunfeld_wide <- 
  Grunfeld |> 
    filter(year %in% c(1935, 1936)) |>
    pivot_wider(id_cols = firm,
                names_from = year,
                values_from = c("invest","value","capital"))
head(Grunfeld_wide)


## ----echo = TRUE--------------------------------------------------------------------------------------------------------
Grunfeld_longer <- 
  Grunfeld |>
    filter(year %in% c(1935, 1936)) |>
    pivot_longer(cols = c("invest","value","capital"),
                 names_to = "Quantity",
                 values_to = "Moneys")
head(Grunfeld_longer)


## ----echo = TRUE, size="footnotesize"-----------------------------------------------------------------------------------
DataWithDelim <- data.frame(Name = c("John",
                                     "Joe"),
                            Groceries = c("Milk;Eggs;Bread",
                                          "Bananas;Milk;Grapes"))
DataWithDelim


## ----echo = TRUE, size="footnotesize"-----------------------------------------------------------------------------------
DataWithDelim |> separate(col = "Groceries",
                          into = paste0("Groceries",c("_1","_2","_3")),
                          sep = ";")


## -----------------------------------------------------------------------------------------------------------------------
DataWithTime <- data.frame(Name = c("John","Bill","Joe"),
                           Graduated = c("5/19/2018","5/16/2020","1/9/2021"))
DataWithTime


## -----------------------------------------------------------------------------------------------------------------------
install.packages("lubridate")

library(lubridate)
DataWithTime |> 
  # time_col is a Date object after passing through mdy()
  mutate(date_col = mdy(Graduated), #mdy = monthdayyear
         min_time = min(date_col))


## -----------------------------------------------------------------------------------------------------------------------
DataWithTime |> 
  # time_col is a Date object after passing through mdy()
  mutate(date_col = mdy(Graduated), #mdy = monthdayyear
         min_time = min(date_col),
         month_graduated = month(date_col),
         day_of_week_grad = wday(date_col, label = TRUE)) |>
  filter(month_graduated == 5)
