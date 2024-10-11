## ----my first list--------------------------------------------------------------------------------------------------
                  # name   = literally anything
cool_list <- list(a_matrix = matrix(1:9,3,3),
                  a_vector_of_numbers = 1:4,
                  a_vector_of_strings = c("apple","banana","carrot"),
                  a_logical = TRUE)
cool_list


## ----List Name Manipulation-----------------------------------------------------------------------------------------
names(cool_list)
names(cool_list)[3] <- "some_fruits"
names(cool_list)


## ----Obtaining an element of a list---------------------------------------------------------------------------------
cool_list$a_matrix


## ----Extraction from list elements----------------------------------------------------------------------------------
cool_list$a_matrix[1,1]


## ----Having a list as a list element--------------------------------------------------------------------------------
cool_list$another_list <- list(three_letters = "MIZ",
                               three_more_letters = "ZOU")


## ----An example-----------------------------------------------------------------------------------------------------
cool_list


## ----unlist example-------------------------------------------------------------------------------------------------
small_list <- list(1:3,4:5,6)
small_list
unlist(small_list)


## ----list_storage---------------------------------------------------------------------------------------------------
# Store the numbers from 1 to 1,000,000
x1 <- seq(from = 0, to = 1e6, by = 1) 
lobstr::obj_size(x1) # Memory use of an object
list_from_x1 <- list(x1, x1, x1, x1, x1, x1)


## ----list_storage_size----------------------------------------------------------------------------------------------
lobstr::obj_size(list_from_x1)


## ----First Data Frame-----------------------------------------------------------------------------------------------
my_dataframe <- data.frame(Day = c("Monday", "Tuesday","Wednesday", "Thursday", "Friday"),
                           Breakfast = c("Cereal", "Eggs", NA, "Cereal", "Yogurt"),
                           Hrs_in_meetings = c(3, 5, 2, 1, 4))


## ----Printing my first DF-------------------------------------------------------------------------------------------
my_dataframe


## ----Creating variables---------------------------------------------------------------------------------------------
my_dataframe$Mins_in_meetings <- 60*my_dataframe$Hrs_in_meetings
my_dataframe


## ----Subsetting-----------------------------------------------------------------------------------------------------
my_dataframe[my_dataframe$Mins_in_meetings < 200,]
my_dataframe[my_dataframe$Day != "Friday",]


## ----Removing rows with missing values------------------------------------------------------------------------------
na.omit(my_dataframe)
# Change missing value to empty string
my_dataframe$Breakfast[is.na(my_dataframe$Breakfast)] <- ""
na.omit(my_dataframe)


## ---- eval=FALSE----------------------------------------------------------------------------------------------------
## install.packages("tidyverse")


## -------------------------------------------------------------------------------------------------------------------
library(tidyverse)


## -------------------------------------------------------------------------------------------------------------------
meeting_data <- data.frame(Day = rep( c("Monday", "Tuesday","Wednesday", "Thursday", "Friday"), times = 4 ),
                           Job = rep( c("Old", "New"), each = 2 * 5),
                           Hrs_in_meetings = c(5, 7, 6, 7, 6, 4, 6, 7, 3, 6, 3, 0, 1, 0, 1, 3, 0, 1, 0, 1))


## -------------------------------------------------------------------------------------------------------------------
head(meeting_data, n = 15)


## ---- eval=FALSE, scho = TRUE---------------------------------------------------------------------------------------
## data |>
##   data_operation1(...) |>
##   data_operation2(...) |>
##   data_operation3(...)


## -------------------------------------------------------------------------------------------------------------------
meeting_data |>
  mutate(Mins_in_meetings = 60*Hrs_in_meetings) |>
  select(Day, Job, Mins_in_meetings) |>
  #select(-Hrs_in_meetings) |> # This also works!
  filter(Day %in% c("Monday","Wednesday"))


## -------------------------------------------------------------------------------------------------------------------
meeting_data |>
  group_by(Job) |>
  summarise(avg_hrs = mean(Hrs_in_meetings)) |>
  arrange(avg_hrs)

