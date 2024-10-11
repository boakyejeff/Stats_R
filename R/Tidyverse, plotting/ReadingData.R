#writing code to remove the all unneccessaries, 
#csv means comma separated values ,
#csv values are test files and they can be opened with a text edited to see the comma separated values

#READING CSV
#you need read.csv(location path and filename.csv) function windows can come from C directorate

read.csv("C:/Users/Dell/Downloads/Apple School/Stats 8110/Week 3/my_data.csv")

read.csv("C:\\Users\\Dell\\Downloads\\Apple School\\Stats 8110\\Week 3\\my_data.csv")
#by default read.csv assumes that the first role is the header

first_data <- read.csv("C:\\Users\\Dell\\Downloads\\Apple School\\Stats 8110\\Week 3\\my_data.csv",header = TRUE)
class(first_data)
head(first_data) #shows the first 6 dataset



second_read <- read.table("C:\\Users\\Dell\\Downloads\\Apple School\\Stats 8110\\Week 3\\my_data.txt"
                                                                                      ,header = TRUE
                                                                                      ,sep = "\t")
head(second_read)

install.packages("readxl")

#You can read tables that are not easily formated
#You can skip rows with the skip functions, you can change the decimal function with from . to comma if the country format requires that
#for example in Europe 10.1 is written as 10,1 in data
#this is set with the command (dec = "." / ",") - varies with cultures
#read.table() - reads table - this has the separator as "whitespace" instead of "comma"

##Header preset to TRUE meaning first role is set as header.
### stringAsaFactors is set to FALSE  columns does not convert to factors hence statistical regression will not work


## ----Directory Example, echo=TRUE, eval=FALSE-----------------------------------------------------------------------------------------------------------
## "~/path/to/folder/file.csv"


## ----Directory Example Windows, echo=TRUE, eval=FALSE---------------------------------------------------------------------------------------------------
## "C:/path/to/folder/file.csv"


## ----Directory Example Windows 2, echo=TRUE, eval=FALSE-----------------
--------------------------------------------------------------------------------
## "C:\\path\\to\\folder\\file.csv"


## ----read.csv Example 1, echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------
read_data <- read.csv("~/STAT8110/my_data.csv")
class(read_data)
head(read_data)


## ----TabDelimited, echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------
read_data <- read.table("~/STAT8110/my_data.txt",
                        sep = "\t",
                        header = TRUE)
head(read_data)


## ----Weird Data, echo=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
read_data <- read.table("~/STAT8110/my_data_weirdsep.txt",
                      sep = "~",
                      skip = 2,
                      header = TRUE)
head(read_data)


## ----Reading Excel files - Package installation, echo=TRUE, eval=FALSE----------------------------------------------------------------------------------
## install.packages("readxl")


## ----Excel Sheets, echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------
library(readxl)
excel_sheets("~/STAT8110/An_Excel_File.xlsx")


## ----Reading Excel Sheets, echo=TRUE, eval=TRUE,message=FALSE-------------------------------------------------------------------------------------------
read_data<-data.frame(read_excel("~/STAT8110/An_Excel_File.xlsx",
                                 sheet = 1, col_names = TRUE),
                      read_excel("~/STAT8110/An_Excel_File.xlsx",
                                 sheet = "y", col_names = TRUE))
names(read_data) <- excel_sheets("~/STAT8110/An_Excel_File.xlsx")
head(read_data)


## ----Writing Files, echo=TRUE, eval=TRUE----------------------------------------------------------------------------------------------------------------
write.csv(x = read_data,
          file = "~/STAT8110/written_file.csv",
          row.names = FALSE)


## ----dotvsunderscore, echo=TRUE, eval=TRUE, cache = TRUE------------------------------------------------------------------------------------------------
some_data <- matrix( 1:(1e7*10), nrow = 1e7, ncol = 10)
write.csv(some_data, file = "some_largeish_csv_file.csv", row.names=FALSE)

t1 <- system.time(read.csv("some_largeish_csv_file.csv"))[3]
t2 <- system.time(as.data.frame(
                    read.csv("some_largeish_csv_file.csv", 
                             colClasses = rep("numeric", 10))))[3]
t3 <- system.time(readr::read_csv("some_largeish_csv_file.csv", show_col_types = FALSE))[3]
c(base_t = t1, base_w_specify_t = t2, readr_t = t3)
unlink("some_largeish_csv_file.csv") # delete file


## ---- echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------------------
read.csv("http://www.johnsnyder.org/my_data.csv") |> head(3)


## ---- echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------------------
library(rvest)
library(xml2)
WeatherUrl <- 
  "https://weather.com/weather/today/l/7bf541a12b0b8ae09b13bfa84cd3a728e03f09f11e5991bdc7d55016fe9e1583"
Weather <- read_html(WeatherUrl)
Weather


## ---- echo=TRUE, eval=TRUE,cache=TRUE-------------------------------------------------------------------------------------------------------------------
Weather |>
  html_node("body") |>
  # Search for a span that contains the class we found
  xml_find_all("//span[contains(@class, 'TodayDetailsCard--feelsLikeTempValue')]") |>
  # Extract the text from this element
  html_text() |>
  # Get rid of the "degrees" sign
  gsub(pattern = "°", replacement = "") |>
  as.numeric()

