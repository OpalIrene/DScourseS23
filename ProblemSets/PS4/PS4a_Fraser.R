library(jsonlite)
library(tidyverse)
library(dplyr)
#5. #dont forget to exit linux!
system('linux shell command')
# Read the JSON data from the file
#a.
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20230209&lang=en"')
#b. 
system('cat dates.json')
#c.
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])
#d.
class(mydf)
class(mydf$date)
#> class(mydf)
#[1] "tbl_df"     "tbl"        "data.frame"
#> class(mydf$date)
#[1] "character"
#e.
head(mydf, n=10)

setwd("/Desktop/ECON 5253/DScourseS23/ProblemSets/PS4/")
source("Script.R") 

