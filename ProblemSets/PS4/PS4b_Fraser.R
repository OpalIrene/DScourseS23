#1. open R in OSCER 

#2. Install spark 

#3.
library(tidyverse)
library(sparklyr)
library(dplyr)
#4.
sc <- spark_connect(master = "local")

#5.
df1 <- as_tibble(iris)
#6. copy to into Spark
df <- copy_to(sc, df1)
#7. verify data types of each
class(df1)
class(df)
#8. 
names(df1)
names(df)
#9.select first 6 rows of Sepal_Length and Species colums of df
df %>% select(Sepal_Length,Species) %>% head(6) %>% print()
#10. 
#Now let’s do another common RDD operation: filter
#(a) List the first 6 rows of all columns of df where Sepal_Length is larger than
#5.5. This can be done by typing 
df %>% filter(Sepal_Length>5.5) %>% head %>% print
#11.
df %>% select(Sepal_Length, Species) %>% filter(Sepal_Length > 5.5) %>% head(6) %>% print()
#12.
df2 <- df %>% 
  group_by(Species) %>% 
  summarize(mean = mean(Sepal_Length), count = n()) %>% 
  head() %>% 
  print()
#13.a
df2 %>% arrange(Species) %>% head() %>% print()
#b.
df2 %>% arrange(Species) %>% head() %>% print()
#7.



