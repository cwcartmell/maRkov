library(dplyr)
library(lubridate)

torn <- read.table("ok_tornado.raw.txt", sep = "\t", fill = TRUE)
torn <- torn[2]
torn <- mutate(torn, date = mdy(V2))
torn <- mutate(torn, year = year(date), day = yday(date))
torn <- distinct(select(torn, year, day))
torn <- mutate(torn, norm_year = year - 1949)
torn_matrix <- matrix(c(0), nrow = 66, ncol = 366)
for (i in 1:length(torn[[1]])) {
  print(torn$norm_year[i])
  print(torn$day[i])
  torn_matrix[torn$norm_year[i], torn$day[i]] <- 1 ;
}

ok_tornado <- as.data.frame(torn_matrix, row.names = as.integer(seq(from = 1950, to = 2015, by = 1)))
colnames(ok_tornado) <- seq(1:366)
