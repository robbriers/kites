## code to prepare `DATASET` dataset goes here
kames2016 <- read.csv("kames2016.csv", stringsAsFactors = TRUE)
usethis::use_data(kames2016, overwrite = TRUE)
