## code to prepare datasets
kames2017 <- read.csv("kames2017.csv", stringsAsFactors = TRUE)
usethis::use_data(kames2017, overwrite = TRUE)
kames2016 <- read.csv("kames2016.csv", stringsAsFactors = TRUE)
usethis::use_data(kames2016, overwrite = TRUE)
