library(readr)
hsb <- read_csv("data-raw/hsb2.csv")

devtools::use_data(hsb, overwrite = TRUE)
