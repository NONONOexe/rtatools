dictionary <- read_csv("data-raw/dictionary.csv")
usethis::use_data(dictionary, overwrite = TRUE, internal = TRUE)
