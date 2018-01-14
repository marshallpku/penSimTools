# http://r-pkgs.had.co.nz/data.html


sampleData <- sample(1000)
devtools::use_data(sampleData, mtcars, internal = TRUE,  overwrite = TRUE)
devtools::use_data(sampleData, mtcars, internal = FALSE, overwrite = TRUE)
devtools::use_data_raw()
