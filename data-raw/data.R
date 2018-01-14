# http://r-pkgs.had.co.nz/data.html


x <- sample(1000)
devtools::use_data(x, mtcars, internal = TRUE,  overwrite = TRUE)
devtools::use_data(x, mtcars, internal = FALSE, overwrite = TRUE)
devtools::use_data_raw()
