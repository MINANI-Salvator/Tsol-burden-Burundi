### TAENIA SOLIUM IN Burundi - RESIDUAL LIFE EXPECTANCY
### last update: 18/11/2021

## define ages for which RLE is available
age <- c(0, 1, 5 * 1:19)

## GBD2019 standard life expectancy table
le_gbd <-
  c(88.87, 88.00, 84.03, 79.05, 74.07, 69.11, 64.15, 59.20,
  54.25, 49.32, 44.43, 39.63, 34.91, 30.25, 25.68, 21.29,
  17.10, 13.24, 9.99, 7.62, 5.92)

## function to interpolate life expectancy table
rle <-
  function(x, le = c("gbd")) {
    le <- match.arg(le)
    LE <- switch(le,
                 gbd = le_gbd)
    approx(age, LE, x)$y
  }



