# RD4S Chapter 10

library(tidyverse)

# As regular data frame
data.frame(iris)

# As tibble
as_tibble(iris)

tibble(
  x = 1:5, 
  y = 1, 
  z = x ^ 2 + y
)

tb <- tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)
tb

# Add data with transposed tibble. tribble() is customised for data entry
# in code: column headings are defined by formulas (i.e. they start with ~), 
#and entries are separated by commas. This makes it possible to lay out small
#amounts of data in easy to read form.

tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)


tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)


# Printing tibbles - set the number of rows

nycflights13::flights %>% 
  print(n = 100, width = Inf)

# ... or use RStudio's data viewer

nycflights13::flights %>% 
  View()

# To return to an old-school data frame

class(as.data.frame(tb))
tb
