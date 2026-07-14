# AÑADIR SUBTOTALES
library(tidyverse)

df <- tribble(
  ~x, ~y,
  "a", 58349353,		
  "b", 1796103,		
  "c", 5388751,		
  "d", 33981588
)

df <- df %>% mutate(prop = y / sum(y))

df %>% 
  add_row(
    x = "total",
    y = sum(df$y),
    prop = sum(df$prop)
  )

