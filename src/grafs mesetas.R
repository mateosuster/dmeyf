#graf meseta
library(tidyverse)
library(ggplot2)

x = seq(11000, 14000, 500)
gcia_E5019 = c(21.64271, 21.65938, 21.67604, 22.14273, 21.82605, 21.67604, 21.78855)

data = data.frame(x, gcia_E5019)

ggplot(data, aes(x,gcia_E5019))+geom_line()
