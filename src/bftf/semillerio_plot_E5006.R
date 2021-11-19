
library(tidyverse)
library(ggplot2)

semillerio <-  read_table("C:/Archivos/maestria/dmeyf/work/work_E5009_E5009_981_epic.txt")

semillerio %>% 
  # ggplot(aes(meseta, ganancia, color = as.factor(semilla)))+
  ggplot(aes(meseta, ganancia, group = meseta))+
  geom_boxplot()+
  geom_vline(xintercept = 9298)+
  theme(legend.position = "none")
