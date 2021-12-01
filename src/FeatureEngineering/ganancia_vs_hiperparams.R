

library(tidyverse)
library(ggplot2)
library(readr)

data <- read_delim("C:/Archivos/maestria/dmeyf/work/E5017/work_E5017_E5017_1421_lgbm_maxbin_BOlog.txt", delim = "\t") 

data <- read_delim("C:/Archivos/maestria/dmeyf/work/E2020/work_E5020_E5020_1421_lgbm_maxbin_rankeo_BOlog.txt", delim = "\t") 

glimpse(data )

params = c( "max_bin",  "learning_rate", "feature_fraction",
            "lambda_l1", "lambda_l2",  "min_gain_to_split",
            "bagging_fraction", "path_smooth"
            ,"gleaf_size", "gnum_leaves"
            )

data %>% 
  select( params, gan_test_acum  ) %>%
  # select( params, ganancia  ) %>% 
  pivot_longer(cols = params) %>% 
  ggplot(aes(gan_test_acum, value , color = name)) +
  # ggplot(aes(ganancia, value , color = name)) +
  geom_point()+
  facet_wrap(~name, scales = "free", ncol=5)+
  scale_x_continuous(labels = scales::unit_format(unit = "", scale = 1e-6))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = "none")+
  labs(title = "Hiperparámetros vs Ganancia en testing",
       subtitle = "Modelo semillerío de Light GBM", y= "",
       x = "Millones de pesos")
ggsave("C:/Archivos/maestria/dmeyf/gcia_vs_hiper_E2020.png")

for (i in params){
 print( data %>% 
    select( i, gan_test_acum  ) %>%
    # select( params, ganancia  ) %>% 
    ggplot(aes(gan_test_acum, get(i) )) +
    # ggplot(aes(ganancia, value , color = name)) +
    geom_point()+
    scale_x_continuous(labels = scales::unit_format(unit = "MM", scale = 1e-6))+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "bottom")+
    labs(title = paste(i, "vs Ganancia en testing"),
         subtitle = "Modelo semillerío de Light GBM",
         x = "Millones de pesos")
 )
}



#########################
#modelos simples
data <- read_delim("C:/Archivos/maestria/dmeyf/work/work_E5006_E5006_962_epic_BOlog.txt", delim = "\t") 

data <- read_delim("C:/Archivos/maestria/dmeyf/work/work_E5016_E5016_962_epic_BOlog.txt", delim = "\t") 
data <- read_delim("C:/Archivos/maestria/dmeyf/work/work_E5018_E5018_962_epic_maxbin_BOlog.txt", delim = "\t") 
data <- read_delim("C:/Archivos/maestria/dmeyf/work/work_E5015_E5015_962_epic_BOlog.txt", delim = "\t") 


params = c( "max_bin",  "learning_rate", "feature_fraction",
            "lambda_l1", "lambda_l2",  "min_gain_to_split",
            "bagging_fraction", "path_smooth")


data %>% 
  select( params, ganancia  ) %>%
  pivot_longer(cols = params) %>% 
  ggplot(aes(ganancia, value , color = name)) +
  geom_point()+
  facet_wrap(~name, scales = "free", ncol=5)+
  scale_x_continuous(labels = scales::unit_format(unit = "MM", scale = 1e-6))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = "none")+
  labs(title = "Hiperparámetros vs Ganancia en testing",
       subtitle = "Modelo Light GBM con FE",
       x = "Millones de pesos")

############### BO logs gcias 142x


