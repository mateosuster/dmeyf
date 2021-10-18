rm( list=ls()) 
gc() 

require("data.table")
require("lightgbm")


#----Modelo a evaluar y donde lo guardo-------------------------------------------


kmodelo <-"1009"
kscript<-"682_epic" #"847_epic"
kgan <- paste0("ganancias/",  kscript, "_E",  kmodelo)
kgan

#----Cargo Dataset---------------------------------------------------------------

#setwd("G:/Mi unidad/Maestria_Data_Science/DM_EyF")
setwd("C:/Archivos/maestria/dmeyf/")

#cargo el dataset
# dataset  <- fread("./datasets/dataset_epic_simple_v010.csv.gz")
#dataset  <- fread( "./datasets/paquete_premium_202009_ext2.csv" )
dataset  <- fread( "./datasets/paquete_premium_202009_ext.csv" )

dataset<-dataset[foto_mes==202009,]

#creo la clase_binaria donde en la misma bolsa estan los BAJA+1 y BAJA+2
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

#control paso anterior
dataset[ , .N, by=clase01]

# load( "./work/E1015/E1015_823_epic.RDATA")
#--------Function Ganancia que incluye particion de datos------------------------

GananciaArbol <- function( semilla, dataset, x, train=0.70) {
  
  #establezco la semilla
  set.seed(semilla)

  #divido en TRAIN/TEST
  train_casos <- caret::createDataPartition(dataset[,get("clase01")], p = 0.7, list = FALSE)


  data_train  <-  dataset[  train_casos, ]
  data_test   <-  dataset[ -train_casos, ]

  
  ##los campos que se van a utilizar
  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01")) 
  
  #dejo los datos en el formato que necesita LightGBM
  d_train <- lgb.Dataset(data= data.matrix(data_train[ , campos_buenos, with=FALSE]),
                                      label= data_train$clase01 )

  
  #genero el modelo con los parametros "params"
  modelo <- lightgbm( data= d_train,
                      params= x) 
  
  
  #Aplico el modelo a los datos de testing

  umbral_prob <- prob_corte 
  prediccion  <- predict( modelo, 
                          data.matrix( data_test[, campos_buenos, with=FALSE ])) #Lo cambio a un formato para predict  
  
  prediccion_train <-predict( modelo, 
                              data.matrix( data_train[, campos_buenos, with=FALSE ]))
  
  #Lo cambio a un formato para predict  
  
  prob_baja  <- as.integer( prediccion > umbral_prob )
 
  ganancia_test <- data_test[ , sum(  (prob_baja>umbral_prob) * ifelse( clase01==1, 48750, -1250) )]
  return( ganancia_test)
  
}

#----Parametros------------------------------------------------------------------------

#hiperparametros modelo a testear

param <- list(objective= "binary",
              metric= "custom",
              # boosting_type = "rf", 
              first_metric_only= TRUE,
               boost_from_average= TRUE,
               feature_pre_filter= FALSE,
               verbosity= -100,
               seed= 999983,
              
              num_iterations= 227,

              force_row_wise= TRUE,
              feature_fraction= 0.582832150636254,
              learning_rate= 0.0276407617343184,
              max_bin= 31,
              num_leaves=678,
              min_data_in_leaf=118,
               lambda_l1= 0,
               lambda_l2= 33,
              min_gain_to_split= 0,
              max_depth= 191,              
              bagging_fraction= 0.552449314777164,
               path_smooth =  0.68547835409546
               
              # bagging_freq=1
               # bagging_seed=310597
               #  
)

prob_corte<-0.0400006286278983 #0.0387528380690692

#----Semillas------------------------------------------------------------------------
#armo un vector de semillas

set.seed(22)
ksemillas <- fread("./dmeyf/src/monday/cache/02_DT_semillas.txt")
ksemillas <-melt(ksemillas)
ksemillas <-sample(ksemillas$value,20, replace=FALSE)

vector_ganancias <- c() #vector donde voy a ir acumulando las ganancias

GananciaArbol( ksemillas[1], dataset, x=param, train=0.70 )

set.seed( ksemillas[1])

# divido en TRAIN/TEST
train_casos <- caret::createDataPartition(dataset[,get("clase01")], p = 0.7, list = FALSE)


data_train  <-  dataset[  train_casos, ]
data_test   <-  dataset[ -train_casos, ]


##los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01"))

#dejo los datos en el formato que necesita LightGBM
d_train <- lgb.Dataset(data= data.matrix(data_train[ , campos_buenos, with=FALSE]),
                       label= data_train$clase01 )


#genero el modelo con los parametros "params"
modelo <- lightgbm( data= d_train,
                    params= param)


#Aplico el modelo a los datos de testing

umbral_prob <- prob_corte
prediccion  <- predict( modelo,
                        data.matrix( data_test[, campos_buenos, with=FALSE ])) #Lo cambio a un formato para predict

prediccion_train <-predict( modelo,
                            data.matrix( data_train[, campos_buenos, with=FALSE ]))

#Lo cambio a un formato para predict

prob_baja  <- as.integer( prediccion > umbral_prob )


ganancia_test <- data_test[ , sum(  (prob_baja>umbral_prob) * ifelse( clase01==1, 48750, -1250) )]
ganancia_test <- data_test[ , sum(  (prediccion>umbral_prob) * ifelse( clase01==1, 48750, -1250) )]



for( semilla in ksemillas)
{
  ganancia <- GananciaArbol( semilla, dataset, x=param, train=0.70 )
  vector_ganancias <- c( vector_ganancias, ganancia)
#  print(semilla, ganancia)
}

#para visualizar en consola
data.table(ksemillas,vector_ganancias)
vector_ganancias
mean(vector_ganancias)

#para grabar en un archivo (indicar carpeta al principio)
fwrite( data.table(ksemillas,vector_ganancias), 
        file= paste0("work/", kgan,".txt"),
        sep=" " )

e1009 = fread("work/ganancias/682_epic_E1009.txt")
e1015 = fread("work/ganancias/823_epic_E1015.txt")
e1031 = fread("work/ganancias/823_epic_E1031.txt") #RF
e1034 = fread("work/ganancias/823_epic_E1034.txt")

ganancias = cbind( "e1009" = e1009$vector_ganancias , "e1015" = e1015$vector_ganancias,
                   "e1031" = e1031$vector_ganancias, "e1034" = e1034$vector_ganancias)
ganancias = melt(ganancias)

require(ggplot2)
ggplot(ganancias, aes(  Var2, value))+
  geom_boxplot()

require(tidyverse)
ganancias %>% 
  group_by(Var2) %>% 
  summarise(mean(value))




