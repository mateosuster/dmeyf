#Arbol elemental con libreria  rpart
require("data.table")
require("rpart")

#Aqui se debe poner la carpeta de la computadora local
setwd( "C:/Archivos/maestria_cs_datos/Materias/DM_EyF/dmeyf/")  

#Establezco el Working Directory

#cargo los datos de 202009 que es donde voy a ENTRENAR el modelo
dtrain  <- fread(  "../datasetsOri/paquete_premium_202009.csv")

str(dtrain)

#genero el modelo
modelo  <- rpart("clase_ternaria ~ .",
                 data = dtrain,
                 xval=0,
                 cp=        -0.34, 
                 minsplit=  430,
                 minbucket=  152,
                 maxdepth=   8 )

head(dtrain$clase_ternaria)
#aplico al modelo  a los datos de 202011

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("../datasetsOri/paquete_premium_202011.csv")

prediccion  <- predict( modelo, dapply , type = "prob") #aplico el modelo

dim(prediccion)
dim(dapply)

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 0.0268) ]

entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

# entrega  <- dapply[   , c(numero_de_cliente, Predicted) ] #genera salida como numeric

#genero el archivo para Kaggle
fwrite( entrega, file="../kaggle/K101_001.csv", sep="," )


