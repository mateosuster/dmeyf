rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("ranger")
require("randomForest")  #solo se usa para imputar nulos

#Aqui se debe poner la carpeta de la computadora local
setwd("C:/Archivos/dmeyf/")  #Establezco el Working Directory

#cargo los datos donde entreno
dtrain  <- fread("./datasetsOri/paquete_premium_202009.csv", stringsAsFactors= TRUE)
dtest <- fread( "./datasetsOri/paquete_premium_202011.csv", stringsAsFactors= TRUE )


# dtrain[ , clase_binaria := as.factor(ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" )) ] # Binaria 1
# dtrain[ , clase_binaria := as.factor(ifelse( clase_ternaria=="CONTINUA",  "NEG", "POS" )) ] # Binaria 2
# dtrain[ , clase_ternaria := NULL ]  #elimino la clase_ternaria, ya no la necesito

#eliminar features con data drifting

del_cols = c("internet", "mactivos_margen", 
  "mcajeros_propios_descuentos", "tmobile_app", "cmobile_app_trx",
  "mtarjeta_visa_descuentos", "mtarjeta_master_descuentos")

#Quito el Data Drifting de  "ccajas_transacciones"  "Master_mpagominimo"
#revisar mactivos_margen, tpaquete1, Master_Fininiciomora, antes de eliminarla. matm_other esta fea
campos_buenos  <- setdiff( 
                # colnames(dtrain),
                colnames(dataset),
                           c("internet", "mactivos_margen", 
                             "mcajeros_propios_descuentos", "tmobile_app", "cmobile_app_trx",
                             "mtarjeta_visa_descuentos", "mtarjeta_master_descuentos",
                             "Master_mpagominimo", "matm_other"
                            , "clase_ternaria", "clase01",
                             ) )

# dtrain[, campos_buenos, with =FALSE ] # Para seleccionar columnas


## Densidades de los campos seleccionados
for (campo in campos_buenos){

  distA  <- quantile(  dtrain[ , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  distB  <- quantile(  dtest[ , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  
  a1  <- pmin( distA[[1]], distB[[1]] )
  a2  <- pmax( distA[[2]], distB[[2]] )
  
  densidadA  <- density( dtrain[ , get(campo) ] , kernel="gaussian", na.rm=TRUE)
  densidadB  <- density( dtest[ , get(campo) ] , kernel="gaussian", na.rm=TRUE)

  plot(densidadA, 
       col="blue",
       xlim= c( a1, a2),
       main= paste0("Densidades    ",  campo), )
  
  lines(densidadB, col="red", lty=2)  
}



## feature engieniering
# dtrain[,  `:=` ("status_all" = rowMeans(.SD, na.rm= T)), .SDcols =c("Visa_status","Master_status")   ]
dtrain[,   `:=` (status_all =   Visa_status + Master_status)  ]

#completo NA's de algunas variables
dtrain[,   `:=` (status_all =   ifelse(is.na(status_all), Visa_status, status_all) )  ]
dtrain[,   `:=` (status_all =   ifelse(is.na(status_all), Master_status,status_all) )  ]

hist(dtrain$status_all)
