#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd("C:/Archivos/maestria/dmeyf/")

datasetA  <- fread( "./datasetsOri/paquete_premium_202009.csv" )
datasetB  <- fread( "./datasetsOri/paquete_premium_202011.csv" )

# FUNCIONES --------------------------------------------------
#funcion densidad
densidades <- function( campo  )
{
  distA  <- quantile(  datasetA[ , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  distB  <- quantile(  datasetB[ , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  
  a1  <- pmin( distA[[1]], distB[[1]] )
  a2  <- pmax( distA[[2]], distB[[2]] )
  
  densidadA  <- density( datasetA[ , get(campo) ] , kernel="gaussian", na.rm=TRUE)
  densidadB  <- density( datasetB[ , get(campo) ] , kernel="gaussian", na.rm=TRUE)
  
  plot(densidadA, 
       col="blue",
       xlim= c( a1, a2),
       main= paste0("Densidades    ",  campo), )
  
  lines(densidadB, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("A", "B"),
           col=c("blue", "red"), lty=c(1,2))
  
}

# funcion histograma
densidades_g <- function( campo , den = T )
{
  sep = data.frame(var =  datasetA[, get(campo)] )
  sep$mes = "sep"
  nov =  data.frame(var =  datasetB[, get(campo)] )
  nov$mes = "nov"
  
  combo <- rbind(sep, nov)
  
  if( den == T){
    ggplot(combo, aes(var, fill = mes))+geom_density(alpha = 0.2)+ labs(title = campo)
  }else{
    ggplot(combo, aes(var, fill = mes))+geom_histogram(alpha = 0.2, position="identity")+ labs(title = campo)
  }
}

# Funcion de summaries
summary_coparado <- function(campo)
{ 
  print("Septiembre \n")
  print(summary(datasetA[, campo ] )  )  
  print("Noviembre \n")
  print(summary(datasetB[, campo ] )  )  
  }
#----------------------------------------------------------------------------------

# matm , tmobile_app, cmobile_app_trx  estan feas


summary_coparado("ccajas_transacciones")

datasetA[ , mv_msaldototal          := mean( c(Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ), by =numero_de_cliente  ]
datasetA$mv_msaldototal

densidades("tpaquete1")
densidades("mcuenta_corriente")
densidades_g("mcuenta_corriente", den =T)


densidades("mcuenta_corriente")
densidades("Visa_Finiciomora")
densidades_g("Master_Finiciomora", den =F)


datasetA[, Master_Finiciomora := Master_Finiciomora+30]
datasetB[, Master_Finiciomora := Master_Finiciomora-30]

datasetA[, Visa_Finiciomora := Visa_Finiciomora+30]
datasetB[, Visa_Finiciomora := Visa_Finiciomora-30]

# funcioncita de summary


campos_malos  <- c( "ccajas_transacciones", "Master_mpagominimo",   #aqui se deben cargar todos los campos culpables del Data Drifting (estas dos ya estaban)
                    "numero_de_cliente", "foto_mes", "cliente_vip", "internet",   "Master_Finiciomora", 
                    "Visa_Finiciomora", "tpaquete1",  "mcuenta_corriente", "mpayroll",      #Agregadas por mi
                    "mtarjeta_master_descuentos", "mforex_buy", "tmobile_app", "cmobile_app_trx", 
                    "Master_madelantopesos", "Master_madelantodolares", "Visa_mconsumodolares"
