#Necesita para correr en Google Cloud
#32 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("ggplot2")


#Aqui comienza el programa
setwd("~/buckets/b1")

### DATASET FE 1
#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/dataset_epic_v951.csv.gz")
# data[, foto_mes := lubridate::ym(as.character(data$foto_mes) ) ]

#ordeno el dataset
setorder( dataset,  foto_mes )

campos_buenos  <-  setdiff(  colnames( dataset),  c("numero_de_cliente","foto_mes","clase_ternaria" ) )
dataset[, (campos_buenos):= lapply(.SD, scale), .SDcols=campos_buenos]

# data = data[sample(nrow(data), 1000), ]


pdf("./work/boxplot_sin_outliers_dataset1.pdf")
for( campo in  campos_buenos ){
  
  print(ggplot(data= dataset, aes(x = foto_mes , y = get(campo), fill = as.factor(clase_ternaria),
                        group = interaction(foto_mes, clase_ternaria)))+
          geom_boxplot(outlier.shape = NA)+
          ylim(-5,5)+
          labs(title= paste("Boxplot de", campo, sep = " ") ,
           subtitle = "Sin outliers",
           y = campo)  ) 
}
dev.off()

pdf("./work/boxplots_dataset1.pdf")
for( campo in  campos_buenos ){
  
  print(ggplot(data= dataset, aes(x = foto_mes , y = get(campo), fill = as.factor(clase_ternaria),
                        group = interaction(foto_mes, clase_ternaria)))+
          geom_boxplot()+
          labs(title= paste("Boxplot de", campo, sep = " ") ,
           subtitle = "",
           y = campo)  ) 
}
dev.off()

############################################

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("ggplot2")


#Aqui comienza el programa
setwd("~/buckets/b1")

### DATASET FE x FI
#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/dataset_epic_vFExFI.csv.gz")
# data[, foto_mes := lubridate::ym(as.character(data$foto_mes) ) ]

#ordeno el dataset
setorder( dataset,  foto_mes )

campos_buenos  <-  setdiff(  colnames( dataset),  c("numero_de_cliente","foto_mes","clase_ternaria" ) )
dataset[, (campos_buenos):= lapply(.SD, scale), .SDcols=campos_buenos]

# data = data[sample(nrow(data), 1000), ]


pdf("./work/boxplot_sin_outliers_datasetFI.pdf")
for( campo in  campos_buenos ){
  
  print(ggplot(data= dataset, aes(x = foto_mes , y = get(campo), fill = as.factor(clase_ternaria),
                                  group = interaction(foto_mes, clase_ternaria)))+
          geom_boxplot(outlier.shape = NA)+
          ylim(-5,5)+
          labs(title= paste("Boxplot de", campo, sep = " ") ,
               subtitle = "Sin outliers",
               y = campo)  ) 
}
dev.off()

pdf("./work/boxplots_datasetFI.pdf")
for( campo in  campos_buenos ){
  
  print(ggplot(data= dataset, aes(x = foto_mes , y = get(campo), fill = as.factor(clase_ternaria),
                                  group = interaction(foto_mes, clase_ternaria)))+
          geom_boxplot()+
          labs(title= paste("Boxplot de", campo, sep = " ") ,
               subtitle = "",
               y = campo)  ) 
}
dev.off()
