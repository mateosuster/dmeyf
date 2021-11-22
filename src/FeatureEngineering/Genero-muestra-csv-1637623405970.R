library(data.table)


#para poder usarlo en la PC y en la nube sin tener que cambiar la ruta
#cambiar aqui las rutas en su maquina
switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "M:\\" },   #Windows
         Darwin  = { directory.root  <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root  <-  "~/buckets/b1/" } #Google Cloud
)
#defino la carpeta donde trabajo
setwd( directory.root )




# Funcion para generar csv de muestra al azar de las filas de el csv grande paquete_premium.csv
#La funcion va leyendo el dataset de a pedazos y tomand una muestra de cada pedazo para finalmente unirlos y guardarlo

##Parámetros:
# n.final.filas: Es el numero final de filas que se va a obtener.
# particiones: En cuantos pedazos va a ir leyendo el csv.original. A más particiones se usa menos memoria pero el proceso es mas largo. Si particiones = 1 equivale a leer el dataset entero y tomar una muestra
# filas.totales: numero de filas que tiene el dataset original. Se obtiene con: length(count.fields("datasetsOri/paquete_premium.csv", sep = ",")) sin necesidad de cargar todo el csv en memoria
# csv.original: path al csv original
# csv.final: path a donde se va a guardar el csv sampleado
# seed: seed

genero.csv.azar <- function(n.final.filas, particiones = 5, filas.totales = 7489082, csv.original = "./datasetsOri/paquete_premium.csv", csv.final = "./datasets/muestra_paquete_premium.csv", seed = 420){
  set.seed(seed)
  
  filas.por.dt <- floor(filas.totales/particiones)
  num.filas.seleccionadas.dt <- floor(n.final.filas/particiones)
  secuencia.filas <- floor(seq(0, filas.totales, length.out = particiones + 1))
  secuencia.filas <- secuencia.filas[1:(length(secuencia.filas)-1)]
  
  #Iteraciones para leer dataset grande:
  dt.final <- data.table()
  for(i in secuencia.filas){
    cat("\rProgreso:", paste0(round(100 * (which(i == secuencia.filas) - 1)/length(secuencia.filas), 2), "%"))
        dt.it <- fread(csv.original, skip = i, nrows = filas.por.dt)
        
        if(i == secuencia.filas[1]){
          nombres.variables <- names(dt.it)
        }else{
          names(dt.it) <- nombres.variables
        }
        
        muestra <- sample(x = 1:filas.por.dt, size = num.filas.seleccionadas.dt)
        dt.it <- dt.it[muestra,]
        dt.final <- rbind(dt.final, dt.it)
  }
  
  cat("\nArchivo creado en", csv.final, "con dimension", dim(dt.final), "\n")
}

genero.csv.azar(n.final.filas = 500000)