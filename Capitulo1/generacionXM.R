#############################################################################################################
# Descripcion: Este documento es un complemento del script 'grafico1.5', utilizando este script, es posible
#              descargar las bases anuales de generacion de electricidad de XM, sin necesidad de entrar a la
#              pagina directamente y tener que descargar las bases manualmente.
#              Es necesario correr este script antes de correr el script 'grafico1.5', si ya se corrio una vez 
#              no es necesario volverlo a correr. Solo aegurarse de que las bases esten almacenadas en el directorio 
#              de trabajo.
# Autor: Gerardo Esteban Gomez-Santiago
# Fecha: 01 de Julio del 2024
##############################################################################################################

rm(list = ls())

library(tidyverse)

years <- c(2006:2015, 2019:2023)
years_sem <- sort(rep(2016:2018, 2))
sem <- rep(c("SEM1", "SEM2"), 3)

getOption("timeout")

options(timeout = "1000")

# First statement

generacion <- c()

for(i in 1:length(years)){
  
  if(years[i] == 2015){
  
    generacion[i] <- str_c("https://sinergox.xm.com.co/oferta/Histricos/Generaci%C3%B3n/Generacion_(kWh)_", years[i], ".xls", sep = "")
  
  } else {
    
    if(years[i] == 2023){
      
      generacion[i] <- str_c("https://sinergox.xm.com.co/oferta/Histricos/Generacion_(kWh)_", years[i], ".xlsx", sep = "")  
      
      
    }else{
      
      generacion[i] <- str_c("https://sinergox.xm.com.co/oferta/Histricos/Generaci%C3%B3n/Generacion_(kWh)_", years[i], ".xlsx", sep = "")
      
      }
    
  }
  
}

generacion_ <- c()

for(i in 1:length(years_sem)){
  
  generacion_[i] <- str_c("https://sinergox.xm.com.co/oferta/Histricos/Generaci%C3%B3n/Generacion_(kWh)_", years_sem[i], sem[i],".xlsx", sep = "")
  
}


url <- c(generacion, generacion_)
filename <- c()

for(i in 1:length(url)){
  
  filename[i] <- basename(url[i])
  
  download.file(url = url[i],
                destfile = filename[i], # Puedes crear una carpeta en tu directorio que conserve estas bases con 'str_c("nombreCarpetaNueva/", filename[i])' en lugar de solamente 'filename[i]'
                mode = "wb")
  
}


