
######################################################################################################
# Titulo: Establecimiento de alcances de umbral
# Descripción: En este script se calcula el momento a partir del cual se alcanzaria el umbral del 107%
#              con la estructura económica del país segun la matriz IP de 2019.
#              De manera puntual se responden dos preguntas:
##              - ¿Cuál debería ser la tasa de crecimiento anual equivalente para 
#               que la economía colombiana utilice la totalidad de la capacidad instalada en generación 
#               eléctrica en 2032?
#               - ¿Para los distintos escenarios de crecimiento (1%, 3% y 6%) 
#               en que año se utilizaría la totalidad de la capacidad instalada en generación eléctrica?
#
#              Ambas preguntas se contestan utilizando el modelo de oferta abierto y cerrado con respecto
#              a los hogares.
#               
######################################################################################################

rm(list = ls())

# Paqueteria

library(tidyverse)
library(readxl)
library(igraph)
library(ioanalysis) # El paquete para hacer el análisis de Matrices I-P
library(ggrepel)
library(ggtext)
library(ggraph)
library(patchwork)
library(forecast)
library(tseries)
library(fpp2)
library(ggridges)

matriz <- list() # Generamos una lista vacia que va a contener las matrices

# Levantamos matriz
# Hoja1: consumos intersectoriales
# Hoja2: demanda final, producto total y valor agregado

for(i in 1:2){
  matriz[[i]] <- read_excel(path = "rmd/bases/maticesIP/matrix_IO_2019.xlsx", sheet = i)
}

# Levantamos indice de precios para actualizar todos los valores a 2022

ipc <- read.csv("rmd/bases/maticesIP/1.2.5.IPC_Serie_variaciones (1).csv")

names(ipc)[3:5] <- c("variacionAnual", "variacionMensual", "variacionAñoCorrido")

ipc <- ipc %>% 
  mutate(across(.cols = 2:5, .fns = ~{as.numeric(str_replace_all(.x, "\\,", "\\."))}))

ipc <- ipc %>% 
  mutate(year = str_sub(Mes.Año, start = 1, end = 4),
         month = str_sub(Mes.Año, start = 5, end = 6),
         year = as.numeric(year),
         month = as.numeric(month)) %>% 
  filter(month == 12)

referencia_22 <- ipc$Indice[2]
referencia_19 <- ipc$Indice[5]  

# Actualizacion de consumos intermedios

matriz[[1]] <- matriz[[1]] %>% 
  mutate(across(.cols = 2:length(matriz[[1]]), .fns = ~{.x*referencia_22/referencia_19}))

# Actualizacion de matriz[[2]]

matriz[[2]] <- matriz[[2]] %>% 
  mutate(across(.cols = 4:(length(matriz[[2]])-2), .fns = ~{.x*referencia_22/referencia_19}))

# Creacion del objeto IO_matrix

m_io <- as.inputoutput(as.matrix(matriz[[1]][,c(2:length(matriz[[1]]))]),
                       X = as.matrix(matriz[[2]][,4]),
                       RS_label = matrix(c(rep("COL",(length(matriz[[1]])-1)), 
                                           as.matrix(matriz[[2]][,3])), 
                                         ncol = 2),
                       f = as.matrix(matriz[[2]][,6:8]),
                       f_label = t(matrix(c(rep("COL", 3),
                                            "Consumo", "FBKF", "Export_Netas"), 
                                          ncol = 2)),
                       V = t(as.matrix(matriz[[2]][,10:14])),
                       V_label = matrix(c("Remun_asalariados",
                                          "Impuestos_subvenciones",
                                          "Ingreso_mixto",
                                          "Exced_Bruto_Explcacion",
                                          "Impuestos_subvenciones_productos"), ncol = 1))

# Agregación de sectores en matriz ----------------------------------------

# Es necesario agregar algunos sectores para poder hacer la comparación con los agregados macroeconómicos 
# de series de tiempo

# Ubicación de sectores a agregar

##
agregados <- list(c(m_io$RS_label[20,2],m_io$RS_label[21,2]),
                  c(m_io$RS_label[26,2],m_io$RS_label[27,2]),
                  c(m_io$RS_label[39,2],m_io$RS_label[40,2],m_io$RS_label[42,2]),
                  c(m_io$RS_label[53,2],m_io$RS_label[54,2]),
                  c(m_io$RS_label[55,2],m_io$RS_label[56,2]),
                  c(m_io$RS_label[57,2],m_io$RS_label[58,2],m_io$RS_label[59,2]),
                  c(m_io$RS_label[66,2],m_io$RS_label[67,2]))

# Nombres a colocar una vez agregados

##
nombres <- c("Textiles",
             "Ind. Quimica",
             "Aguas residuales",
             "Alojamiento y CyB",
             "Información y Comunicaciones",
             "Interm. Financiera",
             "Arte, recreación y otros serv.")

# Se requiere un bucle que agregue de a una especificación, es decir, va a tomar la matri original y va a agregar
# c("20", "21"), posteriormente a la matriz transformada le va a aplicar la agregación c("26", "27") y así sucesivamente...

##
for(i in 1:7){
  
  m_io <- m_io %>% agg.sector(sectors = agregados[[i]], newname = nombres[i])
  
}

# Sabemos que al 0.06 no alcanza, hagamos una secuencia del 0.06 hasta el 0.15 y veamos si 
# en ese intervalo se alcanza el umbral.

tasas <- seq(0.06, 0.20, 0.01)

## Definición de modelo de Oferta

modeloOferta_IO <- function(x){
  
  cambio_x <- t(m_io$G) %*% as.matrix(x)
  
  estimado_io <- tibble(sector = m_io$RS_label[,2],
                        producto_2019 = m_io$X,
                        var_producto = cambio_x,
                        var_porc = (var_producto + producto_2019)/producto_2019 - 1)
  return(estimado_io)
  
}

### PIB

pib_sectores <- read_excel(path = "rmd/bases/maticesIP/agregados_macro_59_sectores.xlsx")

pib_sectores <- pib_sectores %>% 
  gather(key = "year", value = "produccion", 5:length(pib_sectores)) %>% 
  mutate(year = as.numeric(year))

pib_sectores <- pib_sectores %>% 
  left_join(y = ipc %>% 
              select(year, Indice), by = "year")

# Actualizamos sectores segun IPC 2022

pib_sectores <- pib_sectores %>% 
  mutate(produccion_constante = produccion*referencia_22/Indice)


pib_sectores <- pib_sectores %>% 
  mutate(Nombre = ifelse(Nombre == "Alojamiento", "Alojamiento y CyB", 
                         ifelse(Nombre == "Ind. Química", "Ind. Quimica",
                                ifelse(Nombre == "Información y Comunicación", "Información y Comunicaciones", Nombre))))

# Extraemos y almacenamos el valor del PIB, del VA y de los Impuestos, los cuales van
# a ser sacados de la base que se queda con solo los sectores.

pib <- pib_sectores %>% 
  filter(Nombre == "VAB" | Nombre == "PIB" | Nombre == "imp_sub_produc")

# Base con solo sectores

nombres_sectores_matrix <- m_io$RS_label[,2]

pib_sectores <- pib_sectores %>% 
  filter(Nombre != "VAB", Nombre != "PIB", Nombre != "imp_sub_produc") %>%
  mutate(Nombre = factor(Nombre, levels = nombres_sectores_matrix, labels = nombres_sectores_matrix))%>% 
  arrange(Nombre)


# Definición de producción inicial  ---------------------------------------

pib_sectores_2019 <- pib_sectores[pib_sectores$year == 2019, ] 


# Establecimiento de crecimientos para consumir todo en 2032 --------------

# Respuesta a la pregunta ¿Cuál debería ser la tasa de crecimiento anual equivalente para 
# que la economía colombiana utilice la totalidad de la capacidad instalada en generación 
# eléctrica en 2032?

# Definicion de tasas posibles
tasas <- seq(0.06, 0.3, 0.01)

# Aplicación de tasas al vector de producción
aplicacion_tasas <- list()
var_produccion <- list()

for(i in 1:length(tasas)){
  
  aplicacion_tasas[[i]] <- pib_sectores_2019$produccion_constante*(1 + tasas[i])^(2032-2019)
  
  var_produccion[[i]] <- aplicacion_tasas[[i]] - pib_sectores_2019$produccion_constante
  
  var_produccion[[i]][34] <- 0
  
}

# Aplicamos modelioOferta_IO
resultados <- map(.x = var_produccion, .f = ~{
  
  
  p <- modeloOferta_IO(.x)
  
  return(p)
  
})

# Extraemos el crecimiento del sector eléctrico
sector_electrico <- map(.x = resultados, .f = ~{
  
  p <- .x[34, ]
  
  return(p)
  
})

# Convertimos resultados en DataFrame
sector_electrico <- sector_electrico %>% bind_rows()

# Añadimos tasa aplicada para identificar valores
sector_electrico$tasas_eqAn <- tasas






# Teniendo en cuenta que las tasas para alcanzar el umbral del 107% se encuentran entre el
# 12 y 13 por ciento anual, desagregamos las tasas entre esos dos valores para ver con mayor
# detalle cual sería el punto exacto.

# Desagregacion de tasas
tasas_desagregadasUmbral <- seq(0.12,0.13,0.0001)

# Aplicacion de tasas a produccion del 2019
aplicacion_tasas_um <- list()
var_produccion_um <- list()

for(i in 1:length(tasas_desagregadasUmbral)){
  
  aplicacion_tasas_um[[i]] <- pib_sectores_2019$produccion_constante*(1 + tasas_desagregadasUmbral[i])^(2032-2019)
  
  var_produccion_um[[i]] <- aplicacion_tasas_um[[i]] - pib_sectores_2019$produccion_constante
  
  var_produccion_um[[i]][34] <- 0
  
}


# Aplicación de modeloOferta_IO
resultados_um <- map(.x = var_produccion_um, .f = ~{
  
  
  p <- modeloOferta_IO(.x)
  
  return(p)
  
})

# Obtención de resultados para el sector eléctrico
sector_electrico_um <- map(.x = resultados_um, .f = ~{
  
  p <- .x[34, ]
  
  return(p)
  
})

# Convertimos en DataFrame
sector_electrico_um <- sector_electrico_um %>% bind_rows()

# Añadimos variable de tasas aplicadas para identificar valores
sector_electrico_um$tasas_eqAn <- tasas_desagregadasUmbral

View(sector_electrico_um)
# Establecimiento de años que tendrian que pasar para usar la Capacidad Instalada --------

# Respuesta a la pregunta: ¿Para los distintos escenarios de crecimiento (1%, 3% y 6%) 
# en que año se utilizaría la totalidad de la capacidad instalada en generación eléctrica?

# Periodo a considerar
anios <- rep(seq(2032, 2150, 1),3)

# escenarios de crecimiento
t_teoricas <- c(0.01,0.03,0.06)

# Aplicación de anios tasas con map

# Compatibilización de años con tasas
t_teoricas <- sort(rep(t_teoricas, 119))

aplicacion_anios <- map2(.x = anios, .y = t_teoricas, .f = ~{
  
  p <- pib_sectores_2019$produccion_constante*(1 + .y)^(.x - 2019)
  
  m <- p - pib_sectores_2019$produccion_constante
  
  m[34] <- 0
  
  q <- modeloOferta_IO(m)
  
  q <- q[34, ]
  
  return(q)
  
})


aplicacion_anios <- aplicacion_anios %>% bind_rows()

aplicacion_anios$anios <- anios
aplicacion_anios$tasas <- t_teoricas

aplicacion_anios %>% 
  filter(tasas == 0.01 & var_porc >= 1.03) # Nunca maximo alcanzado al 1% anual, 76.6%

max(aplicacion_anios$var_porc[aplicacion_anios$tasas == 0.01])

aplicacion_anios %>% 
  filter(tasas == 0.03 & var_porc >= 1.03) # 2071

aplicacion_anios %>% 
  filter(tasas == 0.06 & var_porc >= 1.0) # 2045


# Mismo analisis con modelo cerrado ----------------------------------------

# Creacion de la matriz cerrada -------------------------------------------

# Hay cuestiones metodológicas importantes que debemos atender para garantizar
# el cumplimiento de las condiciones Hawkin-Simons, en el que todos los elementos
# de la matriz inversa de Leontief son no-negativos.

# Asumiremos la producción de los hogares igual a la sumatoria de la remuneración
# a asalariados.

# Asumiremos la producción de los hogares igual al consumo de los hogares.

# Asumiremos el consumo de los hogares igual a la remuneración total de salarios
# ponderada por la proporción inicial de consumo final en cada sector.

################################################################################
# Definición de producción del sector hogares                                  #
################################################################################

produccion_hogares <- sum(m_io$V[1,])

################################################################################
# Definición de consumo del sector hogares                                     #
################################################################################

consumo_hogares <- tibble(consumo_inicial = m_io$f[,1],
                          proporciones = m_io$f[,1]/sum(m_io$f[,1]))

print(consumo_hogares, n = 60)

consumo_hogares$consumo_actual <- consumo_hogares$proporciones*produccion_hogares

# Inclusion de la fila de salarios

consumosIntermedios <- m_io$Z[c(1:58),c(1:58)]
consumosIntermedios <- rbind(consumosIntermedios, m_io$V[1,-59])

# Inclusión de la columna de consumo hogares

consumosIntermedios <- cbind(consumosIntermedios, as.matrix(consumo_hogares$consumo_actual))

# En la demanda final debe quedar el excedente del consumo final que adjudicamos a los hogares

# 1. Calculamos el excedente

consumo_hogares$consumo_excedente <- consumo_hogares$consumo_inicial - consumo_hogares$consumo_actual

# 2. armamos la matriz que contiene las demandas finales

df <- cbind(as.matrix(consumo_hogares$consumo_excedente), m_io$f[,c(2:3)])

# Generación de matriz de valor agregado

va <- m_io$V[c(2:5),]

# Generación del vector de producción total

produccionTotal <- m_io$X

# cambio en el valor de la producción total de los hogares

produccionTotal[59] <- produccion_hogares

# Crecacion del io_Object

m_io_closed <- as.inputoutput(consumosIntermedios,
                              X = produccionTotal,
                              RS_label = m_io$RS_label,
                              f = df,
                              f_label = t(matrix(c(rep("COL", 3),
                                                   "Consumo_Excedente", "FBKF", "Export_Netas"), 
                                                 ncol = 2)),
                              V = va,
                              V_label = matrix(c("Impuestos_subvenciones",
                                                 "Ingreso_mixto",
                                                 "Exced_Bruto_Explcacion",
                                                 "Impuestos_subvenciones_productos"), ncol = 1))

m_io_closed 

# Definición de modelo cerrado

modeloOferta_IO_closed <- function(x){
  
  cambio_x <- t(m_io_closed$G) %*% as.matrix(x)
  
  estimado_io <- tibble(sector = m_io_closed$RS_label[,2],
                        producto_2019 = m_io_closed$X,
                        var_producto = cambio_x,
                        var_porc = (var_producto + producto_2019)/producto_2019 - 1)
  return(estimado_io)
  
}

# Aplicación de crecimientos en modelo cerrado

tasas <- seq(0.01,0.2,0.01)

# Aplicación de tasas al vector de producción
aplicacion_tasas_closed <- list()
var_produccion_closed <- list()

for(i in 1:length(tasas)){
  
  aplicacion_tasas_closed[[i]] <- colSums(va)*(1 + tasas[i])^(2032-2019)
  
  var_produccion_closed[[i]] <- aplicacion_tasas_closed[[i]] - colSums(va)
  
  var_produccion_closed[[i]][34] <- 0
  
}


# Aplicamos modelioOferta_IO
resultados_closed <- map(.x = var_produccion_closed, .f = ~{
  
  p <- modeloOferta_IO_closed(.x)
  
  return(p)
  
})

# Extraemos el crecimiento del sector eléctrico
sector_electrico_closed <- map(.x = resultados_closed, .f = ~{
  
  p <- .x[34, ]
  
  return(p)
  
})

# Convertimos resultados en DataFrame
sector_electrico_closed <- sector_electrico_closed %>% bind_rows()

# Añadimos tasa aplicada para identificar valores
sector_electrico_closed$tasas_eqAn <- tasas

# Teniendo en cuenta que las tasas para alcanzar el umbral del 107% se encuentran entre el
# 11 y 12 por ciento anual, desagregamos las tasas entre esos dos valores para ver con mayor
# detalle cual sería el punto exacto.

# Desagregacion de tasas
tasas_desagregadasUmbral_cl <- seq(0.11,0.12,0.0001)

# Aplicacion de tasas a produccion del 2019
aplicacion_tasas_um_cl <- list()
var_produccion_um_cl <- list()

for(i in 1:length(tasas_desagregadasUmbral_cl)){
  
  aplicacion_tasas_um_cl[[i]] <- colSums(va)*(1 + tasas_desagregadasUmbral_cl[i])^(2032-2019)
  
  var_produccion_um_cl[[i]] <- aplicacion_tasas_um_cl[[i]] - colSums(va)
  
  var_produccion_um_cl[[i]][34] <- 0
  
}


# Aplicación de modeloOferta_IO
resultados_um_cl <- map(.x = var_produccion_um_cl, .f = ~{
  
  
  p <- modeloOferta_IO_closed(.x)
  
  return(p)
  
})

# Obtención de resultados para el sector eléctrico
sector_electrico_um_cl <- map(.x = resultados_um_cl, .f = ~{
  
  p <- .x[34, ]
  
  return(p)
  
})

# Convertimos en DataFrame
sector_electrico_um_cl <- sector_electrico_um_cl %>% bind_rows()

# Añadimos variable de tasas aplicadas para identificar valores
sector_electrico_um_cl$tasas_eqAn <- tasas_desagregadasUmbral_cl

View(sector_electrico_um_cl)


# Establecimiento de años que tendrian que pasar para usar la Capacidad Instalada --------

# Respuesta a la pregunta: ¿Para los distintos escenarios de crecimiento (1%, 3% y 6%) 
# en que año se utilizaría la totalidad de la capacidad instalada en generación eléctrica?


aplicacion_anios_closed <- map2(.x = anios, .y = t_teoricas, .f = ~{
  
  p <- colSums(va)*(1 + .y)^(.x - 2019)
  
  m <- p - colSums(va)
  
  m[34] <- 0
  
  q <- modeloOferta_IO_closed(m)
  
  q <- q[34, ]
  
  return(q)
  
})


aplicacion_anios_closed <- aplicacion_anios_closed %>% bind_rows()

aplicacion_anios_closed$anios <- anios
aplicacion_anios_closed$tasas <- t_teoricas

aplicacion_anios_closed %>% 
  filter(tasas == 0.01 & var_porc >= 1.03) # Nunca maximo alcanzado al 1% anual, crecimiento del 95.6%

max(aplicacion_anios_closed$var_porc[aplicacion_anios_closed$tasas == 0.01])

aplicacion_anios_closed %>% 
  filter(tasas == 0.03 & var_porc >= 1.03) # 2065

aplicacion_anios_closed %>% 
  filter(tasas == 0.06 & var_porc >= 1.0) # 2042

