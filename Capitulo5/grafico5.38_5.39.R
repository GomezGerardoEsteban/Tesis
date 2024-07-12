#################################################################################
# Modelo I-P de oferta, abierto y cerrado con respecto a los hogares                                 #
#                                                                               
# Autor: Gerardo Esteban Gomez Santiago                                         
# Fecha: 25/05/2024                                                             
# Descripción: El objetivo de este script es utilizar la Matriz I-P del         
#              año 2019 de la economía colombiana, para realizar estimaciones   
#              del sector eléctrico teniendo en cuenta el efecto directo,       
#              indirecto e INDUCIDO por el igreso y gasto de los hogares 
#
# Este script contiene la parte empirica mas relevante de la tesis, en esta se muestra que para distintos escenarios de crecimiento 
# en la economia colombiana, es poco probable que se demande la totalidad de la generación que alcanzaría el país con la cantidad de
# proyectos solares y eólicos incriptos y aprobados.
# Es un uso poco ususal de la matriz IP, la presentación de los resultados esta en los grafico de densidad:
# 5.38 y 5.39
#
#################################################################################

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

unique(pib_sectores$Nombre)

pib_sectores <- pib_sectores %>% 
  mutate(Nombre = ifelse(Nombre == "Alojamiento", "Alojamiento y CyB", 
                         ifelse(Nombre == "Ind. Química", "Ind. Quimica",
                                ifelse(Nombre == "Información y Comunicación", "Información y Comunicaciones", Nombre))))

# Extraemos y almacenamos el valor del PIB, del VA y de los Impuestos, los cuales van
# a ser sacados de la base que se queda con solo los sectores.

pib <- pib_sectores %>% 
  filter(Nombre == "VAB" | Nombre == "PIB" | Nombre == "imp_sub_produc")

# calculamos sus tasas de crecimiento equivalente anual entre 2005 y 2019

tasas_2019_2005_pib <- map(.x = unique(pib$Nombre), .f = ~{
  
  p <- pib %>% 
    filter(Nombre == .x & (year == 2005 | year == 2019))
  
  tasa <- (1+(p$produccion_constante[2]/p$produccion_constante[1] - 1))^(1/(2019-2005)) - 1
  
  data <- tibble(sector = p$Nombre[1],
                 produccion_2005 = p$produccion_constante[1],
                 produccion_19 = p$produccion_constante[2],
                 crecimiento = tasa)
  
  return(data)
  
})

tasas_2019_2005_pib <- tasas_2019_2005_pib %>% bind_rows()

# Base con solo sectores

nombres_sectores_matrix <- m_io$RS_label[,2]

pib_sectores <- pib_sectores %>% 
  filter(Nombre != "VAB", Nombre != "PIB", Nombre != "imp_sub_produc") %>%
  mutate(Nombre = factor(Nombre, levels = nombres_sectores_matrix, labels = nombres_sectores_matrix))%>% 
  arrange(Nombre)


# Generación de escenario observado entre 2005 - 2019 ---------------------

pib_sectores_ts <- pib_sectores %>% 
  select(Nombre, year, produccion_constante) %>% 
  spread(key = Nombre, value = produccion_constante)

pib_sectores_ts_ <- data.matrix(pib_sectores_ts[,2:60])
pib_sectores_ts_ <- ts(data = pib_sectores_ts_, start = 2005, frequency = 1)

nombres_sectores <- names(pib_sectores_ts[,2:60])
                      
# Caclulo de las tasas de crecimiento anual que los diferentes sectores económicos registraron
# entre 2005 y 2019

####################################

tasas_2019_2005 <- map(.x = nombres_sectores, .f = ~{
  
  p <- pib_sectores %>% 
    filter(Nombre == .x & (year == 2005 | year == 2019))
  
  tasa <- (1+(p$produccion_constante[2]/p$produccion_constante[1] - 1))^(1/(2019-2005)) - 1
  
  data <- tibble(sector = p$Nombre[1],
                 produccion_2005 = p$produccion_constante[1],
                 produccion_19 = p$produccion_constante[2],
                 crecimiento = tasa)
  
  return(data)
  
})

tasas_2019_2005 <- tasas_2019_2005 %>% bind_rows()

# Organización de las bases tasas

tasas_2019_2005 <- tasas_2019_2005 %>% 
  mutate(sector = factor(sector, levels = nombres_sectores_matrix, labels = nombres_sectores_matrix))%>% 
  arrange(sector)

tasas_2019_2005 <- tasas_2019_2005 %>% 
  mutate(estimacion_2032 = produccion_19*(1+crecimiento)^(2032 - 2019))

tasas_2019_2005 <- tasas_2019_2005 %>% 
  mutate(variacion_2032_2019 = estimacion_2032 - produccion_19)

tasas_2019_2005 <- tasas_2019_2005 %>%
  mutate(variacion_2032_2019 = ifelse(sector == "Electricidad", 0, variacion_2032_2019))

# Aplicación del modelo de oferta usando las tasas observadas


# Definición del Modelo oferta IO

modeloOferta_IO <- function(x){
  
  cambio_x <- t(m_io$G) %*% as.matrix(x)
  
  estimado_io <- tibble(sector = m_io$RS_label[,2],
                        producto_2019 = m_io$X,
                        var_producto = cambio_x,
                        var_porc = (var_producto + producto_2019)/producto_2019 - 1)
  return(estimado_io)
  
}

# Escenario 3

# Verificacion de compatibilidad de bases para pegar datos

unique(tasas_2019_2005$sector) %in% m_io$RS_label[,2]

matriz[[2]] <- matriz[[2]] %>% 
  mutate(dum = ifelse(Nombre %in% unique(tasas_2019_2005$sector), 1, 0))

matriz[[2]] %>% 
  filter(dum == 0) %>% 
  select(Nombre, agregaciones12)

# Pegado de agregación de sectores a 12 niveles 

tasas_2019_2005 <- tasas_2019_2005 %>% 
  left_join(y = matriz[[2]] %>% 
              select(Nombre, agregaciones25, agregaciones12), by = c("sector" = "Nombre"))

# Corrección por datos perdidos

vector_correct <- c("C", "GHI", "J", "RST")

tasas_2019_2005$agregaciones12 <- ifelse(tasas_2019_2005$sector == "Ind. Quimica", vector_correct[1],
                                         ifelse(tasas_2019_2005$sector == "Alojamiento y CyB", vector_correct[2],
                                                ifelse(tasas_2019_2005$sector == "Información y Comunicaciones", vector_correct[3],
                                                       ifelse(tasas_2019_2005$sector == "Arte, recreación y otros serv.", vector_correct[4], tasas_2019_2005$agregaciones12))))


# Agregación a tres sectores

vector_agregacion12 <- unique(tasas_2019_2005$agregaciones12)

tasas_2019_2005 <- tasas_2019_2005 %>% 
  mutate(sector_agregacion_basica = case_when(
    agregaciones12 == vector_agregacion12[1] ~ 1,
    agregaciones12 == vector_agregacion12[2] ~ 1,
    agregaciones12 == vector_agregacion12[3] ~ 2,
    agregaciones12 == vector_agregacion12[4] ~ 2,
    agregaciones12 == vector_agregacion12[5] ~ 2,
    agregaciones12 == vector_agregacion12[6] ~ 3,
    agregaciones12 == vector_agregacion12[7] ~ 3,
    agregaciones12 == vector_agregacion12[8] ~ 3,
    agregaciones12 == vector_agregacion12[9] ~ 3,
    agregaciones12 == vector_agregacion12[10] ~ 3,
    agregaciones12 == vector_agregacion12[11] ~ 3,
    agregaciones12 == vector_agregacion12[12] ~ 3
  ))

# Numero de sectores en cada agregación

n_sectores <- tasas_2019_2005 %>% 
  group_by(sector_agregacion_basica) %>% 
  tally() %>% 
  select(n) %>% 
  pull()

# Permutaciones para obtener escenarios posibles

t_teoricas <- c(0.01, 0.03, 0.06)

# Intento 1 de permutaciones

contenedor1 <- list()
contenedor2 <- list()
contenedor3 <- list()

set.seed(12)
semillas_ <- sample(x = 1:10000, size = 1000, replace = F)
for(j in 1:1000){
  
  p <- list()
  q <- list()
  r <- list()
  
  for(i in 1:3){
    
    set.seed(semillas_[j])
    p[[i]] <- rnorm(n = n_sectores[1], mean = t_teoricas[i], sd = 0.01)
    set.seed(semillas_[j])
    q[[i]] <- rnorm(n = n_sectores[2], mean = t_teoricas[i], sd = 0.01)
    set.seed(semillas_[j])
    r[[i]] <- rnorm(n = n_sectores[3], mean = t_teoricas[i], sd = 0.01)
    
  }
  
  contenedor1[[j]] <- p
  contenedor2[[j]] <- q
  contenedor3[[j]] <- r
  
}

# -------------------------------------------------------------------------
# Crear todas las posibles combinaciones de distribución ------------------

posibilidades <- cross_df(.l = list(primarios=   1:3, 
                                    secundarios= 1:3, 
                                    terciarios=  1:3)) %>% as.matrix()

posibilidades


guardado <- list()

for(i in 1:1000){
  
  p <- list()
  
  for(j in 1:27){
    
    p[[j]] <- c(contenedor1[[i]][[posibilidades[j,1]]],contenedor2[[i]][[posibilidades[j,2]]], contenedor3[[i]][[posibilidades[j,3]]])
    
  }
  
  guardado[[i]] <- p
  
}


for(i in 1:1000){
  
  guardado[[i]] <- map(.x = 1:27, .f = ~{
    
    guardado[[i]][[.x]] <- guardado[[i]][[.x]] %>% as.matrix()
    
  })
  
}

aplicacion_matriz_c <- list()             

for(i in 1:1000){
  
  aplicacion_matriz_c[[i]] <- map(.x = 1:27, .f = ~{
    
    p <- pib_sectores %>% 
      select(Nombre, year, produccion_constante) %>% 
      filter(year == 2019) %>% 
      mutate(tasa = guardado[[i]][[.x]],
             produccion_2032 = produccion_constante*(1+tasa)^(2032-2019),
             variacion_produccion = produccion_2032 - produccion_constante)
    
    p$variacion_produccion[34] <- 0
    
    q <- modeloOferta_IO(p$variacion_produccion)
    
    return(q)
  
  })  
  
}

aplicacion_matriz_c <- aplicacion_matriz_c %>% bind_rows()

aplicacion_matriz_c <- aplicacion_matriz_c %>% 
  mutate(indicador_primario = rep(c(rep(1,59), rep(2,59), rep(3,59)), 9000),
       indicador_secundario = rep(c(rep(1,59*3), rep(2,59*3), rep(3,59*3)), 3000),
       indicador_terciario = rep(c(rep(1,59*9), rep(2,59*9), rep(3,59*9)), 1000))


histagramas <- aplicacion_matriz_c[aplicacion_matriz_c$sector == "Electricidad", ]

histagramas$ref <- "open"

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

# Tabla que contiene el VA_Excedente de cada sector

tasas_2019_2005 <- tibble(sector = m_io_closed$RS_label[,2],
                          VA_inicial = colSums(x = m_io_closed$V))

tasas_2019_2005 <- tasas_2019_2005 %>% 
  left_join(y = matriz[[2]] %>% 
              select(Nombre, agregaciones25, agregaciones12), by = c("sector" = "Nombre"))

# Corrección por datos perdidos

vector_correct <- c("C", "GHI", "J", "RST")

tasas_2019_2005$agregaciones12 <- ifelse(tasas_2019_2005$sector == "Ind. Quimica", vector_correct[1],
                                         ifelse(tasas_2019_2005$sector == "Alojamiento y CyB", vector_correct[2],
                                                ifelse(tasas_2019_2005$sector == "Información y Comunicaciones", vector_correct[3],
                                                       ifelse(tasas_2019_2005$sector == "Arte, recreación y otros serv.", vector_correct[4], tasas_2019_2005$agregaciones12))))


# Agregación a tres sectores

vector_agregacion12 <- unique(tasas_2019_2005$agregaciones12)

tasas_2019_2005 <- tasas_2019_2005 %>% 
  mutate(sector_agregacion_basica = case_when(
    agregaciones12 == vector_agregacion12[1] ~ 1,
    agregaciones12 == vector_agregacion12[2] ~ 1,
    agregaciones12 == vector_agregacion12[3] ~ 2,
    agregaciones12 == vector_agregacion12[4] ~ 2,
    agregaciones12 == vector_agregacion12[5] ~ 2,
    agregaciones12 == vector_agregacion12[6] ~ 3,
    agregaciones12 == vector_agregacion12[7] ~ 3,
    agregaciones12 == vector_agregacion12[8] ~ 3,
    agregaciones12 == vector_agregacion12[9] ~ 3,
    agregaciones12 == vector_agregacion12[10] ~ 3,
    agregaciones12 == vector_agregacion12[11] ~ 3,
    agregaciones12 == vector_agregacion12[12] ~ 3
  ))

# Numero de sectores en cada agregación

n_sectores <- tasas_2019_2005 %>% 
  group_by(sector_agregacion_basica) %>% 
  tally() %>% 
  select(n) %>% 
  pull()

# Tasas de crecimiento teóricas

t_teoricas <- c(0.01, 0.03, 0.06)

# Intento 1 de permutaciones

contenedor1 <- list()
contenedor2 <- list()
contenedor3 <- list()

set.seed(12)
semillas_ <- sample(x = 1:10000, size = 1000, replace = F)
for(j in 1:1000){
  
  p <- list()
  q <- list()
  r <- list()
  
  for(i in 1:3){
    
    set.seed(semillas_[j])
    p[[i]] <- rnorm(n = n_sectores[1], mean = t_teoricas[i], sd = 0.01)
    set.seed(semillas_[j])
    q[[i]] <- rnorm(n = n_sectores[2], mean = t_teoricas[i], sd = 0.01)
    set.seed(semillas_[j])
    r[[i]] <- rnorm(n = n_sectores[3], mean = t_teoricas[i], sd = 0.01)
    
  }
  
  contenedor1[[j]] <- p
  contenedor2[[j]] <- q
  contenedor3[[j]] <- r
  
}

# -------------------------------------------------------------------------
# Crear todas las posibles combinaciones de distribución ------------------

posibilidades <- cross_df(.l = list(primarios=   1:3, 
                                    secundarios= 1:3, 
                                    terciarios=  1:3)) %>% as.matrix()

posibilidades


guardado <- list()

for(i in 1:1000){
  
  p <- list()
  
  for(j in 1:27){
    
    p[[j]] <- c(contenedor1[[i]][[posibilidades[j,1]]],contenedor2[[i]][[posibilidades[j,2]]], contenedor3[[i]][[posibilidades[j,3]]])
    
  }
  
  guardado[[i]] <- p
  
}


for(i in 1:1000){
  
  guardado[[i]] <- map(.x = 1:27, .f = ~{
    
    guardado[[i]][[.x]] <- guardado[[i]][[.x]] %>% as.matrix()
    
  })
  
}

modeloOferta_IO_closed <- function(x){
  
  cambio_x <- t(m_io_closed$G) %*% as.matrix(x)
  
  estimado_io <- tibble(sector = m_io_closed$RS_label[,2],
                        producto_2019 = m_io_closed$X,
                        var_producto = cambio_x,
                        var_porc = (var_producto + producto_2019)/producto_2019 - 1)
  return(estimado_io)
  
}



aplicacion_matriz_closed <- list()             

for(i in 1:1000){
  
  aplicacion_matriz_closed[[i]] <- map(.x = 1:27, .f = ~{
    
    p <- tasas_2019_2005 %>%  
      select(sector, VA_inicial) %>% 
      mutate(tasa = guardado[[i]][[.x]],
             produccion_2032 = VA_inicial*(1+tasa)^(2032-2019),
             variacion_produccion = produccion_2032 - VA_inicial)
    
    p$variacion_produccion[34] <- 0
    
    q <- modeloOferta_IO_closed(p$variacion_produccion)
    
    return(q)
    
  })  
  
}

aplicacion_matriz_closed <- aplicacion_matriz_closed %>% bind_rows()

aplicacion_matriz_closed <- aplicacion_matriz_closed %>% 
  mutate(indicador_primario = rep(c(rep(1,59), rep(2,59), rep(3,59)), 9000),
         indicador_secundario = rep(c(rep(1,59*3), rep(2,59*3), rep(3,59*3)), 3000),
         indicador_terciario = rep(c(rep(1,59*9), rep(2,59*9), rep(3,59*9)), 1000))


histagramas_closed <- aplicacion_matriz_closed[aplicacion_matriz_closed$sector=="Electricidad", ]

histagramas_closed$ref <- "close"

histagramas_joint <- rbind(histagramas, histagramas_closed)

histagramas_joint <- histagramas_joint %>% 
  mutate(across(.cols = c("indicador_primario", 
                          "indicador_secundario", 
                          "indicador_terciario"), .fns = ~{case_when(.x == 1 ~ 0.01,
                                                                     .x == 2 ~ 0.03,
                                                                     .x == 3 ~ 0.06)}))

g1 <- histagramas_joint %>% 
  ggplot() +
  geom_density(data = histagramas_joint %>% 
                 filter(ref == "close"),
               mapping = aes(x = var_porc), color = "black", fill = "purple4", alpha = 0.5) +
  geom_density(data = histagramas_joint %>% 
                 filter(ref == "open"),
               mapping = aes(x = var_porc), color = "black", fill = "lightgrey", alpha = 0.8) +
  facet_wrap(~ indicador_primario, ncol = 1, nrow = 3) +
  labs(title = "Sector Primario",
       subtitle = "<span style='color:grey;'>Abierto</span>, <span style='color:purple4;'>Cerrado</span>",
       x = NULL,
       y = "Número de casos") +
  scale_y_continuous(limits = c(0,10.5)) +
  theme(plot.subtitle = element_markdown())


g2 <- histagramas_joint %>% 
  ggplot() +
  geom_density(data = histagramas_joint %>% 
                 filter(ref == "close"),
               mapping = aes(x = var_porc), color = "black", fill = "coral", alpha = 0.5) +
  geom_density(data = histagramas_joint %>% 
                 filter(ref == "open"),
               mapping = aes(x = var_porc), color = "black", fill = "wheat3", alpha = 0.9) +
  facet_wrap(~indicador_secundario, nrow = 3, ncol = 1) +
  labs(title = "Sector Secundario",
       subtitle = "<span style='color:wheat3;'>Abierto</span>, <span style='color:coral;'>Cerrado</span>",
       x = "Crecimiento S.Eléctrico",
       y = NULL) +
  scale_y_continuous(limits = c(0,10.5)) +
  theme(plot.subtitle = element_markdown())

g3 <-  histagramas_joint %>% 
  ggplot() +
  geom_density(data = histagramas_joint %>% 
                 filter(ref == "close"),
               mapping = aes(x = var_porc), color = "black", fill = "olivedrab", alpha = 0.5) +
  geom_density(data = histagramas_joint %>% 
                 filter(ref == "open"),
               mapping = aes(x = var_porc), color = "black", fill = "palegoldenrod", alpha = 0.9) +
  facet_wrap(~indicador_terciario, nrow = 3, ncol = 1)+
  labs(title = "Sector Terciario",
       subtitle = "<span style='color:khaki3;'>Abierto</span>, <span style='color:olivedrab;'>Cerrado</span>",
       x = NULL,
       y = NULL) +
  scale_y_continuous(limits = c(0,10.5)) +
  theme(plot.subtitle = element_markdown())


histogramas_graph_esc_3_closed <- g1 + g2 + g3

ggsave(filename = "rmd/resultados/graficos/grafico5.38_densidades_esc_3.png",
       plot = histogramas_graph_esc_3_closed,
       width = 7.4,
       height = 5.81,
       dpi = 500)


# Analisis según intensidades eléctricas ----------------------------------

tasas_2019_2005$int_elec <- m_io$L[34, ]
tasas_2019_2005$int_elec[34] <- 0

tasas_2019_2005$dummy_elec <- ifelse(tasas_2019_2005$int_elec > mean(m_io$L[34,-c(34,59)]), 1, 0)


baseAna_intElec <- tasas_2019_2005 %>% 
  ungroup() %>% 
  select(sector, dummy_elec) %>% 
  arrange(dummy_elec)


n_sectores_int_elec <- baseAna_intElec %>% 
  group_by(dummy_elec) %>% 
  tally() %>% 
  select(n) %>% 
  pull()

cont_1 <- list()
cont_2 <- list()

set.seed(13)
semillas_1 <- sample(x = 1:10000, size = 1000, replace = F)
for(j in 1:1000){
  
  p <- list()
  q <- list()
  
  for(i in 1:3){
    
    set.seed(semillas_1[j])
    p[[i]] <- rnorm(n = n_sectores_int_elec[1], mean = t_teoricas[i], sd = 0.01)
    set.seed(semillas_1[j])
    q[[i]] <- rnorm(n = n_sectores_int_elec[2], mean = t_teoricas[i], sd = 0.01)
    
  }
  
  cont_1[[j]] <- p
  cont_2[[j]] <- q
  
}

posibilidades_ <- cross_df(.l = list(intenso   =   1:3, 
                                     n_intenso =  1:3)) %>% as.matrix()

guardado_ <- list()

for(i in 1:1000){
  
  p <- list()
  
  for(j in 1:9){
    
    t <- tibble(crec = c(cont_1[[i]][[posibilidades_[j,1]]],cont_2[[i]][[posibilidades_[j,2]]]),
                sector = factor(baseAna_intElec$sector, labels = nombres_sectores_matrix, levels = nombres_sectores))
    
    t <- t %>% 
      arrange(sector)
    
    p[[j]] <- t %>% 
      select(crec) %>% 
      pull()
    
  }
  
  guardado_[[i]] <- p
  
}

for(i in 1:1000){
  
  guardado_[[i]] <- map(.x = 1:9, .f = ~{
    
    guardado_[[i]][[.x]] <- guardado_[[i]][[.x]] %>% as.matrix()
    
  })
  
}


aplicacion_matriz_ce <- list()             # Almaceno las aplicaciones de la matriz

for(i in 1:1000){
  
  aplicacion_matriz_ce[[i]] <- map(.x = 1:9, .f = ~{
    
    p <- pib_sectores %>% 
      select(Nombre, year, produccion_constante) %>% 
      filter(year == 2019) %>% 
      mutate(tasa = guardado_[[i]][[.x]],
             produccion_2032 = produccion_constante*(1+tasa)^(2032-2019),
             variacion_produccion = produccion_2032 - produccion_constante)
    
    p$variacion_produccion[34] <- 0
    
    q <- modeloOferta_IO(p$variacion_produccion)
    
    return(q)
    
  })
  
}

aplicacion_matriz_ce <- aplicacion_matriz_ce %>% bind_rows()

aplicacion_matriz_ce <- aplicacion_matriz_ce %>% 
  mutate(indicador_intenso = rep(c(rep(1,59), rep(2,59), rep(3,59)), 3000),
         indicador_n_intenso = rep(c(rep(1,59*3), rep(2,59*3), rep(3,59*3)), 1000))

histagramas_intE <- aplicacion_matriz_ce[aplicacion_matriz_ce$sector == "Electricidad", ]

histagramas_intE$ref <- "open"

## Calculo intensivos_noIntensivo Modelo Cerrado

tasas_2019_2005$int_elec <- m_io_closed$L[34, ]
tasas_2019_2005$int_elec[34] <- 0

tasas_2019_2005$dummy_elec <- ifelse(tasas_2019_2005$int_elec > mean(m_io_closed$L[34,-c(34)]), 1, 0)


baseAna_intElec <- tasas_2019_2005 %>% 
  ungroup() %>% 
  select(sector, dummy_elec) %>% 
  arrange(dummy_elec)


n_sectores_int_elec <- baseAna_intElec %>% 
  group_by(dummy_elec) %>% 
  tally() %>% 
  select(n) %>% 
  pull()

cont_1 <- list()
cont_2 <- list()

set.seed(13)
semillas_1 <- sample(x = 1:10000, size = 1000, replace = F)
for(j in 1:1000){
  
  p <- list()
  q <- list()
  
  for(i in 1:3){
    
    set.seed(semillas_1[j])
    p[[i]] <- rnorm(n = n_sectores_int_elec[1], mean = t_teoricas[i], sd = 0.01)
    set.seed(semillas_1[j])
    q[[i]] <- rnorm(n = n_sectores_int_elec[2], mean = t_teoricas[i], sd = 0.01)
    
  }
  
  cont_1[[j]] <- p
  cont_2[[j]] <- q
  
}

posibilidades_ <- cross_df(.l = list(intenso   =   1:3, 
                                     n_intenso =  1:3)) %>% as.matrix()

guardado_ <- list()

nombres_sectores_matrix <- m_io_closed$RS_label[,2]

for(i in 1:1000){
  
  p <- list()
  
  for(j in 1:9){
    
    t <- tibble(crec = c(cont_1[[i]][[posibilidades_[j,1]]],cont_2[[i]][[posibilidades_[j,2]]]),
                sector = factor(baseAna_intElec$sector, labels = nombres_sectores_matrix, levels = nombres_sectores_matrix))
    
    t <- t %>% 
      arrange(sector)
    
    p[[j]] <- t %>% 
      select(crec) %>% 
      pull()
    
  }
  
  guardado_[[i]] <- p
  
}

for(i in 1:1000){
  
  guardado_[[i]] <- map(.x = 1:9, .f = ~{
    
    guardado_[[i]][[.x]] <- guardado_[[i]][[.x]] %>% as.matrix()
    
  })
  
}

aplicacion_matriz_ce_closed <- list()             # Almaceno las aplicaciones de la matriz

for(i in 1:1000){
  
  aplicacion_matriz_ce_closed[[i]] <- map(.x = 1:9, .f = ~{
    
    
    p <- tasas_2019_2005 %>%  
      select(sector, VA_inicial) %>% 
      mutate(tasa = guardado_[[i]][[.x]],
             produccion_2032 = VA_inicial*(1+tasa)^(2032-2019),
             variacion_produccion = produccion_2032 - VA_inicial)
    
    p$variacion_produccion[34] <- 0
    
    q <- modeloOferta_IO_closed(p$variacion_produccion)
  
    return(q)
    
  })
    
}

aplicacion_matriz_ce_closed <- aplicacion_matriz_ce_closed %>% bind_rows()

aplicacion_matriz_ce_closed <- aplicacion_matriz_ce_closed %>% 
  mutate(indicador_intenso = rep(c(rep(1,59), rep(2,59), rep(3,59)), 3000),
         indicador_n_intenso = rep(c(rep(1,59*3), rep(2,59*3), rep(3,59*3)), 1000))

histagramas_intE_closed <- aplicacion_matriz_ce_closed[aplicacion_matriz_ce_closed$sector == "Electricidad", ]

histagramas_intE_closed$ref <- "closed"

histagramas_intE_joint <- rbind(histagramas_intE, histagramas_intE_closed)

histagramas_intE_joint <- histagramas_intE_joint %>% 
  mutate(across(.cols = c("indicador_intenso", 
                          "indicador_n_intenso"), .fns = ~{case_when(.x == 1 ~ 0.01,
                                                                     .x == 2 ~ 0.03,
                                                                     .x == 3 ~ 0.06)}))

g1_int <- histagramas_intE_joint %>% 
  ggplot() +
  geom_density(data = histagramas_intE_joint %>% 
                 filter(ref == "closed"),
               mapping = aes(x = var_porc), color = "black", fill = "tomato4", alpha = 0.8) +
  geom_density(data = histagramas_intE_joint %>% 
                 filter(ref == "open"),
               mapping = aes(x = var_porc), color = "black", fill = "wheat4", alpha = 0.8) +
  facet_wrap(~ indicador_intenso, ncol = 1, nrow = 3) +
  labs(title = "Sector Intensivos",
       subtitle = "<span style='color:wheat4;'>Abierto</span>, <span style='color:tomato4;'>Cerrado</span>",
       x = "Crecimiento S.Eléctrico",
       y = "Número de casos") +
  scale_y_continuous(limits = c(0,19)) +
  theme(plot.subtitle = element_markdown())



g2_nint <- histagramas_intE_joint %>% 
  ggplot() +
  geom_density(data = histagramas_intE_joint %>% 
                 filter(ref == "closed"),
               mapping = aes(x = var_porc), color = "black", fill = "sienna4", alpha = 0.8) +
  geom_density(data = histagramas_intE_joint %>% 
                 filter(ref == "open"),
               mapping = aes(x = var_porc), color = "black", fill = "slategrey", alpha = 0.8) +
  facet_wrap(~ indicador_n_intenso, ncol = 1, nrow = 3) +
  labs(title = "Sectores No intensivos",
       subtitle = "<span style='color:slategrey;'>Abierto</span>, <span style='color:sienna4;'>Cerrado</span>",
       x = "Crecimiento S.Eléctrico",
       y = NULL) +
  scale_y_continuous(limits = c(0,19)) +
  theme(plot.subtitle = element_markdown())

histogramas_graph_esc_3_int_closed <- g1_int + g2_nint

ggsave(filename = "rmd/resultados/graficos/grafico5.39_histogramas_esc_3_int.png",
       plot = histogramas_graph_esc_3_int_closed,
       width = 7.4,
       height = 5.81,
       dpi = 500)


