
###################################################################################################################################################################
# Titulo: Gráfico 5.37: Histograma del ejercicio de simulación para determinar el crecimiento necesario del sector eléctrico.
# Tema: En este script, a partir de las tasas de crecimiento interanuales observadas en cada uno de los sectores entre 2005 y 2019, se realiza
#       un ejercicio de simulación en el que se asignan de manera aleatoria los crecimientos ente el 2019 y el 2032 en función de las distribuciones
#       de probabilidad de las tasas para cada uno de los sectores.
#       Una vez obtenidos los posibles crecimientos, se aplica el modelo de oferta la matriz IP para ver en cuanto tendria que crecer el sector 
#       eléctrico.
#       
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 9 de Julio de 2024                                                                                                                     
# Base de datos: Para realizar este ejercicio es necesario utilizar el archivo **matrix_IO_2019.xlsx**, **agregados_macro_59_sectores.xlsx** y
#                **1.2.5.IPC_Serie_variaciones (1).csv**
#################################################################################################################################################

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


# Levantamos la base de produccion por sectores ---------------------------

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

# Estimación

estimaciones_crec_io_32_05 <- modeloOferta_IO(tasas_2019_2005[,6])

# View(estimaciones_crec_io_32_05)
print(tasas_2019_2005, n=100)

tasas_2019_2005 <- tasas_2019_2005 %>% 
  left_join(y = estimaciones_crec_io_32_05, by = "sector")

# writexl::write_xlsx(x = tasas_2019_2005, path = "rmd/resultados/tablas/crecimientosectores20052019.xlsx")


# Realizaremos un ejercicio de simulación de tasas de crecimiento, segun las tasas de crecimiento
# observadas anualmente.


# Obtención de distibuciones empiricas para cada sector

distribuciones_empiricas <- map(.x = nombres_sectores_matrix, 
                                .f = ~{
                                  
                                  pib_sectores %>% 
                                    filter(Nombre == .x & year < 2020) %>% 
                                    mutate(tasa_anual = produccion_constante/lag(produccion_constante) - 1) %>% 
                                    filter(!is.na(tasa_anual)) %>% 
                                    select(Nombre, tasa_anual)
                                  
                                })

# Parametros media y varianza, para simular tasas asumiendo normalidad

parametros_distribucion <- distribuciones_empiricas %>% 
  map(.f = ~{
    
    p <- tibble(sector = unique(.x$Nombre),
                media = mean(.x$tasa_anual),
                desvio = sd(.x$tasa_anual))
    
    return(p)
    
  })


# Simulaciones de tasas
set.seed(12)
semillas <- sample(x = 1:1000000, size = 59, replace = F)
estimaciones <- parametros_distribucion %>% 
  map2(.y = semillas, .f = ~{
    
    set.seed(.y)
    p <- rnorm(n = 10000, mean = .x$media[1], sd = .x$desvio[1])
    
    return(p)
    
  })

estimaciones <- estimaciones %>% bind_cols()

names(estimaciones) <- nombres_sectores_matrix

estimaciones <- as.matrix(estimaciones) # Convierto el data.frame con ls simulaciones en matriz,
# para facilitar el código

crecimientos <- list()                  # Almaceno crecimientos
aplicacion_matriz <- list()             # Almaceno las aplicaciones de la matriz
for(i in 1:10000){
  
  crecimientos[[i]] <- pib_sectores %>% 
    select(Nombre, year, produccion_constante) %>% 
    filter(year == 2019) %>% 
    mutate(tasa = estimaciones[i,],
           produccion_2032 = produccion_constante*(1+tasa)^(2032-2019),
           variacion_produccion = produccion_2032 - produccion_constante)
  
  crecimientos[[i]]$variacion_produccion <- ifelse(crecimientos[[i]]$Nombre == "Electricidad", 0, crecimientos[[i]]$variacion_produccion)
  
  
  aplicacion_matriz[[i]] <- modeloOferta_IO(crecimientos[[i]]$variacion_produccion)
  
}


# Obtengo los valores observados para el sector eléctrico

crec_elec <- aplicacion_matriz %>% 
  map(.f = ~{
    
    .x %>% 
      filter(sector == "Electricidad")
    
  })

crec_elec <- crec_elec %>% bind_rows()

crec_elec_intercuartilico <- crec_elec %>%
  filter(var_porc < quantile(crec_elec$var_porc, probs = 0.75) &
           var_porc > quantile(crec_elec$var_porc, probs = 0.25)) 

histograma_simulaciones <- crec_elec_intercuartilico %>%  
  ggplot() +
  geom_histogram(mapping = aes(x = var_porc*100, y = after_stat(density)), 
                 color = "black",
                 fill = "lightgrey") +
  geom_density(mapping = aes(x = var_porc*100), color = "darkred", linewidth = 1) +
  geom_vline(mapping = aes(xintercept = 107), linetype = "dashed", color = "green4", linewidth = 1) +
  geom_vline(mapping = aes(xintercept = 100*mean(var_porc)), linetype = "longdash", color = "orange2", linewidth = 1)+
  geom_vline(mapping = aes(xintercept = 100*median(var_porc)), linetype = "longdash", color = "sandybrown", linewidth = 1)+
  annotate(geom = "text",
           x = c(65,80,113),
           y = c(0.022,0.022,0.022),
           label = c("Mediana", "Media", "Umbral"),
           color = c("sandybrown", "orange2", "green4"),
           size = 4) +
  labs(title = "Resultados del rango intercuartílico del ejercicio de 10,000 simulaciones de crecimiento del sector eléctrico",
       x = "Porcentaje de crecimiento necesario del sector eléctrico según la matriz I-P",
       y = "Densidad") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 11))

histograma_simulaciones

ggsave(filename = "rmd/resultados/graficos/grafico5.37_histograma_simulaciones_crecimiento.png",
       plot = histograma_simulaciones,
       width = 9,
       height = 5,
       dpi = 500)

mean(crec_elec$var_porc)
max(crec_elec$var_porc)
median(crec_elec$var_porc)
quantile(crec_elec$var_porc, probs = seq(0.25,1,0.25))

# t.test para verificar si la media del rango intercuartilico es menor al umbral

t.test(x = crec_elec_intercuartilico$var_porc, mu = 1.07)

crec_elec %>% 
  mutate(dummy = ifelse(var_porc > 1.07, 1, 0)) %>% 
  ungroup() %>% 
  group_by(dummy) %>% 
  tally() %>% 
  mutate(frec = n/sum(n))

crec_elec_intercuartilico %>% 
  mutate(dummy = ifelse(var_porc > 1.07, 1, 0)) %>% 
  ungroup() %>% 
  group_by(dummy) %>% 
  tally() %>% 
  mutate(frec = n/sum(n))
