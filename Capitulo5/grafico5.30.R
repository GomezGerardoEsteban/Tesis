
#################################################################################################################################################
# Titulo: Clasificación de sectores según índices de Rasmussen-Hirschman - 2019
# Tema: Utilizando la matriz IO de la economía colombiana para el año 2019 publicada por el DANE
#       en este script se presenta el calculo de los encadenamientos hacia adelante y hacia atras 
#       en una agregación a 59 sectores y se grafican de manera tal que se obtenga una clasificación
#       de los sectores en función de sus encadenamientos con el resto de la economía.
#
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 9 de Julio de 2024                                                                                                                     
# Base de datos: La matriz insumo producto que se utiliza esta disponible en la pagina oficial del DANE, la base
#                editada para su uso en este script se llama **matrix_IO_2019.xlsx**
# 
#################################################################################################################################################

rm(list = ls())

# Paqueteria

library(tidyverse)
library(readxl)
# library(igraph)
library(ioanalysis) # El paquete para hacer el análisis de Matrices I-P
library(ggrepel)
library(ggtext)
library(ggraph)
# library(patchwork)
# library(forecast)
# library(tseries)
# library(fpp2)

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

# Actualizacion de matriz[[2]] con VA, Demana Final y Producción total

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

# Se requiere un bucle que agregue de a una especificación, es decir, va a tomar la matriz original y va a agregar
# c("20", "21"), posteriormente a la matriz transformada le va a aplicar la agregación c("26", "27") y así sucesivamente...

##
for(i in 1:7){
  
  m_io <- m_io %>% agg.sector(sectors = agregados[[i]], newname = nombres[i])
  
}

# m_io$RS_label


# Clasificacion de los sectores -------------------------------------------


linkages_io  <- linkages(m_io,
                         type = "total",
                         normalize = T)




linkages_io <- linkages_io[[1]] %>% as_tibble()

linkages_io <- linkages_io %>% 
  mutate(Sector = m_io$RS_label[,2]) %>% 
  relocate(c(Sector), .before = BL.tot)


# Levantamos base con valores historicos de produccion por sector


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


# Cambiamos los nombres de algunos sectores para que coincidan con los nombres de la matriz IO
pib_sectores <- pib_sectores %>% 
  mutate(Nombre = ifelse(Nombre == "Alojamiento", "Alojamiento y CyB", 
                         ifelse(Nombre == "Ind. Química", "Ind. Quimica",
                                ifelse(Nombre == "Información y Comunicación", "Información y Comunicaciones", Nombre))))


# Pegamos la producción de 2022 para mostrar el peso en la producción de los sectores

linkages_io <- linkages_io %>% 
  left_join(y = pib_sectores %>% 
              select(year, produccion_constante, Nombre) %>% 
              filter(Nombre != "VAB", 
                     Nombre != "imp_sub_produc", 
                     Nombre != "PIB", 
                     year == 2022), 
            by = c("Sector" = "Nombre"))


linkages_io <- linkages_io %>% 
  mutate(proporcion = produccion_constante/sum(produccion_constante))


linkages_io <- linkages_io %>% 
  mutate(categoria = as.factor(case_when(BL.tot > 1 & FL.tot > 1 ~ "Claves",
                                         BL.tot > 1 & FL.tot <= 1 ~ "Impulsores",
                                         BL.tot <= 1 & FL.tot > 1 ~ "Impulsados",
                                         BL.tot <= 1 & FL.tot <= 1 ~ "Independientes")))

linkages_io <- linkages_io %>% 
  mutate(dummyElec = ifelse(Sector == "Electricidad", 1, 0),
         alfa = ifelse(Sector == "Electricidad", 1, 0))

grafico_mio_2019 <- linkages_io %>%
  filter(Sector != "Hogares") %>% 
  ggplot(mapping = aes(x = BL.tot, y = FL.tot)) +
  geom_point(mapping = aes(color = categoria, size = proporcion, shape = as.factor(dummyElec), alpha = as.factor(alfa)),
             show.legend = F) +
  geom_vline(xintercept = 1, linetype = "longdash") +
  geom_hline(yintercept = 1, linetype = "longdash") +
  geom_text_repel(mapping = aes(x = BL.tot, y = FL.tot, label = Sector, color = categoria),
                  size = 2.5,
                  show.legend = F,
                  alpha = 1) +
  scale_color_manual(values = c("#274862","#995052","#d96831","#93a31c")) +
  scale_shape_manual(values = c(16, 17)) +
  scale_alpha_manual(values = c(0.5, 1)) +
  theme_bw() +
  labs(
    title = "Clasificación sectorial según índices de encadenamiento Rasmussen-Hirschmann - 2019",
    subtitle = "<span style='color:#274862;'>Clave</span>, <span style='color:#995052;'>Impulsados</span>, <span style='color:#d96831;'>Impulsores</span> e <span style='color:#93a31c;'>Independientes</span>",
    x = "Encadenamientos hacia atrás",
    y = "Encadenamientos hacia adelante") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_markdown(size = 9, hjust = 0.5),
        axis.title.x = element_text(size = 7, hjust = 0.5),
        axis.title.y = element_text(size = 7, hjust = 0.5),
        plot.caption = element_text(size = 6, hjust = 0.0),
        axis.text.x = element_text(size = 7, hjust = 0.5),
        axis.text.y = element_text(size = 7, hjust = 0.5))


ggsave(plot = grafico_mio_2019, 
       filename = "rmd/resultados/graficos/grafico5.30_io_encadenamientos_2019.png",
       units = "in", 
       width = 6.99, 
       height = 5.45, 
       dpi = 500)
