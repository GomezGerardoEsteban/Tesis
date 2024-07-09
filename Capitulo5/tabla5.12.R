
#################################################################################################################################################
# Titulo: Empleos por tipo de sector según la clasificación de Rasmussen-Hirschman
# Tema: De este script se obtiene la tabla 5.12 en la que se presenta en una agregación a 25 sectores, la proporción de empleos de cada sector
#       el objetivo de esta tabla es dar un mejor diagnostico de la estructura productiva del país, los sectores considerados CLAVE segun sus 
#       encadenamientos son los que presentan menores niveles de empleo.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 9 de Julio de 2024                                                                                                                     #
# Base de datos: Los datos para realizar estos calculos provienen del Departamento Administrativo Nacional de Estadisticas (DANE)
#                el archivo modificado para cargarlo a R se llama **matriz19.xlsx**
#################################################################################################################################################



rm(list = ls())

library(tidyverse)
library(readxl)
library(igraph)
library(ioanalysis)
library(ggrepel)
library(ggtext)


matriz <- list()

# Levantamos la matriz de I-P
# hoja 1, relaciones entre sectores
# hoja 2, producto total por sector

for(i in 1:3){
  matriz[[i]] <- read_excel(path = "rmd/bases/maticesIP/matriz19.xlsx", sheet = i)
}

# generamos un objeto I-P

m_io_19 <- as.inputoutput(as.matrix(matriz[[1]][,c(2:26)]), X = as.matrix(matriz[[2]][,2]), 
                          RS_label = matrix(c(rep("COL", 25),as.matrix(matriz[[3]][,3])), ncol = 2))

# Calculamos los Bakward Linkages y los Forward Linkages

linkages <- linkages(m_io_19, 
                     type = "total", 
                     normalize = T)


eslabonamientos <- linkages[[1]] %>% as_tibble()

eslabonamientos <- eslabonamientos %>% 
  mutate(sec = 1:25)

eslabonamientos <- eslabonamientos %>% 
  left_join(matriz[[3]] %>% select(-1), by = c("sec" = "id"))

eslabonamientos <- eslabonamientos %>% 
  left_join(matriz[[2]], by = c("sec" = "sector"))

eslabonamientos <- eslabonamientos %>% 
  mutate(propPib = TOT/sum(TOT))

eslabonamientos <- eslabonamientos %>% 
  mutate(categoria = as.factor(case_when(BL.tot > 1 & FL.tot > 1 ~ "Claves",
                                         BL.tot > 1 & FL.tot <= 1 ~ "Impulsores",
                                         BL.tot <= 1 & FL.tot > 1 ~ "Impulsados",
                                         BL.tot <= 1 & FL.tot <= 1 ~ "Independientes")))


eslabonamientos$prop_empleos <- eslabonamientos$Empleos/sum(eslabonamientos$Empleos)

eslabonamientos$int_elec <- m_io_19$A[9,]

writexl::write_xlsx(x = eslabonamientos, path = "rmd/resultados/tablas/sectores_25_empleos.xlsx")


eslabonamientos %>% 
  ggplot(mapping = aes(x = BL.tot, y = FL.tot)) +
  geom_point(mapping = aes(color = categoria, size = propPib), 
             show.legend = F,
             alpha = 0.3) +
  geom_vline(xintercept = 1, linetype = "longdash") +
  geom_hline(yintercept = 1, linetype = "longdash") +
  geom_text_repel(mapping = aes(x = BL.tot, y = FL.tot, label = name, color = categoria), 
                  size = 2.5, 
                  show.legend = F,
                  alpha = 1) +
  scale_color_manual(values = c("#274862","#995052","#d96831","#93a31c")) +
  theme_test() +
  labs(title = "Clasificación sectorial según índices de encadenamiento Rasmussen-Hirschmann",
       subtitle = "Sectores <span style='color:#274862;'>Clave</span>, <span style='color:#995052;'>Impulsados</span>, <span style='color:#d96831;'>Impulsores</span> e <span style='color:#93a31c;'>Independientes</span>",
       x = "Encadenamientos hacia atrás",
       y = "Encadenamientos hacia adelante",
       caption = "Fuente: elaboración propia en base a DANE") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_markdown(size = 9, hjust = 0.5),
        axis.title.x = element_text(size = 7, hjust = 0.5),
        axis.title.y = element_text(size = 7, hjust = 0.5),
        plot.caption = element_text(size = 6, hjust = 0.0),
        axis.text.x = element_text(size = 7, hjust = 0.5),
        axis.text.y = element_text(size = 7, hjust = 0.5))










