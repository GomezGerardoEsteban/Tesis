
#################################################################################################################################################
# Titulo: Evolución de la población urbana y rural de Colombia 1950 - 2019
# Tema: Este script utiliza la información oficial del Dane (retroproyecciones de población urbana y rural) para mostrar como fue el proceso de urbanización 
#       en Colombia en terminos absolutos (millones de habitantes) y en terminos porcentuales (porcentaje de población urbana respecto a la población total).
#
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 8 de Julio de 2024                                                                                                                     
# Base de datos: La base disponible en el DANE se llama **DCD-area-proypoblacion-Nac-1950-2019.xlsx**
#                acáse se muestra como manipularla para generar el gráfico 2.20
# 
#################################################################################################################################################

rm(list = ls())

library(tidyverse)
library(readxl)

base <- read_excel(path = "rmd/bases/DCD-area-proypoblacion-Nac-1950-2019.xlsx", range = "A12:E222")

nombres <- names(base)

nombres <- str_to_lower(nombres)
nombres <- str_replace_all(string = nombres, pattern = " ", replacement = "_")
nombres <- str_replace_all(string = nombres, pattern = "á", replacement = "a")
nombres <- str_replace_all(string = nombres, pattern = "ó", replacement = "o")

names(base) <- nombres

base_tasas <- base[base$año %in% c(1950, 2019), ]

base_tasas <- base_tasas %>% 
  spread(key = area_geografica, value = poblacion)

tasa <- c()
for(i in 1:3){
  
  p <- base_tasas[,4:6]
  
  tasa[i] <- (1 + (p[2,i]/p[1,i] - 1))^(1/(2019-1950)) - 1

}

tasa <- tasa %>% unlist()

b_tasas <- tibble(nombres = str_replace_all(names(p), pattern = " ", "_"),
                  tasas = tasa)


base <- base %>% 
  filter(area_geografica != "Total") %>% 
  group_by(año) %>% 
  mutate(total = sum(poblacion)) %>% 
  ungroup() %>% 
  mutate(porcentaje = poblacion/total)

grafico_poblacion_urbana_rural <- base %>% 
  filter(area_geografica != "Total") %>%
  ggplot(mapping = aes(x = año, 
                       y = poblacion/1000000)) +
  geom_line(mapping = aes(
    group = area_geografica,
    color = area_geografica),
    linewidth = 1,
    show.legend = F)  + 
  geom_text(data = base %>% filter(area_geografica == "Cabecera" & año %in% c(seq(1950, 2010, 10),2019)),
              mapping = aes(x = año,
                            y = 0,
                            label = str_c(round(porcentaje*100, 1), " %", sep = "")),
            size = 3,
            color = "darkblue",
            alpha = 0.9) +
  annotate(geom = "text",
          x = 1943,
          y = 0,
          label = "% Población\nUrbana:",
          size = 3) +
  annotate(geom = "text",
           x = 2021.5,
           y = c(45, base$poblacion[base$año==2019 & base$area_geografica == "Cabecera"]/1000000, 
                 base$poblacion[base$año==2019 & base$area_geografica == "Centros Poblados y Rural Disperso"]/1000000),
           label = c("Crecimiento anual\nEquivalente:",
                     str_c("Urbano\n",round(b_tasas[1,2]*100,2), "%"),
                     str_c("Rural\n",round(b_tasas[2,2]*100,2), "%")),
           size = 3,
           color = c("black", "darkblue", "darkgreen"),
           alpha = c(1,0.7,0.7)) +
  scale_x_continuous(breaks = c(seq(1950, 2010, 10),2019),
                     limits = c(1942, 2023.5)) +
  scale_y_continuous(n.breaks = 12,
                     limits = c(-2, 47)) +
  scale_color_manual(values = c("darkblue", "darkgreen")) +
  labs(title = "Evolución de la población urbana y rural en Colombia de 1950 al 2019",
       x = NULL,
       y = "Millones de habitantes",
       caption = "Fuente: elaboración propia en base a Banco de la República") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.0, size = 10),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle =0, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


ggsave(filename = "rmd/resultados/graficos/grafico2.20_poblacion_urbana_rural.png",
       plot = grafico_poblacion_urbana_rural,
       width = 7.85,
       height = 4.85,
       dpi = 500)  
  
  
