
#################################################################################################################################################
# Titulo: Concentración de la comercialización de electricidad
# Tema: Este script muestra la evolución de la concentración en la comercialización de electricidad a partir del IHH y de
#       la participación en el total comercializado de las primeras 10 y las primeras 4 empresas
#
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 8 de Julio de 2024                                                                                                                     
# Base de datos: La base a utilizar es **resumen_ranking_comercializadoras.xlsx** y **resumen_demanda_comercializador.xlsx** las cuales resume información proveniente de 'sinergox', pagina oficial de 
#                XM para gestionar la información historica del sector eléctrico.
#################################################################################################################################################

rm(list = ls())

library(tidyverse)
library(readxl)


ranking_comercializador <- read_excel(path = "rmd/bases/precios_precipitaciones/contratos/resumen_ranking_comercializadoras.xlsx") 

res_demanda <- read_excel(path = "rmd/bases/precios_precipitaciones/contratos/resumen_demanda_comercializador.xlsx") 

grafico_participacion_empresas <- ranking_comercializador %>% 
  ggplot() +
  geom_line(mapping = aes(x = as.Date(Fecha), y = totPart*100), linetype = "solid", linewidth = 1.2) +
  geom_line(mapping = aes(x = as.Date(Fecha), y = totPart2*100), linetype = "dotted", linewidth = 1) +
  scale_y_continuous(n.breaks = 10, limits = c(0,100)) +
  scale_x_date(breaks = as.Date(seq(from = as.Date("2000-01-01"), to = as.Date("2023-12-01"), by = "year")),
               labels = as.character(2000:2023),
               limits = as.Date(c("2000-01-01", "2028-01-01"))) +
  annotate(geom = "text",
           x = as.Date("2026-12-01"),
           y = c(sum(ranking_comercializador$participacion.x[2871:2880]*100),
                 sum(ranking_comercializador$participacion.y[2871:2880]*100, na.rm = T)),
           label = c(paste("Participación 10 primeras\nEmpresas", round(sum(ranking_comercializador$participacion.x[2871:2880]*100),1), "%", sep = " "),
                     paste("Participación 4 primeras\nEmpresas", round(sum(ranking_comercializador$participacion.y[2871:2880]*100, na.rm = T),1), "%", sep = " ")),
           size = 3.5) +
  labs(title = "Participación de las principales empresas comercializadoras en la electricidad total comercializada",
       subtitle = "Periodo: Enero del 2000 - Diciembre del 2023",
       y = "Porcentaje",
       x = NULL,
       caption = "Fuente: elaboración propia en base a XM") +  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(size=9, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 60, vjust = 0.5),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10),
        axis.title.y.right = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")



grafico_HH_comercializadores <- res_demanda %>% 
  ggplot() +
  geom_line(mapping = aes(x = as.Date(Fecha), y = indice_HH), linewidth = 1.2) +
  geom_hline(mapping = aes(yintercept = 1500), 
             linetype = "twodash",
             color = "darkblue") +
  scale_y_continuous(n.breaks = 10, limits = c(0,1600)) +
  scale_x_date(breaks = as.Date(seq(from = as.Date("2000-01-01"), to = as.Date("2023-12-01"), by = "year")),
               labels = as.character(2000:2023)) +
  annotate(geom = "text",
           x = as.Date("2006-12-01"),
           y = 1550,
           label = "Limite a partir del cual el mercado es moderadamente concentrado (IHH = 1500)",
           color = "darkblue",
           size = 3) +
  labs(title = "Indice de Herfindahl-Hirschman (IHH) en la comercialización de electricidad",
       subtitle = "Periodo: Enero del 2000 - Diciembre del 2023",
       y = "IHH",
       x = NULL,
       caption = "Fuente: elaboración propia en base a XM") +  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(size=9, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 60, vjust = 0.5),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10),
        axis.title.y.right = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


grafico_HH_comercializadores

ggsave(plot = grafico_HH_comercializadores, 
       filename = "rmd/resultados/graficos/grafico3.26.1_IHH_comercializadores.png", 
       units = 'in', 
       width = 9.85,
       height = 4.84,
       dpi = 300)

ggsave(plot = grafico_participacion_empresas, 
       filename = "rmd/resultados/graficos/grafico3.26.2_participacion_comercializadores.png", 
       units = 'in', 
       width = 11,
       height = 5.01,
       dpi = 300)



