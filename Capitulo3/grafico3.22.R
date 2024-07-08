
#################################################################################################################################################
# Titulo: Participación de la electricidad tranzada a través de contratos en el total
# Tema: Este script utiliza la información oficial de XM sobre demanda total de electricidad y compra de electricidad a partir de contratos,
#       la información oficial puede obtenerse a traves de 'sinergox' pero son muchas bases las cuales deben procesarse antes de generar el Join
#       En función de ello la base procesada a partir de la cual se genera el gráfico es 'base_demanda_contratos_bolsa.Rdata'
#
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 8 de Julio de 2024                                                                                                                     
# Base de datos: La base a utilizar para esta visualización es **base_demanda_contratos_bolsa.Rdata**
# 
#################################################################################################################################################

library(tidyverse)

rm(list = ls())

load(file = "rmd/bases/precios_precipitaciones/contratos/base_demanda_contratos_bolsa.Rdata")

grafico <- bases_demanda %>% 
  ggplot() +
  geom_line(mapping = aes(x = fecha, y = prop)) +
  geom_hline(mapping = aes(yintercept = 1), 
             linetype = "twodash",
             color = "darkblue")+
  scale_y_continuous(n.breaks = 10) +
  scale_x_date(breaks = as.Date(c("2000-01-01",
                                  "2005-01-01",
                                  "2010-01-01",
                                  "2015-01-01",
                                  "2020-01-01",
                                  "2023-01-01")),
               labels = c("2000",
                          "2005",
                          "2010",
                          "2015",
                          "2020",
                          "2023")) +
  labs(title = "Porcentaje de energía eléctrica comercializada por día a través de contratos",
       subtitle = "Periodo: Enero del 2000 - Diciembre del 2023",
       y = "Proporción",
       x = NULL,
       caption = "Fuente: elaboración propia en base a XM") +  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(size=9, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10),
        axis.title.y.right = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


ggsave(plot = grafico, 
       filename = "rmd/resultados/graficos/grafico3.22_prop_contratos_dia.png", 
       units = 'in', 
       width = 9.85,
       height = 4.84,
       dpi = 300)



