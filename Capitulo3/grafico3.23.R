
#################################################################################################################################################
# Titulo: Demanda de electricidad de usuarios regulados y no regulados
# Tema: Este script utiliza la información oficial de XM sobre demanda total de electricidad diferenciando por demanda de agentes regulados
#       y demanda de agentes no regulados (aquellos con una demanda mensual mayor a 55 MWh/mes)
#
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 8 de Julio de 2024                                                                                                                     
# Base de datos: Las bases a utilizar para esta visualización se encuentran en 'sinergox'
# 
#################################################################################################################################################

rm(list = ls())

library(tidyverse)
library(readxl)
library(writexl)

res_tipo_ususario <- readxl::read_excel(path = "rmd/bases/precios_precipitaciones/contratos/resumen_regulado_noRegulado.xlsx")

media_2000 <- res_tipo_ususario %>% 
  dplyr::select(year, Mercado, prop) %>% 
  filter(year == 2000) %>% 
  ungroup() %>% 
  group_by(Mercado) %>% 
  reframe(mean = mean(prop))

media_2023 <- res_tipo_ususario %>% 
  dplyr::select(year, Mercado, prop) %>% 
  filter(year == 2023) %>% 
  ungroup() %>% 
  group_by(Mercado) %>% 
  reframe(mean = mean(prop))


grafico_reg_noReg <- res_tipo_ususario %>% 
  filter(Mercado != "CONSUMOS") %>% 
  ggplot() +
  geom_area(mapping = aes(x = as.Date(Fecha), y = total/1000000, fill = Mercado), show.legend = F) +
  scale_fill_manual(values = c("#2E86C1", "#F39C12")) +
  scale_y_continuous(n.breaks = 6) +
  scale_x_date(breaks = as.Date(seq(from = as.Date("2000-01-01"), to = as.Date("2023-12-01"), by = "year")),
               labels = as.character(2000:2023)) +
  annotate(geom = "text",
           x = c(as.Date("2002-03-01"),as.Date("2002-06-01"),
                 as.Date("2021-08-01"),as.Date("2021-06-01")),
           y = c(1500, 3000,
                 2500, 5000),
           label = c(str_c("Regulado 2000 = ", round(media_2000$mean[media_2000$Mercado=="REGULADO"][1]*100,2), "%"),
                     str_c("No Regulado 2000 = ", round(media_2000$mean[media_2000$Mercado=="NO REGULADO"][1]*100,2), "%"),
                     str_c("Regulado 2023 = ", round(media_2023$mean[media_2023$Mercado=="REGULADO"][1]*100,2), "%"),
                     str_c("No Regulado 2023 = ", round(media_2023$mean[media_2023$Mercado=="NO REGULADO"][1]*100,2), "%")),
           size = 3) +
  labs(title = "Participación de agentes Regulados y No Regulados en la demanda de electricidad",
       subtitle = "Periodo: Enero del 2000 - Diciembre del 2023",
       y = "Gigavatios Hora (GWh)",
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


ggsave(plot = grafico_reg_noReg, 
       filename = "rmd/resultados/graficos/grafico3.23_regulado_noRegulado.png", 
       units = 'in', 
       width = 11,
       height = 6.79,
       dpi = 300)
