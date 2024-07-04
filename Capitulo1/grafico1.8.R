
#################################################################################################################################################
# Titulo: Precipitaciones pluviales y precio promedio de la electricidad en Colombia                                                                               #
# Tema: Este gráfico muestra la relación observada en Colombia entre el precio mayorista de la electricidad y las precipitaciones pluviales 
#       entre 1996 y 2020. La base utilizada es "precios_year.Rdata" la cual fue construida a partir de los datos de cambio climatico del
#       Banco Mundial y la información del precio mayorista de electricidad de XM. Para tener precios constantes se utilizo el Indice de Precios
#       al consumidor del Banco de la Republica.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 4 de Julio de 2024                                                                                                                   #
# 
#################################################################################################################################################


rm(list = ls())

library(tidyverse)
library(patchwork)

load("rmd/bases/precios_precipitaciones/precios_year.Rdata")

correlacion <- cor.test(precios_year$precioMedio, precios_year$Annual.Mean)

graficoDisper <- precios_year %>% 
  filter(year > 1995) %>% 
  ggplot()+
  geom_point(mapping = aes(x = Annual.Mean, y = precioMedio)) +
  geom_smooth(aes(x = Annual.Mean, y = precioMedio), method = "lm", se =F) +
  labs(title = "B: Relación entre precipitaciones promedio \n y precio del kilovatio-hora",
       subtitle = NULL,
       y = "Precio kWh - pesos constantes a Dic. 2022",
       x = "Milimetros (mm)")+
       # caption = "Fuente: elaboración propia en base a XM, Banco Mundial y Banrep") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=7, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 9),
        axis.title.y.right = element_text(size = 9, color = "#A54339"),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")
  

graficoPrec_Preci <- precios_year %>% 
  filter(year > 1995) %>% 
  ggplot()+
  geom_line(mapping = aes(x = year, y = Annual.Mean), color = "#395FA5", linewidth = 1.2) +
  geom_line(mapping = aes(x = year, y = precioMedio*10), color = "#A54339", linewidth = 1.2) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~./10,
                                         name = "Precio kWh - pesos constantes a Dic. 2022")) +
  annotate(geom = "text",
           x = 2000,
           y = 4500,
           label = str_c("Correlación Lineal \n de Pearson = ", round(correlacion$estimate, 3)), size = 3) +
  labs(title = "Evolución del precio promedio de la electricidad en el mercado mayorista y las \n precipitaciones anuales en Colombia (1996 - 2020)",
       subtitle = NULL,
       y = "Milimetros (mm)",
       x = NULL,
       caption = "Fuente: elaboración propia en base a XM, Banco Mundial y Banrep") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=7, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 9, color = "#395FA5"),
        axis.title.y.right = element_text(size = 9, color = "#A54339"),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

grafico <- wrap_plots(graficoPrec_Preci, graficoDisper, ncol = 2, widths = c(1, 0.4))


ggsave(plot = grafico, filename = "rmd/resultados/graficos/grafico1.8_precip_precio_elec.png", 
       units = 'in', 
       width = 11.2,
       height = 5.14,
       dpi = 300)

