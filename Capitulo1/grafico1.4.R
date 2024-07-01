
#################################################################################################################################################
# Titulo: Generación de electricidad por hora del día a partir de energía solar y eólica
# Tema: Este gráfico muestra la inestabilidad caracteristica de la energía solar y eólica a lo largo del día
#       a partir de información observacional registada por XM a lo largo del 2022.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 1 de Julio de 2024                                                                                                                    #
# Base de datos: Teniendo en cuenta que eran pocos valores, se hizo un registro en un archivo .csv cuyo titulo es
#                fotovoltica.csv
#################################################################################################################################################


# Grafico intermitencia ---------------------------------------------------

rm(list = ls())

fotovoltaica <- read.csv2("rmd/bases/fotovoltaica.csv")

# Capacidad efectiva neta solar = 278.66
# Capacidad efectiva neta eolica = 18.42

anotacion1 <- glue::glue("Capacidad Efectiva Neta <span style = 'color:#008B0F;'>278.66</span> MW <br> Factor de Planta <span style = 'color:#008B0F;'>0.250</span>")

anotacion2 <- glue::glue("Capacidad Efectiva Neta <span style = 'color:#5A00C1;'>18.42</span> MW <br> Factor de Planta <span style = 'color:#5A00c1;'>0.208</span>")

#008B0F
#C1B500

graph10 <- fotovoltaica %>% 
  ggplot(mapping = aes(x = Hora)) +
  geom_line(aes(y = Media_Solar), col = "#008B0F", linewidth = 1) +
  geom_line(aes(y = Media_Eolica*20), col = "#5A00C1", linewidth = 1) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~./20, name = "MW - Eólica",
                                         breaks = 
                                           seq(from = 0, to = 10, by = 1)), n.breaks = 10) +
  scale_x_continuous(breaks = 1:24) +
  labs(title = "Comportamiento medio en la generación por hora \n de las plantas solares y eólicas en Colombia durante el 2022",
       y = "MW - Solar",
       x = "Hora del día",
       caption = "Fuente: Elaboración propia en base a XM y UPME") +
  annotate(geom = "richtext",
           x = c(4, 21),
           y = c(150, 150),
           label = c(anotacion1, anotacion2),
           size = 3) +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.5, size = 11),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 7, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8, color = "#008B0F"),
        axis.title.y.right = element_text(size = 8, color = "#5A00C1"),
        axis.title.x = element_text(size = 8),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


ggsave(plot = graph10, filename = "rmd/resultados/graficos/grafico1.4_solar_eolica_hora.png",
       units = "in", width = 7.67, height = 4.29)
