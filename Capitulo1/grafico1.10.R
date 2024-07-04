
#################################################################################################################################################
# Titulo: Evolución del costo nivelado de electricidad por tipo de fuente
# Tema: Este gráfico se basa en el ultimo reporte de Lazard's sobre costos nivelados de electricidad a nivel mundial
#       en el script se pone el codigo para descargar el reporte desde la pagina oficial de lazard's
#       La comparación toma el primer y utlimo dato disponible (2009, 2023)
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 4 de Julio de 2024                                                                                                                     #
# Base de datos: Lazard's es una compañia privada conocida por sus informes sobre el sector energético
#                y desde 2009 lleva adelante un seguimiento de los costos nivelados de electricidad por tipo
#                de fuente en distintos paises de del mundo.
#                Su informe de 2023 puede consutarse en la siguiente https://www.lazard.com/media/2ozoovyg/lazards-lcoeplus-april-2023.pdf
#################################################################################################################################################

# Costos nivelados de electricidad Lazard's Descarga del informe mediante API ---------------------------------------

url <- "https://www.lazard.com/media/2ozoovyg/lazards-lcoeplus-april-2023.pdf"

filename <- basename(url)

download.file(url = url,
              destfile = filename)

# limpiamos el ambiente

rm(list = ls())

# paqueteria

library(tidyverse)
library(ggtext)

# Armado de la base

costos_nivelados <- tibble(tipo = c("Eólica",
                                    "Solar (PV)",
                                    "Gas CC",
                                    "Carbón",
                                    "Gas",
                                    "Nuclear"),
                           costo1 = c(136, 359, 83, 111, 276, 123),
                           costo2 = c(50, 60, 70, 117, 168, 180))

costos_nivelados <- costos_nivelados %>% 
  mutate(tasa = (costo2/costo1 - 1 )*100)

costos_nivelados <- costos_nivelados %>% 
  mutate(etiqueta = ifelse(tasa > 0, str_c(tipo, " +", round(tasa, 1), "%",sep = ""),
                           str_c(tipo, round(tasa, 1), "%", sep = " ")),
         tipo = factor(tipo, levels = c("Eólica",
                                        "Solar (PV)",
                                        "Gas CC",
                                        "Carbón",
                                        "Gas",
                                        "Nuclear"), labels = c("Eólica",
                                                               "Solar (PV)",
                                                               "Gas CC",
                                                               "Carbón",
                                                               "Gas",
                                                               "Nuclear")))


graph <- costos_nivelados %>% 
  ggplot(mapping = aes(color = tipo)) +
  geom_point(mapping = aes(x = c(rep(2009,6)), y = costo1), show.legend = F) +
  geom_point(mapping = aes(x = c(rep(2023,6)), y = costo2), show.legend = F) +
  geom_segment(mapping = aes(x = 2009, xend = 2023, y = costo1, yend = costo2), show.legend = F) +
  geom_text(mapping = aes(x = 2024.9, y = costo2, label = etiqueta), show.legend = F, size = 3) +
  geom_rect(aes(xmin=-Inf,xmax=2023.5,ymin=65,ymax=173),
            fill="gray", alpha = 0.04, color = "darkgray")+
  scale_x_continuous(limits = c(2009, 2026), breaks = c(2009, 2023)) +
  scale_color_manual(values = c("#264653",
                                "#2a9d8f",
                                "#3a5a40",
                                "#191919",
                                "#e76f51",
                                "#d62828")) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = NULL,
       y = "Costo Megavatio hora ($/MWh)",
       title = "Comparación del costo nivelado de electricidad \n según tipo de fuente primaria de energía (2009 - 2023)",
       caption = "Fuente: elaboración propia en base a Lazard LCOE 2023") +
  annotate(geom = "text",
           x = 2013,
           y = 180,
           label = "Franja de costos fósiles",
           size = 3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 11),
        plot.subtitle = element_text(hjust = 0.5, size = 8),
        plot.caption = element_markdown(size=7, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

ggsave(plot = graph, filename = "rmd/resultados/graficos/grafico1.10_costos_nivelados_electricidad_fuente.png",
       units = "in", width = 6.26, height = 5.23)




