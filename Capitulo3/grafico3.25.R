
#################################################################################################################################################
# Titulo: Evolución de la capacidad instalada en la generación eléctrica por fuente primaria de energía
# Tema: Este script muestra la evolución de la capacidad instalada en generación de electricidad en Colombia.
#       es importante esta distinción porque es lo que se problematiza, no es lo mismo generación efectiva que 
#       capacidad instalada.
#
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 8 de Julio de 2024                                                                                                                     
# Base de datos: La base a utilizar es **CapaciddEfectivaNeta.xlsx** la cual resume información proveniente de 'sinergox', pagina oficial de 
#                XM para gestionar la información historica del sector eléctrico.
#################################################################################################################################################

rm(list = ls())

library(tidyverse)
library(readxl)
library(patchwork)

r_capacidad_al_fin <- readxl::read_excel(path = "rmd/bases/capacidadEfectivaNeta.xlsx")

alpha_max <- 1
alpha_min <- 0.6
alpha_vals <- c(
  seq(alpha_max, alpha_min, length.out = 3), 
  seq(alpha_min, alpha_max, length.out = 3),
  alpha_max)


graph1 <- r_capacidad_al_fin %>% 
  ggplot(mapping = aes(x = year, y = total/1000, fill = renovable, alpha = combustible)) +
  geom_area(col = "white") +
  # geom_text(data = etiquetas_part_elec,
  #           mapping = aes(x = year, y = valor, label = etiqueta), nudge_y = 5, size = 3) +
  scale_alpha_manual(values = alpha_vals) +
  scale_fill_manual(values = c("#249206", "#283227", "#3667A6")) +
  guides(
    fill = guide_none(),
    alpha = guide_legend(override.aes = list(fill = c(rep("#249206", 3),
                                                      rep("#283227", 3),
                                                      "#3667A6")))
  ) +
  scale_x_continuous(breaks = 2000:2023) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "A: Evolución de la capacidad instalada en generación eléctrica por fuente primaria en Colombia",
       subtitle = "Electricidad medida en Megavatios (MW), (2000 - 2023)",
       x = NULL,
       y = "Megavatios - (MW)",
       caption = "Fuente: Elaboración propia con base en XM") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.0, size = 9),
        plot.subtitle = element_text(hjust = 0.0, size = 8),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 6, angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

graph1


alpha_vals <- c(
  seq(alpha_max, alpha_min, length.out = 3), 
  seq(alpha_min, alpha_max, length.out = 3),
  alpha_max)


etiquetas <- r_capacidad_al_fin %>%
  filter(year == 2023) %>% 
  mutate(etiqueta = ifelse(combustible %in% c("Bioenergía", "Eólica"), NA,
                           str_c(combustible, round(prop*100,1), "%", sep = " ")))

graph2 <- r_capacidad_al_fin %>% 
  filter(year == 2023) %>% 
  ggplot(mapping = aes(x = as.factor(year), y = prop*100, fill = renovable, alpha = combustible)) +
  geom_col(col = "white", width = 1) +
  scale_alpha_manual(values = alpha_vals) +
  scale_fill_manual(values = c("#249206","#283227","#3667A6")) +
  guides(
    fill = guide_none(),
    alpha = guide_legend(override.aes = list(fill = c(rep("#249206", 4),
                                                      rep("#283227", 3),
                                                      "#3667A6")))
  ) +
  scale_y_continuous(position = "right", n.breaks = 10) +
  geom_text(
    data = etiquetas,
    aes(label = etiqueta),
    position = position_stack(vjust = 0.70),
    col = 'white',
    size = 3,
    fontface = 'bold'
  ) +
  labs(title = "B: Participación porcentual por fuente",
       subtitle = "Capacidad instalada - 2023",
       x = NULL,
       y = "Porcentaje") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        plot.subtitle = element_text(hjust = 0.5, size = 8),
        plot.caption = element_text(size=6, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

graph2

grafico4 <- wrap_plots(graph1, graph2, ncol = 2, widths = c(1, 0.4))
grafico4

ggsave(plot = grafico4, filename = "rmd/resultados/graficos/grafico3.25_evolucion_capacidadInstalada_col.png", 
       units = 'in', width = 9, height = 6)


