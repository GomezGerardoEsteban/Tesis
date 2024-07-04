
#################################################################################################################################################
# Titulo: Costos de instalación por tipo de fuente de energía renovable
# Tema: Este gráfico se basa en el informe de IRENA (International Renewable Energy Agency) de 2021 en el que comparan el costo total 
#       de instalación por kWh (USD/kWh), mostrando la diferencia entre la energía solar y eólica respecto a la geotermia y la biomasa
#       En este código, ademas de observar la diferencia en costo total de instalación, se oberva el factor de capacidad y el costo 
#       nivelado de electricidad.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 4 de Julio de 2024                                                                                                                     #
# Base de datos: El documento en base a cual se generan estos gráficos se encuentra disponible en "https://www.irena.org/-/media/Files/IRENA/Agency/Publication/2022/Jul/IRENA_Power_Generation_Costs_2021.pdf?rev=34c22a4b244d434da0accde7de7c73d8"
#                pagina 15.
#################################################################################################################################################

rm(list = ls())

mejor <- tibble(
  "Costos de Instalación" = c(2353, 3991, 857, 1325),
  "Factor de Capacidad" = c(0.68, 0.77, 0.17, 0.39),
  "Costo de kWh" = c(0.067, 0.068, 0.048, 0.033),
  Fuente = c("Bioenergía", "Geotermia", "Solar FV", "Eólica Terr.")
)


mejor <- mejor %>% 
  gather(key = variable, value = valor, 1:3)

mejor <- mejor %>% 
  mutate(Fuente = factor(Fuente, levels = c("Geotermia", "Bioenergía", "Solar FV", "Eólica Terr.")),
         factorP = case_when(Fuente == "Geotermia" | Fuente == "Bioenergía" ~ 1,
                             Fuente == "Solar FV" | Fuente == "Eólica Terr." ~ 0))

alpha_max <- 1
alpha_min <- 0.7
alpha_vals <- c(
  seq(alpha_max, alpha_min, length.out = 2),
  seq(alpha_max, alpha_min, length.out = 2))

g1 <- mejor %>% 
  filter(variable == "Factor de Capacidad") %>% 
  ggplot(mapping = aes(x = valor, y = Fuente, fill = as.factor(factorP), alpha = Fuente)) +
  scale_fill_manual(values = c("#0A9713","#97600A")) +
  scale_alpha_manual(values = alpha_vals) +
  scale_y_discrete(limits = rev(levels(mejor$Fuente)), position = "left") +
  geom_col(show.legend = F) +
  geom_text(aes(label = valor), position = position_nudge(x = 0.03), show.legend = F, size = 3) +
  geom_text(aes(label = Fuente), position = position_stack(vjust = 0.5), show.legend = F, size = 3, color = "black") +
  scale_x_continuous(position = "top", n.breaks = 10) +
  labs(x = "Factor de planta",
       y = NULL) +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.0, size = 8),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=6, hjust=1, face="italic", color="black"),
        axis.text.x = element_text(size = 7, angle = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

g2 <- mejor %>% 
  filter(variable == "Costos de Instalación") %>% 
  ggplot(mapping = aes(x = valor, y = Fuente, fill = as.factor(factorP), alpha = Fuente)) +
  scale_fill_manual(values = c("#0A9713","#97600A")) +
  scale_alpha_manual(values = alpha_vals) +
  scale_y_discrete(limits = rev(levels(mejor$Fuente)), position = "left") +
  geom_col(show.legend = F) +
  geom_text(aes(label = valor), position = position_nudge(x = 125), show.legend = F, size = 3) +
  geom_text(aes(label = Fuente), position = position_stack(vjust = 0.5), show.legend = F, size = 3, color = "black") +
  scale_x_continuous(position = "top", n.breaks = 10) +
  labs(x = "Costo total de instalación por kWh (USD/kWh)",
       y = NULL,
       caption = "Fuente: elaboración propia en base a IRENA 2021") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.0, size = 9),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=7, hjust=1, face="italic", color="black"),
        axis.text.x = element_text(size = 7, angle = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


g3 <- mejor %>% 
  filter(variable == "Costo de kWh") %>% 
  ggplot(mapping = aes(x = valor, y = Fuente, fill = as.factor(factorP), alpha = Fuente)) +
  scale_fill_manual(values = c("#0A9713","#97600A")) +
  scale_alpha_manual(values = alpha_vals) +
  scale_y_discrete(limits = rev(levels(mejor$Fuente)), position = "left") +
  geom_col(show.legend = F) +
  geom_text(aes(label = valor), position = position_nudge(x = 0.003), show.legend = F, size = 3) +
  geom_text(aes(label = Fuente), position = position_stack(vjust = 0.5), show.legend = F, size = 3, color = "black") +
  scale_x_continuous(position = "top", n.breaks = 10) +
  labs(x = "Costo nivelado de la electricidad (USD/kWh)",
       y = NULL,
       caption = "Fuente: elaboración propia en base a IRENA 2021") +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.0, size = 8),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 7, angle = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


Comb3 <- g1 / g2 / g3

ggsave(plot = Comb3, filename = "grafico_fuentes_comparacion_FP_Costo_Inv.png", 
       units = 'in', width = 7, height = 5)

ggsave(plot = g2, filename = "rmd/resultados/graficos/grafico1.9_fuentes_comparacion_Inv.png", 
       units = 'in', width = 7.6, height = 2)

