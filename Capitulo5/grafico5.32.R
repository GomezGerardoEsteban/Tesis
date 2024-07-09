
###################################################################################################################################################################
# Titulo: Comparación entre la capacidad instalada según el Plan Indicativo de Expansión de la UPME y los proyectos aprobados por el organismo a diciembre de 2023
# Tema: Este gráfico hace una comparación de los escenarios de expansión segun el Plan Indicativo de Expansión de la UPME y el que se obtendria de llevarse
#       a cabo la totalidad de los proyectos aprobados a diciembre de 2023.
#       el documento oficial del plan indicativo de expansion en generación puede encontrarse en: 
#       "https://www1.upme.gov.co/siel/Plan_expansin_generacion_transmision/Plan_de_Expansion_Generacion_2023-2037_a_comentarios.pdf"
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 9 de Julio de 2024                                                                                                                     
# Base de datos: Los datos para realizar estos calculos fueron organizados en el documento **escenarios_planIndicativoExpansion_UPME_2023.xlsx**
#                el cual toma los valores del documento oficial reseñado en la descripción y **cambio_esperado_capacidad_Instalada.csv**.
#################################################################################################################################################

rm(list = ls())

library(tidyverse)
library(readxl)

base <- read_excel(path = "rmd/bases/maticesIP/escenarios_planIndicativoExpansion_UPME_2023.xlsx")

base_cambio <- read.csv(file = "rmd/bases/cambio_esperado_capacidad_Instalada.csv")

vec_verif <- unique(base$fuente)

base <- base %>% 
  mutate(categoriaFuente = case_when(fuente == vec_verif[1] ~ "Hidroeléctrica",
                                     fuente == vec_verif[2] ~ "Gas",
                                     fuente == vec_verif[3] ~ "Carbón",
                                     fuente == vec_verif[4] ~ "Fósil",
                                     fuente == vec_verif[5] ~ "Fósil",
                                     fuente == vec_verif[6] ~ "Fósil",
                                     fuente == vec_verif[7] ~ "Menores",
                                     fuente == vec_verif[8] ~ "Menores",
                                     fuente == vec_verif[9] ~ "Geotermia",
                                     fuente == vec_verif[10] ~ "Solar",
                                     fuente == vec_verif[11] ~ "Eólica"
  ),
  renovables = case_when(
    categoriaFuente == "Hidroeléctrica" ~ 2,
    categoriaFuente == "Gas" ~ 1,
    categoriaFuente == "Carbón" ~ 1,
    categoriaFuente == "Fósil" ~ 1,
    categoriaFuente == "Menores" ~ 0,
    categoriaFuente == "Geotermia" ~ 0,
    categoriaFuente == "Solar" ~ 0,
    categoriaFuente == "Eólica" ~ 0
  ))

base <- base %>% 
  mutate(renovables = factor(renovables))

base <- base %>% 
  mutate(categoriaFuente = factor(categoriaFuente, levels  = c("Solar", "Eólica",
                                                               "Menores", "Geotermia",
                                                               "Gas", "Carbón", "Fósil",
                                                               "Hidroeléctrica"),
                                  labels = c("Solar", "Eólica",
                                             "Menores", "Geotermia",
                                             "Gas", "Carbón", "Fósil",
                                             "Hidroeléctrica")))


base <- base %>% 
  filter(categoriaFuente != "Geotermia") %>% 
  group_by(Escenario, renovables, categoriaFuente) %>% 
  summarise(capacidad_alFinal = sum(capacidad_alFinal, na.rm = T))

base <- base %>% 
  group_by(Escenario) %>% 
  mutate(Total = sum(capacidad_alFinal, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Porcentaje = capacidad_alFinal/Total)

base <- base %>% 
  mutate(Escenario = factor(Escenario, labels = c("Escenario\nBase",
                                                  "Escenario\nBase + compromisos",
                                                  "Escenario\nSimulación 1",
                                                  "Escenario\nSimulación 2",
                                                  "Escenario\nSimulación 3"
  ),
  levels = c("escenarioBase",
             "escenarioBaseCompromisos",
             "escenario1",
             "escenario2",
             "escenario3")))

etiquetas <- base %>% 
  mutate(Etiqueta = ifelse(Escenario == "Escenario\nBase" & categoriaFuente %in% c("Solar", "Eólica"), 
                           NA, str_c(categoriaFuente, round(Porcentaje*100,1), "%", sep = " ")))


totales <- base %>% 
  group_by(Escenario) %>% 
  summarise(total = sum(capacidad_alFinal))


base_cambio <- base_cambio %>% 
  mutate(combustible = factor(case_when(
    combustible == "Solar" ~ "Solar",
    combustible == "Eólica" ~ "Eólica",
    combustible == "Bioenergía" ~ "Menores",
    combustible == "Gas" ~ "Gas",
    combustible == "Carbón" ~ "Carbón",
    combustible == "Petróleo" ~ "Fósil",
    combustible == "Hidroeléctrica" ~ "Hidroeléctrica",
  ), levels = c("Solar", "Eólica",
                "Menores",
                "Gas", "Carbón", "Fósil",
                "Hidroeléctrica"),
  labels = c("Solar", "Eólica",
             "Menores",
             "Gas", "Carbón", "Fósil",
             "Hidroeléctrica")))

base_cambio <- base_cambio %>% 
  mutate(renovable = factor(renovable),
         porcentaje = futura/sum(futura),
         Escenario = "Escenario\nProyectos Aprobados",
         etiqueta = ifelse(combustible == "Menores", NA, str_c(combustible, round(porcentaje*100,1), "%", sep = " ")))



alpha_max <- 1
alpha_min <- 0.6
alpha_vals <- c(
  seq(alpha_max, alpha_min, length.out = 3), 
  seq(alpha_max, alpha_min, length.out = 3),
  alpha_max)


grafico_escenarios_expansion_UPME <- base %>% 
  ggplot() +
  geom_col(mapping = aes(x = Escenario, y = capacidad_alFinal, fill = renovables, alpha = categoriaFuente),
           col = "white", width = 0.8, show.legend = F) +
  geom_text(data = etiquetas,
            aes(x = Escenario,
                y = capacidad_alFinal,
                fill = renovables, 
                alpha = categoriaFuente,
                label = Etiqueta),
            position = position_stack(vjust = 0.70),
            col = 'white',
            size = 2.5,
            fontface = 'bold',
            show.legend = F) +
  scale_fill_manual(values = c( "#249206","#000000","#3667A6")) +
  scale_alpha_manual(values = alpha_vals)+
  geom_text(data = totales,
            mapping = aes(x = Escenario, y = total+1000, label = str_c(total, "MW", sep = " "))) +
  geom_col(data = base_cambio,
           mapping = aes(x = Escenario, y = futura, fill = renovable, alpha = combustible),
           col = "white", width = 0.8, show.legend = F) +
  geom_text(data = base_cambio,
            aes(x = Escenario,
                y = futura,
                fill = renovable, 
                alpha = combustible,
                label = etiqueta),
            position = position_stack(vjust = 0.70),
            col = 'white',
            size = 2.5,
            fontface = 'bold',
            show.legend = F) +
  geom_text(data = base_cambio,
            mapping = aes(x = Escenario, y = sum(futura)+1000, label = str_c(round(sum(futura),0), "MW", sep = " "))) +
  geom_vline(mapping = aes(xintercept = 5.5),
             color = "darkred",
             linetype = "dashed",
             linewidth = 1.2)+
  labs(title = "Escenarios de crecimiento en la capacidad de generación eléctrica",
       subtitle = "Plan Indicativo de Expansión UPME al 2037 ~ Proyectos Aprobados por la UPME a Dic. 2023",
       x = NULL,
       y = "Megavativos (MW)",
       caption = "Fuente: Elaboración propia en base a UPME y XM") +
  scale_y_continuous(n.breaks = 10) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 0),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

ggsave(plot = grafico_escenarios_expansion_UPME,
       filename = "rmd/resultados/graficos/grafico5.32_escenariosUpme.png",
       dpi = 500,
       width =  9.69,
       height = 6.5
)