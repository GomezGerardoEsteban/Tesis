
###################################################################################################################################################################
# Titulo: Cambios en capacidad instalada y en ENFICC
# Tema: Este gráfico compara el escenario base y el de proyectos aprobados en capacidad instalada y en Energía Firme para Cargo por Confiabilidad
#       (ENFICC), como puede observarse, cambios significativos en la capacidad instalada no necesariamente implican cambios significativos en 
#       ENFICC.
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

grafico_a <- base %>% 
  filter(Escenario == "Escenario\nBase") %>% 
  ggplot() +
  geom_col(mapping = aes(x = Escenario, y = capacidad_alFinal, fill = renovables, alpha = categoriaFuente),
           col = "white", width = 0.8, show.legend = F) +
  geom_text(data = etiquetas %>% 
              filter(Escenario == "Escenario\nBase"),
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
  geom_text(data = totales %>% 
              filter(Escenario == "Escenario\nBase"),
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
  annotate(geom = "text",
           x = 1,
           y = 40000,
           label = str_c("Variación\nPorcentual\n+", round((42702/18918 - 1)*100, 1), " %"),
           size = 3,
           color = "darkred") +
  labs(
    # title = "Escenarios de crecimiento en la capacidad de generación eléctrica",
    subtitle = "Comparación del cambio en terminos de capacidad instalada",
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


base <- base %>% 
  mutate(factor_ENFICC = ifelse(categoriaFuente == "Eólica", 0.14,
                                ifelse(categoriaFuente == "Hidroeléctrica", 0.34,
                                       ifelse(categoriaFuente == "Solar", 0.10,
                                              0.90))))


# ENFICC año en GWh

base$ENFICC_año_GW <- (base$capacidad_alFinal/1000)*24*365*base$factor_ENFICC

base_cambio$factor_ENFICC <- c(0.1, 0.14, 0.9, 0.9, 0.9, 0.9, 0.34)

base_cambio$ENFICC_año_GW <- (base_cambio$futura/1000)*24*365*base_cambio$factor_ENFICC

base_cambio <- base_cambio %>% 
  select(Escenario, renovable, combustible, futura, porcentaje, factor_ENFICC, ENFICC_año_GW) %>% 
  rename(renovables = renovable,
         categoriaFuente = combustible,
         capacidad_alFinal = futura,
         Porcentaje = porcentaje)

base_cambio <- base_cambio %>% 
  mutate(Total = sum(capacidad_alFinal)) %>% 
  relocate(Total, .before = Porcentaje)

base <- rbind(base, base_cambio)

unique(base$Escenario)

base <- base %>% 
  mutate(etiquetas_ENFICC = ifelse((Escenario == "Escenario\nBase" & categoriaFuente %in% c("Solar", "Eólica")) | (Escenario == "Escenario\nProyectos Aprobados" & categoriaFuente == "Menores"),
                                   NA, str_c(categoriaFuente, round(ENFICC_año_GW, 1), "GWh", sep = " ")))

base <- base %>% 
  group_by(Escenario) %>% 
  mutate(totalEnficc = sum(ENFICC_año_GW))


var_porc_ENFICC <- base$totalEnficc[base$Escenario == "Escenario\nProyectos Aprobados"][1]/base$totalEnficc[base$Escenario == "Escenario\nBase"][1] - 1


grafico_b <- base %>% 
  filter(Escenario %in% c("Escenario\nBase", "Escenario\nProyectos Aprobados")) %>% 
  ggplot() +
  geom_col(mapping = aes(x = Escenario, y = ENFICC_año_GW, fill = renovables, alpha = categoriaFuente),
           col = "white", width = 0.8, show.legend = F) +
  geom_text(aes(x = Escenario,
                y = ENFICC_año_GW,
                fill = renovables, 
                alpha = categoriaFuente,
                label = etiquetas_ENFICC),
            position = position_stack(vjust = 0.70),
            col = 'white',
            size = 2.5,
            fontface = 'bold',
            show.legend = F)+
  scale_fill_manual(values = c( "#249206","#000000","#3667A6")) +
  scale_alpha_manual(values = alpha_vals) +
  geom_text(mapping = aes(x = Escenario, y = totalEnficc + 3000, 
                          label = str_c(round(totalEnficc,0), "GWh", sep = " "))) +
  annotate(geom = "text",
           x = 1, 
           y = 115000,
           label = str_c("Variación\nProcentual\n+", round(var_porc_ENFICC*100, 1), " %"),
           color = "darkred",
           size = 3)+
  labs(
    #title = "Comparación de la Energía Firme para Cargo por Confiabilidad (ENFICC)",
    subtitle = "Comparación del cambio en terminos de ENFICC",
    x = NULL,
    y = "Gigavatios-Hora (GWh)",
    # caption = "Fuente: Elaboración propia en base a UPME y XM"
  ) +
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


library(patchwork)

grafico_ENFICC <- grafico_a + grafico_b

ggsave(plot = grafico_ENFICC,
       filename = "rmd/resultados/graficos/grafico5.33_enficc.png",
       dpi = 500,
       width =  9.23,
       height = 5.41
)
