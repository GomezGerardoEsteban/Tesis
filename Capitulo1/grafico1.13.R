
#################################################################################################################################################
# Titulo: Consumo de electricidad per cápita según porcentaje de población urbana
# Tema: Utilizando datos de 2019, este gráfico muestra el subconsumo de electricidad en Colombia, teniendo en cuenta su nivel de urbanización.
#       Es decir, siendo uno de los paises con mayor porcentaje de población urbana, tiene un consumo de electricidad por habitante muy inferior
#       al cuartil de urbanización al que pertenece.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 6 de Julio de 2024                                                                                                                     #
# Base de datos: Para generar esta visualización es necesario correr los scripts complementarios de **baseEnergia.R**
#                y de **descargaBaseEnergia.R**
#################################################################################################################################################

source("rmd/scripts/baseEnergia.R")

urban <- base %>% 
  filter(year == 2019 & !is.na(urban) & !is.na(per_capita_electricity) & per_capita_electricity != 0) %>% 
  select(country, iso_code, urban, per_capita_electricity)

breaksUrban <- quantile(urban$urban,
                        probs = seq(0.25, 1, 0.25))

urban <- urban %>% 
  mutate(categoriaCuantil = case_when(
    urban <= breaksUrban[1] ~ 1,
    urban >  breaksUrban[1] & urban <= breaksUrban[2] ~ 2,
    urban >  breaksUrban[2] & urban <= breaksUrban[3] ~ 3,
    urban >  breaksUrban[3] & urban <= breaksUrban[4] ~ 4))

urban %>% group_by(categoriaCuantil) %>% summarise(mean = mean(per_capita_electricity))

point_Col <- base %>%
  filter(iso_code == "COL" & year == 2019) %>%
  select(per_capita_electricity, hdicode, pib_percapita, iso_code) %>%
  group_by(hdicode) %>%
  summarise(mean = mean(per_capita_electricity),
            iso_code = iso_code,
            pib_percapita = pib_percapita,
            per_capita_electricity = per_capita_electricity)

point_Col <- point_Col %>% 
  mutate(hdicode = factor(hdicode, 
                          labels = c("Muy Alto",
                                     "Alto",
                                     "Medio",
                                     "Bajo"),
                          levels = c("Very High",
                                     "High",
                                     "Medium",
                                     "Low"))) 


point_urban_col <- urban %>% 
  filter(iso_code == "COL") %>% 
  mutate(categoriaCuantil = factor(categoriaCuantil, 
                                   labels = c("13.2 - 42.2",
                                              "42.3 - 60.8",
                                              "60.8 - 79.6",
                                              "79.6 - 100"
                                   ),
                                   levels = c(1, 2, 3, 4)),
         mean = per_capita_electricity)

Grafico_urban <- urban %>%
  group_by(categoriaCuantil) %>% 
  summarise(n = n(),
            mean = mean(per_capita_electricity),
            sd = sd(per_capita_electricity),
            quantile = qt(0.975, df = n - 1),
            se = sd/sqrt(n),
            li = mean - se*quantile,
            ls = mean + se*quantile) %>% 
  ungroup() %>% 
  mutate(categoriaCuantil = factor(categoriaCuantil, 
                                   labels = c("13.2 - 42.2",
                                              "42.3 - 60.8",
                                              "60.8 - 79.6",
                                              "79.6 - 100"
                                   ),
                                   levels = c(1, 2, 3, 4))) %>% 
  ggplot(mapping = aes(x = categoriaCuantil, y = mean)) +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = li, ymax = ls)) +
  geom_point(data = point_urban_col, mapping = aes(x = categoriaCuantil, y = mean), shape = 17, size = 3, color = "#E3651D") +
  geom_text(mapping = aes(x = 4, y = point_Col$mean[1] - 500, label = "COL"), size = 3) +
  coord_flip() +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "Intervalos de confianza al 0.95 del valor medio \n de consumo de electricidad por habitante según grado de urbanización",
       subtitle = NULL,
       y = "Kilovatios-Hora por habitante",
       x = "% de población urbana",
       caption = "Fuente: elaboración propia en base a OWiD y Banco Mundial") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=6, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 7, angle = 0),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


ggsave(filename = "rmd/resultados/graficos/grafico1.13_urban_elect_per_capita.png",
       plot = Grafico_urban,
       dpi = 500,
       width = 7.46,
       height = 2.94)



