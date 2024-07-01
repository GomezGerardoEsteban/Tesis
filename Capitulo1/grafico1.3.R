
#################################################################################################################################################
# Titulo: Evolución de las emisiones del sector eléctrico y la generación de electricidad 1990 - 2020                                                                                #
# Tema: Este gráfico muestra la evolución de las emisiones de GEI en CO2 equivalente correspondientes al sector eléctrico y
#       la evolución de la generación de eléctricidad entre 1990 y 2020, ambos en terminos per capita
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 1 de Julio de 2023                                                                                                                     #
# Comentarios adicionales: Este Script primero ejecuta el script baseEnergia.R, el cual utiliza la base de Our World in Data sobre energía
#                          comentarios sobre la limpieza de esa base para poder empezar a gráficar se encuentran en ese script. 
#################################################################################################################################################

source("rmd/scripts/baseEnergia.R")

pop <- base %>%  
  select(country, iso_code, year, population)

emisiones_capita_elec <- baseCo2 %>% 
  mutate(iso_code = ifelse(Country == "WORLD", "WOR", Country),
         year = as.numeric(year)) %>%
  left_join(y=pop %>% select(-country), by = c("iso_code", "year")) %>% 
  mutate(capita = valor/population*1000000000) %>% 
  filter(iso_code == "WOR" & year >= 1990 & categorica == "Electricidad")

# Grafico de participación de fuentes

generacion_capita_elec <- base %>% filter(country == "Colombia" | country == "World") %>% 
  select(country, year, iso_code, ends_with("electricity"), greenhouse_gas_emissions, population)

generacion_capita_elec <- generacion_capita_elec %>% 
  gather(key = tipo, value = valor, 4:length(generacion_capita_elec))

generacion_capita_elec <- generacion_capita_elec %>% 
  filter(country == "World" & tipo == "per_capita_electricity" & year >= 1990)


generacion_capita_elec <- generacion_capita_elec %>%
  ungroup() %>% 
  left_join(y = emisiones_capita_elec %>% 
              ungroup() %>% 
              select(year, capita), by = ("year"))

indice1 <- generacion_capita_elec %>% 
  filter(year == 1990) %>% 
  select(valor)

indice2 <- generacion_capita_elec %>% 
  filter(year == 1990) %>% 
  select(capita)

indice1.1 <- generacion_capita_elec %>% 
  filter(year == 2020) %>% 
  select(valor)

indice2.2 <- generacion_capita_elec %>% 
  filter(year == 2020) %>% 
  select(capita)


crec_generacion <- ((1+(indice1.1$valor[1]/indice1$valor[1] - 1))^(1/(2020-1990)) - 1)*100
crec_emisiones <- ((1+(indice2.2$capita[1]/indice2$capita[1] - 1))^(1/(2020-1990)) - 1)*100

grafico_gen_emi <- generacion_capita_elec %>% 
  filter(year <= 2020) %>% 
  mutate(indice_generacion = valor/indice1$valor[1]*100,
         indice_emision = capita/indice2$capita[1]*100) %>% 
  ggplot(mapping = aes(x = year)) +
  geom_rect(mapping = aes(xmin = 2013, xmax = 2020,
                          ymin = -Inf, ymax = Inf),
            fill = "lightgrey",
            alpha = 0.025)+
  geom_line(mapping = aes(y = indice_generacion), color = "#A85D51", linewidth = 1.1) +
  geom_line(mapping = aes(y = indice_emision), color = "#6EA851", lnewidth = 1.1) +
  scale_x_continuous(breaks = seq(1990, 2020, 5),
                     limits = c(1990, 2024)) +
  annotate(geom = "text",
           x = c(2023, 2023),
           y = c(148, 119),
           label = c(paste("Generación \n","+", round(crec_generacion, 2), "% ", "anual", sep = ""),
                     paste("Emisiones \n","+", round(crec_emisiones, 2), "% ", "anual", sep = "")),
           size = c(4,4),
           color = c("#A85D51", "#6EA851")) +
  labs(x = NULL,
       y = "Indice base 100 = 1990",
       title = "Crecimiento per cápita de la generación y las emisiones del sector eléctrico",
       # subtitle = "Mundo <span style='color:#FF5733;'>1990</span> y <span style='color:#900C3F;'>2020</span>",
       caption = "Fuente: Elaboración propia en base a Climate Watch y Owid") +
  # Set theme settings
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 11),
        plot.subtitle = element_markdown(hjust = 0.5, size = 9, face = "italic"),
        plot.caption = element_text(size=8, hjust=1, face="italic", color="black"),
        axis.text.x = element_text(size = 7, angle = 0),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")
  
ggsave(filename = "rmd/resultados/graficos/grafico1.3_emisiones_generacion_per_capita.png",
       plot = grafico_gen_emi,
       dpi = 500,
       width = 9.58,
       height = 3.92)

