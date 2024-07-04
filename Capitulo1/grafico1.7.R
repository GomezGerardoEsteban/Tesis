
#################################################################################################################################################
# Titulo: Comparación de emisiones per cápita en la generación de electricidad
# Tema: Este gráfico muestra la comparación en las emisiones de GEI producidas por el sector eléctrico entre ciertos paises
#       Costa Rica, Bolivia, Colombia, Brasil, Peru, Ecuador, Uruguay, Suecia, Argentina, Mexico, World, Chile, Iran, China, Alemania, Estados Unidos
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 4 de Julio de 2024                                                                                                                     #
# Base de datos: Este gráfico usa la base sobre emisiones de climate watch cuya manipulación se genera en el script 
#                **baseEnergia.R**
#################################################################################################################################################

source("rmd/scripts/baseEnergia.R")

## Grafico colombia vs Mundo
paises1 <- c("Argentina",
             "Bolivia",
             "Chile",
             "Colombia",
             "Costa Rica",
             "Mexico",
             "Brazil",
             "Ecuador",
             "Uruguay",
             "Peru",
             "World",
             "United States",
             "Germany",
             "Sweden",
             "Iran",
             "China")


CO2Emissions <- paises1 %>% map(.f = ~{
  
  base %>%  
    select(country, year, iso_code, population, gdp, greenhouse_gas_emissions) %>% 
    filter(country == .x) %>% 
    mutate(perCapita = (greenhouse_gas_emissions*1000000/population))
  
})


CO2Emissions <- CO2Emissions %>% bind_rows()

CO2Emissions <- CO2Emissions %>% 
  filter(!is.na(perCapita))

CO2Emissions <- CO2Emissions %>% 
  group_by(country) %>% 
  mutate(mediana = median(perCapita),
         media = mean(perCapita))

CO2Emissions <- CO2Emissions %>% 
  mutate(relleno = ifelse(iso_code == "COL", 1,
                          ifelse(iso_code == "WOR", 2, 0)))


B <- CO2Emissions %>% 
  ggplot(mapping = aes(x = reorder(iso_code, mediana), y = perCapita, fill = as.factor(relleno))) +
  geom_boxplot(show.legend = F, width = 0.5) +
  scale_fill_manual(values = c("white", "#FFC300", "#3667A6")) +
  geom_rect(aes(xmin=-Inf,xmax=2.5,ymin=-Inf,ymax=Inf),
            fill="lightgrey", alpha = 0.015)+
  geom_rect(aes(xmin=3.5,xmax=10.5,ymin=-Inf,ymax=Inf),
            fill="lightgrey", alpha = 0.015)+
  geom_rect(aes(xmin=11.5,xmax=Inf,ymin=-Inf,ymax=Inf),
            fill="lightgrey", alpha = 0.015)+
  scale_y_continuous(n.breaks = 10) +
  labs(title = "Emisiones de CO2 per cápita en la generación de electricidad",
       subtitle = "Varios países (<span style='color:#FFC300;'>Colombia</span> y <span style='color:#3667A6;'>Mundo</span>)",
       y = "Toneladas de CO2 equivalente por habitante",
       x = NULL,
       caption = "Fuente: elaboración propia en base a Climate Watch \n Los datos corresponden al periodo 2000 - 2022") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        plot.subtitle = element_markdown(hjust = 0.5, size = 9, face = "italic"),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 7, angle = 0, vjust = 0.5),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

ggsave(plot = B, filename = "rmd/resultados/graficos/grafico1.7_colombia_emisiones_elec_Colombia_vs_paises.png",
       units = "in", 
       width = 6.28*(4/5),
       height = 4.94*(4/5),
       dpi = 300)
