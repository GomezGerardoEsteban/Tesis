
#################################################################################################################################################
# Titulo: Crecimiento de la demanda de electricidad y el PIB en Colombia (1980 - 2022)
# Tema: Utilizando datos de la agencia internacional de energia y de OurWorldinData, se muestra la evolución del PIB y de la 
#       demanda de electricidad en valores absolutos a traves de un indice que toma el año 200 = 100.
#       se añaden ademas las estimaciones de crecimiento de ambas variables según información oficial de la UPME y del FMI.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 6 de Julio de 2024                                                                                                                     #
# Base de datos: Para generar esta visualización es necesario correr los scripts complementarios de **baseEnergia.R**
#                y de **descargaBaseEnergia.R**, otra base adicional es **baseConsumoElectricidadEIA.xlsx**
#################################################################################################################################################

rm(list = ls())

library(tidyverse)
library(readxl)
library(patchwork)

source("rmd/scripts/baseEnergia.R")


paises <- c("Argentina",
            "Bolivia",
            "Chile",
            "Uruguay",
            "Paraguay",
            "Brazil",
            "Ecuador",
            "Peru",
            # "Venezuela",
            # "World",
            "Colombia",
            "Panama",
            "Costa Rica",
            "Nicaragua",
            "Guatemala",
            "El Salvador",
            "Honduras",
            "Mexico")

pib_percapita_paisesLatam <- base %>% 
  filter(country %in% paises) %>% 
  select(year, iso_code, pib_percapita, population)

paises_pib_elec <- base %>% 
  select(country, iso_code, year, pib) %>% 
  filter(country %in% paises)


baseConsumo <- read_excel(path = "rmd/bases/baseConsumoElectricidadEIA.xlsx")

baseConsumo <- baseConsumo %>% 
  gather(key = year, value = valor, 4:length(baseConsumo))

baseConsumo <- baseConsumo %>% 
  mutate(valor = as.numeric(valor),
         year = as.numeric(year))

electricidad_latam <- baseConsumo %>% 
  filter(Country %in% paises)


electricidad_latam <- electricidad_latam %>% 
  left_join(y = pib_percapita_paisesLatam, by = c("year" = "year",
                                                  "Code.GDP" = "iso_code"))

electricidad_latam <- electricidad_latam %>% 
  mutate(consumo_per_capita = valor/population)

# View(electricidad_latam)

crecimientos <- paises %>% map(.f = ~{
  
  
  p <- electricidad_latam %>% 
    filter(Country == .x & (year == 1980 | year == 2021)) %>% 
    select(Country, pib_percapita, consumo_per_capita, Code.GDP)
  
  q <- (p$pib_percapita[2]/p$pib_percapita[1] - 1)
  e <- (p$consumo_per_capita[2]/p$consumo_per_capita[1] - 1)
  
  res <- tibble(country = .x, 
                crecimientoPibCapita = q,
                crecimientoConsumoElec = e,
                tasaEquivalenteAnualPIB = (1 + crecimientoPibCapita)^(1/(2021-1980)) - 1,
                tasaEquivalenteAnualElec = (1 + crecimientoConsumoElec)^(1/(2021-1980)) - 1)
  
  res <- res %>% 
    left_join(y = p %>% 
                select(Country, Code.GDP), by = c("country" = "Country"))
  
  return(res)
})

crecimientos <- crecimientos %>% bind_rows() %>% distinct()

View(crecimientos %>% arrange(desc(tasaEquivalenteAnualElec)))

tabla_datos_elas_ing_elec_1980_2021 <- crecimientos %>% 
  mutate(elasticidad = tasaEquivalenteAnualElec/tasaEquivalenteAnualPIB) %>% 
  select(country, elasticidad, tasaEquivalenteAnualElec, tasaEquivalenteAnualPIB) %>% 
  arrange(desc(elasticidad))


paises_pib_elec <- paises_pib_elec %>% 
  left_join(y = baseConsumo %>% 
              select(-Country, - Country.WDI), by = c("iso_code" = "Code.GDP",
                                                      "year" = "year"))


crecimiento_absolutos <- paises %>% map(.f = ~{
  
  
  p <- paises_pib_elec %>% 
    filter(country == .x & (year == 1980 | year == 2021)) %>% 
    select(country, pib, valor, iso_code)
  
  q <- (p$pib[2]/p$pib[1] - 1)
  e <- (p$valor[2]/p$valor[1] - 1)
  
  res <- tibble(country = .x, 
                crecimientoPib = q,
                crecimientoElec = e,
                tasaEquivalenteAnualPIB = (1 + crecimientoPib)^(1/(2021-1980)) - 1,
                tasaEquivalenteAnualElec = (1 + crecimientoElec)^(1/(2021-1980)) - 1)
  
  res <- res %>% 
    left_join(y = p %>% 
                select(country, iso_code), by = c("country" = "country"))
  
  return(res)
})

crecimiento_absolutos <- crecimiento_absolutos %>% bind_rows() %>% distinct()

crecimiento_absolutos %>% arrange(desc(tasaEquivalenteAnualElec))

crecimiento_absolutos_col <- paises_pib_elec %>% 
  filter(country == "Colombia" & year >= 1980) %>% 
  select(country, pib, valor, iso_code, year)

crecimiento_absolutos_col$valor[43] <- crecimiento_absolutos_col$valor[42]*(1+(84.98/81.37-1))

p <- crecimiento_absolutos_col[crecimiento_absolutos_col$year == 1980 | crecimiento_absolutos_col$year == 2022, ]

q <- (p$pib[2]/p$pib[1] - 1)
e <- (p$valor[2]/p$valor[1] - 1)

res <- tibble(country = "Colombia", 
              crecimientoPib = q,
              crecimientoElec = e,
              tasaEquivalenteAnualPIB = (1 + crecimientoPib)^(1/(2021-1980)) - 1,
              tasaEquivalenteAnualElec = (1 + crecimientoElec)^(1/(2021-1980)) - 1)

res <- res %>% 
  left_join(y = p %>% 
              select(country, iso_code), by = c("country" = "country"))

res <- res[1,]

tasaUpme <- ((1 + ((121647.824531864/80268.1014062248)-1))^(1/14) - 1)
tasaIfm <- ((1 + ((456.063/363.835)-1))^(1/5) - 1)

tasaObservPib <- res$tasaEquivalenteAnualPIB[1]
tasaObservElec <- res$tasaEquivalenteAnualElec[1]


vec1_elec <- c(100)
vec1_pib <- c(100)
for(i in 1:42){
  
  vec1_elec[i+1] <- vec1_elec[i]*(1+tasaObservElec)
  vec1_pib[i+1] <- vec1_pib[i]*(1+tasaObservPib)
  
}


vec2_elec <- c(vec1_elec[length(vec1_elec)])
vec2_pib <- c(vec1_pib[length(vec1_pib)])

for(i in 1:15){
  
  vec2_elec[i+1] <- vec2_elec[i]*(1 + tasaUpme)
  
  
}

for(i in 1:6){
  
  vec2_pib[i+1] <- vec2_pib[i]*(1+tasaIfm)
  
}

vec2_pib <- vec2_pib[2:length(vec2_pib)]
vec2_elec <- vec2_elec[2:length(vec2_elec)]

vec_elec <- c(vec1_elec, vec2_elec)
vec_pib <- c(vec1_pib, vec2_pib)

simulacionCrec <- tibble(year = 1980:2037,
                         demanda = vec_elec,
                         pib_ = c(vec_pib, rep(NA, 9)))

referencia2000 <- simulacionCrec[simulacionCrec$year == 2000,]

simulacionCrec <- simulacionCrec %>% 
  mutate(demanda = demanda/referencia2000$demanda[1]*100,
         pib_ = pib_/referencia2000$pib_[1]*100)

simulacionCrec <- simulacionCrec %>% 
  left_join(y = crecimiento_absolutos_col %>% 
              select(year, pib, valor),
            by = c("year" = "year"))

referencia2000 <- simulacionCrec[simulacionCrec$year == 2000,]

simulacionCrec <- simulacionCrec %>% 
  mutate(indicePib = pib*100/referencia2000$pib[1],
         indiceElec = valor*100/referencia2000$valor[1])

simulacionCrec$color <- ifelse(simulacionCrec$year <= 2022, 1, 0)

simulCrec <- simulacionCrec %>%
  ggplot(mapping = aes(x = year)) +
  geom_line(aes(y = indiceElec), color = "#4254FC", linewidth = 1.2) +
  geom_line(aes(y = demanda, linetype = as.factor(color)), color = "#4254FC", show.legend = F, linewidth = 1) +
  geom_line(aes(y = indicePib), color = "#FC5642", linewidth = 1.2) +
  geom_line(aes(y = pib_, linetype = as.factor(color)), color = "#FC5642", show.legend = F, linewidth = 1) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(breaks = seq(1980,2037,3)) +
  annotate(geom = "text",
           x = c(1992),
           y = c(350,325,310),
           size = c(3.2, 3, 3),
           color = c("black","#FC5642","#4254FC"),
           label = c("Crecimiento anual equivalente (1980 - 2022)",
                     str_c("PIB =", round(tasaObservPib, 4)*100, "%", sep = " "), 
                     str_c("Demanda Elec. =", round(tasaObservElec, 4)*100, "%", sep = " "))) +
  annotate(geom = "text",
           x = c(2030,2024),
           y = c(235,310),
           size = c(3, 3),
           color = c("#FC5642","#4254FC"),
           label = c(str_c("Estimado FMI =", round(tasaIfm, 4)*100, "%", sep = " "), 
                     str_c("Estimado UPME =", round(tasaUpme, 4)*100, "%", sep = " "))) +
  annotate(geom = "text",
           x = c(2030,2024),
           y = c(225,300),
           size = c(2.5, 2.5),
           color = c("#FC5642","#4254FC"),
           label = c("2023 - 2028", 
                     "2023 - 2037")) +
  labs(title = "Evolución de la demanda de electricidad y el PIB en Colombia",
       subtitle = NULL,
       y = "Indices de crecimiento (Año 2000 = 100)",
       x = NULL,
       caption = "Fuente: elaboración propia en base a EIA, BM, UPME y FMI") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 11),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 0),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


ggsave(filename = "rmd/resultados/graficos/grafico1.14_absoluto_pib_elec_1980_2022.png",
       plot = simulCrec,
       dpi = 500,
       width = 8.53,
       height = 4
)


