

#################################################################################################################################################
# Titulo: Evolución del PIB per cápita y de la demanda de electricidad por habitante en países latinoamericanos (1980 – 2022)
# Tema: Utilizando datos de la agencia internacional de energia y de OurWorldinData, se muestra el crecimiento del PIB per capita, del 
#       consumo de electricidad per cápita y se hace un calculo de la elasticidad ingreso del consumo de electricidad para 16 paises de 
#       America Latina.
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


# datos de EIA -----------------------------------------------------------

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

# View(crecimientos %>% arrange(desc(tasaEquivalenteAnualElec)))

tabla_datos_elas_ing_elec_1980_2021 <- crecimientos %>% 
  mutate(elasticidad = tasaEquivalenteAnualElec/tasaEquivalenteAnualPIB) %>% 
  select(country, elasticidad, tasaEquivalenteAnualElec, tasaEquivalenteAnualPIB) %>% 
  arrange(desc(elasticidad))

write_xlsx(x = tabla_datos_elas_ing_elec_1980_2021,
           path = "rmd/resultados/tablas/tabla1.3_eslasticidad_paises_1980_2021.xlsx")
