#################################################################################################################################################
# Descripción: Este codigo ordena la base de energía de OWiD para poder generar varios de los gráficos que se encuentran en la tesis.
#              Incorpora información de ClimateWatch para ver las emisiones de GEI por tipo de sector según el IPCC y variables del Banco mundial
#              (World Development Indicators) sobre población, Indice de Desarrollo Humano (IDH), industria manufacturera, PIB, PIB per cápita e 
#              industria total.
#              Hace los joins de las distintas bases por paises y años y acomoda los datos en formato largo.
# 
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 3 de Julio de 2024                                                                                                                     #
# Bases de datos: Este código utiliza un total de 8 bases de datos, la de OWiD, 5 bases con indicadores de Banco Mundial, la base de IDH y la base
#                 de climateWacth. En el script **descargaBaseEnergia.R** se muestra la manera de obtener cada una de estas bases, ejecutado ese 
#                 script, solo debe asegurarse que las bases se enuentren en el directorio de trabajo.
# Productos: los productos que se obtienen de este script a partir del cual se generan graficos en otros documentos son:
#            - base (base de datos con la información de energia a la cual se le añaden las bases de banco mundial e IDH)
#            - baseCo2 (base de datos con la información de emisiones por sector según la categorización de IPCC)
# Detalles adicionales: Los productos de este scripts se utilizan en los documentos que tengan al inicio de la ejecución el comando 
#                       'source("baseEnergia.R")'
#################################################################################################################################################

rm(list = ls())

library(tidyverse)
library(readxl)
library(patchwork)
library(ggtext)
library(glue)
library(ggrepel)
library(stargazer)
library(latex2exp)
library(igraph)
library(ioanalysis)
library(writexl)


# Base de energia OWiD ----------------------------------------------------

base <- read.csv("rmd/bases/owid-energy-data.csv")


base <- base %>% 
  mutate(iso_code = ifelse(country == "World", "WOR", iso_code))

base <- base %>% 
  filter(iso_code != "") # Nos quedamos solamente con datos de los paises


base %>% 
  filter(iso_code == "WOR") %>% 
  select(year, oil_production, oil_electricity, gas_production, gas_electricity, coal_production, coal_electricity) %>% 
  transmute(year = year,
            petroleo = oil_electricity/oil_production,
            gas = gas_electricity/gas_production,
            coall = coal_electricity/coal_production)


# Carga idh ---------------------------------------------------------------

idh <- read.csv("rmd/bases/HDR21-22_Composite_indices_complete_time_series.csv") %>% 
  as_tibble()

{
  idh_hdi <- idh %>% 
    select(1:36)
  
  idh_hdiL <- idh_hdi %>% 
    gather(key = year,
           value = idh, 5:36)
  
  idh_hdiL <- idh_hdiL %>% 
    filter(year != "hdi_rank_2021")
  
  idh_hdiL <- idh_hdiL %>% 
    mutate(year = as.numeric(str_sub(year, start = 5, end = 8)))
  
  base <- 
    base %>% 
    left_join(y = idh_hdiL %>% 
                select(iso3, year, hdicode, idh), 
              by = c("year" = "year",
                     "iso_code" = "iso3"), 
              relationship = "many-to-many")
  }

# Carga worldBank Data -----------------------------------------------------

wbData <- readxl::read_excel(path = "WDIEXCEL.xlsx", sheet = 1)

indicators.code <- c("NV.IND.TOTL.ZS", # Industrial total
                     "NV.IND.MANF.ZS", # Industria manufacturera
                     "NY.GDP.PCAP.KD", # PIB per capita
                     "SP.URB.TOTL.IN.ZS", # % poblacion urban
                     "NY.GDP.MKTP.KD") # PIB


wbData <- wbData %>% 
  filter(`Indicator Code` %in% indicators.code)

wbData <- wbData %>% 
  gather(key = year, value = valor, 5:length(wbData))

wbData <- wbData %>% 
  select(-`Indicator Name`) %>% 
  spread(key = `Indicator Code`, value = valor)


nombres <- c("CountryName", "CountryCode", "year", 
             "manufactura", "industria", "pib", "pib_percapita", "urban")

names(wbData) <- nombres

wbData$year <- as.numeric(wbData$year)

base <- base %>% 
  left_join(y = wbData %>% 
              select(-CountryName), 
            by = c("iso_code" = "CountryCode",
                   "year" = "year"))

# Emisiones por actividad y fuentes en generacion -------------------------

baseCo2 <- read.csv("rmd/bases/CW_HistoricalEmissions_ClimateWatch.csv")

# Reshape the data using the gather function from the tidyr package
baseCo2 <- baseCo2 %>% 
  gather(key = year, value = valor, 5:35)

# Extract the four-digit year from the 'year' column
baseCo2 <- baseCo2 %>% 
  mutate(year = str_sub(year, start = 2, end = 5))

# Replace spaces with underscores in the 'Sector' column
baseCo2 <- baseCo2 %>% 
  mutate(Sector = str_replace_all(Sector, pattern = " ", replacement = "_"))

# Filter rows where 'Gas' is "All GHG" and create a new column 'agrupamientoR'
baseCo2 <- baseCo2 %>% 
  filter(Gas == "All GHG") %>% 
  mutate(agrupamientoR = ifelse(Sector == "Energy_", 1, 
                                ifelse(Sector == "Electricity/Heat_" | Sector == "Building" | Sector == "Manufacturing/Construction_"
                                       | Sector == "Transportation_" | Sector == "Other_Fuel_Combustion_" | Sector == "Fugitive_Emissions_", 0, 2)))

# Create a new categorical column based on the 'Sector' column
baseCo2 <- baseCo2 %>% 
  mutate(categorica = case_when(
    Sector == "Electricity/Heat_" ~ "Electricidad",
    Sector == "Building" ~ "Construcción",
    Sector == "Bunker_Fuels_" ~ "Combust. Marino",
    Sector == "Agriculture_" ~ "Agricultura",
    Sector == "Fugitive_Emissions_" ~ "Emisiones por Fugas",
    Sector == "Industrial_Processes_" ~ "Proc. Industriales",
    Sector == "Land-Use_Change_and_Forestry_" ~ "CUS y Deforestación",
    Sector == "Other_Fuel_Combustion_" ~ "Otras combustiones",
    Sector == "Transportation_" ~ "Transporte",
    Sector == "Waste_" ~ "Desperdicios",
    Sector == "Manufacturing/Construction_" ~ "Manufactura",
    Sector == "Energy_" ~ "Energía",
    Sector == "Total_including_LUCF" ~ "Total con CUSD",
    Sector == "Total_excluding_LUCF" ~ "Total sin CUSD"
  ))

# Define a vector of sectors to be excluded from the analysis
vector_filtro <- c("Total_excluding_LUCF", "Total_including_LUCF", "Energy_")

# Filter out rows where 'Sector' is in the exclusion vector
baseCo2 <- baseCo2 %>% 
  filter(!(baseCo2$Sector %in% vector_filtro))

# Group data by 'Country' and 'year', and calculate 'total' and 'participacion' columns
baseCo2 <- baseCo2 %>% 
  group_by(Country, year) %>% 
  mutate(total = sum(valor, na.rm = T),
         participacion = valor/total)


