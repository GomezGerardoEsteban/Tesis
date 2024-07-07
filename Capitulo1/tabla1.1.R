#################################################################################################################################################
# Titulo: Crecimiento del PIB ~ generación de electricidad
# Tema: Este script contiene el modelo presentado en la pagina 20 Tabla 1.1, en el cual se verifica a traves de un modelo generalizado 
#       de momentos el efecto causal de la electricidad en el crecimiento económico.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 6 de Julio de 2024                                                                                                                     
# Base de datos: Estas estimaciones utilizan informacion de generación proveniente de la Administración de Información de Energía de los Estados unidos (EIA)
#                información del PIB per capita proveniente del Departamento de Agricultura de lo Estados Unidos (USDA) y como variables de control información
#                de los World Development Indicators del Banco Mundial.
# 
#################################################################################################################################################

rm(list = ls())

# Paquteria

library(tidyverse)
library(readxl)
library(writexl)
library(stargazer)

# Variables a usar WDI

variables <- c("EG.CFT.ACCS.ZS",
               "EG.CFT.ACCS.RU.ZS",
               "EG.CFT.ACCS.UR.ZS",
               "EG.ELC.ACCS.ZS",
               "EG.ELC.ACCS.RU.ZS",
               "EG.ELC.ACCS.UR.ZS",
               "SE.PRM.TENR",
               "EG.USE.COMM.CL.ZS",
               "EN.ATM.CO2E.KD.GD",
               "EN.ATM.CO2E.PP.GD.KD",
               "EN.ATM.CO2E.PP.GD",
               "EN.ATM.CO2E.KT",
               "EN.ATM.CO2E.PC",
               "EN.CO2.ETOT.ZS",
               "EN.ATM.CO2E.GF.ZS",
               "EN.ATM.CO2E.GF.KT",
               "EN.ATM.CO2E.LF.ZS",
               "EN.ATM.CO2E.LF.KT",
               "EN.CO2.MANF.ZS",
               "EN.CO2.OTHX.ZS",
               "EN.CO2.BLDG.ZS",
               "EN.ATM.CO2E.SF.ZS",
               "EN.ATM.CO2E.SF.KT",
               "EN.CO2.TRAN.ZS",
               "EN.ATM.CO2E.EG.ZS",
               "FP.CPI.TOTL",
               "EG.USE.ELEC.KH.PC",
               "EG.ELC.LOSS.ZS",
               "EG.ELC.COAL.ZS",
               "EG.ELC.HYRO.ZS",
               "EG.ELC.NGAS.ZS",
               "EG.ELC.NUCL.ZS",
               "EG.ELC.PETR.ZS",
               "EG.ELC.FOSL.ZS",
               "EG.ELC.RNWX.ZS",
               "EG.ELC.RNWX.KH",
               "EG.IMP.CONS.ZS",
               "EG.EGY.PRIM.PP.KD",
               "EN.ATM.METH.EG.ZS",
               "EG.USE.PCAP.KG.OE",
               "EG.USE.COMM.GD.PP.KD",
               "SP.DYN.TFRT.IN",
               "BX.KLT.DINV.WD.GD.ZS",
               "BM.KLT.DINV.WD.GD.ZS",
               "NY.GDP.MKTP.KD",
               "NY.GDP.MKTP.KN",
               "NY.GDP.MKTP.CN",
               "NY.GDP.MKTP.CD",
               "NY.GDP.DEFL.ZS",
               "NY.GDP.DEFL.ZS.AD",
               "NY.GDP.MKTP.KD.ZG",
               "NY.GDP.PCAP.KD",
               "NY.GDP.PCAP.KN",
               "NY.GDP.PCAP.CN",
               "NY.GDP.PCAP.CD",
               "NY.GDP.PCAP.KD.ZG",
               "NY.GDP.PCAP.PP.KD",
               "NY.GDP.PCAP.PP.CD",
               "SL.GDP.PCAP.EM.KD",
               "EG.GDP.PUSE.KO.PP.KD",
               "EG.GDP.PUSE.KO.PP",
               "NY.GDP.MKTP.PP.KD",
               "NY.GDP.MKTP.PP.CD",
               "NY.GDP.MKTP.CN.AD",
               "NE.CON.GOVT.ZS",
               "SI.POV.GINI",
               "NE.GDI.TOTL.ZS",
               "NY.GDS.TOTL.ZS",
               "NE.GDI.FTOT.ZS",
               "NY.GNS.ICTR.ZS",
               "TM.UVI.MRCH.XD.WD",
               "NV.IND.TOTL.ZS",
               "FP.CPI.TOTL.ZG",
               "NY.GDP.DEFL.KD.ZG",
               "NY.GDP.DEFL.KD.ZG.AD",
               "SP.DYN.LE00.IN",
               "TG.VAL.TOTL.GD.ZS",
               "EN.URB.MCTY.TL.ZS",
               "EG.ELC.RNEW.ZS",
               "EG.FEC.RNEW.ZS",
               "SE.PRM.ENRR",
               "SE.PRM.NENR",
               "IC.ELC.TIME",
               "IC.ELC.DURS",
               "NE.TRD.GNFS.ZS",
               "SP.URB.TOTL.IN.ZS",
               "IC.FRM.OUTG.ZS",
               "SP.POP.TOTL")

# Levantamos WDI completa

baseWDI <- read_excel(path = "rmd/bases/baseWDI.xlsx", sheet = 1)

baseWDI_description <- baseWDI %>% 
  select(3,4) %>% 
  distinct()

# write_xlsx(x = baseWDI_description, path = "descriptores_WDI.xlsx")

# Filtramos por variables de interes 

baseFiltradaWDI <- baseWDI %>% 
  filter(IndicatorCode %in% variables)

# Guardamos descripción de variables

descripciones_var <- baseFiltradaWDI %>% 
  select(3,4) %>% 
  distinct()

# write_xlsx(x = descripciones_var, path = "vars_use_panelElectricityToGrowth.xlsx")

# Transformamos la base a formato largo

baseFiltradaWDI <- baseFiltradaWDI %>% 
  gather(key = year, value = valor, 5:length(baseFiltradaWDI))

baseFiltradaWDI <- baseFiltradaWDI %>% 
  select(-IndicatorName) %>% 
  spread(key = IndicatorCode, value = valor)

baseFiltradaWDI <- baseFiltradaWDI %>% 
  mutate(year = as.numeric(year))

# Levantamos bases de GDP -------------------------------------------------

basesGDP <- list()

for(i in 1:2){
  basesGDP[[i]] <- read_excel(path = "rmd/bases/baseGrowth_Levels_GDP.xlsx", sheet = i)
}

for(i in 1:2){
  basesGDP[[i]] <- basesGDP[[i]] %>% 
    gather(key = year, value = valor, 3:length(basesGDP[[i]]))
}

nombresGDP <- c("GDPlevels", "GDPgrowth")

for(i in 1:2){
  
  basesGDP[[i]] <- basesGDP[[i]] %>% 
    mutate(year = as.numeric(str_sub(year, 2, 5)))
  
  colnames(basesGDP[[i]])[4] <- nombresGDP[i]
  
}


basesGDP <- basesGDP[[1]] %>% 
  left_join(basesGDP[[2]] %>% 
              select(CODE, year, GDPgrowth), by = c("CODE", "year"))

basesGDP <- basesGDP %>% 
  arrange(Country)

View(basesGDP)

# Levantamos base de generacion  ------------------------------------------


baseGeneracion <- read_excel(path = "rmd/bases/baseGeneration.xlsx")

baseGeneracion <- baseGeneracion %>% 
  gather(key = year, value = valor, 7:length(baseGeneracion))

old_names_generation <- baseGeneracion %>% select(Tipo) %>% distinct()
old_names_generation <- old_names_generation[[1]]

new_names_generation <- c("Generation"            
                          ,"Nuclear"
                          ,"Fossil_fuels"
                          ,"Renewables"            
                          ,"Hydroelectricity"
                          ,"Non_hydroelectric_renewables"
                          ,"Geothermal"         
                          ,"Solar_tide_wave"
                          ,"Tide_wave"
                          ,"Solar"
                          ,"Wind"
                          ,"Biomass"
                          ,"Hydro_pumped_storage")


baseGeneracion <- baseGeneracion %>% 
  mutate(Tipo = case_when(Tipo == old_names_generation[1] ~ new_names_generation[1],
                          Tipo == old_names_generation[2] ~ new_names_generation[2],
                          Tipo == old_names_generation[3] ~ new_names_generation[3],
                          Tipo == old_names_generation[4] ~ new_names_generation[4],
                          Tipo == old_names_generation[5] ~ new_names_generation[5],
                          Tipo == old_names_generation[6] ~ new_names_generation[6],
                          Tipo == old_names_generation[7] ~ new_names_generation[7],
                          Tipo == old_names_generation[8] ~ new_names_generation[8],
                          Tipo == old_names_generation[9] ~ new_names_generation[9],
                          Tipo == old_names_generation[10] ~ new_names_generation[10],
                          Tipo == old_names_generation[11] ~ new_names_generation[11],
                          Tipo == old_names_generation[12] ~ new_names_generation[12],
                          Tipo == old_names_generation[13] ~ new_names_generation[13]))

baseGeneracion <- baseGeneracion %>% 
  select(-API) %>% 
  spread(key = Tipo, value = valor)

baseGeneracion <- baseGeneracion %>% 
  mutate(year = as.numeric(year))


# Joins de bases ----------------------------------------------------------

base <- baseFiltradaWDI %>% 
  left_join(y = baseGeneracion %>% 
              select(-c(Country, Country.WDI)), by = c("CountryCode" = "Code.WDI",
                                                       "year" = "year"))

base <- base %>% 
  left_join(y = basesGDP %>% 
              select(-Country), by = c("CountryCode" = "CODE",
                                       "year" = "year"))



# Estimacioones -----------------------------------------------------------

# View(base)

base <- base %>%
  mutate(GDP_percapita = GDPlevels/SP.POP.TOTL,
         GDP_percapita_g = (GDP_percapita - lag(GDP_percapita))/lag(GDP_percapita))


# write_xlsx(x = basesGDP, path = "baseGDP.xlsx")

# View(base %>% 
#   filter(!is.na(new_names_generation[3])) %>% 
#   select(year) %>% 
#   distinct())

base <- base %>% 
  mutate(across(.cols = all_of(new_names_generation), .fns = ~{as.numeric(.x)}))

base <- base %>% 
  mutate(across(.cols = all_of(new_names_generation), .fns = ~{ifelse(is.na(.x), 0, .x)}))

base <- base %>% 
  group_by(CountryCode) %>% 
  mutate(generation_g_r = (Generation/lag(Generation) - 1),
         renewables_g_r = (Renewables/lag(Renewables) - 1),
         fossil_g_r = (Fossil_fuels/lag(Fossil_fuels) - 1),
         generation_g = Generation - lag(Generation),
         renewables_g = Renewables - lag(Renewables),
         fossil_g = Fossil_fuels - lag(Fossil_fuels))

controles <- c("SE.PRM.TENR",   
               "SP.DYN.TFRT.IN",   
               "BX.KLT.DINV.WD.GD.ZS",
               "NE.CON.GOVT.ZS",   
               "SI.POV.GINI",          
               "NY.GNS.ICTR.ZS",   
               "FP.CPI.TOTL.ZG",   
               "SP.DYN.LE00.IN",   
               "NE.TRD.GNFS.ZS")

# SE.PRM.TENR,          2019 Net enrollment rate (% of primary school age children)
# SP.DYN.TFRT.IN,       2021 Fertility rate (births pper woman)
# BX.KLT.DINV.WD.GD.ZS, 2021 Foreign direct investment, net inflows (% of GDP)
# NE.CON.GOVT.ZS,       2022 General goverment final consumption expenditure (% of GDP)
# SI.POV.GINI,          2020 Gini index
# NY.GNS.ICTR.ZS,       2022 Gross savings (% of GDP)
# FP.CPI.TOTL.ZG,       2022 Inflation, consumer prices (annual %)
# SP.DYN.LE00.IN,       2021 Life expectancy at birth, total (years)
# NE.TRD.GNFS.ZS,       2022 Trade (% of GDP)

base_e1 <- base %>% 
  filter(year >= 1980 & year <= 2020) %>% 
  select(all_of(new_names_generation), all_of(controles), year, CountryCode,
         generation_g_r, generation_g, renewables_g_r, renewables_g, fossil_g_r, fossil_g,
         GDP_percapita, GDP_percapita_g)

# View(base_e1)

base_e1 <- base_e1 %>% 
  mutate(across(.cols = c("generation_g_r",
                          "renewables_g_r", 
                          "fossil_g_r", 
                          "GDP_percapita_g"), .fns = ~ifelse(.x == Inf, 0, .x)))

# write_xlsx(base_e1, "base_e1.xlsx")

library(plm)

mod1 <- plm(GDP_percapita_g ~ generation_g +
              # lag(log(GDP_percapita), 1) +
              SE.PRM.TENR + 
              log(SP.DYN.TFRT.IN) +
              BX.KLT.DINV.WD.GD.ZS +
              NE.CON.GOVT.ZS +
              NY.GNS.ICTR.ZS +
              FP.CPI.TOTL.ZG +
              SP.DYN.LE00.IN +
              NE.TRD.GNFS.ZS,          
            data = base_e1,            
            index = c("CountryCode", "year"),   
            model = "within",
            effect = "twoways")

summary(mod1)

mod2 <- plm(GDP_percapita_g ~ generation_g +
              lag(log(GDP_percapita), 1) +
              SE.PRM.TENR + 
              log(SP.DYN.TFRT.IN) +
              BX.KLT.DINV.WD.GD.ZS +
              NE.CON.GOVT.ZS +
              NY.GNS.ICTR.ZS +
              FP.CPI.TOTL.ZG +
              SP.DYN.LE00.IN +
              NE.TRD.GNFS.ZS,          
            data = base_e1,            
            index = c("CountryCode", "year"),   
            model = "within",
            effect = "twoways")

summary(mod2)

mod3 <- plm(GDP_percapita_g ~ renewables_g +
              # fossil_g +
              lag(log(GDP_percapita), 1) +
              SE.PRM.TENR + 
              log(SP.DYN.TFRT.IN) +
              BX.KLT.DINV.WD.GD.ZS +
              NE.CON.GOVT.ZS +
              NY.GNS.ICTR.ZS +
              FP.CPI.TOTL.ZG +
              SP.DYN.LE00.IN +
              NE.TRD.GNFS.ZS,          
            data = base_e1,            
            index = c("CountryCode", "year"),   
            model = "within",
            effect = "twoways")

summary(mod3)

mod4 <- plm(GDP_percapita_g ~ 
              fossil_g +
              lag(log(GDP_percapita), 1) +
              SE.PRM.TENR + 
              log(SP.DYN.TFRT.IN) +
              BX.KLT.DINV.WD.GD.ZS +
              NE.CON.GOVT.ZS +
              NY.GNS.ICTR.ZS +
              FP.CPI.TOTL.ZG +
              SP.DYN.LE00.IN +
              NE.TRD.GNFS.ZS,          
            data = base_e1,            
            index = c("CountryCode", "year"),   
            model = "within",
            effect = "twoways")

summary(mod4)

mod5 <- plm(GDP_percapita_g ~ renewables_g +
              fossil_g +
              lag(log(GDP_percapita), 1) +
              SE.PRM.TENR + 
              log(SP.DYN.TFRT.IN) +
              BX.KLT.DINV.WD.GD.ZS +
              NE.CON.GOVT.ZS +
              NY.GNS.ICTR.ZS +
              FP.CPI.TOTL.ZG +
              SP.DYN.LE00.IN +
              NE.TRD.GNFS.ZS,          
            data = base_e1,            
            index = c("CountryCode", "year"),   
            model = "within",
            effect = "twoways")

summary(mod5)


mod6 <- pgmm(GDP_percapita_g ~ generation_g +
               lag(log(GDP_percapita), 1) +
               SE.PRM.TENR + 
               log(SP.DYN.TFRT.IN) +
               BX.KLT.DINV.WD.GD.ZS +
               NE.CON.GOVT.ZS +
               NY.GNS.ICTR.ZS +
               FP.CPI.TOTL.ZG +
               SP.DYN.LE00.IN +
               NE.TRD.GNFS.ZS | lag(log(GDP_percapita), 2:3) +
               lag(generation_g, 2:3),          
             data = base_e1,            
             index = c("CountryCode", "year"),
             effect = "individual", 
             model = "twosteps", 
             transformation = "ld",
             collapse = T)

summary(mod6, robust = T)

library(lmtest)
library(sandwich)

fixed.r <- coeftest(mod6, vcov = vcovHC, type = "HC1")
summary(fixed.r)
# nos quedamos con estos 6 modelos ----------------------------------------

# TABLA PRESENTADA EN EL DOCUMENTO.

tabla1.1 <- stargazer(mod1, mod2, mod6,
          covariate.labels = c("Generación",
                               "log(PBI per cápita t-1)", "Matricula primaria",
                               "log(Tasa fertilidad)", 
                               "IED",
                               "Gasto Gobierno",
                               "Tasa de ahorro", 
                               "Inflación",
                               "Expec. Vida", 
                               "Comercio"),
          dep.var.labels = c("Crecimiento PBI per cápita"),type = "text")

write(x = tabla1.1, file = "rmd/resultados/tablas/tabla1.1_estimaciones_crecimientoEconomico_generacionElectrica.txt")

# stargazer(mod3, mod4, mod5, 
#           covariate.labels = c("Renovables",
#                                "Fósiles",
#                                "log(PBI per cápita t-1)",
#                                "Matricula primaria",
#                                "log(Tasa fertilidad)",
#                                "IED",
#                                "Gasto Gobierno",
#                                "Tasa de ahorro",
#                                "Inflación",
#                                "Expec. Vida",
#                                "Comercio"),
#           dep.var.labels = c("Crecimiento PBI per cápita"))

