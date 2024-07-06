
#################################################################################################################################################
# Titulo: Rezago en el consumo eléctrico de Colombia según su PIB per cápita
# Tema: En base a la regresión mostrada en el grafico1.12, esta tabla resume el subsonsumo que presenta Colombia
#       según su nivel de PIB per Capita
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 6 de Julio de 2024                                                                                                                     #
# Base de datos: Para generar esta tabla es necesario correr los scripts complementarios de **baseEnergia.R**
#                y de **descargaBaseEnergia.R**
#################################################################################################################################################

source("rmd/scripts/baseEnergia.R")

baseGraph <- base %>% 
  filter(year == 2019 &
           hdicode != "" & 
           !is.na(hdicode) & 
           !is.na(pib_percapita) & 
           !is.na(per_capita_electricity)) %>% 
  mutate(colombia = ifelse(iso_code == "COL", 2, 1)) %>%
  select(hdicode, per_capita_electricity, pib_percapita, iso_code, population, colombia)

baseGraph$hdicode <- factor(baseGraph$hdicode, levels = c("Low", "Medium",
                                                          "High", "Very High"),
                            labels = c("Bajo", "Medio", "Alto", "Muy Alto"))


Reg1 <- lm(formula = log(per_capita_electricity) ~ log(pib_percapita), 
           data = baseGraph, 
           weights = sqrt(population))

summ_reg1 <- summary(Reg1)

base %>% 
  filter(iso_code == "COL" & year == 2019) %>% 
  select(pib_percapita)

base %>% 
  filter(iso_code == "COL" & year == 2019) %>% 
  select(per_capita_electricity) %>% 
  pull()

summ_reg1$coefficients[1,1] + summ_reg1$coefficients[2,1]*log(6404.109)

1557.164 - exp(summ_reg1$coefficients[1,1] + summ_reg1$coefficients[2,1]*log(6404.109))

abs((1557.164 - exp(summ_reg1$coefficients[1,1] + summ_reg1$coefficients[2,1]*log(6404.109)))/exp(summ_reg1$coefficients[1,1] + summ_reg1$coefficients[2,1]*log(6404.109)))*100

Reg2 <- lm(formula = log(per_capita_electricity) ~ log(pib_percapita), 
           data = baseGraph %>% filter(hdicode != "Bajo"), 
           weights = sqrt(population))

summ_reg2 <- summary(Reg2)

summ_reg2$coefficients[1,1] + summ_reg2$coefficients[2,1]*log(6404.109)

abs((1557.164 - exp(summ_reg2$coefficients[1,1] + summ_reg2$coefficients[2,1]*log(6404.109)))/exp(summ_reg2$coefficients[1,1] + summ_reg2$coefficients[2,1]*log(6404.109)))

1557.164 - exp(summ_reg2$coefficients[1,1] + summ_reg2$coefficients[2,1]*log(6404.109))


tabla1.2 <- tribble(~"Modelo", 
                    ~"Valor Observado", 
                    ~"Valor Estimado", 
                    ~"Diferencia",
                    "Inicial", 
                    str_c(round(base %>% 
                                  filter(iso_code == "COL" & year == 2019) %>% 
                                  select(per_capita_electricity) %>% 
                                  pull(),1)," kWh", sep = ""),
                    str_c(round(exp(summ_reg1$coefficients[1,1] + summ_reg1$coefficients[2,1]*log(6404.109)),1), " kWh", sep = ""),
                    str_c(round(1557.164 - exp(summ_reg1$coefficients[1,1] + summ_reg1$coefficients[2,1]*log(6404.109)),1), " kWh ",
                          "(-", round(abs((1557.164 - exp(summ_reg1$coefficients[1,1] + summ_reg1$coefficients[2,1]*log(6404.109)))/exp(summ_reg1$coefficients[1,1] + summ_reg1$coefficients[2,1]*log(6404.109)))*100, 1), "%)"),
                    "Con corrección de paises con IDH bajo",
                    str_c(round(base %>% 
                                  filter(iso_code == "COL" & year == 2019) %>% 
                                  select(per_capita_electricity) %>% 
                                  pull(),1)," kWh", sep = ""),
                    str_c(round(exp(summ_reg2$coefficients[1,1] + summ_reg2$coefficients[2,1]*log(6404.109)),1), " kWh", sep = ""),
                    str_c(round(1557.164 - exp(summ_reg2$coefficients[1,1] + summ_reg2$coefficients[2,1]*log(6404.109)),1), " kWh ",
                          "(-", round(abs((1557.164 - exp(summ_reg2$coefficients[1,1] + summ_reg2$coefficients[2,1]*log(6404.109)))/exp(summ_reg2$coefficients[1,1] + summ_reg2$coefficients[2,1]*log(6404.109)))*100, 1), "%)"))


writexl::write_xlsx(x = tabla1.2, path = "rmd/resultados/tablas/tabla1.2_subconsumo_electrico_Colombia.xlsx")   

