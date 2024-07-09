
###################################################################################################################################################################
# Titulo: Cambio en el aporte de energía eléctrica de plantas no solares
# Tema: Este grafico compara el aporte actual de las fuentes distintas a las solares y el aporte esperado con el incremento en la capacidad
#       solar, lo que da lugar a la conocida curva de pato.
#
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 9 de Julio de 2024                                                                                                                     
# Base de datos: Los datos para realizar estos calculos fueron organizados en el documento **escenarios_planIndicativoExpansion_UPME_2023.xlsx**
#                el cual toma los valores del documento oficial reseñado en la descripción y **cambio_esperado_capacidad_Instalada.csv**.
#################################################################################################################################################

rm(list = ls())

library(tidyverse)
library(readxl)

list_generacion <- list.files(path = "rmd/bases/precios_precipitaciones/capacidad_generacion_19_23", pattern = "Generacion*")
list_capacidad <- list.files(path = "rmd/bases/precios_precipitaciones/capacidad_generacion_19_23", pattern = "Capacidad*")

bases_g <- list()
bases_c <- list()

for(i in 1:5){
  
  bases_g[[i]] <- readxl::read_excel(path = str_c("rmd/bases/precios_precipitaciones/capacidad_generacion_19_23/", list_generacion[i], sep = ""))
  bases_c[[i]] <- readxl::read_excel(path = str_c("rmd/bases/precios_precipitaciones/capacidad_generacion_19_23/", list_capacidad[i], sep = ""))
  
}


bases_ge <- list()
bases_ca <- list()
for(i in 1:5){
  
  bases_g[[i]] <- bases_g[[i]] %>% 
    gather(key = hora, value = generacion, 10:length(bases_g[[i]]))
  
  bases_g[[i]] <- bases_g[[i]] %>% 
    mutate(Fecha = as.character(Fecha),
           Combustible = ifelse(Combustible == "GAS NI", "GAS", 
                                ifelse(Combustible %in% c("COMBUSTOLEO",
                                                          "ACPM",
                                                          "MEZCLA GAS - JET-A1",
                                                          "JET-A1",
                                                          "CRUDO"), "PETROLEO", 
                                       ifelse(Combustible %in% c("BIOGAS", "BAGAZO"), "BIOENERGIA", Combustible))))
  
  
  bases_c[[i]] <- bases_c[[i]] %>% 
    mutate(Combustible = ifelse(`Combustible por defecto` == "GAS NI", "GAS", 
                                ifelse(`Combustible por defecto` %in% c("COMBUSTOLEO",
                                                                        "ACPM",
                                                                        "MEZCLA GAS - JET-A1",
                                                                        "JET-A1",
                                                                        "CRUDO"), "PETROLEO", 
                                       ifelse(`Combustible por defecto` %in% c("BIOGAS", "BAGAZO"), "BIOENERGIA", `Combustible por defecto`))))
  
  r <- unique(bases_g[[i]]$Recurso[bases_g[[i]]$Combustible == "RAD SOLAR"]) %in% unique(bases_c[[i]]$Recurso[bases_c[[i]]$Combustible == "RAD SOLAR"])
  
  p <- tibble(var = unique(bases_g[[i]]$Recurso[bases_g[[i]]$Combustible == "RAD SOLAR"]),
              ind = r)
  
  p <- p %>% 
    filter(ind == T) %>% 
    select(var) %>% 
    pull()
  
  
  bases_g[[i]] <- bases_g[[i]] %>% 
    mutate(solares = ifelse(Combustible != "RAD SOLAR", 1,
                            ifelse(Combustible == "RAD SOLAR" & Recurso %in% p, Recurso, 0)))
  
  bases_ge[[i]] <- bases_g[[i]] %>% 
    filter(solares != 0) %>% 
    group_by(Fecha, Combustible, hora) %>% 
    summarise(generacion = sum(generacion, na.rm = T)) %>% 
    ungroup()
  
  bases_c[[i]] <- bases_c[[i]] %>% 
    mutate(solares = ifelse(Combustible != "RAD SOLAR", 1,
                            ifelse(Combustible == "RAD SOLAR" & Recurso %in% p, Recurso, 0)))
  
  
  bases_ca[[i]] <- bases_c[[i]] %>% 
    filter(solares != 0) %>% 
    group_by(Fecha, Combustible) %>% 
    summarise(capacidad = sum(`Capacidad Efectiva Neta kW`, na.rm = T)) %>% 
    ungroup()
  
  bases_ge[[i]] <- bases_ge[[i]] %>% 
    left_join(y = bases_ca[[i]], by = c("Fecha" = "Fecha",
                                        "Combustible" = "Combustible"))
  
}

bases_ge <- bases_ge %>% 
  map(.f = ~{
    
    .x %>% 
      mutate(factor_utilizacion = generacion/capacidad)
    
  })


bases_ge <- bases_ge %>% 
  map(.f = ~{
    
    .x <- .x %>% 
      separate(Fecha, into = c("year", "month", "day"))
    
  })

bases_ge_df <- bases_ge %>% bind_rows()

combustibles <- unique(bases_ge_df$Combustible)

base_fp <- tibble(combustible = combustibles,
                  fp = c(
                    mean(bases_ge_df$factor_utilizacion[bases_ge_df$Combustible == combustibles[1]]),
                    mean(bases_ge_df$factor_utilizacion[bases_ge_df$Combustible == combustibles[2]]),
                    mean(bases_ge_df$factor_utilizacion[bases_ge_df$Combustible == combustibles[3]]),
                    mean(bases_ge_df$factor_utilizacion[bases_ge_df$Combustible == combustibles[4]]),
                    mean(bases_ge_df$factor_utilizacion[bases_ge_df$Combustible == combustibles[5]]),
                    mean(bases_ge_df$factor_utilizacion[bases_ge_df$Combustible == combustibles[6]]),
                    mean(bases_ge_df$factor_utilizacion[bases_ge_df$Combustible == combustibles[7]])
                  ))

# writexl::write_xlsx(x = base_fp, path = "rmd/resultados/tablas/factorPlanta_fuente.xlsx")

bases_ge_dia <- bases_ge_df %>%
  group_by(Combustible, hora) %>% 
  summarise(mean = mean(factor_utilizacion, na.rm = T),
            min = min(factor_utilizacion, na.rm = T),
            max = max(factor_utilizacion, na.rm = T)) 

# Archivos de demanda  ----------------------------------------------------

lista_Demanda <- list.files(path = "rmd/bases/precios_precipitaciones/demandaComercialCIIU/demandaComercializador/demanda_19_23")
bases_d <- list()

for(i in 1:length(lista_Demanda)){
  
  lista_Demanda[i] <- str_c("rmd/bases/precios_precipitaciones/demandaComercialCIIU/demandaComercializador/demanda_19_23/", 
                            lista_Demanda[i], 
                            sep = "")
  
  bases_d[[i]] <- read_excel(path = lista_Demanda[i])
  
}


res_tipo_ususario <- bases_d %>% map(.f = ~{
  
  names(.x) <- .x[1, ]
  
  .x <- .x[-1,-length(.x)]
  
  .x <- .x %>% 
    gather(key = hora, value = valor, 4:length(.x))
  
  if(nchar(.x[1,1]) == 5){
    
    .x <- .x %>% 
      mutate(Fecha = as.Date(as.numeric(Fecha), origin = "1899-12-30"))
    
    .x <- .x %>% 
      mutate(Fecha = as.character.Date(Fecha),
             valor = as.numeric(valor))
    
    .x <- .x %>% 
      rename(comercializador = `Codigo Comercializador`)
    
    .x <- .x %>% 
      separate(Fecha, into = c("year", "month", "day"), sep = "-")
    
  }else{
    
    .x <- .x %>% 
      mutate(Fecha = as.character.Date(Fecha),
             valor = as.numeric(valor))
    
    .x <- .x %>% 
      rename(comercializador = `Codigo Comercializador`)
    
    .x <- .x %>% 
      separate(Fecha, into = c("year", "month", "day"), sep = "-")
    
  }
}
)


res_tipo_ususario <- res_tipo_ususario %>% 
  map(.f = ~{
    
    .x <- .x %>% 
      group_by(year, month, day, hora) %>% 
      summarise(demanda = sum(valor, na.rm = T))
    
    
  })


res_tipo_ususario <- res_tipo_ususario %>% bind_rows()
res_tipo_ususario <- res_tipo_ususario %>% 
  mutate(Fecha = as.Date(str_c(year, month, day, sep = "-")))

res_tipo_ususario$hora <- factor(res_tipo_ususario$hora,
                                 labels = as.character(0:23),
                                 levels = as.character(0:23))

res_tipo_ususario %>% 
  # filter(Fecha < "2020-01-01") %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = hora, y = demanda))


res_tipo_ususario <- res_tipo_ususario %>% 
  group_by(Fecha) %>% 
  mutate(totalDemanda_dia = sum(demanda, na.rm = T))

proporciones <- res_tipo_ususario %>% 
  ungroup() %>% 
  mutate(proporciones = demanda/totalDemanda_dia) %>% 
  group_by(hora) %>% 
  summarise(mean = mean(proporciones),
            min = min(proporciones),
            max = max(proporciones))


res_tipo_ususario[res_tipo_ususario$year == 2023,  ]

# calculo de generacion ---------------------------------------------------

base_cambio <- read.csv(file = "rmd/bases/cambio_esperado_capacidad_Instalada.csv")

base_fp$combustible_ <- case_when(
  base_fp$combustible == combustibles[1] ~ "Hidroeléctrica",
  base_fp$combustible == combustibles[2] ~ "Bioenergía",
  base_fp$combustible == combustibles[3] ~ "Carbón",
  base_fp$combustible == combustibles[4] ~ "Gas",
  base_fp$combustible == combustibles[5] ~ "Petróleo",
  base_fp$combustible == combustibles[6] ~ "Solar",
  base_fp$combustible == combustibles[7] ~ "Eólica"
)

base_cambio <- left_join(x = base_cambio, y = base_fp, by = c("combustible" = "combustible_"))
base_cambio$fp[2] <- 0.40

base_cambio$generacion_futura <- (base_cambio$futura*base_cambio$fp*24*365)/1000

sum(base_cambio$generacion_futura) # Generación hipótetica del 2032

sum(bases_ge_df$generacion[bases_ge_df$year == 2019])/1000000 # Generación del 2019



bases_ge_dia <- bases_ge_dia %>% 
  mutate(Combustible_ = case_when(
    Combustible == "AGUA" ~ "Hidroeléctrica",
    Combustible == "BIOENERGIA" ~ "Bioenergía",
    Combustible == "RAD SOLAR" ~ "Solar",
    Combustible == "VIENTO" ~ "Eólica",
    Combustible == "GAS" ~ "Gas",
    Combustible == "CARBON" ~ "Carbón",
    Combustible == "PETROLEO" ~ "Petróleo"))

bases_ge_dia <- bases_ge_dia %>% 
  left_join(y = base_cambio %>% 
              select(combustible, renovable, futura, total), by = c("Combustible_" = "combustible"))

bases_ge_dia <- bases_ge_dia %>% 
  mutate(futura_kw = futura*1000,
         actual_kw = total*1000)

bases_ge_dia <- bases_ge_dia %>% 
  mutate(oferta_kWh = futura_kw*mean,
         oferta_actua_kwh = actual_kw*mean)


# calculo de demanda

base_demanda_upme <- read_excel(path = "rmd/bases/demandaAnualUPME_2023_2037.xlsx", sheet = 2)

base_demanda_upme <- base_demanda_upme %>% 
  mutate(across(.cols = 2:6, .fns = ~{.x*1000000/365}, .names = "{col}_kwh_dia"))

proporciones <- proporciones %>% 
  mutate(escenario_2032 = base_demanda_upme$Esc.Medio_kwh_dia[base_demanda_upme$year == 2032],
         escenario_2037 = base_demanda_upme$Esc.Medio_kwh_dia[base_demanda_upme$year == 2037])

proporciones <- proporciones %>% 
  mutate(escenario_dia_2032 = escenario_2032*mean,
         escenario_dia_2037 = escenario_2037*mean)


bases_ge_dia <- bases_ge_dia %>% 
  left_join(y = proporciones %>% 
              select(hora, escenario_dia_2032, escenario_dia_2037), by = c("hora" = "hora"))

demand_2023 <- res_tipo_ususario[res_tipo_ususario$year == 2023 & res_tipo_ususario$month == 12 & res_tipo_ususario$day == 18, ]

demand_2023$hora <- as.character(demand_2023$hora)


bases_ge_dia$demanda_23 <- case_when(
  bases_ge_dia$hora == "0" ~ demand_2023$demanda[1],
  bases_ge_dia$hora == "1" ~ demand_2023$demanda[2],
  bases_ge_dia$hora == "10" ~ demand_2023$demanda[3],
  bases_ge_dia$hora == "11" ~ demand_2023$demanda[4],
  bases_ge_dia$hora == "12" ~ demand_2023$demanda[5],
  bases_ge_dia$hora == "13" ~ demand_2023$demanda[6],
  bases_ge_dia$hora == "14" ~ demand_2023$demanda[7],
  bases_ge_dia$hora == "15" ~ demand_2023$demanda[8],
  bases_ge_dia$hora == "16" ~ demand_2023$demanda[9],
  bases_ge_dia$hora == "17" ~ demand_2023$demanda[10],
  bases_ge_dia$hora == "18" ~ demand_2023$demanda[11],
  bases_ge_dia$hora == "19" ~ demand_2023$demanda[12],
  bases_ge_dia$hora == "2" ~ demand_2023$demanda[13],
  bases_ge_dia$hora == "20" ~ demand_2023$demanda[14],
  bases_ge_dia$hora == "21" ~ demand_2023$demanda[14],
  bases_ge_dia$hora == "22" ~ demand_2023$demanda[16],
  bases_ge_dia$hora == "23" ~ demand_2023$demanda[17],
  bases_ge_dia$hora == "3" ~ demand_2023$demanda[18],
  bases_ge_dia$hora == "4" ~ demand_2023$demanda[19],
  bases_ge_dia$hora == "5" ~ demand_2023$demanda[20],
  bases_ge_dia$hora == "6" ~ demand_2023$demanda[21],
  bases_ge_dia$hora == "7" ~ demand_2023$demanda[22],
  bases_ge_dia$hora == "8" ~ demand_2023$demanda[23],
  bases_ge_dia$hora == "9" ~ demand_2023$demanda[24]
)


bases_ge_dia$hora <- factor(bases_ge_dia$hora,
                            levels = as.character(0:23),
                            labels = as.character(0:23))


bases_ge_dia$Combustible_ <- factor(bases_ge_dia$Combustible_,
                                    levels = rev(c("Solar", "Eólica", "Hidroeléctrica", "Bioenergía", "Gas", "Carbón", "Petróleo")),
                                    labels = rev(c("Solar", "Eólica", "Hidroeléctrica", "Bioenergía", "Gas", "Carbón", "Petróleo")))


base_curvaPato <- bases_ge_dia[bases_ge_dia$Combustible_ %in% c("Solar"), ]


base_curvaPato <- base_curvaPato %>% 
  mutate(demanda_exc = escenario_dia_2032 - oferta_kWh,
         demanda_exc_actual = demanda_23 - oferta_actua_kwh)

base_curvaPato$demanda_exc <- ifelse(base_curvaPato$demanda_exc < 0, 0, base_curvaPato$demanda_exc)
base_curvaPato$demanda_exc_actual <- ifelse(base_curvaPato$demanda_exc_actual < 0, 0, base_curvaPato$demanda_exc_actual)

base_curvaPato$ind_exc <- base_curvaPato$demanda_exc*100/base_curvaPato$demanda_exc[1]
base_curvaPato$ind_exc_actual <- base_curvaPato$demanda_exc_actual*100/base_curvaPato$demanda_exc_actual[1]


# base_curvaPato$demanda_exc_low <- ifelse(base_curvaPato$demanda_exc_low < 0, 0, base_curvaPato$demanda_exc_low)
# base_curvaPato$demanda_exc_high <- ifelse(base_curvaPato$demanda_exc_high < 0, 0, base_curvaPato$demanda_exc_high)

base_curvaPato$rampa <- ifelse(base_curvaPato$hora %in% as.character(13:19), 1, 0)

grafico_curvaPato <- base_curvaPato %>% 
  mutate(hora = as.numeric(hora)) %>% 
  ggplot() +
  # geom_line(mapping = aes(x = hora, y = escenario_dia_2032/1000000+0.2), color = "red", linewidth = 1.2) +
  geom_line(mapping = aes(x = hora, y = ind_exc), color = "darkblue", linewidth = 1) +
  geom_point(mapping = aes(x = hora, y = ind_exc, shape = as.factor(rampa)), color = "darkblue", size = 2, show.legend = F) +
  geom_line(mapping = aes(x = hora, y = ind_exc_actual), color = "darkgreen", linewidth = 1) +
  # geom_point(mapping = aes(x = hora, y = ind_exc_actual), color = "darkgreen", size = 2, shape = 17) +
  geom_text(data = tibble(x = c(25.5,25.5),
                          y = c(base_curvaPato$ind_exc[base_curvaPato$hora == 23],
                                base_curvaPato$ind_exc_actual[base_curvaPato$hora == 23]),
                          label = c("Comportamiento\nFuturo","Comportamiento\nActual")),
            mapping = aes(x = x, y = y, label = label, color = label), 
            show.legend = F,
            size = 3) +
  scale_color_manual(values = c("darkgreen", "darkblue")) +
  scale_shape_manual(values = c(NA, 17)) +
  scale_x_continuous(breaks = 1:24,
                     labels = 0:23,
                     limits = c(0,26)) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "Aporte eléctrico de fuentes distintas a la energía solar en el sistema eléctrico colombiano",
       x = "Hora",
       y = "Indice de aporte, 0 horas = 100",
       caption = "Fuente: Elaboración propia en base a UPME y XM")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_markdown(hjust = 0.5, size = 10.5),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


ggsave(filename = "rmd/resultados/graficos/grafico5.35_curva_pato.png",
       plot = grafico_curvaPato,
       dpi = 500,
       width = 8,
       height = 4.82)