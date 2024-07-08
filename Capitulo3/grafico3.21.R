
#################################################################################################################################################
# Titulo: Evolución de los precios del kWh en el mercado eléctrico (1995 - 2023)
# Tema: Este script utiliza la información oficial de XM sobre precios de la electricidad en el mercado spot y en contratos.
#       la información esta disponible desde 1995 y hasta 2023, debido a que la información de los precios esta disponible por
#       día por hora, se tomo el promedio mensual para facilitar la visualización.
#
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 8 de Julio de 2024                                                                                                                     
# Base de datos: las bases están disponibles en 'sinergox' de XM, es posible decargarlas via API como se muestra en el archivo
#               **descargaPreciosXM.R**
# 
#################################################################################################################################################

rm(list = ls())

library(tidyverse)
library(readxl)
library(haven)
library(PCAmixdata)
library(FactoMineR)
library(factoextra)
library(GGally)


# Cargamos achivos de precios de electricidad en la bolsa nacional --------

archivos <- list.files(path = "rmd/bases/precios_precipitaciones/", pattern = "Precio*")

archivos <- archivos[-1]

for(i in 1:length(archivos)){
  
  archivos[i] <- str_c("rmd/bases/precios_precipitaciones/", archivos[i], sep = "")
  
}

# archivos <- archivos[2:27] # Eliminamos el archivo que ccorresponde a 1995


# Map para acomodar la tabla en promedios mensuales

precios <- archivos %>% map(.f = ~{
  
  p <- read_excel(.x, sheet = 1)
  
  p <- p %>% 
    mutate(Fecha = as.character.Date(Fecha))
  
  p <- p %>% 
    separate(Fecha, into = c("year", "month", "day"), sep = "-")
  
  p <- p %>% 
    gather(key = hora, value = precio, 4:length(p))
  
  return(p)
  
}) 

precios_mes <- precios %>% 
  map(.f = ~{
    
    p <- .x %>% 
      mutate(year = as.numeric(year),
             month = as.numeric(month)) %>% 
      group_by(year, month) %>% 
      summarise(precioMedio = mean(precio, na.rm = T))
    
  })

precios_mes <- precios_mes %>% 
  bind_rows() %>% 
  na.omit()

# levantamos base de precios de contratos ---------------------------------

base_contratos <- read_excel(path = "rmd/bases/precios_precipitaciones/contratos/Precios_contratos.xlsx")

meses <- unique(base_contratos$Mes)

base_contratos <- base_contratos %>% 
  mutate(month = case_when(
    Mes == meses[1] ~ 1,
    Mes == meses[2] ~ 2,
    Mes == meses[3] ~ 3,
    Mes == meses[4] ~ 4,
    Mes == meses[5] ~ 5,
    Mes == meses[6] ~ 6,
    Mes == meses[7] ~ 7,
    Mes == meses[8] ~ 8,
    Mes == meses[9] ~ 9,
    Mes == meses[10] ~ 10,
    Mes == meses[11] ~ 11,
    Mes == meses[12] ~ 12
  ),
  year = as.numeric(Año))

precios_mes <- precios_mes %>% 
  left_join(y = base_contratos[,c(8,9,10,11,12)], by = c("year", "month"))

nombres <- names(precios_mes)
nombres <- str_replace_all(nombres, pattern = " ", replacement = "_")

names(precios_mes) <- nombres

# Cargamos Indice de precios de BanRep para actualizar los precios de la electricidad
# a DIC-2022

ipc <- read.csv(file = "rmd/bases/precios_precipitaciones/1.3.1.1 IPP_Segun actividad economica_mensual.csv")

ipc <- separate(ipc, Año.mes, into = c("year", "month"), sep = 4)

ipc <- ipc %>% 
  mutate(IPP = as.numeric(str_replace_all(IPP, pattern = "\\,", replacement = "\\.")))

ipc <- ipc[ipc$Índice.descripción == "Oferta Interna" & ipc$Año < 2024 & ipc$Código.clasificación == "T", ]

referencia <- ipc$IPP[1]

ipc <- ipc %>% 
  mutate(year = as.numeric(year),
         month = as.numeric(month))

precios_mes <- precios_mes %>% 
  left_join(y = ipc[,c(2,3,8)], by = c("year" = "year",
                                    "month" = "month"))

precios_mes <- precios_mes %>% 
  ungroup() %>% 
  mutate(across(.cols = 3:6, .fns = ~{.x*referencia/IPP}, .names = "{.col}_constante"))

precio_G <- precios_mes %>% 
  dplyr::select(year, month, ends_with("constante"))

precio_G <- precio_G %>% 
  gather(key = "tipoPrecio", value = "precio", 3:length(precio_G))

s <- as.Date("1995-07-01")
e <- as.Date("2023-12-01")

precio_G$fecha <- rep(as.Date(seq(from = s, to = e, by = "month")), length(unique(precio_G$tipoPrecio)))

tiposPrecio <- unique(precio_G$tipoPrecio)

precio_G <- precio_G %>% 
  mutate(categorica = case_when(
    tipoPrecio %in% c(tiposPrecio[1], tiposPrecio[2]) ~ 1,
    tipoPrecio %in% c(tiposPrecio[3], tiposPrecio[4]) ~ 2
  ),
  tipoPrecio = factor(tipoPrecio,
                      labels = tiposPrecio,
                      levels = tiposPrecio))

colores <- c("#0063AA","#AA2400","#05AA00","#5D00AA")

grafico <- precio_G %>% 
  ggplot() +
  geom_line(mapping = aes(x = fecha, y = precio, color = tipoPrecio, linetype = as.factor(categorica)), show.legend = F) +
  geom_segment(mapping = aes(x = as.Date("2001-01-01"),
                             xend = as.Date("2001-10-01"),
                             y = c(1600),
                             yend = c(1600)),
               linetype = "solid",
               color = colores[1]) +
  geom_segment(mapping = aes(x = as.Date("2001-01-01"),
                             xend = as.Date("2001-10-01"),
                             y = c(1530),
                             yend = c(1530)),
               linetype = "solid",
               color = colores[2]) +
  geom_segment(mapping = aes(x = as.Date("2001-01-01"),
                             xend = as.Date("2001-10-01"),
                             y = c(1460),
                             yend = c(1460)),
               linetype = "twodash",
               color = colores[3]) +
  geom_segment(mapping = aes(x = as.Date("2001-01-01"),
                             xend = as.Date("2001-10-01"),
                             y = c(1390),
                             yend = c(1390)),
               linetype = "twodash",
               color = colores[4]) +
  annotate(geom = "text",
          x = as.Date("1998-01-01"),
          y = c(1600, 1530, 1460, 1390),
          label = c("Precio Spot",
                    "Precio Contratos",
                    "Precio Con. Regulados",
                    "Precio Con. No Regulados"),
          size = 3,
          color = colores) +
  scale_color_manual(values = colores) +
  scale_linetype_manual(values = c("solid", "twodash")) +
  scale_x_date(breaks = as.Date(c("1995-12-01",
                                  "2000-12-01",
                                  "2005-12-01",
                                  "2010-12-01",
                                  "2015-12-01",
                                  "2020-12-01",
                                  "2023-12-01")),
               labels = c("Dic. 1995",
                          "Dic. 2000",
                          "Dic. 2005",
                          "Dic. 2010",
                          "Dic. 2015",
                          "Dic. 2020",
                          "Dic. 2023")) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "Evolución del precio promedio mensual del kWh en el mercado de corto plazo y en contratos de largo plazo",
       subtitle = "Periodo: Julio de 1995 - Diciembre de 2023",
       y = "Precio kWh - pesos constantes a Dic. 2023",
       x = NULL,
       caption = "Fuente: elaboración propia en base a XM") +  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(size=7, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10),
        axis.title.y.right = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")
  
  
ggsave(plot = grafico,
      filename = "rmd/resultados/graficos/grafico3.21_precio_elec_spot_contratos.png",
      dpi = 500,
      width = 10.8,
      height = 6)
