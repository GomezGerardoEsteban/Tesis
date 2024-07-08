
#################################################################################################################################################
# Titulo: Evolución de la concentración en la generación de electricidad
# Tema: Este script utiliza la información oficial de XM sobre generación de electricidad por empresa generadora para mostrar tres cosas:
#       - El número de empresas generadoras a lo largo del tiempo
#       - La participación de las primeras 10 y las primeras 4 empresas en la energía total generada
#       - La evolución del indice de Herfindahl-Hirschmann en el que es posible observar que el grado de concentración
#         no ha tenido ninguna variación significativa a lo largo de estos años.
#
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 8 de Julio de 2024                                                                                                                     
# Base de datos: La información utilizada proviene de 'sinergox' pagina oficial de XM para gestionar
#                la información de precios y cantidades desde la apertura del mercado en 1995.
# 
#################################################################################################################################################


rm(list=ls())

library(tidyverse)
library(readxl)

base_generadores <- readxl::read_excel("rmd/bases/precios_precipitaciones/contratos/resumen_oferta_generacion_empresas.xlsx")

years <- 1995:2023
months <- 1:12

years <- sort(rep(years, 12))
years <- years[-c(1:6)]

months <- rep(months, 348/12)
months <- months[-c(1:6)]

base_generadores$mes <- as.numeric(base_generadores$month)
years <- as.character(years)

n_empresas <-  map2(.x = years, .y = months, .f = ~{
  
  p <- base_generadores %>% 
    filter(year == .x, mes == .y) %>% 
    dplyr::select(year, mes, codigoAgente) %>% 
    distinct(codigoAgente) 
  
  p <- p %>% 
    mutate(n_empresas = nrow(p),
           year = .x,
           month = .y)
  
  return(p)
  
})


n_empresas <- n_empresas %>% bind_rows()

n_empresas <- n_empresas %>% 
  mutate(month = ifelse(nchar(month) == 1, str_c("0",month,sep=""), month),
         Fecha = as.Date(str_c(year, month, "01", sep = "-")))

grafico_numero_empresas_Gen <- n_empresas %>% 
  distinct(n_empresas, Fecha) %>% 
  ggplot() +
  geom_line(mapping = aes(x = as.Date(Fecha), y = n_empresas), linewidth = 0.8) +
  geom_text(data = n_empresas %>% 
              filter(n_empresas == min(n_empresas) | n_empresas == max(n_empresas)) %>% 
              distinct(Fecha, n_empresas) %>% 
              filter(Fecha == "1995-07-01" | Fecha == "2023-12-01"), 
            mapping = aes(x = as.Date(Fecha), y = n_empresas, label = n_empresas),
            nudge_x = c(-200,200)) +
  scale_y_continuous(n.breaks = 15) +
  scale_x_date(breaks = as.Date(seq(from = as.Date("1995-07-01"), to = as.Date("2023-12-01"), by = "year")),
               labels = as.character(1995:2023)) +
  labs(title = "Número de empresas generadoras",
       subtitle = "Periodo: Enero del 1995 - Diciembre del 2023",
       y = "Número de empresas",
       x = NULL,
       caption = "Fuente: elaboración propia en base a XM") +  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(size=9, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 60, vjust = 0.5),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10),
        axis.title.y.right = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

ggsave(plot = grafico_numero_empresas_Gen, 
       filename = "rmd/resultados/graficos/grafico3.24.1_numero_empresas_Gen.png", 
       units = 'in', 
       width = 9.85,
       height = 4.84,
       dpi = 300)

ranking_generador <-  
  map2(.x = years, .y = months, .f = ~{
    
    p <- base_generadores %>% 
      filter(year == .x, mes == .y) %>% 
      distinct(codigoAgente, prop) %>% 
      arrange(desc(prop)) %>% 
      mutate(year = .x,
             month = .y)
    
    p <- p[1:10, ]
    
    p <- p %>% 
      mutate(ranking = 1:10,
             top4 = ifelse(ranking <= 4, 1, 0))
    
    return(p)
    
  })

ranking_generador <- ranking_generador %>% bind_rows()

ranking_generador <- ranking_generador %>% 
  mutate(month = ifelse(nchar(month) == 1, str_c("0", month, sep=""), month),
         Fecha = as.Date(str_c(year, month, "01", sep = "-")))

ranking_generador <- ranking_generador %>% 
  group_by(Fecha) %>% 
  mutate(totPart = sum(prop, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Fecha, top4) %>% 
  mutate(totPart2 = sum(prop, na.rm = T))


grafico_participacion_empresas_Gen <- ranking_generador %>% 
  ggplot() +
  geom_line(mapping = aes(x = as.Date(Fecha), y = totPart), linetype = "solid", linewidth = 1.2) +
  geom_line(data = ranking_generador %>% filter(top4 == 1),
            mapping = aes(x = as.Date(Fecha), y = totPart2), linetype = "longdash", linewidth = 1) +
  scale_y_continuous(n.breaks = 10, limits = c(0,100)) +
  scale_x_date(breaks = as.Date(seq(from = as.Date("1995-07-01"), to = as.Date("2023-12-01"), by = "year")),
               labels = as.character(1995:2023),
               limits = as.Date(c("1995-01-01", "2028-01-01"))) +
  annotate(geom = "text",
           x = as.Date("2026-12-01"),
           y = c(ranking_generador$totPart[3420],
                 ranking_generador$totPart2[3411]),
           label = c(paste("Participación 10 primeras\nEmpresas", round(ranking_generador$totPart[3420],1), "%", sep = " "),
                     paste("Participación 4 primeras\nEmpresas", round(ranking_generador$totPart2[3411],1), "%", sep = " ")),
           size = 3.5) +
  labs(title = "Participación de las principales empresas generadoras en la electricidad total generada",
       subtitle = "Periodo: Enero del 1995 - Diciembre del 2023",
       y = "Porcentaje",
       x = NULL,
       caption = "Fuente: elaboración propia en base a XM") +  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(size=9, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 60, vjust = 0.5),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10),
        axis.title.y.right = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

ggsave(plot = grafico_participacion_empresas_Gen, 
       filename = "rmd/resultados/graficos/grafico3.24.2_participacion_generadoras.png", 
       units = 'in', 
       width = 11.5,
       height = 5.01,
       dpi = 300)


rm(list=ls())

# library(tidyverse)
# library(readxl)

archivos_generacion <- list.files(path = "rmd/bases/precios_precipitaciones/", pattern = "Generacion*")

generacion <- list()

resumenes <- list()


for(i in 1:length(archivos_generacion)){
  
  archivos_generacion[i] <- str_c("rmd/bases/precios_precipitaciones/", archivos_generacion[i], sep = "") 
  
  generacion[[i]] <- read_excel(path = archivos_generacion[i])
  
}

for(i in 1:length(archivos_generacion)){
  
  generacion[[i]] <- generacion[[i]] %>% 
    gather(key = hora, value = kwh, (length(generacion[[i]])-23):length(generacion[[i]]))
  
  
  generacion[[i]] <- generacion[[i]]  %>% 
    mutate(Fecha = as.character.Date(Fecha)) %>% 
    separate(Fecha, into = c("year", "month", "day"), sep = "-")
  
  if(("Código Agente" %in% names(generacion[[i]])) & ("Tipo Generación" %in% names(generacion[[i]]))){
    
    resumenes[[i]] <- generacion[[i]] %>%
      group_by_at(.vars = c("year", "month", "Código Agente", "Tipo Generación")) %>% 
      summarise(genaracion_kWh = sum(kwh, na.rm = T)) %>% 
      ungroup() %>% 
      rename_at(.vars = "Tipo Generación", .funs = ~{"tipoGeneracion"}) %>% 
      rename(codigoAgente = `Código Agente`)
    
    
  }else{
    
    if(("Código Agente" %in% names(generacion[[i]])) & ("Tipo Generacion" %in% names(generacion[[i]]))){
      
      resumenes[[i]] <- generacion[[i]] %>%
        group_by_at(.vars = c("year", "month", "Código Agente", "Tipo Generacion")) %>% 
        summarise(genaracion_kWh = sum(kwh, na.rm = T)) %>% 
        ungroup() %>% 
        rename_at(.vars = "Tipo Generacion", .funs = ~{"tipoGeneracion"}) %>% 
        rename(codigoAgente = `Código Agente`)
    }
    
    else{
      
      if(("Codigo Agente" %in% names(generacion[[i]])) & ("Tipo Generación" %in% names(generacion[[i]]))){
        
        resumenes[[i]] <- generacion[[i]] %>%
          group_by_at(.vars = c("year", "month", "Codigo Agente", "Tipo Generación")) %>% 
          summarise(genaracion_kWh = sum(kwh, na.rm = T)) %>% 
          ungroup() %>% 
          rename_at(.vars = "Tipo Generación", .funs = ~{"tipoGeneracion"}) %>% 
          rename(codigoAgente = `Codigo Agente`)
        
        
      }else{
        
        if(("Codigo Agente" %in% names(generacion[[i]])) & ("Tipo Generacion" %in% names(generacion[[i]]))){
          
          resumenes[[i]] <- generacion[[i]] %>%
            group_by_at(.vars = c("year", "month", "Codigo Agente", "Tipo Generacion")) %>% 
            summarise(genaracion_kWh = sum(kwh, na.rm = T)) %>% 
            ungroup() %>% 
            rename_at(.vars = "Tipo Generacion", .funs = ~{"tipoGeneracion"}) %>% 
            rename(codigoAgente = `Codigo Agente`)
          
          
        }
      }
    }
  }
}


resumenes <- resumenes %>% bind_rows()

resumenes <- resumenes %>%
  mutate(Fecha = as.Date(str_c(year, month, "01", sep = "-"))) %>% 
  group_by(Fecha, codigoAgente) %>% 
  mutate(totAgente = sum(genaracion_kWh, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  mutate(totPeriodo = sum(genaracion_kWh, na.rm = T),
         prop = totAgente/totPeriodo*100,
         propCuadrado = prop^2,
         indice_HH = sum(propCuadrado, na.rm = T))

years <- 1995:2023
months <- 1:12

years <- sort(rep(years, 12))
years <- years[-c(1:6)]

months <- rep(months, 348/12)
months <- months[-c(1:6)]

resumenes$mes <- as.numeric(resumenes$month)
years <- as.character(years)

calculo_IHH <- map2(.x = years, .y = months, .f = ~{
  
  resumenes %>% 
    filter(year == .x, mes == .y) %>% 
    distinct(codigoAgente, prop)
  
})

calculo_IHH <- calculo_IHH %>% bind_rows()

calculo_IHH <- calculo_IHH %>% 
  mutate(propCuadrado = prop^2) %>% 
  group_by(Fecha) %>% 
  mutate(IHH = sum(propCuadrado, na.rm = T))

grafico_HH_generacion <- calculo_IHH %>% 
  ggplot() +
  geom_line(mapping = aes(x = as.Date(Fecha), y = IHH), linewidth = 1.2) +
  geom_hline(mapping = aes(yintercept = 1500),
             color = "darkblue", alpha = 0.8, linetype = "twodash") +
  geom_hline(mapping = aes(yintercept = 2500),
             color = "darkred", alpha = 0.8, linetype = "twodash") +
  scale_y_continuous(n.breaks = 10, limits = c(0,3000)) +
  scale_x_date(breaks = as.Date(seq(from = as.Date("1995-07-01"), to = as.Date("2023-12-01"), by = "year")),
               labels = as.character(1995:2023)) +
  annotate(geom = "text",
           x = as.Date(c("2000-12-01", "2000-12-01", "2000-12-01")),
           y = c(200,2200,2700),
           label = c("Mercado competitivo (IHH < 1500)",
                     "Mercado moderadamente concentrado (1500 < IHH < 2500)",
                     "Mercado altamente concentrado (IHH > 2500)"),
           color = c("darkgreen", "darkblue", "darkred"),
           alpha = 0.7,
           size = 3) +
  labs(title = "Indice de Herfindahl-Hirschman (IHH) en la generación de electricidad",
       subtitle = "Periodo: Enero del 2000 - Diciembre del 2023",
       y = "IHH",
       x = NULL,
       caption = "Fuente: elaboración propia en base a XM") +  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(size=9, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 60, vjust = 0.5),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10),
        axis.title.y.right = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

grafico_HH_generacion

ggsave(plot = grafico_HH_generacion, 
       filename = "rmd/resultados/graficos/grafico3.24.3_HH_generacion.png", 
       units = 'in', 
       width = 11,
       height = 5.01,
       dpi = 300)

