
#################################################################################################################################################
# Titulo: Graficos 4 etapa del sector eléctrico colombiano
# Tema: Este script contiene los gráficos 3.27, 3.28 y 3.29 de la tesis, son gráficos a partir de los cuales busco justificar
#       que es posible que en Colombia el sector eléctrico este entrando a una nueva etapa marcada por la autogeneración y por 
#       la generación solar y eólica
#
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 8 de Julio de 2024                                                                                                                     
# Base de datos: La base a utilizar provienen de 'sinergox', pagina oficial de 
#                XM para gestionar la información historica del sector eléctrico.
#################################################################################################################################################


# limpieza de ambiente ----------------------------------------------------

rm(list = ls())

# paquetes ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(patchwork)
library(waffle)

# Datos de capacidad de generación de XM ----------------------------------

lista_bases <- list.files(path = "rmd/bases/precios_precipitaciones/", pattern = "Capacidad*")

bases <- list()
for(i in 1:length(lista_bases)){
  
  lista_bases[i] <- str_c(getwd(), "/rmd/bases/precios_precipitaciones/", lista_bases[i], sep = "")
  
  bases[[i]] <- read_excel(path = lista_bases[i])
  
}

dimensiones <- c()
nombres_columnas <- list()

for(i in 1:26){
  
  dimensiones[i] <- dim(bases[[i]])[2]
  nombres_columnas[[i]] <- names(bases[[i]])
}



capacidad_al_fin <- list()
for(i in 1:26){
  
  p <- bases[[i]] %>% 
    separate(Fecha, into = c("year", "mes", "dia"), sep = "-")
  
  p <- p %>% 
    filter(mes == "12" & dia == "31") 
  
  nombres <- names(p)
  
  for(j in 1:length(nombres)){
    
    nombres[j] <- str_replace_all(nombres[j], " ", "_")
    
  }
  
  names(p) <- nombres
  
  capacidad_al_fin[[i]] <- p
  
}

vector_combustibles <- capacidad_al_fin %>% map(.f = ~{
  
  p <- .x %>% 
    dplyr::select(Combustible_por_defecto) %>% 
    distinct()
  
  return(p)
  
})

vector_combustibles <- vector_combustibles %>% bind_rows() %>% distinct() %>% pull()

years <- 2000:2023

capacidad_al_fin[[19]] <- NULL
capacidad_al_fin[[17]] <- NULL

r_capacidad_al_fin <- list()

for(i in 1:24){
  
  if("Capacidad_Efectiva_Neta_kW" %in% names(capacidad_al_fin[[i]])){
    
    r_capacidad_al_fin[[i]] <- capacidad_al_fin[[i]] %>% 
      mutate(combustible = case_when(Combustible_por_defecto == vector_combustibles[1] ~ "Hidroeléctrica",
                                     Combustible_por_defecto == vector_combustibles[2] ~ "Gas",
                                     Combustible_por_defecto == vector_combustibles[3] ~ "Petróleo",
                                     Combustible_por_defecto == vector_combustibles[4] ~ "Bioenergía",
                                     Combustible_por_defecto == vector_combustibles[5] ~ "Carbón",
                                     Combustible_por_defecto == vector_combustibles[6] ~ "Eólica",
                                     Combustible_por_defecto == vector_combustibles[7] ~ "Bioenergía",
                                     Combustible_por_defecto == vector_combustibles[8] ~ "Petróleo",
                                     Combustible_por_defecto == vector_combustibles[9] ~ "Petróleo",
                                     Combustible_por_defecto == vector_combustibles[10] ~ "Petróleo",
                                     Combustible_por_defecto == vector_combustibles[11] ~ "Petróleo",
                                     Combustible_por_defecto == vector_combustibles[12] ~ "Bioenergía",
                                     Combustible_por_defecto == vector_combustibles[13] ~ "Solar"
      )) %>% 
      mutate(combustible = factor(combustible,
                                  levels = c("Solar", "Eólica", "Bioenergía",
                                             "Gas", "Petróleo", "Carbón",
                                             "Hidroeléctrica"),
                                  labels = c("Solar", "Eólica", "Bioenergía",
                                             "Gas", "Petróleo", "Carbón",
                                             "Hidroeléctrica"))) %>% 
      group_by(combustible) %>% 
      summarise(n = n(),
                media = mean(Capacidad_Efectiva_Neta_kW, na.rm = T),
                desvEst = sd(Capacidad_Efectiva_Neta_kW, na.rm = T),
                total = sum(Capacidad_Efectiva_Neta_kW, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(prop = total/sum(total),
             year = years[i],
             renovable = factor(case_when(
               combustible == "Solar" ~ 0,
               combustible == "Eólica" ~ 0,
               combustible == "Bioenergía" ~ 0,
               combustible == "Gas" ~ 1,
               combustible == "Petróleo" ~ 1,
               combustible == "Carbón" ~ 1,
               combustible == "Hidroeléctrica" ~ 2)))
  }else{
    
    r_capacidad_al_fin[[i]] <- capacidad_al_fin[[i]] %>% 
      mutate(combustible = case_when(Combustible_por_defecto == vector_combustibles[1] ~ "Hidroeléctrica",
                                     Combustible_por_defecto == vector_combustibles[2] ~ "Gas",
                                     Combustible_por_defecto == vector_combustibles[3] ~ "Petróleo",
                                     Combustible_por_defecto == vector_combustibles[4] ~ "Bioenergía",
                                     Combustible_por_defecto == vector_combustibles[5] ~ "Carbón",
                                     Combustible_por_defecto == vector_combustibles[6] ~ "Eólica",
                                     Combustible_por_defecto == vector_combustibles[7] ~ "Bioenergía",
                                     Combustible_por_defecto == vector_combustibles[8] ~ "Petróleo",
                                     Combustible_por_defecto == vector_combustibles[9] ~ "Petróleo",
                                     Combustible_por_defecto == vector_combustibles[10] ~ "Petróleo",
                                     Combustible_por_defecto == vector_combustibles[11] ~ "Petróleo",
                                     Combustible_por_defecto == vector_combustibles[12] ~ "Bioenergía",
                                     Combustible_por_defecto == vector_combustibles[13] ~ "Solar"
      )) %>%  
      mutate(combustible = factor(combustible,
                                  levels = c("Solar", "Eólica", "Bioenergía",
                                             "Gas", "Petróleo", "Carbón",
                                             "Hidroeléctrica"),
                                  labels = c("Solar", "Eólica", "Bioenergía",
                                             "Gas", "Petróleo", "Carbón",
                                             "Hidroeléctrica"))) %>% 
      group_by(combustible) %>% 
      summarise(n = n(),
                media = mean(Capacidad_Efectiva_Neta, na.rm = T),
                desvEst = sd(Capacidad_Efectiva_Neta, na.rm = T),
                total = sum(Capacidad_Efectiva_Neta, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(prop = total/sum(total),
             year = years[i],
             renovable = factor(case_when(
               combustible == "Solar" ~ 0,
               combustible == "Eólica" ~ 0,
               combustible == "Bioenergía" ~ 0,
               combustible == "Gas" ~ 1,
               combustible == "Petróleo" ~ 1,
               combustible == "Carbón" ~ 1,
               combustible == "Hidroeléctrica" ~ 2)))
    
  }
  
}

r_capacidad_al_fin <- r_capacidad_al_fin %>% 
  bind_rows()

r_capacidad_al_fin %>% filter(year == 2023)

# identificacion de autogeneracion ----------------------------------------

variables <- list()
for(i in 1:length(capacidad_al_fin)){
  
  variables[[i]] <- names(capacidad_al_fin[[i]])
  
}

autogeneradores <- list()
years <- 2000:2023
for(i in 1:length(capacidad_al_fin)){
  
  if(("Es_Autogenerador" %in% names(capacidad_al_fin[[i]])) & ("Capacidad_Efectiva_Neta_kW" %in% names(capacidad_al_fin[[i]]))){
    
    autogeneradores[[i]] <- capacidad_al_fin[[i]] %>% 
      group_by(Tipo_de_Generación, Es_Autogenerador, Combustible_por_defecto) %>% 
      summarise(n = n(),
                media = mean(Capacidad_Efectiva_Neta_kW, na.rm = T),
                desvEst = sd(Capacidad_Efectiva_Neta_kW, na.rm = T),
                total = sum(Capacidad_Efectiva_Neta_kW, na.rm = T)) %>% 
      mutate(year = years[i]) %>% 
      rename(clasificacion = Es_Autogenerador)
    
  }else{
    
    if(("Es_Autogenerador" %in% names(capacidad_al_fin[[i]])) & ("Capacidad_Efectiva_Neta" %in% names(capacidad_al_fin[[i]]))){
    
      
      autogeneradores[[i]] <- capacidad_al_fin[[i]] %>% 
        group_by(Tipo_de_Generación, Es_Autogenerador, Combustible_por_defecto) %>% 
        summarise(n = n(),
                  media = mean(Capacidad_Efectiva_Neta, na.rm = T),
                  desvEst = sd(Capacidad_Efectiva_Neta, na.rm = T),
                  total = sum(Capacidad_Efectiva_Neta, na.rm = T)) %>% 
        mutate(year = years[i]) %>% 
        rename(clasificacion = Es_Autogenerador)
      
    }else{
      
    if(("Clasificación" %in% names(capacidad_al_fin[[i]])) & ("Capacidad_Efectiva_Neta_kW" %in% names(capacidad_al_fin[[i]]))){
    
    autogeneradores[[i]] <- capacidad_al_fin[[i]] %>% 
      group_by(Tipo_de_Generación, Clasificación, Combustible_por_defecto) %>% 
      summarise(n = n(),
                media = mean(Capacidad_Efectiva_Neta_kW, na.rm = T),
                desvEst = sd(Capacidad_Efectiva_Neta_kW, na.rm = T),
                total = sum(Capacidad_Efectiva_Neta_kW, na.rm = T)) %>% 
      mutate(year = years[i]) %>% 
      rename(clasificacion = Clasificación)
    
    }else{
      
      if(("Clasificación" %in% names(capacidad_al_fin[[i]])) & ("Capacidad_Efectiva_Neta" %in% names(capacidad_al_fin[[i]]))){
        
        autogeneradores[[i]] <- capacidad_al_fin[[i]] %>% 
          group_by(Tipo_de_Generación, Clasificación, Combustible_por_defecto) %>% 
          summarise(n = n(),
                    media = mean(Capacidad_Efectiva_Neta, na.rm = T),
                    desvEst = sd(Capacidad_Efectiva_Neta, na.rm = T),
                    total = sum(Capacidad_Efectiva_Neta, na.rm = T)) %>% 
          mutate(year = years[i]) %>% 
          rename(clasificacion = Clasificación)
        
      }else{
      
        if(("Capacidad_Efectiva_Neta" %in% names(capacidad_al_fin[[i]]))){
        
      autogeneradores[[i]] <- capacidad_al_fin[[i]] %>% 
        group_by(Tipo_de_Generación, Combustible_por_defecto) %>% 
        summarise(n = n(),
                  media = mean(Capacidad_Efectiva_Neta, na.rm = T),
                  desvEst = sd(Capacidad_Efectiva_Neta, na.rm = T),
                  total = sum(Capacidad_Efectiva_Neta, na.rm = T)) %>% 
        mutate(year = years[i])
      
        }else{
          
          
          if(("Capacidad_Efectiva_Neta_kW" %in% names(capacidad_al_fin[[i]]))){
          
          autogeneradores[[i]] <- capacidad_al_fin[[i]] %>% 
            group_by(Tipo_de_Generación, Combustible_por_defecto) %>% 
            summarise(n = n(),
                      media = mean(Capacidad_Efectiva_Neta_kW, na.rm = T),
                      desvEst = sd(Capacidad_Efectiva_Neta_kW, na.rm = T),
                      total = sum(Capacidad_Efectiva_Neta_kW, na.rm = T)) %>% 
            mutate(year = years[i])
          
          
      }    
     }
    }
   }
  }
 }
}

autogeneradores2 <- list()
for(i in 1:length(autogeneradores)){
  
  if("clasificacion" %in% names(autogeneradores[[i]])){
    
    p <- autogeneradores[[i]]$clasificacion
    
    if(("SI" %in% p) | ("NO" %in% p)){
      
      autogeneradores2[[i]] <- autogeneradores[[i]] %>% 
        mutate(autogenerador = ifelse(clasificacion == "SI" | Tipo_de_Generación == "COGENERADOR", 
                                      "autogenerador", 
                                      "no autogenerador"))
      
      
    }else{
      
      autogeneradores2[[i]] <- autogeneradores[[i]] %>% 
        mutate(autogenerador = ifelse(clasificacion %in% c("COGENERADOR",
                                                           "AUTOGENERADOR",
                                                           "AUTOG PEQ. ESCALA",
                                                           "GEN. DISTRIBUIDA"), "autogenerador", "no autogenerador"))
      
      
    }
    
    
  }else{
    
    autogeneradores2[[i]] <- autogeneradores[[i]] %>% 
      mutate(autogenerador = ifelse(Tipo_de_Generación == "COGENERADOR", "autogenerador", "no autogenerador"))
    
    
    
  }
  
}

vector_combustibles <- list()
for(i in 1:length(autogeneradores2)){
  
  vector_combustibles[[i]] <- autogeneradores2[[i]]$Combustible_por_defecto
  
}

vector_combustibles <- vector_combustibles %>% unlist()

vector_combustibles <- sort(unique(vector_combustibles))

for(i in 1:length(autogeneradores2)){
  
  autogeneradores2[[i]] <- autogeneradores2[[i]] %>% 
    ungroup() %>% 
    mutate(combustible = case_when(
      Combustible_por_defecto == vector_combustibles[1] ~ "Petróleo",
      Combustible_por_defecto == vector_combustibles[2] ~ "Hidroeléctrica",
      Combustible_por_defecto == vector_combustibles[3] ~ "Bioenergía",
      Combustible_por_defecto == vector_combustibles[4] ~ "Bioenergía",
      Combustible_por_defecto == vector_combustibles[5] ~ "Bioenergía",
      Combustible_por_defecto == vector_combustibles[6] ~ "Carbón",
      Combustible_por_defecto == vector_combustibles[7] ~ "Petróleo",
      Combustible_por_defecto == vector_combustibles[8] ~ "Petróleo",
      Combustible_por_defecto == vector_combustibles[9] ~ "Gas",
      Combustible_por_defecto == vector_combustibles[10] ~ "Petróleo",
      Combustible_por_defecto == vector_combustibles[11] ~ "Petróleo",
      Combustible_por_defecto == vector_combustibles[12] ~ "Solar",
      Combustible_por_defecto == vector_combustibles[13] ~ "Eólica",
    ),
    renovable = ifelse(combustible %in% c("Bioenergía","Solar", "Eólica"), 0, 
                       ifelse(combustible %in% c("Carbón","Gas","Petróleo"), 1, 2)),
    renovable = factor(renovable),
    combustible = factor(combustible, 
                         levels = c("Bioenergía",
                                    "Solar",
                                    "Eólica",
                                    "Carbón",
                                    "Petróleo",
                                    "Gas",
                                    "Hidroeléctrica"),
                         labels = c("Bioenergía",
                                    "Solar",
                                    "Eólica",
                                    "Carbón",
                                    "Petróleo",
                                    "Gas",
                                    "Hidroeléctrica")))
  
}

autogeneradores2 <- autogeneradores2 %>% 
  map(.f = ~{
    
    .x <- .x %>% 
      group_by(autogenerador, combustible) %>% 
      summarise(n = sum(n, na.rm = T),
                total = sum(total, na.rm = T)) %>% 
      ungroup()
    
  })

autogeneradores2 <- autogeneradores2 %>% 
  map2(.y = years, .f = ~{
    
    .x <- .x %>% 
      mutate(year = .y,
             renovable = ifelse(combustible %in% c("Bioenergía","Solar", "Eólica"), 0, 
                                ifelse(combustible %in% c("Carbón","Gas","Petróleo"), 1, 2)),
             renovable = factor(renovable))
    
  })

autogeneradores2 <- autogeneradores2 %>% bind_rows()


# graficos ----------------------------------------------------------------

a_min <- 0.8
a_max <- 1
al_vals <- c(seq(a_max, a_min, length.out = 2),
             seq(a_max, a_min, length.out = 2),
             a_max)

n_plantas <- autogeneradores2 %>% 
  filter(autogenerador == "autogenerador") %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(n_total = sum(n)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = year, 
                         y = n, 
                         fill = renovable, 
                         alpha = combustible),
           color = "white",
           show.legend = F) +
  geom_text(mapping = aes(x = year, y = n_total + 10, label = n_total)) +
  scale_alpha_manual(values = al_vals) +
  scale_fill_manual(values = c("#249206", "#283227", "#3667A6")) +
  geom_text(data = data.frame(year = rep(2024.8, 4),
                              n = c(330, 200, 30, 10),
                              label = factor(c("Bioenergía",
                                        "Solar",
                                        "Fósiles",
                                        "Hidroeléctrica"),
                                        labels = c("Bioenergía",
                                                   "Solar",
                                                   "Fósiles",
                                                   "Hidroeléctrica"),
                                        levels = c("Bioenergía",
                                                   "Solar",
                                                   "Fósiles",
                                                   "Hidroeléctrica")),
                              renovable = as.factor(c(0, 0, 1, 2)),
                              n2 = c(16, 287, 9, 28)) %>% 
                              mutate(etiqueta = str_c(label, n2, sep = " ")),
            mapping = aes(x = year, 
                          y = n, 
                          label = etiqueta,
                          color = renovable),
            show.legend = F,
            size = 3.5) +
  scale_color_manual(values = c("#249206", "#283227", "#3667A6")) +
  scale_x_continuous(breaks = 2000:2023) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "Evolución del número de plantas de autogeneración por fuente primaria de energía en Colombia",
       subtitle = "Periodo: (2000 - 2023)",
       x = NULL,
       y = "Número de plantas",
       caption = "Fuente: Elaboración propia con base en XM") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.0, size = 12),
        plot.subtitle = element_text(hjust = 0.0, size = 10),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")
  
ggsave(filename = "rmd/resultados/graficos/grafico3.27_n_plantas_autogeneradoras.png",
       plot = n_plantas,
       width = 12.7,
       height = 5.84,
       dpi = 500)

etiquetas <- autogeneradores2 %>% 
  filter(autogenerador == "autogenerador" & year == 2023) %>% 
  mutate(etiqueta = str_c(combustible, "\n", round(total/1000, 1), " MW", sep = ""),
         ejeY = c(300, 150, 105, 70, 30))

capacidad <- autogeneradores2 %>% filter(autogenerador == "autogenerador") %>%
  group_by(year) %>% 
  mutate(capacidad_total = sum(total)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = year, 
                         y = total/1000, 
                         fill = renovable, 
                         alpha = combustible),
           color = "white",
           show.legend = F) +
  geom_text(mapping = aes(x = year, y = capacidad_total/1000 + 10, label = round(capacidad_total/1000, 1)),
            size = 3) +
  geom_text(data = etiquetas,
            mapping = aes(x = 2024.8,
                          y = ejeY,
                          label = etiqueta,
                          color = renovable,
                          alpha = combustible), 
            size = 3,
            show.legend = F) +
  scale_alpha_manual(values = al_vals) +
  scale_fill_manual(values = c("#249206", "#283227", "#3667A6")) +
  scale_color_manual(values = c("#249206", "#283227", "#3667A6")) +
  scale_x_continuous(breaks = 2000:2023) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "Evolución de la capacidad instalada para autogeneración por fuente primaria de energía en Colombia",
       subtitle = "Periodo: (2000 - 2023)",
       x = NULL,
       y = "Megavatios (MW)",
       caption = "Fuente: Elaboración propia con base en XM") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.0, size = 12),
        plot.subtitle = element_text(hjust = 0.0, size = 10),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

ggsave(filename = "rmd/resultados/graficos/grafico3.28_capacidad_autogeneracion.png",
       plot = capacidad,
       width = 12.7,
       height = 5.84,
       dpi = 500)


# Grafico de generacion solar ---------------------------------------------

solar <- r_capacidad_al_fin[r_capacidad_al_fin$combustible == "Solar", ]

n <- solar %>% 
  ggplot(mapping = aes(x = year, y = n)) +
  geom_col(color = "white",
           fill = "#ff6700") +
  geom_text(mapping = aes(label = n), nudge_y = 10) +
  scale_x_continuous(breaks = 2017:2023) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "A: Número de plantas solares en Colombia",
       subtitle = "Periodo: (2017 - 2023)",
       x = NULL,
       y = "Número de plantas",
       caption = "Fuente: Elaboración propia con base en XM") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.0, size = 12),
        plot.subtitle = element_text(hjust = 0.0, size = 10),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 0, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

media <- solar %>% 
  ggplot(mapping = aes(x = year, y = media)) +
  geom_col(color = "white",
           fill = "#CF1F06") +
  geom_text(mapping = aes(label = round(media, 1)), nudge_y = 250) +
  scale_x_continuous(breaks = 2017:2023) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "B: Capacidad media en kW de plantas solares en Colombia",
       subtitle = "Periodo: (2017 - 2023)",
       x = NULL,
       y = "Kilovatios - kW",
       caption = "Fuente: Elaboración propia con base en XM") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.0, size = 12),
        plot.subtitle = element_text(hjust = 0.0, size = 10),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 0, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


conjun_solar <- n + media

ggsave(filename = "rmd/resultados/graficos/grafico3.29_conjun_solar.png",
       plot = conjun_solar,
       width = 12.8,
       height = 7.36,
       dpi = 500)

