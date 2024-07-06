
#################################################################################################################################################
# Titulo: Consumo medio de electricidad por hogares en Colombia
# Tema: Utilizando datos de la superintendencia de servicios públicos de Colombia, se muestra el consumo medio de electricidad
#       por habitante por departamentos y por estratos entre 2003 y 2023.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 6 de Julio de 2024                                                                                                                     #
# Base de datos: La pagina de la superintendencia de servicios publicos no es nada amigable, la descarga debe realizarse por indicador y por año
#               siendo una tarea muy tediosa, las bases obtenidas estan cargadas en la carpeta **basesSSPD** de este mismo repositorio.
#################################################################################################################################################


rm(list = ls())

library(tidyverse)
library(readxl)
library(patchwork)

lista <- list.files(path = "rmd/bases/InfoServPubli", pattern = "data*")


p <- c()
for(i in 1:length(lista)){
  if(nchar(lista[i]) == 13){
    
    p[i] <- str_sub(string = lista[i], start = 7, end = 7)
    
  }else{
    
    p[i] <- str_sub(string = lista[i], start = 7, end = 8)
    
  }
}


bases <- list()
for(i in 1:length(lista)){
  
  lista[i] <- str_c("rmd/bases/InfoServPubli/", lista[i], sep = "")
  
  bases[[i]] <- read_excel(path = lista[i])
  
  
}

for(i in 1:length(bases)){
  
  nombres <- bases[[i]][2, ]
  
  names(bases[[i]]) <- nombres
  
  bases[[i]] <- bases[[i]][-c(1:2), ]
  
}

referenciArchivos <- read_excel(path = "rmd/bases/InfoServPubli/referenciaArchivos.xlsx")

referenciArchivos <- referenciArchivos %>% 
  gather(key = tipo, value = archivo, 2:length(referenciArchivos))

referenciArchivos <- referenciArchivos %>% 
  mutate(archivo = ifelse(nchar(archivo) == 5, str_sub(archivo, 5, 5), str_sub(archivo, 5, 6)))

variableCalculada <- c("Consumo Empresa Departamento", "Valor Consumo", "usuarios Empresa Departamento")

referenciArchivos <- referenciArchivos %>% 
  mutate(variable = case_when(tipo == "suscriptores" ~ variableCalculada[3],
                              tipo == "consumo" ~ variableCalculada[1],
                              tipo == "valor consumo" ~ variableCalculada[2]))

for(i in 1:length(bases)){
  
  bases[[i]] <- bases[[i]] %>% 
    mutate(p = p[i])
  
  bases[[i]] <- bases[[i]] %>% 
    left_join(y = referenciArchivos %>% 
                select(-tipo), by = c("Variable Calculada" = "variable",
                                      "p" = "archivo"))
  
}

bases <- bases %>% bind_rows()

names(bases)[3] <- "variable"

bases <- bases %>% 
  select(-p) %>% 
  relocate(año, .before = Departamento)

bases <- bases %>% 
  gather(key = tipo, value = valor, 5:length(bases))

bases$valor <- as.numeric(bases$valor)

bases <- bases %>% 
  spread(key = variable, value = valor)

names(bases)[5:7] <- c("Consumo","Usuarios","ValorConsumo")

bases <- bases %>% 
  mutate(ValorConsumoMesUsu = ifelse(Usuarios == 0, NA, (ValorConsumo/Usuarios)/12))

estratos <- unique(bases$tipo)
estratos <- estratos[2:7]

base_residencial <- bases %>% 
  filter(tipo %in% estratos)

base_residencial <- base_residencial %>% 
  group_by(año, Departamento) %>% 
  mutate(totalUsu = sum(Usuarios, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(año, Departamento, Empresa) %>%
  mutate(totalUsuEmp = sum(Usuarios))

years <- unique(base_residencial$año)
deptos <- unique(base_residencial$Departamento)
years <- rep(years, length(deptos))
years <- sort(years)
deptos <- rep(deptos, 21)

rankEmpresaDeptoYear <- map2(.x = years, .y = deptos, .f = ~{
  
  p <- base_residencial %>% 
    filter(año == .x & Departamento == .y) %>% 
    arrange(desc(totalUsuEmp))
  
  p <- p[1:6, ]
  
  return(p)
  
})

rankEmpresaDeptoYear <- rankEmpresaDeptoYear %>% bind_rows()

ipc <- read.csv("rmd/bases/InfoServPubli/1.2.5.IPC_Serie_variaciones (1).csv")

names(ipc)[3:5] <- c("variacionAnual", "variacionMensual", "variacionAñoCorrido")

ipc <- ipc %>% 
  mutate(across(.cols = 2:5, .fns = ~{as.numeric(str_replace_all(.x, "\\,", "\\."))}))

ipc <- ipc %>% 
  mutate(year = str_sub(Mes.Año, start = 1, end = 4),
         month = str_sub(Mes.Año, start = 5, end = 6),
         year = as.numeric(year),
         month = as.numeric(month)) %>% 
  filter(month == 12)

rankEmpresaDeptoYear <- rankEmpresaDeptoYear %>% 
  left_join(y = ipc %>% select(year, Indice), by = c("año" = "year"))

referencia <- ipc$Indice[1]

rankEmpresaDeptoYear <- rankEmpresaDeptoYear %>% 
  mutate(ValorConsumoMesUsu_actual = ValorConsumoMesUsu*referencia/Indice)

rankEmpresaDeptoYear <- rankEmpresaDeptoYear %>% 
  mutate(consumoMedio = (Consumo/Usuarios)/12)


resumenesEstratoDepto <- rankEmpresaDeptoYear %>% 
  group_by(Departamento, tipo) %>% 
  summarise(media_pago = mean(ValorConsumoMesUsu_actual, na.rm = T),
            desvio_pago = sd(ValorConsumoMesUsu_actual, na.rm = T),
            n_pago = n(),
            limiteInferior_pago = media_pago - (desvio_pago/sqrt(n_pago))*qt(0.975, df = n_pago),
            limiteSuperior_pago = media_pago + (desvio_pago/sqrt(n_pago))*qt(0.975, df = n_pago),
            media_consumo = mean(consumoMedio, na.rm = T),
            desvio_consumo = sd(consumoMedio, na.rm = T),
            n_consumo = n(),
            limiteInferior_consumo = media_consumo - (desvio_consumo/sqrt(n_consumo))*qt(0.975, df = n_consumo),
            limiteSuperior_consumo = media_consumo + (desvio_consumo/sqrt(n_consumo))*qt(0.975, df = n_consumo))

resumenesEstratoDepto <- resumenesEstratoDepto %>% 
  mutate(name = str_sub(Departamento, start = 1, end = 4))

resumenesEstratoDepto <- resumenesEstratoDepto %>% 
  filter(Departamento != "GUAINIA")

resumenesEstratoDepto <- resumenesEstratoDepto %>% 
  mutate(name = ifelse(name == "D.C.", "BOGO", name))

resumenesEstratoDepto <- resumenesEstratoDepto[-c(41:42,93:95,135:137), ]

graph_deptos <- resumenesEstratoDepto %>% 
  filter(tipo == "Estrato 1" & media_consumo != Inf) %>% 
  ggplot() +
  geom_errorbar(mapping = aes(x = reorder(name, media_consumo), 
                              y = media_consumo, 
                              ymin = limiteInferior_consumo,
                              ymax = limiteSuperior_consumo)) +
  geom_point(mapping = aes(x = reorder(name, media_consumo), y = media_consumo)) +
  geom_errorbar(data = resumenesEstratoDepto %>% 
                  filter(tipo == "Estrato 4"& media_consumo != Inf),
                mapping = aes(x = reorder(name, media_consumo), 
                              y = media_consumo, 
                              ymin = limiteInferior_consumo,
                              ymax = limiteSuperior_consumo),
                color = "darkgreen") +
  geom_point(data = resumenesEstratoDepto %>% 
               filter(tipo == "Estrato 4"& media_consumo != Inf),
             mapping = aes(x = name, y = media_consumo),
             color = "darkgreen",
             shape = 17) +
  geom_errorbar(data = resumenesEstratoDepto %>% 
                  filter(tipo == "Estrato 6"& media_consumo != Inf),
                mapping = aes(x = name, 
                              y = media_consumo, 
                              ymin = limiteInferior_consumo,
                              ymax = limiteSuperior_consumo),
                color = "darkblue") +
  geom_point(data = resumenesEstratoDepto %>% 
               filter(tipo == "Estrato 6"& media_consumo != Inf),
             mapping = aes(x = name, y = media_consumo),
             color = "darkblue",
             shape = 15) +
  scale_y_continuous(limits = c(0,800),
                     n.breaks = 10) +
  labs(title = "Medias de consumo eléctrico residencial en Colombia",
       subtitle = "A: Diferenciación por departamentos y estratos",
       y = "Kilovatios-hora (kWh)",
       x = NULL,
       caption = "Fuente: Elaboración propia en base a SSPD") +
  theme_bw() +
  annotate(geom = "text",
           x = 16,
           y = c(800, 775, 750),
           label = c("Estrato 6",
                     "Estrato 4",
                     "Estrato 1"),
           color = c("darkblue", "darkgreen", "black"),
           size = 3)+
  geom_point(mapping = aes(x = 17, y = 800), color = "darkblue", shape = 15)+
  geom_point(mapping = aes(x = 17, y = 775), color = "darkgreen", shape = 17)+
  geom_point(mapping = aes(x = 17, y = 750))+
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.0, size = 10),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none") 

resumenesEstrato <- rankEmpresaDeptoYear %>%
  filter(ValorConsumoMesUsu_actual != Inf & !is.na(ValorConsumoMesUsu_actual) & consumoMedio != Inf & !is.na(consumoMedio)) %>% 
  group_by(tipo) %>% 
  summarise(numeroUsuarios = sum(Usuarios, na.rm = T),
            media_pago = mean(ValorConsumoMesUsu_actual, na.rm = T),
            desvio_pago = sd(ValorConsumoMesUsu_actual, na.rm = T),
            n_pago = n(),
            limiteInferior_pago = media_pago - (desvio_pago/sqrt(n_pago))*qt(0.975, df = n_pago),
            limiteSuperior_pago = media_pago + (desvio_pago/sqrt(n_pago))*qt(0.975, df = n_pago),
            media_consumo = mean(consumoMedio, na.rm = T),
            desvio_consumo = sd(consumoMedio, na.rm = T),
            n_consumo = n(),
            limiteInferior_consumo = media_consumo - (desvio_consumo/sqrt(n_consumo))*qt(0.975, df = n_consumo),
            limiteSuperior_consumo = media_consumo + (desvio_consumo/sqrt(n_consumo))*qt(0.975, df = n_consumo))

resumenesEstrato <- resumenesEstrato %>% 
  mutate(categorica = case_when(tipo == "Estrato 1" ~ 1,
                                tipo == "Estrato 2" ~ 2,
                                tipo == "Estrato 3" ~ 2,
                                tipo == "Estrato 4" ~ 3,
                                tipo == "Estrato 5" ~ 2,
                                tipo == "Estrato 6" ~ 4))


resumenesEstrato_2023 <- rankEmpresaDeptoYear %>%
  filter(ValorConsumoMesUsu_actual != Inf & !is.na(ValorConsumoMesUsu_actual) & consumoMedio != Inf & !is.na(consumoMedio) & año == 2023) %>% 
  group_by(tipo) %>% 
  summarise(numeroUsuarios = sum(Usuarios, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(prop = numeroUsuarios/sum(numeroUsuarios, na.rm = T))

graph_estratos <- resumenesEstrato %>% 
  ggplot() +
  geom_errorbar(mapping = aes(x = tipo, 
                              y = media_consumo, 
                              ymin = limiteInferior_consumo,
                              ymax = limiteSuperior_consumo,
                              color = as.factor(categorica)),
                show.legend = F) +
  geom_point(mapping = aes(x = tipo, 
                           y = media_consumo, 
                           color = as.factor(categorica),
                           shape = as.factor(categorica)),
             show.legend = F) +
  scale_color_manual(values = c("black", "darkgrey", "darkgreen", "darkblue")) +
  scale_shape_manual(values = c(16, 16, 17, 15)) +
  scale_y_continuous(limits = c(0,800),
                     n.breaks = 10) +
  labs(y = NULL,
       x = NULL,
       subtitle = "B: Diferenciación por estratos") +
  annotate(geom = "text",
           x = 3.5,
           y = 65,
           label = "Proporción de usuarios al 2023",
           size = 3.5) +
  geom_text(data = resumenesEstrato_2023,
            mapping = aes(x = tipo,
                          y = 25,
                          label = str_c(round(prop*100,1), "%", sep = " ")),
            size = 3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.0, size = 12),
        plot.subtitle = element_text(hjust = 0.0, size = 10),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none") 


grafico_estratos_deptos <- wrap_plots(graph_deptos, graph_estratos, ncol = 2, widths = c(1, 0.45))


ggsave(filename = "rmd/resultados/graficos/grafico1.15_estratos_deptos.png",
       plot = grafico_estratos_deptos,
       width = 12.2,
       height = 5.81,
       dpi = 500)

