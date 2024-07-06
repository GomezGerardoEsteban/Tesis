
#################################################################################################################################################
# Titulo: Valor pagado mensualmente por el servicio eléctrico en Colombia
# Tema: Utilizando datos de la superintendencia de servicios públicos de Colombia, se muestra a traves de un grafico de cajas, la distribución 
#       por departamentos y estratos del pago de electricidad.
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


rankEmpresaDeptoYear <- rankEmpresaDeptoYear %>% 
  group_by(año, Departamento) %>% 
  mutate(UsuariosTotales_estrato = sum(Usuarios, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(proporcionEstrato = Usuarios/UsuariosTotales_estrato)

ponderadoYearDepto <- rankEmpresaDeptoYear %>% 
  group_by(año, Departamento) %>% 
  summarise(mediaConsumo = weighted.mean(x = ValorConsumoMesUsu_actual, w = proporcionEstrato, na.rm = T))

ponderadoYearDepto <- ponderadoYearDepto %>% 
  filter(!is.na(mediaConsumo) & mediaConsumo > 0) %>% 
  mutate(Departamento = ifelse(Departamento == "D.C.", "BOGOTA", Departamento),
         name = str_sub(Departamento, 1, 4))

g1_valor_consumo <- ponderadoYearDepto %>% 
  group_by(Departamento) %>% 
  mutate(mediana = median(mediaConsumo, na.rm = T)) %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = reorder(name,mediana), y = mediaConsumo/1000)) +
  geom_rect(mapping = aes(xmin = 21.5, xmax = 28.5,
                          ymin = 0, ymax = 300),
            color = "lightblue",
            alpha = 0.008) +
  scale_y_continuous(n.breaks = 10,
                     limits = c(0,300)) +
  labs(x = NULL,
       y = "Miles de Pesos Colombianos - Valores Constantes Dic. 2023",
       title = "Valor del consumo residencial de electricidad en Colombia (2003 - 2023)",
       subtitle = "A: Medias ponderadas por estrato en departamentos",
       caption = "Fuente: Elaboración propia en base a Banco de la República y SSPD") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.0, size = 10),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none") +
  annotate(geom = "text",
           x = 25,
           y = 285,
           label = "Deptos. Región Caribe",
           color = "black",
           size = 3)


valorEstratosNacional <- rankEmpresaDeptoYear %>% 
  ungroup() %>% 
  group_by(año, tipo) %>% 
  mutate(totalNacionalEstratosUsuarios = sum(Usuarios)) %>% 
  ungroup() %>% 
  mutate(proporcionEstratoNacional = Usuarios/totalNacionalEstratosUsuarios) %>% 
  group_by(año, tipo) %>% 
  summarise(mediaEstrato = weighted.mean(x = ValorConsumoMesUsu_actual, w = proporcionEstratoNacional, na.rm = T))

g2_valor_consumo <- valorEstratosNacional %>% 
  na.omit() %>%
  ggplot() +
  geom_boxplot(mapping = aes(x=tipo,y=mediaEstrato/1000)) +
  scale_y_continuous(n.breaks = 10,
                     limits = c(0,300)) +
  labs(x = NULL,
       y = NULL,
       # title = "Valor del consumo residencial de electricidad",
       subtitle = "B: Medias ponderadas por ususarios en Deptos.")+
  # caption = "Fuente: Elaboración propia en base a Banco de la República y SSPD") +
  theme_bw()+
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

grafico <- wrap_plots(g1_valor_consumo, g2_valor_consumo, ncol = 2, widths = c(1,0.4)) 

ggsave(filename = "rmd/resultados/graficos/grafico1.16_valor_ponderado_consumo_residencial.png",
       plot = grafico,
       width = 11.6,
       height = 5.52,
       dpi = 300)