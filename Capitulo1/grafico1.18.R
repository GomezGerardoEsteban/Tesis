#################################################################################################################################################
# Titulo: Proporción del ingreso destinado al pago de electricidad en hogares estrato 1
# Tema: Utilizando datos de la Encuesta de Condiciones de Vida (2023) de Colombia, se muestra la proporcion del ingreso del hogar
#       destinado al gasto en electricidad en hogares estrato 1.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 6 de Julio de 2024                                                                                                                     #
# Base de datos: Para poder realizar este analisis es necesario descargar tres bases del DANE de la ECV
#               en este codigo se muestra el join de esas bases para poder manipularlas.
#               se hace uso de los factores de expansión.
#################################################################################################################################################

rm(list = ls())

library(tidyverse)
library(haven)
library(patchwork)

lista_archivos <- list.files(path = "rmd/bases/ECV_2023", pattern = "*.DTA")

bases <- list()

for(i in 1:3){
  
  lista_archivos[i] <- str_c("rmd/bases/ECV_2023/", lista_archivos[i], sep = "")
  
  bases[[i]] <- read_dta(file = lista_archivos[i])
  
}

bases[[3]] <- bases[[3]] %>% 
  left_join(y = bases[[2]] %>% select(DIRECTORIO, P8520S1A1, P1_DEPARTAMENTO), by = ("DIRECTORIO" = "DIRECTORIO"))

bases3 <- bases[[3]]

bases3 %>% glimpse()

bases3$elec_percapita <- (bases3$P5018/bases3$P5018S1)/bases3$CANT_PERSONAS_HOGAR



# Ingreso

e <- 1
l <- 0
Ing <- vector("numeric")
for(i in 1:length(bases3$PERCAPITA)){
  
  m <- rep(bases3$PERCAPITA[i], bases3$FEX_C[i])
  l <- l + length(m)
  Ing[e:l] <- m
  e <- l+1
  
}
Ing <- as.data.frame(Ing)

# Departamento

e <- 1
l <- 0
Dept <- vector("numeric")
for(i in 1:length(bases3$PERCAPITA)){
  
  m <- rep(bases3$P1_DEPARTAMENTO[i], bases3$FEX_C[i])
  l <- l + length(m)
  Dept[e:l] <- m
  e <- l+1
  
}
Dept <- as.data.frame(Dept)

# Gasto energia

e <- 1
l <- 0
Ener <- vector("numeric")
for(i in 1:length(bases3$PERCAPITA)){
  
  m <- rep(bases3$elec_percapita[i], bases3$FEX_C[i])
  l <- l + length(m)
  Ener[e:l] <- m
  e <- l+1
  
}
Ener <- as.data.frame(Ener)


# eSTRATOS

e <- 1
l <- 0
Estratos <- vector("numeric")
for(i in 1:length(bases3$PERCAPITA)){
  
  m <- rep(bases3$P8520S1A1[i], bases3$FEX_C[i])
  l <- l + length(m)
  Estratos[e:l] <- m
  e <- l+1
  
}
Estratos <- as.data.frame(Estratos)

base_Expandida <- cbind(Ing,Dept,Ener,Estratos)

base_Expandida <- base_Expandida %>% 
  filter(!is.na(Estratos) & Ing < 60000000 & Ing > 10000)

rangosDeciles <- quantile(x = base_Expandida$Ing, probs = seq(0.1,1,0.1), na.rm = T)

base_Expandida$decil <- case_when(
  base_Expandida$Ing <= rangosDeciles[1] ~ 1,
  base_Expandida$Ing > rangosDeciles[1] & base_Expandida$Ing <= rangosDeciles[2] ~ 2,
  base_Expandida$Ing > rangosDeciles[2] & base_Expandida$Ing <= rangosDeciles[3] ~ 3,
  base_Expandida$Ing > rangosDeciles[3] & base_Expandida$Ing <= rangosDeciles[4] ~ 4,
  base_Expandida$Ing > rangosDeciles[4] & base_Expandida$Ing <= rangosDeciles[5] ~ 5,
  base_Expandida$Ing > rangosDeciles[5] & base_Expandida$Ing <= rangosDeciles[6] ~ 6,
  base_Expandida$Ing > rangosDeciles[6] & base_Expandida$Ing <= rangosDeciles[7] ~ 7,
  base_Expandida$Ing > rangosDeciles[7] & base_Expandida$Ing <= rangosDeciles[8] ~ 8,
  base_Expandida$Ing > rangosDeciles[8] & base_Expandida$Ing <= rangosDeciles[9] ~ 9,
  base_Expandida$Ing > rangosDeciles[9] ~ 10
)

base_Expandida$prop.gasto <- base_Expandida$Ener/base_Expandida$Ing

AgregadoDecil <- base_Expandida %>% group_by(decil) %>% 
  summarise(n = n(),
            ingreso = mean(Ing, na.rm = T),
            gasto = mean(Ener, na.rm = T),
            prop.gast = mean(prop.gasto, na.rm = T),
            limiteInferior = prop.gast - sd(prop.gasto, na.rm = T)/sqrt(n)*qt(0.975, df = n),
            limiteSuperior = prop.gast + sd(prop.gasto, na.rm = T)/sqrt(n)*qt(0.975, df = n))


decilDeptos <- base_Expandida %>% group_by(Dept, decil) %>% 
  summarise(n = n(),
            ingreso = mean(Ing, na.rm = T),
            gasto = mean(Ener, na.rm = T),
            prop.gast = mean(prop.gasto, na.rm = T),
            limiteInferior = prop.gast - sd(prop.gasto, na.rm = T)/sqrt(n)*qt(0.975, df = n),
            limiteSuperior = prop.gast + sd(prop.gasto, na.rm = T)/sqrt(n)*qt(0.975, df = n))

AgregadoEstratos <- base_Expandida %>% group_by(Estratos) %>% 
  summarise(n = n(),
            ingreso = mean(Ing, na.rm = T),
            gasto = mean(Ener, na.rm = T),
            prop.gast = mean(prop.gasto, na.rm = T),
            limiteInferior = prop.gast - sd(prop.gasto, na.rm = T)/sqrt(n)*qt(0.975, df = n),
            limiteSuperior = prop.gast + sd(prop.gasto, na.rm = T)/sqrt(n)*qt(0.975, df = n))

estratosDepto <- base_Expandida %>% group_by(Dept, Estratos) %>% 
  summarise(n = n(),
            ingreso = mean(Ing, na.rm = T),
            gasto = mean(Ener, na.rm = T),
            prop.gast = mean(prop.gasto, na.rm = T),
            limiteInferior = prop.gast - sd(prop.gasto, na.rm = T)/sqrt(n)*qt(0.975, df = n),
            limiteSuperior = prop.gast + sd(prop.gasto, na.rm = T)/sqrt(n)*qt(0.975, df = n))


departamentos <- read.csv2(file = "rmd/bases/ECV_2023/Departamentos.csv")

departamentos <- departamentos %>% 
  mutate(Cod = ifelse(nchar(Cod) == 1, str_c("0",Cod,sep=""), Cod))

decilDeptos <- decilDeptos %>% 
  left_join(y = departamentos, by = c("Dept" = "Cod"))

decilDeptos <- decilDeptos %>% 
  mutate(name = str_sub(Depto, 1, 4))

grafico_deptos_decil_proporcion <- decilDeptos %>% 
  filter(decil == 1 & Dept != "88") %>% 
  ggplot() +
  geom_errorbar(mapping = aes(x = reorder(name,prop.gast), y = prop.gast*100, ymin = limiteInferior*100, ymax = limiteSuperior*100)) +
  geom_point(mapping = aes(x = reorder(name,prop.gast), y = prop.gast*100)) +
  geom_errorbar(data = decilDeptos %>% 
                  filter(decil == 3 & Dept != "88"),
                mapping = aes(x = name, y = prop.gast*100, ymin = limiteInferior*100, ymax = limiteSuperior*100),
                color = "darkgreen") +
  geom_point(data = decilDeptos %>% 
               filter(decil == 3 & Dept != "88"),
             mapping = aes(x = name, y = prop.gast*100),
             color = "darkgreen",
             shape = 17) +
  geom_rect(mapping = aes(xmin = 29.5, xmax = 32.5, ymin = 0, ymax = 60),
            color = "lightblue",
            alpha = 0.01) +
  geom_point(mapping = aes(x = 10, y = 55), size = 1.5) +
  geom_point(mapping = aes(x = 10, y = 52), size = 1.5, color = "darkgreen", shape = 17) +
  annotate(geom = "text",
           x = 9,
           y = c(52,55),
           label = c("Decil 3", "Decil 1"),
           color = c("darkgreen", "black"),
           size = 3) +
  scale_y_continuous(n.breaks = 10,
                     limits = c(0, 60)) +
  labs(title = "Proporción del ingreso de los hogares destinado al pago de la electricidad en departamentos de Colombia - 2023",
       # subtitle = "",
       y = "Porcentaje",
       x = NULL,
       caption = "Fuente: Elaboración propia en base a la ECV DANE") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        plot.subtitle = element_text(hjust = 0.0, size = 10),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 60, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

ggsave(filename = "rmd/resultados/graficos/grafico1.18_deptos_decil_proporcion.png",
       plot = grafico_deptos_decil_proporcion,
       width = 8.52,
       height = 4.39,
       dpi = 500)

