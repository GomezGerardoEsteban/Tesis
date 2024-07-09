
#################################################################################################################################################
# Titulo: Crecimiento económico y encadenamiento hacia atrás con el sector eléctrico
# Tema: Este gráfico muestra como entre 2005 y 2021 los sectores que segun sus encadenamientos hacia atras mas impulsarían la demanda de 
#       electricidad, no son lo que mas crecieron en la economía colombiana.
#       Para ello utilizo los coeficientes correspondientes a la fila del sector eléctrico en la matriz inversa de Leontief y el calculo 
#       de las tasas de crecimiento equivalente anual entre 2005 y 2021.
#       Tiene una estructura casi identica a la del grafico1.11, pero en lugar de utilizar los coeficientes tecnicos, utiliza los encadenamientos
#       hacia atras.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 9 de Julio de 2024                                                                                                                     #
# Base de datos: Los datos para realizar estos calculos provienen del Departamento Administrativo Nacional de Estadisticas (DANE)
#                la matriz I-P es la del 2019 a 69 sectores y las series de tiempo de VA por sector tienen una desagregacion a 61 sectores
#                para poder hacer homologables los datos, se tuvieron que agregar dos sectores de las series de tiempo y 10 sectores de la 
#                matriz I-P. Estas agregaciones se presentan en el código.
#################################################################################################################################################


rm(list = ls())

# Paqueteria

##
library(tidyverse)
library(readxl)
library(igraph)
library(ioanalysis) # El paquete para hacer el análisis de Matrices I-P
library(ggrepel)
library(ggtext)
library(ggraph)
library(patchwork)
library(writexl)

##
matriz <- list() # Generamos una lista vacia que va a contener las matrices

##
for(i in 1:6){
  matriz[[i]] <- read_excel(path = "rmd/bases/maticesIP/m19.xlsx", sheet = i)
}

# generamos objetos I-P para ello necesitamos la matriz de Consumos intermedios 
# y la producción total por sector

##
m_io_19 <- as.inputoutput(as.matrix(matriz[[1]][,c(2:length(matriz[[1]]))]),
                          X = as.matrix(matriz[[4]][,4]),
                          RS_label = matrix(c(rep("COL",(length(matriz[[1]])-1)), 
                                              as.character(1:(length(matriz[[1]])-1))), 
                                            ncol = 2),
                          V = t(as.matrix(matriz[[4]][,9])),
                          V_label = as.matrix("Total"))

# Agregación de sectores en matriz ----------------------------------------

# Es necesario agregar algunos sectores para poder hacer la comparación con los agregados macroeconómicos 
# de series de tiempo

# Ubicación de sectores a agregar

##
agregados <- list(c("20","21"),
                  c("26","27"),
                  c("39","40","42"),
                  c("53","54"),
                  c("55","56"),
                  c("57","58","59"))

# Nombres a colocar una vez agregados

##
nombres <- c("Textiles",
             "Ind. Quimica",
             "Aguas residuales",
             "Alojamiento y CyB",
             "Información y Comunicaciones",
             "Interm. Financiera")

# Se requiere un bucle que agregue de a una especificación, es decir, va a tomar la matri original y va a agregar
# c("20", "21"), posteriormente a la matriz transformada le va a aplicar la agregación c("26", "27") y así sucesivamente...

##
for(i in 1:6){
  
  m_io_19 <- m_io_19 %>% agg.sector(sectors = agregados[[i]], newname = nombres[i])
  
}

m_io_19$RS_label

# Levantamos base a precios corrientes de producción por sector para obtener: 
# Crecimiento equivalente entre 2005 - 2021
# Participación de sectores en el PIB 2021

bases_macro <- list()

for(i in 1:2){
  bases_macro[[i]] <- read_excel(path = "rmd/bases/maticesIP/agregados_macro_25_sectores.xlsx", sheet = i)
}

# Convertimos en formato largo

base_macro_sectores <- bases_macro[[2]] %>% 
  gather(key = year, value = valor, 5:length(bases_macro[[2]]))

# Función para calcular tasas de crecimiento

calculoTasas <- function(base){
  
  bases_macro_sec1 <- base %>% 
    filter(year == "2005")
  
  PIB <- base %>% filter(year == "2021" & Nombre == "PIB") %>% select(valor)
  
  bases_macro_sec2 <- base %>% 
    filter(year == "2021") %>% 
    mutate(prop = valor/PIB$valor[1])
  
  
  bases_macro_sec <- bases_macro_sec1 %>% 
    left_join(bases_macro_sec2 %>% select(4,6:7), 
              by = c("Nombre" = "Nombre"), 
              suffix = c(".05", ".21"))
  
  bases_macro_sec <- bases_macro_sec %>% 
    mutate(tasa_crecimiento = ((valor.21/valor.05)-1),
           tasa_equiv_anual = (1 + tasa_crecimiento)^(1/(2021-2005))-1)
  
  
  return(bases_macro_sec)
  
} 

# Aplicamos sobre base en formato largo

bases_macro_sec <- calculoTasas(base = base_macro_sectores)

# Generamos una base que solo incluya los sectores

bases_solo_sec <- bases_macro_sec %>% 
  filter(Nombre != "VAB" & Nombre != "PIB") %>% 
  mutate(int_elec = m_io_19$A[34,-59])


m_io_19$RS_label


# Gráfico de intensidad electrica y crecimiento económico buscando ver porque en Colombia a mayor
# crecimiento del PIB no se poduce una mayor demanda de electricidad


bases_solo_sec$categoricaConsumCrec <- factor(case_when(
  (bases_solo_sec$tasa_equiv_anual > bases_macro_sec$tasa_equiv_anual[bases_macro_sec$Nombre == "PIB"]) & (bases_solo_sec$int_elec > weighted.mean(x = bases_solo_sec$int_elec[-c(59,34)], w = bases_solo_sec$prop[-c(59,34)]))~ "PositivoIntensivo",
  (bases_solo_sec$tasa_equiv_anual < bases_macro_sec$tasa_equiv_anual[bases_macro_sec$Nombre == "PIB"]) & (bases_solo_sec$int_elec > weighted.mean(x = bases_solo_sec$int_elec[-c(59,34)], w = bases_solo_sec$prop[-c(59,34)]))~ "NegativoIntensivo",
  (bases_solo_sec$tasa_equiv_anual > bases_macro_sec$tasa_equiv_anual[bases_macro_sec$Nombre == "PIB"]) & (bases_solo_sec$int_elec < weighted.mean(x = bases_solo_sec$int_elec[-c(59,34)], w = bases_solo_sec$prop[-c(59,34)]))~ "PositivoNoIntensivo",
  (bases_solo_sec$tasa_equiv_anual < bases_macro_sec$tasa_equiv_anual[bases_macro_sec$Nombre == "PIB"]) & (bases_solo_sec$int_elec < weighted.mean(x = bases_solo_sec$int_elec[-c(59,34)], w = bases_solo_sec$prop[-c(59,34)]))~ "NegativoNoIntensivo"),
  levels = c("PositivoIntensivo",
             "NegativoIntensivo",
             "PositivoNoIntensivo",
             "NegativoNoIntensivo"),
  labels = c("PositivoIntensivo",
             "NegativoIntensivo",
             "PositivoNoIntensivo",
             "NegativoNoIntensivo"))


bases_solo_sec %>% group_by(categoricaConsumCrec) %>% summarise(propsum = sum(prop, na.rm = T))


# Generamos una base que solo incluya los sectores

bases_solo_sec <- bases_macro_sec %>% 
  filter(Nombre != "VAB" & Nombre != "PIB") %>% 
  mutate(int_elec = m_io_19$L[34,-59])


m_io_19$RS_label


# Gráfico de intensidad electrica y crecimiento económico buscando ver porque en Colombia a mayor
# crecimiento del PIB no se poduce una mayor demanda de electricidad


bases_solo_sec$categoricaConsumCrec <- factor(case_when(
  (bases_solo_sec$tasa_equiv_anual > bases_macro_sec$tasa_equiv_anual[bases_macro_sec$Nombre == "PIB"]) & (bases_solo_sec$int_elec > weighted.mean(x = bases_solo_sec$int_elec[-c(59,34)], w = bases_solo_sec$prop[-c(59,34)]))~ "PositivoIntensivo",
  (bases_solo_sec$tasa_equiv_anual < bases_macro_sec$tasa_equiv_anual[bases_macro_sec$Nombre == "PIB"]) & (bases_solo_sec$int_elec > weighted.mean(x = bases_solo_sec$int_elec[-c(59,34)], w = bases_solo_sec$prop[-c(59,34)]))~ "NegativoIntensivo",
  (bases_solo_sec$tasa_equiv_anual > bases_macro_sec$tasa_equiv_anual[bases_macro_sec$Nombre == "PIB"]) & (bases_solo_sec$int_elec < weighted.mean(x = bases_solo_sec$int_elec[-c(59,34)], w = bases_solo_sec$prop[-c(59,34)]))~ "PositivoNoIntensivo",
  (bases_solo_sec$tasa_equiv_anual < bases_macro_sec$tasa_equiv_anual[bases_macro_sec$Nombre == "PIB"]) & (bases_solo_sec$int_elec < weighted.mean(x = bases_solo_sec$int_elec[-c(59,34)], w = bases_solo_sec$prop[-c(59,34)]))~ "NegativoNoIntensivo"),
  levels = c("PositivoIntensivo",
             "NegativoIntensivo",
             "PositivoNoIntensivo",
             "NegativoNoIntensivo"),
  labels = c("PositivoIntensivo",
             "NegativoIntensivo",
             "PositivoNoIntensivo",
             "NegativoNoIntensivo"))


bases_solo_sec %>% group_by(categoricaConsumCrec) %>% summarise(propsum = sum(prop, na.rm = T))



graph_Crecimiento_Elec_ <- bases_solo_sec %>% 
  filter(Nombre != "Electricidad" & Nombre != "Hogares") %>% 
  ggplot(mapping = aes(y = tasa_equiv_anual, 
                       x = log(int_elec), 
                       color = categoricaConsumCrec)) +
  geom_point(mapping = aes(size = prop),
             alpha = 0.5,
             show.legend = F) +
  geom_text_repel(mapping = aes(label = Nombre), 
                  size = 3, 
                  show.legend = F) +
  geom_hline(data = bases_macro_sec %>% 
               filter(Nombre == "PIB"), 
             mapping = aes(yintercept = tasa_equiv_anual), 
             linetype = "longdash",
             color = "darkblue") +
  geom_vline(mapping = aes(xintercept = log(weighted.mean(x = bases_solo_sec$int_elec[-c(59,34)], 
                                                          w = bases_solo_sec$prop[-c(59,34)]))), 
             linetype = "longdash",
             color = "darkred") +
  scale_color_manual(values = c("#900C3F",
                                "#365486",
                                "#E3651D",
                                "#597E52"))+
  scale_size(range = c(1,10)) +
  annotate(geom = "text",
           x = c(-5.5, -3.4),
           y = c(0.038, -0.018),
           label = c(str_c("Crecimiento PIB = ", round(bases_macro_sec$tasa_equiv_anual[bases_macro_sec$Nombre == "PIB"]*100, 2), " %", sep = ""),
                     str_c("Media ponderada del\nEncadenamiento = ", round(weighted.mean(x = bases_solo_sec$int_elec[-c(59,34)], w = bases_solo_sec$prop[-c(59,34)]), 4)*100, sep = "")),
           color = c("darkblue", "darkred"),
           size = 3) +
  labs(title = "Relación entre crecimiento económico y el encadenamiento hacia atrás con el sector eléctrico según la matriz inversa de Leontief",
       subtitle = "Tasa de crecimiento anual equivalente (2005-2021) ~ Matriz I-P 2019, el tamaño de los circulos corresponde a la participación en el PIB de cada sector",
       y = "Tasa de crecimiento económico",
       x = "Logaritmo de la intensidad eléctrica",
       caption = "Fuente: Elaboración propia en base al DANE") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.0, size = 12),
        plot.subtitle = element_text(hjust = 0.0, size = 10),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 0, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

graph_Crecimiento_Elec_

ggsave(filename = "rmd/resultados/graficos/grafico5.31_crecimiento_ecadenamientoElec.png",
       plot = graph_Crecimiento_Elec_,
       # width = 10,
       # height = 5.81,
       dpi = 500)
