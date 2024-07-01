
#################################################################################################################################################
# Titulo: Generación de electricidad por fuente primaria de energía en Colombia (1980 - 2023)
# Tema: Este gráfico muestra la evolución de la generación de eléctricidad por fuente en colombia, desde 1980 hasta 2023
#       los datos desde 1980 hasta el 2005 corresponden a información oficial de la Agencia de Información de Energía de los Estados Unidos
#       y desde el 2006 corresponde a información oficial de XM.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 1 de Julio de 2024                                                                                                                     #
# Base de datos: La base de datos de la agencia internacional de energía esta almacenada como baseGeneration.xlsx
#                con respecto a la información de XM, existe un archivo por cada uno de los años, por lo que hay otro
#                documento (generacionXM.R) en el que se muestra la descarga via API de los documentos que necesitamos.
#################################################################################################################################################

rm(list = ls())

library(tidyverse)
library(writexl)
library(readxl)
library(patchwork)


# datos de EIA -----------------------------------------------------------

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

baseGeneracion_Col <- baseGeneracion %>% 
  filter(Code.GDP == "COL")

baseGeneracion_Col <- baseGeneracion_Col %>% 
  gather(key = tipo, value = valor, 6:length(baseGeneracion_Col))

filtro <- c("Biomass", "Fossil_fuels",
            "Hydroelectricity", "Solar", "Wind")

baseGeneracion_Col <- baseGeneracion_Col %>% 
  filter(tipo %in% filtro)

baseGeneracion_Col <- baseGeneracion_Col %>% 
  mutate(renovable = case_when(
    tipo == "Biomass" ~ 0,
    tipo == "Hydroelectricity" ~ 2, 
    tipo == "Solar" ~ 0, 
    tipo == "Wind" ~ 0,
    tipo == "Fossil_fuels" ~ 1),
    nombre = case_when(
      tipo == "Biomass" ~ "Bioenergía",
      tipo == "Hydroelectricity" ~ "Hidroeléctrica", 
      tipo == "Solar" ~ "Solar", 
      tipo == "Wind" ~ "Eólica", 
      tipo == "Fossil_fuels" ~ "Fósiles"))


baseGeneracion_Col <- baseGeneracion_Col %>% 
  mutate(renovable = factor(renovable),
         valor = as.numeric(valor))

# datos por dia por hora de archivos de XM --------------------------------

lista_bases <- list.files(path = "rmd/bases/precios_precipitaciones", pattern = "Generacion*")

lista_bases <- lista_bases[(length(lista_bases)-20):length(lista_bases)]

bases <- list()
for(i in 1:length(lista_bases)){
  
  lista_bases[i] <- str_c(getwd(), "/rmd/bases/precios_precipitaciones/", lista_bases[i], sep = "")
  
  bases[[i]] <- read_excel(path = lista_bases[i])
  
}

seleccion_ <- c("Fecha", "Recurso", "Código Agente", "Tipo Generación", "Tipo Despacho", "Combustible",
                0:23)


for(i in 1:length(bases)){
  
  bases[[i]] <- bases[[i]] %>%
    select_at(all_of(seleccion_))
  
}

col_names_3 <- c("Fecha", "Recurso", "Codigo Agente", "Tipo Generacion", "Tipo Despacho", "Combustible",
                 0:23)


for(i in 1:length(bases)){
  colnames(bases[[i]]) <- col_names_3
}

bases <- bases %>% 
  map(.f = ~{
    
    p <- .x
    
    p <- p %>% 
      mutate(Fecha = as.character.Date(Fecha))
    
    p <- p %>% 
      separate(Fecha, into = c("year", "month", "day"), sep = "-")
    
    p <- p %>% 
      gather(key = hora, value = kWh, 9:length(p))
    
    return(p)
    
    
  })

bases <- bases %>% bind_rows()

combustibles <- unique(bases$Combustible)

bases_ <- bases %>% 
  mutate(nombre = case_when(
    Combustible == combustibles[1] ~ "Hidroeléctrica",
    Combustible == combustibles[2] ~ "Petróleo",
    Combustible == combustibles[3] ~ "Bioenergía",
    Combustible == combustibles[4] ~ "Gas",
    Combustible == combustibles[5] ~ "Carbón",
    Combustible == combustibles[6] ~ "Eólica",
    Combustible == combustibles[7] ~ "Bioenergía",
    Combustible == combustibles[8] ~ "Petróleo",
    Combustible == combustibles[9] ~ "Petróleo",
    Combustible == combustibles[10] ~ "Petróleo",
    Combustible == combustibles[11] ~ "Petróleo",
    Combustible == combustibles[12] ~ "Bioenergía",
    Combustible == combustibles[13] ~ "Gas",
    Combustible == combustibles[14] ~ "Solar",
    Combustible == combustibles[15] ~ "Petróleo",
  )) %>% 
  mutate(renovable = case_when(
    nombre == "Solar" ~ 0,
    nombre == "Eólica" ~ 0,
    nombre == "Bioenergía" ~ 0,
    nombre == "Gas" ~ 1,
    nombre == "Petróleo" ~ 1,
    nombre == "Carbón" ~ 1,
    nombre == "Hidroeléctrica" ~ 2
  ),
  renovable = factor(renovable))


base_xm_2006_2023 <- bases_ %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year > 2005) %>% 
  ungroup() %>% 
  group_by(year, renovable, nombre) %>% 
  summarise(valor = sum(kWh, na.rm = T)/1000000000)

base_xm_2006_2023 <- base_xm_2006_2023 %>% 
  mutate(Code.WDI = "COL")

baseGeneracion_Col <- baseGeneracion_Col %>% 
  select(-c("Country", "Country.WDI", "Code.GDP", "tipo"))

base_xm_2006_2023 <- base_xm_2006_2023 %>% 
  select(Code.WDI, year, valor, renovable, nombre)

baseGeneracion_Col_graph <- rbind(baseGeneracion_Col %>% 
                                    filter(year <= 2005), base_xm_2006_2023)

baseGeneracion_Col_graph$nombre <- factor(baseGeneracion_Col_graph$nombre,
                                          levels = c("Solar", "Eólica", "Bioenergía",
                                                     "Gas", "Fósiles", "Petróleo", "Carbón",
                                                     "Hidroeléctrica"),
                                          labels = c("Solar", "Eólica", "Bioenergía",
                                                     "Gas", "Fósiles", "Petróleo", "Carbón",
                                                     "Hidroeléctrica"))


# Acá se calculan las tasas de crecimiento por fuente que se ponen en el gráfico

etiquetas_part_elec <- baseGeneracion_Col_graph %>%
  ungroup() %>%
  filter(year %in% c(1980,2023)) %>%
  group_by(year, renovable) %>% 
  summarise(valor = sum(valor)) %>% 
  spread(key = year, value = valor) %>% 
  mutate(crecimiento = `2023`/`1980` - 1,
         crecimiento_anual = ((`2023`/`1980`)^(1/(2023-1980))-1)*100)

alpha_max <- 1
alpha_min <- 0.6
alpha_vals <- c(
  seq(alpha_max, alpha_min, length.out = 3),
  seq(alpha_min, alpha_max, length.out = 4),
  alpha_max)

grafico_eia_xm <- baseGeneracion_Col_graph %>% 
  ggplot() +
  geom_area(mapping = aes(x = year, y = valor, fill = renovable, alpha = nombre),
            col = "white", show.legend = F) +
  scale_alpha_manual(values = alpha_vals) +
  scale_fill_manual(values = c("#249206", "#283227", "#3667A6")) +
  geom_rect(mapping = aes(xmin = 1991, xmax = 1993, ymin = 0, ymax = 40), alpha = 0.015, fill = "lightgrey")+
  geom_rect(mapping = aes(xmin = 2013, xmax = 2016, ymin = 0, ymax = 70), alpha = 0.015, fill = "lightgrey")+
  scale_x_continuous(breaks = 1980:2023) +
  scale_y_continuous(n.breaks = 10) +
  annotate(geom = "text",
           x = c(1990, 1988, 1988, 1988),
           y = c(80, 75, 72.5, 70),
           label = c("Crecimiento Anual Equivalente (1980 - 2023):",
                     str_c("Renovables No Convenc.", round(etiquetas_part_elec$crecimiento_anual[1], 1), "%", sep = " "),
                     str_c("Fósiles", round(etiquetas_part_elec$crecimiento_anual[2], 1), "%", sep = " "),
                     str_c("Hidroeléctrica", round(etiquetas_part_elec$crecimiento_anual[3], 1), "%", sep = " ")),
           color = c("black","#249206", "#4A4B4D", "#3667A6"),
           size = c(3.2,3,3,3)) +
  labs(title = "A: Evolución de la generación de electricidad por fuente primaria en Colombia (1980 - 2023)",
       subtitle = "Electricidad medida en Teravatios-Hora (TWh)",
       x = NULL,
       y = "Teravatios-Hora",
       caption = "Fuente: Elaboración propia con base en EIA y XM") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.0, size = 9),
        plot.subtitle = element_text(hjust = 0.0, size = 8),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 6, angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

# Base de generación de XM correspondiente al 2022 ------------------------

agregado <- baseGeneracion_Col_graph %>%
  filter(year == 2023) %>% 
  group_by(renovable, nombre) %>% 
  summarise(generacion = sum(valor, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(prop = generacion/sum(generacion)*100,
         etiqueta = str_c(nombre, round(prop,1), "%", sep = " ")) %>% 
  ungroup() %>% 
  group_by(renovable) %>% 
  mutate(prop2 = sum(prop),
         etiqueta2 = ifelse(renovable == "0", str_c("Renovables", round(prop2,1), "%", sep = " "),
                            etiqueta))

alpha_vals <- c(
  seq(alpha_max, alpha_min, length.out = 3), 
  seq(alpha_min, alpha_max, length.out = 3),
  alpha_max)

grafico_stock_2023 <- agregado %>% 
  ggplot(mapping = aes(x = as.factor(2023), y = prop, fill = renovable, alpha = nombre)) +
  geom_col(col = "white", width = 1) +
  scale_alpha_manual(values = alpha_vals) +
  scale_fill_manual(values = c("#249206", "#283227", "#3667A6")) +
  scale_y_continuous(n.breaks = 10, position = "right") +
  guides(
    fill = guide_none(),
    alpha = guide_legend(override.aes = list(fill = c(rep("#249206", 3),
                                                      rep("#283227", 3),
                                                      "#3667A6")))
  ) +
  geom_text(
    data = agregado %>% filter(nombre %in% c("Hidroeléctrica",
                                             "Carbón",
                                             "Gas",
                                             "Solar")),
    aes(label = etiqueta2),
    position = position_stack(vjust = 0.7),
    col = c("black",'white','white','white'),
    size = 3,
    fontface = 'bold'
  ) +
  labs(title = "B: Participación porcentual por fuente - 2023",
       # subtitle = "Comparación entre Colombia y el promedio mundial",
       x = NULL,
       y = "Porcentaje") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        plot.subtitle = element_text(hjust = 0.5, size = 8),
        plot.caption = element_text(size=6, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

grafico <- wrap_plots(grafico_eia_xm, grafico_stock_2023, ncol = 2, widths = c(1, 0.4))

ggsave(plot = grafico, 
       filename = "rmd/resultados/graficos/grafico1.5_evolucion_generacion_col_EIA_XM2.png", 
       units = 'in', 
       width = 9, 
       height = 6,
       dpi = 500)
