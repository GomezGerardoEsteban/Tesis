
#################################################################################################################################################
# Titulo: Cuencas hidrográficas en las que se divide el territorio colombiano
# Tema: Este script muestra las 5 macrocuencas en la que se divide el territorio colombiano, el gráfico fue realizado a partir del tutorial de 
#       @milos-make-maps disponible en 'https://www.youtube.com/watch?v=HugGwjogPv0&pp=ygUQbWlsb3MgbWFrZXMgbWFwcw%3D%3D'

# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 8 de Julio de 2024                                                                                                                     
# Base de datos: La obtención de las bases de datos para generar estas visualización estan muy bien explicitadas en el tutorial de 
#                @milos-make-maps.
# 
#################################################################################################################################################

## Mapa de rios
#-------------------------

rm(list = ls())

# Verificación de que paquetes estan descargados
#-------------------------------------------------

libs <- c("tidyverse", "sf", "giscoR")

installed_libraries <- libs %in% rownames(
  installed.packages()
) 

# Instalacion de paquetes en caso de no estar instalados
#--------------------------------------------------------

if(any(installed_libraries == F)){  # Con este if, lo que hacemos es instalar paquetes que no estan instalados
  
  install.packages(
    libs[!installed_libraries]
  )
}

# Activacion de paquetes
#--------------------------

invisible(                       # Es una forma elegante de activar los paquetes, 'invisible' permite que no se impriman
  lapply(libs,                   # los comandos especificos de "library(paquete)" y el "lapply" permite aplicar la activación
         library,                # a la lista que contiene los paquetes.
         character.only = T)
)

# Obtencion de bordes del pais de interes
#-------------------------------------------

# Hay problemas con el servidor de giscoR, por ello vamos a usar el .shp
# de colombia estraido del DANE.

get_country_borders <- function(pais){

   country_borders <- giscoR::gisco_get_countries(
     resolution = "3",
     country = pais
   )

   return(country_borders)

 }

country <- get_country_borders(pais = "CO")

region <- get_country_borders(pais = c("CO","BR","EC", "PA", "PE", "VE"))


# colombis_dept <- st_read(dsn = "MGN_DPTO_POLITICO.shp")
# 
# colombis_dept <- colombis_dept %>% 
#   filter(DPTO_CNMBR != "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA")
# 
# colombia <- colombis_dept %>% 
#   mutate(union = 1) %>% 
#   group_by(union) %>% 
#   tally() %>% 
#   ungroup()
# 
# 
# colombia %>% 
#   ggplot() +
#   geom_sf()


# Obtencion de datos de rios
#------------------------------

# "https://www.hydrosheds.org/products/hydrobasins"

hydroBaseAmerica <- sf::st_read(dsn = "rmd/bases/geo/hybas_sa_lev03_v1c.shp")

# Pegado de shapes
#--------------------


sf_use_s2(F)

# st_crs(hydroBaseAmerica)
# st_crs(colombia)

# colombia <- st_set_crs(colombia,
#                        st_crs(hydroBaseAmerica))


colombia_rios_base <- hydroBaseAmerica %>% 
  sf::st_intersection(country) %>% 
  dplyr::select(HYBAS_ID)

# Obtenemos datos de los rios
#-----------------------------

hydroRiosAmerica <- st_read(dsn = "rmd/bases/geo/HydroRIVERS_v10_sa.shp")

colombia_rios <- hydroRiosAmerica %>% 
  dplyr::select(ORD_FLOW) %>% 
  st_intersection(country)

 
# Pegamos los datos hidrologicos de Colombia
#------------------------------------------

colombiaHydro <- st_intersection(
  colombia_rios,
  colombia_rios_base
)

# Debido a las caracteristicas de los rios, es poco discernible el mapa
# por ello, es necesario hacer que el ancho de las lineas de los rios, 
# dependan de la longitud del rio.

# river width
#--------------------

unique(colombiaHydro$ORD_FLOW)

colombia_width <- colombiaHydro %>% 
  mutate(width = as.numeric(ORD_FLOW),
         width = case_when(
           # width == 1 ~ .8,
           width == 2 ~ .7,
           width == 3 ~ .6,
           width == 4 ~ .5,
           width == 5 ~ .4,
           width == 6 ~ .3,
           width == 7 ~ .2,
           width == 8 ~ .15,
           # width == 9 ~  .1,
           TRUE ~ 0,
         )) %>% 
  st_as_sf()

# City names

filename <- "geonames-population.csv"

# get_geonames_data <- function(){
#     table_link <- "https://documentation-resources.opendatasoft.com/api/explore/v2.1/catalog/datasets/doc-geonames-cities-5000/exports/csv?lang=en&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"
#     res <- httr::GET(
#         table_link,
#         httr::write_disk(
#             filename
#         ),
#         httr::progress()
#     )
# }
# 
# get_geonames_data()

load_geonames_data <- function(){
  places_df <- read.csv(
    paste(getwd(), "/rmd/bases/geo/", filename, sep = ""),
    sep = ";"
  )
  return(places_df)
}

places_df <- load_geonames_data()

head(places_df)

places_modified_df <- places_df[, c(2, 7, 13, 18)]
names(places_modified_df) <- c(
  "name", "country_code", "pop", "coords")

places_modified_df[c("lat", "long")] <-
  stringr::str_split_fixed(
    places_modified_df$coords, ",", 2
  )

places_clean_sf <- places_modified_df |>
  dplyr::filter(country_code == "CO") |>
  dplyr::slice_max(
    pop,
    n = 4
  ) |>
  dplyr::select(
    -coords,
    -country_code,
    # -pop
  ) |>
  sf::st_as_sf(
    coords = c(
      "long",
      "lat"
    ),
    crs = 4326
  )


#################################################################################################################################################
# Titulo: Relevancia de la macrocuenca Magdalena-Cauca en la generación eléctrica
# Tema: Este script muestra la ubicación de las plantas hidroeléctricas en Colombia utilizando como base el mapa de macrocuencas hidrograficas,
#       la información que contiene la ubicación espacial de las plantas se encuentra disponible en el archivo 'Listado_Recursos_Generacion_xm.xlsx'
#
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 8 de Julio de 2024                                                                                                                     
# Base de datos: Esta vicualización combina las bases geoespaciales de HydroBasis en que se basa el mapa anterior ('Cuencas hidrograficas de Colombia')
#                y la base que contiene la ubicación geoespacial de cada planta hidroeléctrica, la cual se obtuvo a partir de la busqueda manual en 
#                google maps y la lista de recursos de generación de XM.
# 
#################################################################################################################################################


# Levantado de base Global Power Plant Database
library(readxl)

wppd <- read_excel("rmd/bases/InfoServPubli/Listado_Recursos_Generacion_xm.xlsx")

wppd %>% glimpse()

wppd$geo

wppd[c("lat", "long")] <-
  stringr::str_split_fixed(
    wppd$geo, ",", 2
  )

nombres <- names(wppd)

nombres <- str_replace_all(nombres, " ", "_")

names(wppd) <- nombres

wppd <- wppd %>% 
  dplyr::filter(!is.na(geo)) %>% 
  dplyr::select(`Capacidad_Efectiva_Neta_[MW]`,
         lat,
         long) %>% 
  dplyr::mutate(lat = as.numeric(lat),
                long = as.numeric(long)) %>% 
  sf::st_as_sf(
    coords = c(
      "long",
      "lat"
    ),
    crs = 4326
  )


# Grafico
#-------------------

unique(colombia_width$HYBAS_ID)

# m1 <- ggplot() +
#   geom_sf(
#   data = colombia_width,
#   aes(color = factor(macrocuenca))) +
#   scale_color_manual(values = hcl.colors(
#     5, "Dark 3", alpha = 1
#   )
#   )
# 
# ggsave(plot = m1,
#        filename = "m1.png", width = 8, height = 10)


colombia_width <- colombia_width %>% 
  mutate(macrocuenca = case_when(
    HYBAS_ID == 6030000010 ~ "Caribe",
    HYBAS_ID == 6030000740 ~ "Magdalena-Cauca",
    HYBAS_ID == 6030000750 ~ "Caribe",
    HYBAS_ID == 6030004470 ~ "Orinoco",
    HYBAS_ID == 6030007000 ~ "Amazonas",
    HYBAS_ID == 6030032290 ~ "Pacífico"
  ))

wppd <- wppd %>% 
  mutate(tamanios = ifelse(`Capacidad_Efectiva_Neta_[MW]` >= 500 & `Capacidad_Efectiva_Neta_[MW]` < 1000, 2,
                           ifelse(`Capacidad_Efectiva_Neta_[MW]` >= 1000, 3, 1)))

unique(colombia_width$macrocuenca)

colores <- hcl.colors(
  5, "Dark 3", alpha = 1
)


# class(places_clean_sf)
p <- ggplot() +
  geom_sf(data = region, fill = "lightgrey") +
  geom_sf_text(data = region %>% 
                 filter(CNTR_NAME != "Colombia"), 
               aes(label = NAME_ENGL), 
               size = 3) +
  geom_sf(
    data = colombia_width,
    aes(color = factor(macrocuenca),
        size = width,
        alpha = width),
    show.legend = F
  ) +
  # geom_sf_text(data = colombia_width,
  #               aes(label = macrocuenca)) +
  scale_color_manual(
    name = "",
    values = hcl.colors(
      5, "Dark 3", alpha = 1
    )
  ) +
  scale_size(
    range = c(0.1, .7)
  ) +
  scale_alpha(
    range = c(0.01, .7)
  ) +
  geom_sf(data = places_clean_sf,
          shape = 18,
          color = "darkred",
          size = 2.5,
          alpha = 0.6) +
  geom_sf_text(data = places_clean_sf,
               aes(label = name),
               nudge_x = c(0.7, 0.5, -0.7, 1),
               size = 3) +
  # geom_sf(data = wppd,
  #         aes(shape = as.factor(tamanios)),
  #         color = "blue",
  #         alpha = 0.6,
  #         show.legend = F) +
  # scale_shape_manual(values = c(16, 17, 15)) +
  coord_sf(xlim = extent(colombia_width)[1:2],
           ylim = c(-4, 13)) +
  labs(title = 'Cuencas hidrográfica de Colombia',
       subtitle = "<span style = 'color:#E16A86FF;'>Amazonas</span>, <span style = 'color:#AA9000FF;'>Caribe</span>, <span style = 'color:#00AA5AFF;'>Magdalena-Cauca</span>, <span style = 'color:#00A6CAFF;'>Orinoco</span> y <span style = 'color:#B675E0FF;'>Pacífico</span>",
       caption = "Fuente: elaboración propia en base HydroSHEDS.org",
       x = 'Lon',
       y = 'Lat') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0.5, size = 9),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        panel.grid.major = element_line(color = gray(.5), linetype = 'dashed', linewidth = 0.5),
        panel.background = element_rect(fill = '#63799B')) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         # pad_x = unit(0.65, "in"), pad_y = unit(0.4, "in"),
                         height = unit(1.2, 'cm'), width = unit(0.9, 'cm'),
                         style = north_arrow_fancy_orienteering)


p2 <- ggplot() +
  geom_sf(data = region, fill = "lightgrey") +
  geom_sf_text(data = region %>% 
                 filter(CNTR_NAME != "Colombia"), 
               aes(label = NAME_ENGL), 
               size = 3) +
  geom_sf(
    data = colombia_width,
    aes(color = factor(macrocuenca),
        size = width,
        alpha = width),
    show.legend = F
  ) +
  scale_color_manual(
    name = "",
    values = hcl.colors(
      5, "Dark 3", alpha = 1
    )
  ) +
  scale_size(
    range = c(0.1, .7)
  ) +
  scale_alpha(
    range = c(0.01, .7)
  ) +
  geom_sf(data = places_clean_sf,
          shape = 18,
          color = "darkred",
          size = 2.5,
          alpha = 0.6) +
  geom_sf_text(data = places_clean_sf,
               aes(label = name),
               nudge_x = c(0.3, 0.3, -0.3, 1),
               size = 3) +
  geom_sf(data = wppd,
          aes(shape = as.factor(tamanios)),
          color = "blue",
          alpha = 0.6,
          show.legend = F) +
  scale_shape_manual(values = c(16, 17, 15)) +
  coord_sf(xlim = c(-78,-72.5), 
           ylim = c(0.5, 8.5)) +
  labs(title = 'Ubicación de las plantas de generación hidroeléctrica en Colombia',
       subtitle = "Los puntos azules señalan las plantas hidroeléctricas al 2023",
       caption = "Fuente: elaboración propia en base HydroSHEDS.org y XM",
       x = 'Lon',
       y = 'Lat') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0.5, size = 9),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        panel.grid.major = element_line(color = gray(.5), linetype = 'dashed', linewidth = 0.5),
        panel.background = element_rect(fill = '#63799B')) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         # pad_x = unit(0.65, "in"), pad_y = unit(0.4, "in"),
                         height = unit(1.2, 'cm'), width = unit(0.9, 'cm'),
                         style = north_arrow_fancy_orienteering)

  


ggsave(plot = p, 
       filename = paste(getwd(), "/rmd/resultados/graficos/", 'mapa3_rios_col.png', sep = ""),
       units = 'in', width = 8, height = 10, dpi = 300)

ggsave(plot = p2, 
       filename = paste(getwd(), "/rmd/resultados/graficos/", 'mapa4_rios_col2.png', sep = ""),
       units = 'in', width = 8, height = 10, dpi = 300)

places_clean_sf %>% filter(name %in% c("Bogotá","Medellín","Cali","Manizales","Pereira","Ibagué")) %>% 
  dplyr::select(pop) %>% 
  as_tibble() %>% 
  summarise(sum(pop))

