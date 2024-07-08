
#################################################################################################################################################
# Titulo: División del territorio colombiano entre SIN y ZNI
# Tema: Este script muestra de manera aproximada a partir de información oficial de la SSPD y del Ministerio de Minas y Energía de Colombia
#       los cuales tienen información del consumo de diesel en zonas no interconectadas, los territorios que contaban con algún registro se 
#       consideraron no interconectados, mientras los municipios que no contaban con registro se consideraron interconectados.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 8 de Julio de 2024                                                                                                                     
# Base de datos: Parte de los datos georeferenciados se obtienen del paquete giscoR, la diferenciación interna del pais esta especificada en 
#                el archivo .shp 'cobertura_electrica_Colombia.shp'
# 
#################################################################################################################################################


rm(list = ls())

libs <- c(
  "geodata",
  "elevatr",
  "terra", 
  "sf",
  "tidyverse",
  "classInt",
  "giscoR",
  "rayshader",
  "magick",
  "raster",
  "ggspatial",
  "ggtext"
)

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


# mupos <- read_sf("rmd/bases/geo/MGN_MPIO_POLITICO.shp", quiet = T)
# 
# supervicios <- list.files(path = "rmd/bases",
#                           pattern = "Superservicios*")
# 
# for(i in 1:length(supervicios)){
# 
#   supervicios[i] <- paste("rmd/bases/", supervicios[i], sep = "")
# 
# }
# 
# super <- lapply(supervicios, read.csv)
# 
# super[[4]] <- super[[4]] %>%
#   mutate(Código.Localidad = as.character(Código.Localidad),
#          CODIGO_DANE = ifelse(nchar(Código.Localidad) < 13,
#                                    str_sub(Código.Localidad, start = 1, end = 7),
#                                    str_sub(Código.Localidad, start = 1, end = 8)))
# 
# 
# 
# vecs <- list(super[[1]],super[[2]],super[[3]], super[[4]]) %>% map(.f = ~{
# 
#   .x %>%
#     mutate(CODIGO_DANE = as.character(CODIGO_DANE),
#            llave = ifelse(nchar(CODIGO_DANE) < 8,
#                               str_c("0", CODIGO_DANE, sep = ""),
#                               CODIGO_DANE),
#            llave = str_sub(llave, start = 1, end = 5)) %>%
#     dplyr::select(llave) %>%
#     distinct()
#   })
# 
# 
# vecs <- vecs %>% bind_rows()
# vecs <- vecs %>% distinct()
# vecs <- vecs %>%
#   mutate(ZNI = 1)
# 
#  
# mupos <- mupos %>%
#   left_join(y = vecs,
#             by = c("MPIO_CCNCT" = "llave"))
# 
# mupos <- mupos %>%
#   mutate(ZNI = ifelse(is.na(ZNI), 0, ZNI))
#  
# mupos <- mupos %>%
#   mutate(ZNI = ifelse(DPTO_CNMBR == "GUAINÍA" | DPTO_CNMBR == "VAUPÉS", 1, ZNI))
# 
# muposElect <- mupos %>%
#    group_by(ZNI) %>%
#    tally() %>%
#    ungroup()
# 
# st_write(obj = muposElect,
#          dsn = "D:/GERARDO/Documents/Escritorio/FLACSO/Tesis/petroleo/Tesis/rmd/bases/geo",
#          layer = 'cobertura_electrica_Colombia',
#          driver = 'ESRI Shapefile')
  

get_country_borders <- function(pais){
  
  country_borders <- giscoR::gisco_get_countries(
    resolution = "3",
    country = pais
  )
  
  return(country_borders)
  
}

# country <- get_country_borders(pais = "CO")

region <- get_country_borders(pais = c("CO","BR","EC", "PA", "PE", "VE"))

muposElect <- read_sf("rmd/bases/geo/cobertura_electrica_Colombia.shp", quiet = T)

muposElect <- muposElect %>% 
  sf::st_as_sf(crs = 4326)

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
  dplyr::filter(name %in% c("Bogotá",
                            "Cali",
                            "Medellín",
                            "Barranquilla",
                            "Cartagena",
                            "Cúcuta",
                            "Bucaramanga",
                            "Pereira",
                            "Santa Marta",
                            "Ibagué",
                            "Pasto",
                            "Manizales",
                            "Neiva",
                            "Villavicencio",
                            "Tunja",
                            "Popayán")) %>% 
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

places_clean_sf <- places_clean_sf %>% 
  mutate(cate = ifelse(name %in% c("Medellín", "Bogotá", "Cali", "Barranquilla"), "capitalPrincipal", "capital"))

unique(places_clean_sf$name)

vec_nudge <- ifelse(nchar(places_clean_sf$name) <= 5 | places_clean_sf %in% c("Pereira", "Cúcuta", "Ibagué"), 0.4,
                    ifelse(nchar(places_clean_sf$name) >= 6 & nchar(places_clean_sf$name) <= 8, 0.5,
                           ifelse(nchar(places_clean_sf$name) == 9, 0.7, 0.8)))


# Grafico

mapa_SIN <- ggplot() +
  geom_sf(data = region, fill = "lightgrey") +
  geom_sf_text(data = region %>% 
                 filter(CNTR_NAME != "Colombia"), 
               aes(label = NAME_ENGL), 
               size = 3) +
  geom_sf(data = muposElect,
          mapping = aes(fill = as.factor(ZNI)),
          color = "white",
          show.legend = F) +
  scale_fill_manual(values = c("#AF601A", "#117A65")) +
  geom_sf(data = places_clean_sf,
          aes(size = pop,
              shape = cate,
              color = cate),
          alpha = 0.5,
          show.legend = F) +
  geom_sf_text(data = places_clean_sf,
               aes(label = name),
               nudge_x = vec_nudge,
               size = 3) +
  coord_sf(xlim = c(-80,extent(muposElect)[2]), 
           ylim = c(-4, 13)) +
  scale_shape_manual(values = c(16, 18)) +
  scale_color_manual(values = c("black", "darkred")) +
  labs(title = "Cobertura del sistema eléctrico por municipio",
       subtitle = "<span style = 'color:#AF601A;'>Sistema Interconectado Nacional (SIN)</span> y <span style = 'color:#117A65;'>Zonas No Interconectadas (ZNI)</span>",
       caption = "Fuente: elaboración propia en base a información del Ministerio de Minas y Energía",
       x = 'Lon',
       y = 'Lat')+
  theme_test() +
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


ggsave(plot = mapa_SIN, 
       filename = paste(getwd(), "/rmd/resultados/graficos/", 'mapa5_SIN_col.png', sep = ""),
       units = 'in', width = 8, height = 10, dpi = 300)


