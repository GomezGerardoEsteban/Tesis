
#################################################################################################################################################
# Titulo: Distribución poblacional de Colombia y ubicación de principales ciudades
# Tema: Este script utiliza información geoespacial de GHS, European Comission, esta realizado en base al tutorial de 
#       @milos-make-maps, disponible en "https://www.youtube.com/watch?v=2lDu2oq0bUA&t=568s&pp=ygUQbWlsb3MgbWFrZXMgbWFwcw%3D%3D"
#
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 8 de Julio de 2024                                                                                                                     
# Base de datos: La descarga via API que contiene la información de población a 30 arc-sec, se encuentra bien explicada en el tutorial de 
#                @milos-make-maps, es replicable para cualquier país.
# 
#################################################################################################################################################


getwd()

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
  "raster",
  "magick",
  "ggspatial",
  "ggtext"
)

installed_libraries <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libraries == F)){
  install.packages(
    libs[!installed_libraries],
    dependencies = T
  )
}

invisible(
  lapply(
    libs, library, character.only = T
  )
)

# Levantado de bases

wrld <- giscoR::gisco_get_countries(
  country = c("CO","BR","EC", "PA", "PE", "VE"),
  resolution = "3")

# Levantado de colombia

col_sf <- giscoR::gisco_get_countries(
  country = c("CO"),
  resolution = "3")

# Datos de poblacion segun Non-Residential Database of Europe Union

file_name <- paste(getwd(), "/rmd/bases/geo/", "GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.tif", sep = "")

pop <- terra::rast(file_name)

# 3. Colombia SHAPEFILE
#-----------------------

# 4. CROP Colombia GHSL
#-----------------------

colombian_pop <- terra::crop(
  x = pop,
  y = terra::vect(col_sf),
  snap = "in",
  mask = T
)

# 5. RASTER TO DATAFRAME
#-----------------------

colombian_pop_df <- as.data.frame(
  colombian_pop,
  xy = T, na.rm = T
)

head(colombian_pop_df)

names(colombian_pop_df)[3] <- "val"

colombian_pop_df <- colombian_pop_df %>% 
  dplyr::mutate(
    cat = dplyr::if_else(
      val > 0, "Si", "No"
    )
  )

colombian_pop_df$cat <- as.factor(
  colombian_pop_df$cat
)

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

vec_nudge <- ifelse(nchar(places_clean_sf$name) <= 5 | places_clean_sf %in% c("Pereira", "Cúcuta", "Popayán", "Ibagué"), 0.5,
                    ifelse(nchar(places_clean_sf$name) >= 6 & nchar(places_clean_sf$name) <= 8, 0.7,
                           ifelse(nchar(places_clean_sf$name) == 9, 0.8, 1)))
# grafico

colores <- c("#0E6251", "#D35400")

pob_map <- ggplot() +
  geom_sf(data = wrld, fill = "lightgrey") +
  geom_sf_text(data = wrld %>% 
                 filter(CNTR_NAME != "Colombia"), 
               aes(label = NAME_ENGL), 
               size = 3) +
  geom_tile(data = colombian_pop_df, 
            aes(x = x, y = y, fill = cat),
            show.legend = F) +
  scale_fill_manual(
    values = colores,
    na.value = "#0E6251"
  ) +
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
  scale_shape_manual(values = c(16, 18)) +
  scale_color_manual(values = c("black", "darkred")) +
  coord_sf(xlim = extent(colombian_pop_df)[1:2], 
           ylim = c(-4, 13)) +
  labs(title = 'Distribución poblacional de Colombia 2023',
       subtitle = "Territorios <span style = 'color:#D35400;'>CON</span> y <span style = 'color:#0E6251;'>SIN</span> población",
       caption = "Fuente: elaboración propia en base a GHS, European Comission",
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


ggsave(plot = pob_map, 
       filename = paste(getwd(), "/rmd/resultados/graficos/", 'mapa1_pob_col.png', sep = ""),
       units = 'in', width = 8, height = 10, dpi = 300)  











