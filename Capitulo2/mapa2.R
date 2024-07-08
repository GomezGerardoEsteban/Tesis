
#################################################################################################################################################
# Titulo: Topografía de Colombia y ubicación de principales ciudades
# Tema: Este script utiliza información Open Topography Global Database, esta realizado en base al tutorial de 
#       @milos-make-maps, disponible en "https://www.youtube.com/watch?v=NY34O3H9qQQ&t=1926s&pp=ygUQbWlsb3MgbWFrZXMgbWFwcw%3D%3D"
#
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        
# Fecha: 8 de Julio de 2024                                                                                                                     
# Base de datos: La obtención de los datos de elevación provienen del paquete 'elevatr', se encuentra bien explicada en el tutorial de 
#                @milos-make-maps, es replicable para cualquier país.
#                pdta: No incluye la parte en que desarrolla la visualización en tercera dimensión
# 
#################################################################################################################################################



rm(list=ls())

# To made a map with population distribution and topographic conditions, 
# we need generate two dataframes from raster files, the first with elevation information
# and the second with population information, after, we can draw the map using both dataframes


# 1. PACKAGES


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

## Levantado de bases

# Archivo paises vecinos y col

wrld <- giscoR::gisco_get_countries(
  country = c("CO","BR","EC", "PA", "PE", "VE"),
  resolution = "3")

plot(wrld)
# Archivo de colombia

col_sf <- giscoR::gisco_get_countries(
  country = c("CO"),
  resolution = "3")

plot(col_sf)

# Archivo con informacion de altura 

elev <- elevatr::get_elev_raster(
  locations = col_sf,
  z = 8, 
  clip = "locations"
)

elev_lambert <- elev |>
  terra::rast()

elev_df <- as.data.frame(
  elev_lambert,
  xy = TRUE
)

names(elev_df)[3] <- "dem"

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

# 2D MAP
#----------

summary(elev_df$dem)

elev_df$dem <- ifelse(elev_df$dem < 0, 0, elev_df$dem)

breaks <- classInt::classIntervals(
  elev_df$dem,
  n = 7,
  style = "pretty"
)$brks

colors <- hcl.colors(
  n = length(breaks),
  palette = "Zissou 1"
)

texture <- colorRampPalette(
  colors
)(512)

places_clean_sf <- places_clean_sf %>% 
  mutate(cate = ifelse(name %in% c("Medellín", "Bogotá", "Cali", "Barranquilla"), "capitalPrincipal", "capital"))

unique(places_clean_sf$name)

vec_nudge <- ifelse(nchar(places_clean_sf$name) <= 5 | places_clean_sf %in% c("Pereira", "Cúcuta", "Popayán", "Ibagué"), 0.5,
                ifelse(nchar(places_clean_sf$name) >= 6 & nchar(places_clean_sf$name) <= 8, 0.7,
                       ifelse(nchar(places_clean_sf$name) == 9, 0.8, 1)))

# MAPA

map_geo <- ggplot() +
  geom_sf(data = wrld, fill = "lightgrey") +
  geom_sf_text(data = wrld %>% 
                 filter(CNTR_NAME != "Colombia"), 
               aes(label = NAME_ENGL), 
               size = 3) +
  geom_raster(data = elev_df, 
            aes(x = x, y = y, fill = dem)) +
  scale_fill_gradientn(
    name = "Elev. (mt)",
    colors = texture,
    breaks = breaks,
    labels = round(breaks, 0),
    limits = c(
      min(elev_df$dem),
      max(elev_df$dem)
    )
    )+
    guides(
        fill = guide_colorbar(
          direction = "vertical",
          barheight = unit(30, "mm"),
          barwidth = unit(3, "mm"),
          title.position = "top",
          label.position = "right",
          title.hjust = .5,
          label.hjust = .5,
          ncol = 1,
          byrow = FALSE
        )
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
      coord_sf(xlim = extent(elev_df)[1:2], 
               ylim = c(-4, 13)) +
      labs(title = 'Topografía de Colombia',
           caption = "Fuente: elaboración propia en base a Open Topography Global Datasets",
           x = 'Lon',
           y = 'Lat') +
      theme_bw() +
      theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
            # plot.subtitle = element_markdown(hjust = 0.5, size = 9),
            axis.title.y = element_text(size = 7),
            axis.title.x = element_text(size = 7),
            panel.grid.major = element_line(color = gray(.5), linetype = 'dashed', linewidth = 0.5),
            panel.background = element_rect(fill = '#63799B')) +
      annotation_scale(location = "bl", width_hint = 0.2) +
      annotation_north_arrow(location = "tr", which_north = "true",
                             # pad_x = unit(0.65, "in"), pad_y = unit(0.4, "in"),
                             height = unit(1.2, 'cm'), width = unit(0.9, 'cm'),
                             style = north_arrow_fancy_orienteering)
    
ggsave(plot = map_geo, 
           filename = paste(getwd(), "/rmd/resultados/graficos/", 'mapa2_geo_col.png', sep = ""),
           units = 'in', width = 8, height = 9, dpi = 300)
    


# mapa 3d -----------------------------------------------------------------


h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

dev.off()

rayshader::plot_gg(
  ggobj = map_geo,
  width = w / 800,
  height = h / 800,
  scale = 150,
  solid = FALSE,
  shadow = TRUE,
  shadowcolor = "white",
  shadowwidth = 0,
  shadow_intensity = 1,
  zoom = .7,
  phi = 90,
  theta = 0
  # window.size = c(800, 800)
)


url <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- basename(url)

download.file(
  url = url,
  destfile = hdri_file,
  mode = "wb"
)

file_name <- "3d-TOPOGRAPHY-col.png"

rayshader::render_highquality(
  filename = file_name,
  preview = TRUE,
  light = TRUE,
  environment_light = hdri_file,
  intensity_env = .6,
  interactive = FALSE,
  camera_location = 
  width = w,
  height = h
)

