
#################################################################################################################################################
# Titulo: Emisiones y matriz de generación eléctrica en el mundo                                                                               #
# Tema: Este gráfico muestra la evolución de las emisiones por sector según el IPCC entre 1990 y 2020                                                                                   # 
#       junto con la participación por fuente de energía primaria en la generación de eléctricidad en el mundo
#       al 2022 
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 1 de Julio de 2023                                                                                                                     #
# Comentarios adicionales: Este Script primero ejecuta el script baseEnergia.R, el cual utiliza la base de Our World in Data sobre energía
#                          comentarios sobre la limpieza de la base para poder empezar a gráficar se encuentran en ese script. 
#################################################################################################################################################

source("rmd/scripts/baseEnergia.R")

# Filter data for creating the first graph (baseCo2Graph1)
baseCo2Graph1 <- baseCo2 %>% 
  filter(Country == "WORLD" | Country == "COL",
         year == "1990" | year == "2020") %>%
  select(Country,year, Sector, categorica, participacion) %>% 
  spread(key = year, value = participacion) %>% 
  mutate(x1990 = `1990`,
         x2020 = `2020`,
         constante1990 = 1,
         constante2020 = 2)


# Create a ggplot object (g3) for the first graph
A <- baseCo2Graph1 %>% 
  filter(Country == "WORLD") %>% 
  ggplot() +
  # Scatter plot for 1990
  geom_point(mapping = aes(x = x1990, y = reorder(categorica, x2020), col = as.factor(constante1990)), show.legend = F, size = 2) +
  # Scatter plot for 2020
  geom_point(mapping = aes(x = x2020, y = reorder(categorica, x2020), col = as.factor(constante2020)), show.legend = F, size = 4) +
  # Set manual color values
  scale_color_manual(values = c("#FF5733", "#900C3F")) +
  # Add line segments connecting the points
  geom_segment(mapping = aes(x = x1990, xend = x2020, y = categorica, yend = categorica)) +
  # Add a vertical line
  geom_vline(aes(xintercept = 0.1), linewidth = 0.7, linetype = "longdash", alpha = 0.6) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=10.5,ymax=11.5),
            fill="gray", alpha = 0.04)+
  # Set x-axis breaks
  scale_x_continuous(n.breaks = 10) +
  # Set plot labels and titles
  labs(x = NULL,
       y = NULL,
       title = "A: Proporción de emisiones según categorías IPCC",
       subtitle = "Mundo <span style='color:#FF5733;'>1990</span> y <span style='color:#900C3F;'>2020</span>",
       caption = "Fuente: Elaboración propia en base a Climate Watch") +
  # Set theme settings
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 11),
        plot.subtitle = element_markdown(hjust = 0.5, size = 9, face = "italic"),
        plot.caption = element_text(size=8, hjust=1, face="italic", color="black"),
        axis.text.x = element_text(size = 7, angle = 0),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

baseCo2Graph1 %>% 
  filter(Country == "WORLD")

# Grafico de participación de fuentes

EnerCol <- base %>% filter(country == "Colombia" | country == "World") %>% 
  select(country, year, iso_code, ends_with("electricity"))

EnerCol <- EnerCol %>% 
  gather(key = tipo, value = valor, 4:length(EnerCol))

source_elec <- c("biofuel_electricity",
                 "hydro_electricity",
                 "coal_electricity",
                 "gas_electricity",
                 "oil_electricity",
                 "wind_electricity",
                 "solar_electricity",
                 "other_renewable_exc_biofuel_electricity")

map1 <- source_elec %>% 
  map(.f = ~{
    
    EnerCol %>% 
      filter(tipo == .x,
             year > 1989)
  })


map1 <- map1 %>% bind_rows()

map1 <- map1 %>% 
  mutate(renovable = case_when(
    tipo == "biofuel_electricity" ~ 1,
    tipo == "hydro_electricity" ~ 2, 
    tipo == "solar_electricity" ~ 1, 
    tipo == "wind_electricity" ~ 1,
    tipo == "other_renewable_exc_biofuel_electricity" ~ 1,
    tipo == "coal_electricity" ~ 0,
    tipo == "gas_electricity" ~ 0,
    tipo == "oil_electricity" ~ 0),
    nombre = case_when(
      tipo == "biofuel_electricity" ~ "Bioenergía",
      tipo == "hydro_electricity" ~ "Hidroeléct.", 
      tipo == "solar_electricity" ~ "Solar", 
      tipo == "wind_electricity" ~ "Eólica", 
      tipo == "coal_electricity" ~ "Carbón",
      tipo == "gas_electricity" ~ "Gas",
      tipo == "oil_electricity" ~ "Oil",
      tipo == "other_renewable_exc_biofuel_electricity" ~ "Otras Renov."))

map1 <- map1 %>% 
  mutate(nombre = factor(nombre, levels = c("Carbón", "Oil", "Gas", 
                                            "Bioenergía", "Solar", "Eólica", "Otras Renov.",
                                            "Hidroeléct.")),
         renovable = factor(renovable))

map2 <- map1 %>% 
  filter(year == 2022) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(prop = round(valor/sum(valor, na.rm = T)*100,2)) %>% 
  ungroup() %>% 
  mutate(etiqueta = paste(nombre, prop, "%"),
         etiquetas = ifelse(country == "Colombia" & renovable == 1, NA,
                            ifelse(nombre == "Otras Renov.", NA, etiqueta)))


fuentes_evolucion <- map1 %>%
  filter(year > 1989 & country == "World") %>%
  ungroup() %>%
  group_by(year, country) %>%
  mutate(prop = valor/sum(valor, na.rm = T)) %>%
  ungroup() %>%
  mutate(etiqueta = paste(nombre, prop, "%"),
         etiquetas = ifelse(country == "Colombia" & renovable == 1, NA,
                            ifelse(nombre == "Otras Renov.", NA, etiqueta))) %>%
  ungroup() %>%
  group_by(year, nombre) %>%
  summarise(tot = sum(valor, na.rm = T))%>%
  spread(key = nombre, value = tot)

fuentes_evolucion <- fuentes_evolucion %>% 
  gather(key = tipo, value = valor, 2:9)

fuentes_evolucion1 <- fuentes_evolucion %>% 
  filter(year == 1990)

fuentes_evolucion1 <- fuentes_evolucion1 %>% 
  left_join(y = fuentes_evolucion %>% 
              filter(year == 2020), by = c("tipo"))


fuentes_evolucion1 <- fuentes_evolucion1 %>% 
  mutate(crec_anual = ((1+(valor.y/valor.x - 1))^(1/30)-1)*100)


alpha_max <- 1
alpha_min <- 0.6
alpha_vals <- c(
  seq(alpha_max, alpha_min, length.out = 3), 
  seq(alpha_max, alpha_min, length.out = 4),
  alpha_max)


B <- map2 %>% 
  filter(country == "World") %>% 
  ggplot(mapping = aes(x = as.factor(country), y = prop, fill = renovable, alpha = nombre)) +
  geom_col(col = "white", width = 1) +
  scale_alpha_manual(values = alpha_vals) +
  scale_fill_manual(values = c("#000000", "#249206", "#3667A6")) +
  guides(
    fill = guide_none(),
    alpha = guide_legend(override.aes = list(fill = c(rep("#283227", 3),
                                                      rep("#249206", 4),
                                                      "#3667A6")))
  ) +
  geom_text(
    data = map2 %>% filter(country == "World"),
    aes(label = etiquetas),
    position = position_stack(vjust = 0.70),
    col = 'white',
    size = 3,
    fontface = 'bold'
  ) +
  labs(title = "B: Participación porcentual por fuente",
       subtitle = "generación de electricidad en el mundo 2022",
       x = NULL,
       y = "Porcentaje") +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.5, size = 11),
        plot.subtitle = element_text(hjust = 0.5, size = 8),
        plot.caption = element_text(size=6, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

## Pegado de los graficos y guardado

graph_emisiones_fuentes_mundo <- wrap_plots(A, B, ncol = 2, widths = c(0.8, 0.5))

ggsave(plot = graph_emisiones_fuentes_mundo, filename = "rmd/resultados/graficos/grafico1.2_emisiones_fuentes_mundo.png",
       units = "in", width = 10.1, height = 4.81)

