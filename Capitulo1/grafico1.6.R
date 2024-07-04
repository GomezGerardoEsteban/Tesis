
#################################################################################################################################################
# Titulo: Emisiones en Colombia por categorías del IPCC
# Tema: Este gráfico muestra la evolución de las emisiones de GEI en CO2 equivalente en Colombia, por categorias del IPCC.
#       Es un grafico que muestra la evolución entre 1990 y 2020.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 4 de Julio de 2024                                                                                                                     #
# Base de datos: Este gráfico usa la base sobre emisiones de climate watch cuya manipulación se genera en el script 
#                **baseEnergia.R**
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

# Grafico de colombia emisiones ipcc y vs mundo  --------------------------

## Grafico colombia ipcc

A <- baseCo2Graph1 %>% 
  filter(Country == "COL") %>% 
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
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=7.5,ymax=8.5),
            fill="gray", alpha = 0.04)+
  # Set x-axis breaks
  scale_x_continuous(n.breaks = 10) +
  # Set plot labels and titles
  labs(x = NULL,
       y = NULL,
       title = "Proporción de emisiones según categorías del IPCC en Colombia",
       subtitle = "<span style='color:#FF5733;'>1990</span> y <span style='color:#900C3F;'>2020</span>",
       caption = "Fuente: Elaboración propia en base a Climate Watch") +
  # Set theme settings
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_markdown(hjust = 0.5, size = 11, face = "italic"),
        plot.caption = element_text(size=8, hjust=1, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

ggsave(plot = A, filename = "rmd/resultados/graficos/grafico1.6_colombia_emisiones_ipcc_Colombia.png",
       units = "in", 
       width = 9.66,
       height = 3.5*1.3,
       dpi = 300)


