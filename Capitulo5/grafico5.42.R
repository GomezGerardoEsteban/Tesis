rm(list = ls())

library(tidyverse)
library(readxl)

base <- read_excel(path = "rmd/bases/base_pib_banrep_1905_2021.xlsx")

base %>% glimpse()

crec <- c()
for(i in 1:(nrow(base)-1)){
  
  crec[i] <- base$PIB[i+1]/base$PIB[i] - 1
  
}

base$crec <- c(NA, crec)

base$categoria <- case_when(base$crec <= 0 ~ "decrecimiento",
                            base$crec > 0 & base$crec <= .01 ~ "lento",
                            base$crec > .01 & base$crec <= .03 ~ "moderado",
                            base$crec > .03 & base$crec <= .06 ~ "acelerado",
                            base$crec > .06 ~ "muy acelerado")

table(base$categoria)

histograma <- base %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = crec, y = after_stat(density)), 
                 color = "black",
                 fill = "lightgrey",
                 bins = 25) +
  geom_density(mapping = aes(x = crec), color = "darkred", linewidth = 1) +
  labs(title = "Histograma de las tasas de crecimiento anual de la economía colombiana entre 1906 y 2021",
       x = "Tasa de crecimiento anual",
       y = "Densidad",
       caption = "Fuente: Elaboración propia en base a Banco de la República") +
  scale_x_continuous(n.breaks = 10) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 11),
        plot.subtitle = element_markdown(hjust = 0.0, size = 9, face = "italic"),
        plot.caption = element_text(size=8, hjust=1, face="italic", color="black"),
        axis.text.x = element_text(size = 9, angle = 0),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

ggsave(filename = "rmd/resultados/graficos/grafico5.42_histogramaCrec.png",
       plot = histograma,
       width = 8.7,
       height = 4.6,
       dpi = 500)


