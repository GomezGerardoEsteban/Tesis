
#################################################################################################################################################
# Titulo: Consumo de electricidad per cápita según PIB per cápita e IDH
# Tema: Utilizando datos de 2019, este gráfico muestra la relación log-log entre consumo de electricidad per capita
#       y consumo de electricidad per capita, ademas el consumo medio de electricidad y los respectivos intervalos 
#       de confianza al 95% según categorias del Indice de Desarrollo Humano.
#       De los graficos se concluye que Colombia tiene un consumo de electricidad menor al que se esperaría según su
#       nivel de PIB per Capita y nivel de desarrollo económico
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 6 de Julio de 2024                                                                                                                     #
# Base de datos: Para generar esta visualización es necesario correr los scripts complementarios de **baseEnergia.R**
#                y de **descargaBaseEnergia.R**
#################################################################################################################################################

source("rmd/scripts/baseEnergia.R")

baseGraph <- base %>% 
  filter(year == 2019 &
           hdicode != "" & 
           !is.na(hdicode) & 
           !is.na(pib_percapita) & 
           !is.na(per_capita_electricity)) %>% 
  mutate(colombia = ifelse(iso_code == "COL", 2, 1)) %>%
  select(hdicode, per_capita_electricity, pib_percapita, iso_code, population, colombia)

baseGraph$hdicode <- factor(baseGraph$hdicode, levels = c("Low", "Medium",
                                                          "High", "Very High"),
                            labels = c("Bajo", "Medio", "Alto", "Muy Alto"))

# baseGraph <- baseGraph %>% 
#   mutate(alpa = ifelse(hdicode == "High" | hdicode == "Very High", 1, 0))

# point_Col <- base %>% 
#   filter(iso_code == "COL" & year == 2019) %>% 
#   select(per_capita_electricity, hdicode, pib_percapita, iso_code) %>% 
#   group_by(hdicode) %>% 
#   summarise(mean = mean(per_capita_electricity),
#             iso_code = iso_code,
#             pib_percapita = pib_percapita,
#             per_capita_electricity = per_capita_electricity)

Reg1 <- lm(formula = log(per_capita_electricity) ~ log(pib_percapita), 
           data = baseGraph, 
           weights = sqrt(population))

summ_reg1 <- summary(Reg1)

base %>% 
  filter(iso_code == "COL" & year == 2019) %>% 
  select(pib_percapita)

base %>% 
  filter(iso_code == "COL" & year == 2019) %>% 
  select(per_capita_electricity)

log(1557.164)

summ_reg1$coefficients[1,1] + summ_reg1$coefficients[2,1]*log(6404.109)

1557.164 - exp(summ_reg1$coefficients[1,1] + summ_reg1$coefficients[2,1]*log(6404.109))

abs((1557.164 - exp(summ_reg1$coefficients[1,1] + summ_reg1$coefficients[2,1]*log(6404.109)))/exp(summ_reg1$coefficients[1,1] + summ_reg1$coefficients[2,1]*log(6404.109)))*100

Reg2 <- lm(formula = log(per_capita_electricity) ~ log(pib_percapita), 
           data = baseGraph %>% filter(hdicode != "Bajo"), 
           weights = sqrt(population))

summ_reg2 <- summary(Reg2)

summ_reg2$coefficients[1,1] + summ_reg2$coefficients[2,1]*log(6404.109)

abs((1557.164 - exp(summ_reg2$coefficients[1,1] + summ_reg2$coefficients[2,1]*log(6404.109)))/exp(summ_reg2$coefficients[1,1] + summ_reg2$coefficients[2,1]*log(6404.109)))

1557.164 - exp(summ_reg2$coefficients[1,1] + summ_reg2$coefficients[2,1]*log(6404.109))

point_Col <- base %>%
  filter(iso_code == "COL" & year == 2019) %>%
  select(per_capita_electricity, hdicode, pib_percapita, iso_code) %>%
  group_by(hdicode) %>%
  summarise(mean = mean(per_capita_electricity),
            iso_code = iso_code,
            pib_percapita = pib_percapita,
            per_capita_electricity = per_capita_electricity)

A <- baseGraph %>% 
  mutate(alfa = ifelse(iso_code == "COL", 1, 0)) %>% 
  ggplot(mapping = aes(x = log(pib_percapita), y = log(per_capita_electricity))) +
  geom_point(mapping = aes(color = hdicode, shape = hdicode, alpha = as.factor(alfa), size = as.factor(alfa)), show.legend = F) +
  scale_size_manual(values = c(1.5, 3)) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_color_manual(values = c("#900C3F",
                                "#365486",
                                "#E3651D",
                                "#597E52")) +
  scale_shape_manual(values = c(15,16,17,3)) +
  # scale_alpha_manual(values = alpha_vals) +
  geom_label_repel(data = point_Col, mapping = aes(x = log(pib_percapita), y = log(per_capita_electricity), label = iso_code), size = 2, nudge_y = 3) +
  geom_smooth(method = "lm", se = F, linewidth = 1, color = "lightgray", alpha = 0.5) +
  # geom_line(aes(x = log(pib_percapita), y = ajuste)) +
  # geom_line(aes(x = log(pib_percapita), y = li), linetype = "longdash") +
  # geom_line(aes(x = log(pib_percapita), y = ls), linetype = "longdash") +
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "A: Consumo de electricidad según PIB per cápita - 2019",
       subtitle = "IDH: <span style='color:#900C3F;'>Bajo</span>, <span style='color:#365486;'>Medio, </span><span style='color:#E3651D;'>Alto</span> y <span style='color:#597E52;'>Muy Alto</span>",
       y = "Logaritmo de Consumo de electricidad per cápita",
       x = "Logaritmo de PIB per cápita",
       caption = "Fuente: elaboración propia en base a Our World in Data y Banco Mundial") +
  annotate(geom = "text",
           x = c(6.5, 6.5),
           y = c(10, 9.2),
           label = c(TeX("$R^2 = 0.77$"),
                     TeX("$\\hat{\\beta} = 1.038$")),
           size = c(3,3)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        plot.subtitle = element_markdown(hjust = 0.5, size = 9),
        plot.caption = element_text(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 6, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

point_Col <- point_Col %>% 
  mutate(hdicode = factor(hdicode, 
                          labels = c("Muy Alto",
                                     "Alto",
                                     "Medio",
                                     "Bajo"),
                          levels = c("Very High",
                                     "High",
                                     "Medium",
                                     "Low"))) 


B <- base %>% 
  mutate(hdicode = factor(hdicode, 
                          labels = c("Muy Alto",
                                     "Alto",
                                     "Medio",
                                     "Bajo"),
                          levels = c("Very High",
                                     "High",
                                     "Medium",
                                     "Low"))) %>% 
  filter(year == 2019 & hdicode != "" & !is.na(hdicode) & !is.na(per_capita_electricity)) %>% 
  group_by(hdicode) %>% 
  summarise(n = n(),
            mean = mean(per_capita_electricity),
            sd = sd(per_capita_electricity),
            quantile = qt(0.975, df = n - 1),
            se = sd/sqrt(n),
            li = mean - se*quantile,
            ls = mean + se*quantile) %>% 
  ggplot(mapping = aes(x = hdicode, y = mean)) +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = li, ymax = ls)) +
  geom_point(data = point_Col, mapping = aes(x = hdicode, y = mean), shape = 17, size = 3, color = "#E3651D") +
  geom_text(mapping = aes(x = 1.6, y = point_Col$mean[1], label = "COL"), size = 3) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "B: Intervalos de confianza al 0.95 del valor medio \n de consumo de electricidad por habitante según IDH",
       subtitle = NULL,
       y = "Kilovatios-Hora por habitante",
       x = NULL)+
  # caption = "Fuente: elaboración propia en base a Our World in Data") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=6, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

grafico <- wrap_plots(A, B, ncol = 2, widths = c(1, 0.5))

ggsave(plot = grafico, filename = "rmd/resultados/graficos/grafico1.12_pib_idh_consumo_elec.png", 
       units = 'in', width = 9.5, height = 5)

