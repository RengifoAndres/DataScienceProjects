rm(list = ls())

library(pacman)
p_load(tidyverse, scales)

epm <- readRDS("../data/processed_data/epm_ronda1_clean.RDS")

# Palette: ordered from no schooling (red) to postgrad (dark blue)
edu_colors <- c(
  "Ninguno"             = "#D73027",
  "Preescolar"          = "#FC8D59",
  "Básica primaria"     = "#FEE090",
  "Bachillerato"        = "#E0F3F8",
  "Técnico/Tecnológico" = "#91BFDB",
  "Universitario"       = "#4575B4",
  "Posgrado"            = "#1A237E"
)

################
#### Plot 1: Education level by year of arrival (weighted proportions)
################

edu_anio <- epm |>
  filter(!is.na(anio_llegada), !is.na(educacion), anio_llegada >= 2013) |>
  group_by(anio_llegada, educacion) |>
  summarise(n_w = sum(FEX, na.rm = TRUE), .groups = "drop") |>
  group_by(anio_llegada) |>
  mutate(pct = n_w / sum(n_w)) |>
  ungroup()

p_edu <- ggplot(edu_anio, aes(x = factor(anio_llegada), y = pct, fill = educacion)) +
  geom_col(position = "stack", width = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = edu_colors) +
  labs(
    title    = "Nivel educativo de migrantes venezolanos por año de llegada",
    subtitle = "Colombia 2021 · Encuesta Pulso Migrante Ronda 1 · Ponderado con FEX",
    x        = "Año de última entrada a Colombia (mi3)",
    y        = "Proporción",
    fill     = "Nivel educativo",
    caption  = "Fuente: DANE — Encuesta Pulso Migrante 2021"
  ) +
  theme_bw() +
  theme(
    plot.title       = element_text(size = 18, face = "bold"),
    plot.subtitle    = element_text(size = 13, color = "grey40"),
    axis.text        = element_text(size = 12),
    axis.title       = element_text(size = 13),
    legend.text      = element_text(size = 11),
    legend.title     = element_text(size = 12, face = "bold"),
    plot.caption     = element_text(size = 9, color = "grey50"),
    panel.grid.major.x = element_blank()
  )

p_edu
ggsave("../output/educacion_por_anio_llegada.png", p_edu,
       width = 14, height = 8, units = "in", dpi = 300)

################
#### Plot 2: PEP rate by year of arrival (weighted)
################

pep_anio <- epm |>
  filter(!is.na(anio_llegada), anio_llegada >= 2013) |>
  group_by(anio_llegada) |>
  summarise(
    total_w = sum(FEX, na.rm = TRUE),
    pep_w   = sum(FEX * tiene_pep, na.rm = TRUE),
    pct_pep = pep_w / total_w,
    n       = n(),
    .groups = "drop"
  )

p_pep <- ggplot(pep_anio, aes(x = factor(anio_llegada), y = pct_pep)) +
  geom_col(fill = "#1B6CA8", alpha = 0.85, width = 0.7) +
  geom_text(
    aes(label = percent(pct_pep, accuracy = 1)),
    vjust = -0.5, size = 4.5, fontface = "bold", color = "#1B6CA8"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.1)) +
  labs(
    title    = "Vinculación al PEP por año de llegada a Colombia",
    subtitle = "Migrantes venezolanos · Encuesta Pulso Migrante Ronda 1 · Ponderado con FEX",
    x        = "Año de última entrada a Colombia (mi3)",
    y        = "% con PEP",
    caption  = "Fuente: DANE — Encuesta Pulso Migrante 2021"
  ) +
  theme_bw() +
  theme(
    plot.title         = element_text(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 13, color = "grey40"),
    axis.text          = element_text(size = 12),
    axis.title         = element_text(size = 13),
    plot.caption       = element_text(size = 9, color = "grey50"),
    panel.grid.major.x = element_blank()
  )

p_pep
ggsave("../output/pep_por_anio_llegada.png", p_pep,
       width = 14, height = 8, units = "in", dpi = 300)
