rm(list = ls())

library(pacman)
p_load(tidyverse, forcats, ggwordcloud)

################
#### Load data
################

alcaldes <- readRDS("../data/processed_data/alcaldes_clean.RDS")

################
#### Base frequency table
#### One row per unique alcalde; both primer and segundo apellido counted
################

freq_apellidos <- alcaldes |>
  distinct(nombre_clean, .keep_all = TRUE) |>
  select(nombre_clean, era, primer_apellido, segundo_apellido) |>
  pivot_longer(c(primer_apellido, segundo_apellido),
               names_to  = "tipo",
               values_to = "apellido") |>
  filter(!is.na(apellido), apellido != "")

################
#### Top 15 apellidos en toda la historia
################

top_apellidos <- freq_apellidos |>
  count(apellido, sort = TRUE) |>
  slice_head(n = 15) |>
  mutate(apellido = fct_reorder(apellido, n))

p_top <- ggplot(top_apellidos, aes(x = apellido, y = n)) +
  geom_col(fill = "#1B6CA8", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.2, size = 4.5, color = "grey20") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Los 15 apellidos más frecuentes entre los Alcaldes de Bogotá",
    subtitle = "Historia 1538 - 2022 · Cada alcalde cuenta una sola vez · Incluye primer y segundo apellido",
    x        = NULL,
    y        = "Número de alcaldes",
    caption  = "Fuente: Alcaldía Mayor de Bogotá - Secretaría General"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "grey40"),
    axis.text.y   = element_text(size = 12),
    axis.text.x   = element_text(size = 11),
    axis.title.x  = element_text(size = 12),
    plot.caption  = element_text(size = 9, color = "grey50"),
    panel.grid.major.y = element_blank()
  )

p_top

ggsave("../output/top_apellidos.png", p_top, width = 12, height = 7, units = "in", dpi = 300)

################
#### Top apellidos por era histórica
################

top_por_era <- freq_apellidos |>
  count(era, apellido, sort = TRUE) |>
  group_by(era) |>
  slice_head(n = 5) |>
  mutate(apellido = fct_reorder(apellido, n))

p_era <- ggplot(top_por_era, aes(x = apellido, y = n, fill = era)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  coord_flip() +
  facet_wrap(~era, scales = "free_y", ncol = 2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = c(
    "Colonial temprana (1538-1699)"   = "#8B4513",
    "Colonial tardía (1700-1809)"     = "#CD853F",
    "Siglo XIX (1810-1899)"           = "#4682B4",
    "Primera mitad S.XX (1900-1959)"  = "#2E8B57",
    "Segunda mitad S.XX (1960-1999)"  = "#E66100",
    "Siglo XXI (2000-hoy)"            = "#6A0DAD"
  )) +
  labs(
    title    = "Top 5 apellidos de Alcaldes por era histórica",
    subtitle = "Bogotá 1538 - 2022 · Cada alcalde cuenta una sola vez · Incluye primer y segundo apellido",
    x        = NULL,
    y        = "Número de alcaldes",
    caption  = "Fuente: Alcaldía Mayor de Bogotá - Secretaría General"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "grey40"),
    axis.text.y   = element_text(size = 10),
    axis.text.x   = element_text(size = 9),
    strip.text    = element_text(size = 10, face = "bold"),
    plot.caption  = element_text(size = 9, color = "grey50"),
    panel.grid.major.y = element_blank()
  )

p_era
ggsave("../output/apellidos_por_era.png", p_era, width = 14, height = 10, units = "in", dpi = 300)

################
#### Word cloud — historia completa
################

p_cloud <- freq_apellidos |>
  count(apellido, sort = TRUE) |>
  slice_head(n = 20) |>
  ggplot(aes(label = apellido, size = n, color = n)) +
  geom_text_wordcloud(seed = 42, max_steps = 1000) +
  scale_size_area(max_size = 22) +
  scale_color_gradient(low = "#AEC6E8", high = "#1B6CA8") +
  labs(
    title    = "Nube de apellidos — Alcaldes de Bogotá",
    subtitle = "1538 - 2022 · Tamaño proporcional a la frecuencia",
    caption  = "Fuente: Alcaldía Mayor de Bogotá - Secretaría General"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "grey40", hjust = 0.5),
    plot.caption  = element_text(size = 9,  color = "grey50", hjust = 0.5)
  )

p_cloud
ggsave("../output/wordcloud_apellidos.png", p_cloud, width = 14, height = 9, units = "in", dpi = 300)

################
#### Word cloud por era histórica
################

era_colors <- c(
  "Colonial temprana (1538-1699)"   = "#8B4513",
  "Colonial tardía (1700-1809)"     = "#CD853F",
  "Siglo XIX (1810-1899)"           = "#4682B4",
  "Primera mitad S.XX (1900-1959)"  = "#2E8B57",
  "Segunda mitad S.XX (1960-1999)"  = "#E66100",
  "Siglo XXI (2000-hoy)"            = "#6A0DAD"
)

p_cloud_era <- freq_apellidos |>
  count(era, apellido, sort = TRUE) |>
  group_by(era) |>
  slice_head(n = 20) |>
  ungroup() |>
  ggplot(aes(label = apellido, size = n, color = era)) +
  geom_text_wordcloud(seed = 42, max_steps = 1000) +
  scale_size_area(max_size = 12) +
  scale_color_manual(values = era_colors) +
  facet_wrap(~era, ncol = 2) +
  labs(
    title    = "Nube de apellidos por era histórica — Alcaldes de Bogotá",
    subtitle = "Tamaño proporcional a la frecuencia dentro de cada era",
    caption  = "Fuente: Alcaldía Mayor de Bogotá - Secretaría General"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "grey40", hjust = 0.5),
    plot.caption  = element_text(size = 9,  color = "grey50", hjust = 0.5),
    strip.text    = element_text(size = 10, face = "bold"),
    legend.position = "none"
  )

p_cloud_era
ggsave("../output/wordcloud_por_era.png", p_cloud_era, width = 14, height = 12, units = "in", dpi = 300)

################
#### Número de alcaldes únicos por año (densidad histórica)
################

alcaldes_por_anio <- alcaldes |>
  distinct(anio, nombre_clean) |>
  count(anio, name = "n_alcaldes")

p_densidad <- ggplot(alcaldes_por_anio, aes(x = anio, y = n_alcaldes)) +
  geom_col(fill = "#1B6CA8", width = 0.9, alpha = 0.8) +
  geom_vline(xintercept = c(1700, 1810, 1900, 1960, 2000),
             linetype = "dashed", color = "grey50", linewidth = 0.5) +
  annotate("text", x = c(1619, 1755, 1855, 1930, 1980, 2012),
           y = max(alcaldes_por_anio$n_alcaldes) * 0.9,
           label = c("Colonial\ntemprana", "Colonial\ntardía",
                     "Siglo XIX", "S.XX\ntemprano", "S.XX\ntardío", "S.XXI"),
           size = 3, color = "grey40", hjust = 0.5) +
  labs(
    title    = "Número de alcaldes por año en Bogotá",
    subtitle = "Años con más de un alcalde reflejan transiciones políticas o inestabilidad",
    x        = "Año",
    y        = "Número de alcaldes",
    caption  = "Fuente: Alcaldía Mayor de Bogotá - Secretaría General"
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.text     = element_text(size = 11),
    axis.title    = element_text(size = 12),
    plot.caption  = element_text(size = 9, color = "grey50"),
    panel.grid.major.x = element_blank()
  )

p_densidad

ggsave("../output/alcaldes_por_anio.png", p_densidad, width = 14, height = 6, units = "in", dpi = 300)
