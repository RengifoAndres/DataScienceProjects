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
  filter(!is.na(anio_llegada), !is.na(educacion), anio_llegada >= 2015) |>
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
  filter(!is.na(anio_llegada), anio_llegada >= 2015) |>
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

################
#### Plot 3: Age distribution by year of arrival (violin + boxplot)
################

edad_anio <- epm |>
  filter(!is.na(anio_llegada), !is.na(Ide10), anio_llegada >= 2015,
         Ide10 >= 10, Ide10 <= 80)

p_edad <- ggplot(edad_anio, aes(x = factor(anio_llegada), y = Ide10, weight = FEX)) +
  geom_violin(fill = "#91BFDB", color = NA, alpha = 0.7) +
  geom_boxplot(width = 0.15, fill = "white", color = "#1B6CA8",
               outlier.shape = NA) +
  labs(
    title    = "Distribución de edad por año de llegada a Colombia",
    subtitle = "Migrantes venezolanos · Encuesta Pulso Migrante Ronda 1 · Ponderado con FEX",
    x        = "Año de última entrada a Colombia (mi3)",
    y        = "Edad (años cumplidos)",
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

p_edad
ggsave("../output/edad_por_anio_llegada.png", p_edad,
       width = 14, height = 8, units = "in", dpi = 300)

################
#### Plot 4: Gender composition by year of arrival
################

sexo_anio <- epm |>
  filter(!is.na(anio_llegada), !is.na(Ide9), anio_llegada >= 2015) |>
  mutate(sexo = if_else(Ide9 == 1, "Hombre", "Mujer")) |>
  group_by(anio_llegada, sexo) |>
  summarise(n_w = sum(FEX, na.rm = TRUE), .groups = "drop") |>
  group_by(anio_llegada) |>
  mutate(pct = n_w / sum(n_w)) |>
  ungroup()

p_sexo <- ggplot(sexo_anio, aes(x = factor(anio_llegada), y = pct, fill = sexo)) +
  geom_col(position = "stack", width = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey40", linewidth = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Hombre" = "#4575B4", "Mujer" = "#D73027")) +
  labs(
    title    = "Composición por sexo de migrantes venezolanos por año de llegada",
    subtitle = "Colombia 2021 · Encuesta Pulso Migrante Ronda 1 · Ponderado con FEX",
    x        = "Año de última entrada a Colombia (mi3)",
    y        = "Proporción",
    fill     = NULL,
    caption  = "Fuente: DANE — Encuesta Pulso Migrante 2021"
  ) +
  theme_bw() +
  theme(
    plot.title         = element_text(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 13, color = "grey40"),
    axis.text          = element_text(size = 12),
    axis.title         = element_text(size = 13),
    legend.text        = element_text(size = 12),
    plot.caption       = element_text(size = 9, color = "grey50"),
    panel.grid.major.x = element_blank()
  )

p_sexo
ggsave("../output/sexo_por_anio_llegada.png", p_sexo,
       width = 14, height = 8, units = "in", dpi = 300)

################
#### Plot 5: Migration reasons (% of total migrants citing each reason)
################

# mi4__1=Económica, mi4__2=COVID-19, mi4__3=Salud, mi4__4=Familia,
# mi4__5=Inseguridad/violencia, mi4__6=Política, mi4__7=Otra
razones_labels <- c(
  "mi4__1" = "Económica",
  "mi4__2" = "COVID-19",
  "mi4__3" = "Salud",
  "mi4__4" = "Reunificación familiar",
  "mi4__5" = "Inseguridad / violencia",
  "mi4__6" = "Razones políticas",
  "mi4__7" = "Otra"
)

mi4_cols <- names(epm)[str_detect(names(epm), "^mi4__")]

razones <- epm |>
  summarise(across(all_of(mi4_cols), ~ sum(. * FEX, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_w") |>
  mutate(
    razon = recode(variable, !!!razones_labels),
    razon = factor(razon, levels = razones_labels[order(razones_labels)]),
    pct   = n_w / sum(epm$FEX, na.rm = TRUE)
  ) |>
  arrange(desc(pct))

p_razones <- ggplot(razones, aes(x = pct, y = reorder(razon, pct))) +
  geom_col(fill = "#2E8B57", alpha = 0.85, width = 0.65) +
  geom_text(aes(label = percent(pct, accuracy = 1)),
            hjust = -0.15, size = 4.5, fontface = "bold", color = "#2E8B57") +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1.05)) +
  labs(
    title    = "Razones de migración de venezolanos en Colombia",
    subtitle = "Encuesta Pulso Migrante Ronda 1 · Ponderado con FEX · Respuesta múltiple",
    x        = "% de migrantes que citaron esta razón",
    y        = NULL,
    caption  = "Fuente: DANE — Encuesta Pulso Migrante 2021"
  ) +
  theme_bw() +
  theme(
    plot.title         = element_text(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 13, color = "grey40"),
    axis.text          = element_text(size = 12),
    axis.title.x       = element_text(size = 13),
    plot.caption       = element_text(size = 9, color = "grey50"),
    panel.grid.major.y = element_blank()
  )

p_razones
ggsave("../output/razones_migracion.png", p_razones,
       width = 14, height = 8, units = "in", dpi = 300)

################
#### Plot 6: Employment status by year of arrival
################

# ml5: 1=Empleado, 2=Desempleado, 3=Estudiante,
#       4=Oficios del hogar, 5=Jubilado, 6=Otra
actividad_labels <- c(
  "1" = "Empleado/a",
  "2" = "Desempleado/a",
  "3" = "Estudiante",
  "4" = "Oficios del hogar",
  "5" = "Jubilado/a",
  "6" = "Otra"
)

actividad_colors <- c(
  "Empleado/a"       = "#2E8B57",
  "Desempleado/a"    = "#D73027",
  "Estudiante"       = "#4575B4",
  "Oficios del hogar"= "#E66100",
  "Jubilado/a"       = "#9970AB",
  "Otra"             = "#AAAAAA"
)

empleo_anio <- epm |>
  filter(!is.na(anio_llegada), !is.na(ml5), anio_llegada >= 2015) |>
  mutate(actividad = factor(
    recode(as.character(ml5), !!!actividad_labels),
    levels = actividad_labels
  )) |>
  group_by(anio_llegada, actividad) |>
  summarise(n_w = sum(FEX, na.rm = TRUE), .groups = "drop") |>
  group_by(anio_llegada) |>
  mutate(pct = n_w / sum(n_w)) |>
  ungroup()

p_empleo <- ggplot(empleo_anio, aes(x = factor(anio_llegada), y = pct, fill = actividad)) +
  geom_col(position = "stack", width = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = actividad_colors) +
  labs(
    title    = "Situación laboral de migrantes venezolanos por año de llegada",
    subtitle = "Colombia 2021 · Encuesta Pulso Migrante Ronda 1 · Ponderado con FEX",
    x        = "Año de última entrada a Colombia (mi3)",
    y        = "Proporción",
    fill     = "Actividad principal",
    caption  = "Fuente: DANE — Encuesta Pulso Migrante 2021"
  ) +
  theme_bw() +
  theme(
    plot.title         = element_text(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 13, color = "grey40"),
    axis.text          = element_text(size = 12),
    axis.title         = element_text(size = 13),
    legend.text        = element_text(size = 11),
    legend.title       = element_text(size = 12, face = "bold"),
    plot.caption       = element_text(size = 9, color = "grey50"),
    panel.grid.major.x = element_blank()
  )

p_empleo
ggsave("../output/empleo_por_anio_llegada.png", p_empleo,
       width = 14, height = 8, units = "in", dpi = 300)
