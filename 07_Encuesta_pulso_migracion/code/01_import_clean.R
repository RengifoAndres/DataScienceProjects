rm(list = ls())

library(pacman)
p_load(tidyverse, lubridate)

################
#### Load raw data
################

epm_raw <- read_csv("../data/epm_ronda1_fx18.csv", show_col_types = FALSE)

################
#### Parse year of arrival from mi3
#### mi3: date of most recent entry to Colombia to stay
#### Raw format: DDmmmYYYY (e.g. "26jul2020") — Stata date string
################

epm <- epm_raw |>
  mutate(
    fecha_llegada = dmy(mi3),
    anio_llegada  = year(fecha_llegada)
  )

################
#### Recode education level (ml1)
#### ml1: highest educational diploma received
#### 1=Ninguno, 2=Preescolar, 3=Básica primaria,
#### 4=Bachillerato, 5=Técnico/Tecnológico,
#### 6=Universitario, 7=Posgrado
################

niveles_edu <- c(
  "1" = "Ninguno",
  "2" = "Preescolar",
  "3" = "Básica primaria",
  "4" = "Bachillerato",
  "5" = "Técnico/Tecnológico",
  "6" = "Universitario",
  "7" = "Posgrado"
)

epm <- epm |>
  mutate(
    educacion = factor(
      recode(as.character(ml1), !!!niveles_edu),
      levels = niveles_edu
    )
  )

################
#### PEP indicator
#### mi17__1 to mi17__8: binary reasons for having obtained PEP
#### Any row sum > 0 means the person has PEP
################

mi17_cols <- names(epm)[str_detect(names(epm), "^mi17__")]

epm <- epm |>
  mutate(
    tiene_pep = if_else(
      rowSums(pick(all_of(mi17_cols)), na.rm = TRUE) > 0, 1L, 0L
    )
  )

################
#### Deduplicate: one row per person
#### Unique ID: directorio + secuencia_p + orden
################

epm_clean <- epm |>
  distinct(directorio, secuencia_p, orden, .keep_all = TRUE)

################
#### Save
################

saveRDS(epm_clean, "../data/processed_data/epm_ronda1_clean.RDS")

glimpse(epm_clean)
