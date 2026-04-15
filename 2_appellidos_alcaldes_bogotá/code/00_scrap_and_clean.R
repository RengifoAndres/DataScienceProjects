

rm(list = ls())

library(pacman)
p_load(rvest, httr, tidyverse)

################
#### Scrape table from Alcaldía de Bogotá
#### Note: data is in table[3]; table[1] is a JS loading spinner
################

url <- "https://www.alcaldiabogota.gov.co/sisjur/normas/Norma1.jsp?i=4925"

pagina <- read_html(url)
 
 tabla_raw <- pagina |>
     html_elements("table")  |>
      html_table(fill = TRUE)
 
 alcaldes_raw <- as_tibble( tabla_raw[[2]]) |>
  filter(row_number()>3) |>
  select(1,2,3)

colnames(alcaldes_raw)  <- c("anio", "funcion", "nombre") 
  
# Preview raw data
head(df, 20)

################
#### Clean column names and filter header rows
################

alcaldes_raw <- alcaldes_raw |>
  mutate(
    anio    = na_if(anio, ""),
    funcion = na_if(funcion, "")
  ) |>
  filter(!anio %in% c("Año", NA)) |>
  filter(!str_detect(anio, "^(Nota|JEFATURA|ALCALDIA|DISTRITO|Ley|Ordenanza|ALCALDÍA)")) |>
  fill(anio, funcion, .direction = "down")

################
#### Remove empty or header-like name rows
################

alcaldes_clean <- alcaldes_raw |>
  filter(!is.na(nombre), nombre != "", nombre != "Nombre") |>
  mutate(anio = as.integer(str_extract(anio, "\\d{4}"))) |>
  filter(!is.na(anio))

alcaldes_clean <- alcaldes_clean |>
  mutate(nombre = str_squish(nombre))

################
#### Remove honorifics and parenthetical date notes
################

alcaldes_clean <- alcaldes_clean |>
  mutate(
    nombre_clean = nombre |>
      str_remove_all("^(Don|Doña|Capitán|Coronel|Maestre de Campo)\\s+") |>
      str_remove_all("\\(.*?\\)") |>   # remove dates/notes in parentheses
      str_remove_all("\\+\\d{4}") |>   # remove death year marks like +1692
      str_squish()
  )

################
#### Extract apellido(s)
#### Colombian convention: first apellido = paternal (second word in full name)
#### For colonial names with "de/del/de la/de los": keep compound apellido
################

# Common Spanish/Colombian given names (masculine + feminine).
# Used to resolve the 3-token ambiguity: "Juan Antonio García" vs "Juan García López"
# Sources: Registraduría Nacional, INE Spain, colonial Spanish naming conventions
nombres_conocidos <- c(
  # Masculine
  "Abad", "Adolfo", "Agustín", "Alberto", "Alejandro", "Alfonso", "Alfredo",
  "Álvaro", "Andrés", "Antonio", "Arturo", "Baltasar", "Bernardo", "Carlos",
  "Cayetano", "Cristóbal", "Daniel", "David", "Diego", "Eduardo", "Emilio",
  "Enrique", "Ernesto", "Esteban", "Federico", "Felipe", "Fernando", "Francisco",
  "Gabriel", "Germán", "Gonzalo", "Guillermo", "Gustavo", "Hernando", "Ignacio",
  "Iván", "Jaime", "Javier", "Jesús", "Jorge", "José", "Juan", "Julio",
  "Leonardo", "Lorenzo", "Lucas", "Luis", "Manuel", "Marco", "Marcos", "Mario",
  "Martín", "Mauricio", "Miguel", "Nicolás", "Octavio", "Orlando", "Pablo",
  "Pedro", "Rafael", "Ramón", "Ricardo", "Roberto", "Rodrigo", "Salvador",
  "Samuel", "Santiago", "Sebastián", "Sergio", "Simón", "Tomás", "Vicente",
  "Victor", "Víctor", "Xavier",
  # Feminine
  "Ana", "Angela", "Ángela", "Beatriz", "Camila", "Carmen", "Carolina",
  "Catalina", "Cecilia", "Clara", "Claudia", "Diana", "Elena", "Emilia",
  "Gloria", "Isabel", "Josefa", "Juana", "Laura", "Lucía", "Luz", "María",
  "Mariana", "Marta", "Mercedes", "Natalia", "Patricia", "Paula", "Rosa",
  "Sandra", "Sara", "Sofía", "Teresa", "Valentina", "Valeria", "Verónica"
) |> str_to_lower()

########################
##### extract_apellidos: returns list(primer, segundo) from a clean full name.
##### Rules for locating where apellidos start (position of word[2]):
#####   - word[2] is a particle            → apellidos start at 2
#####   - word[2] is a known given name    → second given name, skip to 3
#####   - word[2] unknown + n >= 4         → assume second given name, skip to 3
#####   - word[2] unknown + n <= 3         → it is the first apellido, start at 2
########################
extract_apellidos <- function(nombre) {
  nombre <- str_remove(nombre, "^N\\.?\\s*")
  words  <- str_split(nombre, "\\s+")[[1]]
  n      <- length(words)

  na_result <- list(primer = NA_character_, segundo = NA_character_)
  if (n == 0 || nombre == "") return(na_result)
  if (n == 1) return(list(primer = words[1], segundo = NA_character_))

  particles    <- c("de", "del", "la", "los", "las")
  connectors   <- c("y", "e")   # separate: link apellidos but are not part of one

  start <- if (str_to_lower(words[2]) %in% particles)               2L
           else if (str_to_lower(words[2]) %in% nombres_conocidos)  3L
           else if (n >= 4)                                          3L
           else                                                      2L

  # If the heuristic lands start on a connector (y/e), the skipped word was
  # actually the primer apellido (e.g. "Juan Salinas y Berrio" → start=3 hits "y")
  if (start <= n && str_to_lower(words[start]) %in% connectors) start <- start - 1L

  # Walk a single apellido token (particles + the following word)
  walk_apellido <- function(pos) {
    result <- character(0)
    i <- pos
    while (i <= n) {
      result <- c(result, words[i])
      if (str_to_lower(words[i]) %in% particles && i < n) {
        i <- i + 1
      } else {
        i <- i + 1
        break
      }
    }
    list(token = paste(result, collapse = " "), next_pos = i)
  }

  ap1    <- walk_apellido(start)
  primer <- ap1$token
  i      <- ap1$next_pos

  # Skip "y"/"e" connector between apellidos
  if (i <= n && str_to_lower(words[i]) %in% connectors) i <- i + 1

  if (i > n) return(list(primer = primer, segundo = NA_character_))

  ap2    <- walk_apellido(i)
  segundo <- ap2$token

  list(primer = primer, segundo = segundo)
}

alcaldes_clean <- alcaldes_clean |>
  mutate(ap = map(nombre_clean, extract_apellidos),
         primer_apellido  = map_chr(ap, "primer"),
         segundo_apellido = map_chr(ap, "segundo")) |>
  select(-ap)

################
#### Normalize apellidos (title case, trim)
################

alcaldes_clean <- alcaldes_clean |>
  mutate(
    primer_apellido  = str_to_title(str_squish(primer_apellido)),
    segundo_apellido = str_to_title(str_squish(segundo_apellido))
  )

################
#### Add era categories
################

alcaldes_clean <- alcaldes_clean |>
  mutate(
    era = case_when(
      anio < 1700 ~ "Colonial temprana (1538-1699)",
      anio < 1810 ~ "Colonial tardía (1700-1809)",
      anio < 1900 ~ "Siglo XIX (1810-1899)",
      anio < 1960 ~ "Primera mitad S.XX (1900-1959)",
      anio < 2000 ~ "Segunda mitad S.XX (1960-1999)",
      TRUE        ~ "Siglo XXI (2000-hoy)"
    ),
    era = factor(era, levels = c(
      "Colonial temprana (1538-1699)",
      "Colonial tardía (1700-1809)",
      "Siglo XIX (1810-1899)",
      "Primera mitad S.XX (1900-1959)",
      "Segunda mitad S.XX (1960-1999)",
      "Siglo XXI (2000-hoy)"
    ))
  )

################
#### Save
################

saveRDS(alcaldes_clean, "../data/processed_data/alcaldes_clean.RDS")
#write_csv(alcaldes_clean, "../data/processed_data/alcaldes_clean.csv")

glimpse(alcaldes_clean)
