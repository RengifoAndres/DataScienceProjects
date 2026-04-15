# CLAUDE.md — Coding Conventions

This repository contains data science and data visualization projects, primarily in R.

---

## Project Structure

Each project is a numbered folder: `N_project_name/` (e.g., `1_viajes_presidenciales`).

Within each project:
```
N_project_name/
├── code/          # All scripts
├── data/
│   ├── raw/       # Original, unmodified data
│   └── processed_data/  # Cleaned/transformed outputs
└── output/        # Final charts, reports, exports
```

Scripts use a **2-digit numeric prefix** to indicate execution order and stage:
- `00_` – Scraping / raw data ingestion
- `01_` – Merging / integration
- `02_+` – Additional processing
- `11_+` – Visualization / output generation

File names are `snake_case`.

---

## R Conventions

### Package Management
Always use `pacman` with `p_load()`:
```r
library(pacman)
p_load(dplyr, tidyr, readr, ggplot2)
```

### Core Libraries
- **Data manipulation:** `dplyr`, `tidyr`, `purrr`
- **Strings:** `stringr`
- **Dates:** `lubridate`
- **I/O:** `readr`
- **Scraping:** `rvest`
- **Visualization:** `ggplot2`, `ggrepel`

### Style
- **Indentation:** 2 spaces
- **Naming:** `snake_case` for variables, columns, and functions
- **Pipe operator:** Prefer `|>` (native pipe); `%>%` is also acceptable
- **Column names:** Lowercase after cleaning (`rename_all(tolower)`)
- **Saved data format:** `.RDS` for processed R objects

### Paths
Use relative paths from the script's location:
```r
saveRDS(data, "../data/processed_data/output.RDS")
ggsave("../output/chart.png", ...)
```

### Script Structure
Each script starts by clearing the environment:
```r
rm(list = ls())
```

Sections are separated with comment headers:
```r
################
#### Section title
################
```

Functions get their own header block:
```r
########################
##### function description
########################
my_function <- function(x) { ... }
```

### Visualization Style
- **Theme:** `theme_bw()` as the base
- **Resolution:** `ggsave(..., dpi = 300)`
- **Size:** Wide format, e.g., `width = 14, height = 8, units = "in"`
- **Font sizes:** Large and readable (axis text 12pt+, titles 16–18pt)
- **Labels:** Use `ggrepel` (`geom_text_repel`, `geom_label_repel`) to avoid overlap
- **Colors:** Explicit hex codes for consistency across a project

---

## General Practices

- Projects analyze Colombian public policy / socioeconomic data
- Data sources include Wikipedia scraping, government open data portals
- Comments and variable names are in English; data content may be in Spanish
- Hardcode reference dates and constants directly in the merge/processing scripts with a comment explaining them
- NA handling is done explicitly with `if_else(is.na(...), fallback_value, col)`
