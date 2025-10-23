##########
### Graph Días Afuera suma acumulada. 
#########



#### Import packages
library(pacman)

p_load(dplyr, 
       tidyr,
       readr, 
       ggplot2, 
       ggrepel)


rm(list= ls())

all_trips <- read_rds("../data/processed_data/all_trips_clean.RDS")

country_president <- all_trips |>
  group_by(president,country) |>
  summarise(duration_total= sum(duration),
            .groups = "drop") |>
  arrange(desc(duration_total))
  
  
country_president <- country_president |>
  arrange(president,desc(duration_total)) |>
  group_by(president) |>
  slice(1:10)

 


########

ggplot(country_president |> filter(president=="Ivan Duque"), aes(x=country, y=duration_total )) +
  geom_col() +
  #scale_color_manual(values = c("#E66100", "#1B6CA8")) +
  labs(
    size= "Duración del Viaje (Dias)", 
    title= "Viajes Presidenciales", 
    caption= "Datos Extraidos de Wikipedia y procesados por @RengifoJAndres", 
    subtitle= "Días Acumulados Fuera del País en Función del Tiempo desde Posesión", 
    x = "Días desde Posesión",
    y = "Días Acumulados fuera del País", 
    color= "Presidente"
  ) +
  theme_bw()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    ## axis size
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 18), 
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 18, hjust = 0.0),  # hjust = 0.5 lo centra
    plot.subtitle = element_text(size = 14),
    legend.text = element_text(size = 12), 
    legend.key.size = unit(2, "lines"))


 