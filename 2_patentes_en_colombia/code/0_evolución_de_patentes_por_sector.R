

library(pacman)
#p_load(gander)

p_load(tidyverse, 
       rio, 
       janitor)

rm(list= ls())

raw_path<- "../data/raw"

df<- import(paste0(raw_path, "/Patentes_concedidas_20250711.csv" )) |>
  clean_names()


table(df$pais)
table(df$tramite)
table(df$via_presntac)
table(df$naturaleza)



df_nacional<- df |> 
  filter(via_presntac=="Nacional")



df_sector_anio<- df_nacional |> 
  mutate(number_patents= if_else(naturaleza=="Patente de Invención Nacional", 
                                 1, 0)) |> 
  group_by(sector,ano_concesion) |> 
  summarise(number_patents= sum(number_patents))|> 
  ungroup() |> 
  filter(sector!="")|> 
  mutate(
    ano_concesion = as.numeric(gsub(",", "", ano_concesion))
  )
  


ggplot(df_sector_anio, aes(x= ano_concesion, y=number_patents, colour=  sector )) + 
  geom_point() + 
  geom_line(linewidth= 1.2) +
  theme_bw()+
  theme(
    ## axis size
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 18), 
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 18, hjust = 0.5),  # hjust = 0.5 lo centra
    plot.subtitle = element_text(size = 14),
    legend.text = element_text(size = 8), 
    legend.key.size = unit(2, "lines"))+
  labs(
    title= "Patentes Por Sector", 
    caption= "Elaborado por @RengifoJAndres", 
    subtitle= "", 
    x = "Año",
    y = "N. Patentes", 
    color= "Sector"
  ) 

  
