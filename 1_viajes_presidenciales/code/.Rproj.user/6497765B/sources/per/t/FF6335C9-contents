
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



last_days<- all_trips |>
  group_by(president) |>
  arrange(desc(relative_time)) |>
  slice_head(n = 1) 

terms_data<-tibble(
  relative_time= c(365, 365, 730, 730,1095, 1095) ,
  cum_days=  c(100, 110 , 130, 140, 150, 160), 
  president= c( "Gustavo Petro","Ivan Duque", "Gustavo Petro", "Ivan Duque", "Gustavo Petro", "Ivan Duque" ), 
  fecha= c( "7-AGO-2023","7-AGO-2019", "7-AGO-2024", "7-AGO-2020",  "7-AGO-2025", "7-AGO-2021" )
)

#### 

ggplot(all_trips, aes(x=relative_time, y=cum_days, colour=president )) +
  geom_line(linewidth= 1.2) +
  geom_point(aes(size = duration)) +
  geom_vline(xintercept=365, linetype= "dashed", color= "grey") +
  geom_vline(xintercept=730, linetype= "dashed", color= "grey") +
  geom_vline(xintercept=1095, linetype= "dashed", color= "grey") +
  scale_x_continuous(breaks = seq(0, 1500, 200)) +
  scale_color_manual(values = c("#E66100", "#1B6CA8"))+
  geom_text_repel(
    data=terms_data,
    mapping=aes( label = fecha ), 
    fontface = "bold", size = 5, nudge_y = 2, 
    show.legend = FALSE
  )+
  geom_label_repel(
    data=last_days,
    mapping=aes( label = cum_days ), 
    fontface = "bold", size = 5, nudge_x = 2, 
    show.legend = FALSE
  )+
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


ggsave( "../output/dias_fuera_acumm.png", 
        width = 14, height = 8, units = "in", dpi = 300)



