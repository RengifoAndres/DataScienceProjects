

 year_data %>%
  filter(year>=2016) %>%
ggplot(aes(x=as.factor(year), y=total_casos )) +
   geom_col() 



### inputs for the graph 
month_names <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                 "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

month_data %>%
  filter(year>=2022) %>%
  arrange(month) %>%
  mutate(month_name = factor(month, levels = 1:12, labels = month_names)) %>%
  ggplot(aes(x=month_name, y=total_casos, color= as.factor(year) )) +
  geom_point() +
  geom_line(aes(x=month), linewidth= 0.8) +
  #scale_x_continuous(breaks = seq(1, 12, 2)) +
  #scale_color_manual(values = c("#E66100", "#1B6CA8", "#2A924A", "#A100A1")) +
  scale_color_manual(values = c("#E66100", "#1B6CA8", "#2A924A")) +
  labs(
    title= "Figura 1: Evolución de los casos de Terrorismo", 
    subtitle = "", 
    color= "Año",
    caption=  "Fuente: Sistema de Información Estadístico Delincuencial y Contravencional SIEDCO - PONAL",
    x = "Mes",
    y = "Casos", 
  ) +
  theme_bw()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    ## axis size
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.0),  # hjust = 0.5 lo centra
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 7),
    legend.text = element_text(size = 12), 
    legend.key.size = unit(1, "lines")) 

  ggplot(aes(x= nmonth, y=total_casos))