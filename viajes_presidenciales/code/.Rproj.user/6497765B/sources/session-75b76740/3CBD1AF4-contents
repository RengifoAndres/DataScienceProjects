
#### Import packages
library(pacman)

p_load(rvest,
       dplyr, 
       lubridate,
       stringr, 
       tidyr,
       purrr)


rm(list= ls())

# URL de la página
url_petro <- "https://es.wikipedia.org/wiki/Anexo:Viajes_internacionales_del_presidente_Gustavo_Petro"
url_duque <- "https://es.wikipedia.org/wiki/Anexo:Visitas_oficiales_al_exterior_del_presidente_Iv%C3%A1n_Duque"

# Leer el HTML
pagina <- read_html(url_petro)

# Extraer todas las tablas
tablas <- pagina %>% html_elements("table")

# Revisar cuántas tablas hay
length(tablas)


##

# Convertir todas las tablas a data frames
lista_tablas <- tablas %>% lapply(html_table)

## pegar todas las tablas en un solo dataset y crear variable year
petro_trips<- tibble()
for (i in 3:6) {
  
  tabla <- lista_tablas[[i]]
  tabla<- tabla %>% 
    mutate(year=2019+i)
  petro_trips<- petro_trips %>%
    bind_rows(tabla)
}


#----
###  Crear las funciones para limpiar el texto
### que define las fechas.
### para limpiarlo,  estandarizarlo y convertirlas en formato date

########################
##### function to convert month text to number 
########################

month_number <- function(month) {
  #month <- "ss"
  meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
             "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  
  num <- match(tolower(month), meses)
  
  if (is.na(num)) {
    return(NA) 
  } else {
    return(sprintf("%02d", num))
  }
  
}

########################
##### function to get the initial date of visit
########################


months_regex <- "enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre"
first_date<- function(date_str, year) {
  
  #date_str<- "18 de septiembre de 2022 - 24 de septiembre de 2022"
  #year<- "2022"
  number <-  str_extract_all(date_str, "\\b\\d{1,2}\\b")
  month  <- str_extract_all(date_str, months_regex, simplify = FALSE)
  
  number <- number[[1]]
  month  <- month[[1]]
  
  date_in <- NA  # inicializar

    date_in<- paste0(number[1], " ",  month[1] )
    a1<- number[1]
    a2<-  month[1]
 
     ## Note: here we are calling the functio  month number
  final_date<- paste0(as.character(year), "-", month_number(a2), "-", sprintf("%02d", as.numeric(a1)))
  
  return(final_date)
}


########################
##### function to get the final date of visit
########################


end_date<- function(date_str, year) {
  
  
  #date_str<- "del 30 al 31 de septiembre"
  #year<- 2024
  number <-  str_extract_all(date_str, "\\b\\d{1,2}\\b")
  month  <- str_extract_all(date_str, months_regex, simplify = FALSE)
  
  number <- number[[1]]
  month  <- month[[1]]
  
  date_end <- NA  # inicializar
  
  ### only 1 day
  if (length(month)==1 & length(number)==1)  {
    
    date_end<- paste0(number[1], " ",  month[1] )
    a1<- number[1]
    a2<-  month[1]
  }
  
  ## two days same month 
  if ( length(month)==1 & length(number)==2) {
    
    date_end<- paste0(number[2], " ",  month[1] )
    a1<- number[2]
    a2<-  month[1]
    
  }
  ## two days  two months 
  if ( length(month)==2 & length(number)==2) {
    
    date_end<- paste0(number[2], " ",  month[2] )
    a1<- number[2]
    a2<-  month[2]
  }
  
  ## 1 days  two months 
  if ( length(month)==2 & length(number)==1) {
    
    date_end<- paste0(number[2], " ",  month[2] )
    a1<- number[2]
    a2<-  month[2]
  }
  ## Note: here we are calling the function  month number
  final_date<- paste0(as.character(year), "-", month_number(a2), "-", sprintf("%02d", as.numeric(a1)))
  return(final_date[[1]])
  
  }


petro_trips_long <- petro_trips %>%
  select(-`Ciudad(es)`, -`Fotografía`) %>%
  rename_all(tolower) %>%
  mutate(first_date_col = map2(fecha, year, first_date),
         end_date_col   = map2(fecha, year, end_date)) 

###


petro_trips_long <- petro_trips_long %>%
  unnest(first_date_col) %>%
  unnest(end_date_col)  %>%
  mutate(first_date_col = as.Date(first_date_col),
         end_date_col   = as.Date(end_date_col) )

## this produces 1 NA since september does not have 31 days. 
## lets assume that it lasted 2 days
#----- format variables

petro_trips_long <- petro_trips_long %>%
  rename(country=`país`, 
         date=fecha, 
         motive= `motivo principal - actividad`) %>%
  select(country, motive, year, motive, first_date_col, end_date_col) %>%
  mutate(end_date_col= if_else(is.na(end_date_col)==TRUE, as.Date("2024-10-01"), end_date_col )) %>%
  mutate(country= if_else(country=="Vaticano", "Ciudad del Vaticano", country))



petro_trips_long <- petro_trips_long %>%
  mutate(duration = as.numeric(end_date_col - first_date_col) + 1)

saveRDS(petro_trips_long, "../data/processed_data/petro_trips_clean.RDS")








  library(ggplot2)
### generate duration of trips 


ggplot(petro_trips_long, aes(x = first_date_col, y = reorder(country, first_date_col), size = duration)) +
  geom_point(alpha = 0.7, color = "tomato") +
  theme_minimal() +
  labs(title = "Duración y destino de visitas internacionales",
       x = "Fecha de inicio", y = "País", size = "Días")




for (i in 7:12) {
  
  tabla <- lista_tablas[[i]]
  tabla<- tabla %>% 
    mutate(year=2019+i)
  
}

