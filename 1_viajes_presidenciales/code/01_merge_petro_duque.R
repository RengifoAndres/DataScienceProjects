##########
### This code merge the data from 
###  each term
#########

#### Import packages
library(pacman)

p_load(dplyr, 
       tidyr,
       readr)


rm(list= ls())

petro_trips<- read_rds("../data/processed_data/petro_trips_clean.RDS")
duque_trips<- read_rds("../data/processed_data/duque_trips_clean.RDS")


petro_trips<- petro_trips |>
  arrange( first_date_col)|>
  mutate(relative_time= first_date_col- as.Date("2022-08-07"), 
         cum_days= cumsum(duration), 
         president= "Gustavo Petro")
  
duque_trips<- duque_trips |>
  arrange( first_date_col)|>
  mutate(relative_time= first_date_col- as.Date("2018-08-07"), 
         cum_days= cumsum(duration),
         president= "Ivan Duque")



all_trips<- duque_trips |>
  bind_rows(petro_trips)


saveRDS(all_trips, "../data/processed_data/all_trips_clean.RDS")


