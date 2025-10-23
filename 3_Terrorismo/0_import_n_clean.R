########
#  
#
#  
#######


library(pacman)
p_load(tidyverse,
       readxl,
       janitor, 
       rio)

rm(list= ls())

data_list <- list()


for(t in c(2010:2019, 2023:2024)) {
  sheet= t-2009
  data_path<- paste0("data/", t, ".xlsx")
  print(data_path) 
  
  temp<- import(data_path, skip = 9) %>%
      clean_names() %>% 
      distinct()
 
   ### variable names
  if ("fecha_hecho" %in% names(temp)) {
    temp <- temp %>% rename(fecha= fecha_hecho)
  } 
  
  if ("armas_medios" %in% names(temp)) {
    temp <- temp %>% rename(arma_medio= armas_medios)
  } 
    
    data_list[[sheet]] <- temp
}


for(t in seq(2020, 2022)) {
  sheet= t-2009
  data_path<- paste0("data/", t, ".xls")
  print(data_path) 
  
  if (t== 2020) {
    temp <- import("data/2020_0.xls", skip = 9) %>%
      clean_names()
    
    temp2 <- import("data/2020_1.xls", skip = 9) %>%
      clean_names()
    
    temp <- temp %>%
      bind_rows(temp2) %>% 
      distinct()
  }
  
  else  {
  
    temp <- import(data_path, skip = 9) %>%
      clean_names() %>% 
      distinct()
  }
  
  
  
  
  ### variable names
  if ("fecha_hecho" %in% names(temp)) {
    temp <- temp %>% rename(fecha= fecha_hecho)
  } 
  
  if ("armas_medios" %in% names(temp)) {
    temp <- temp %>% rename(arma_medio= armas_medios)
  } 

  data_list[[sheet]] <- temp
}




temp <- import("data/2025.xlsx", skip = 10) %>%
  clean_names() %>% 
  distinct()


combined_data <- bind_rows(data_list)
combined_data <- combined_data %>% 
mutate(year = year(fecha) , 
       month= month(fecha), 
       nmonth=format( fecha, "%Y-%m") )



##### Collapse year data
year_data<- combined_data %>% 
  group_by(year) %>%
  summarise(total_casos=sum(cantidad)) %>%
  ungroup()



##### Collapse nmonth data
month_data<- combined_data %>% 
  group_by(year, month, nmonth) %>%
  summarise(total_casos=sum(cantidad)) %>%
  ungroup()



  