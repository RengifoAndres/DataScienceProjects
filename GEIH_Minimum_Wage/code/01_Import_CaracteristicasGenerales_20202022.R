
library(pacman)
p_load(rvest, 
       httr, 
       stringr, 
       zip, 
       tidyverse, 
       stringr, 
       readr, 
       janitor)

### step 0 clear all

rm(list= ls())

#### define paths



data_path <- "../data"
raw <- paste0(data_path, "/raw" )
import <- paste0(data_path, "/import" )


#######

df <- tibble()
for (year in 2013:2025) {
  folder_path <- paste0(raw, "/", year )
  files <- list.files(path = folder_path)
  
  
  for (p in seq_along(files)) {
    
    archive_path <- paste0(folder_path, "/", files[p] )
    # do something with archive_path
    print(archive_path)
    compress_files <- tibble( zip::zip_list(archive_path))
    compress_files <- compress_files |> 
      mutate(archive= files[p], 
             year= year ) |> 
      select(year, archive, filename)
    df<- rbind(df, compress_files)
  }
}



#### extract general characteristics

####################
#####     2020 
####################
  year_ind<- 2022
  #  Remove folder if it exists
  unlink(file.path(import, year_ind), recursive = TRUE, force = TRUE)
  # Create folder if it doesn't exist
  dir.create(file.path(import, year_ind), showWarnings = FALSE)

  temp_df <- df |> 
    filter(year== year_ind) |>
    filter((str_detect(filename, "CSV") | str_detect(filename, "csv") ) &
             str_detect(filename, "generales")  )  
  
  temp_df2  <-  tibble()
  
  if (nrow(temp_df)!= 12) {
    print("==++++++++++++++++++++DESTROY EVERYTINHG ++++++++++++++++++++++++++++++++++++++++++++++")
    print(year_ind)
  }
  
  for (j in 1:nrow(temp_df)) {
    
    print(temp_df$archive[j])
    print(temp_df$filename[j])
    
    zip_file <- paste0(raw, "/", year_ind, "/", temp_df$archive[j] )
    
    ##
    temp <- read_delim(unz(zip_file, temp_df$filename[j]),  delim = ";") |>
      clean_names()
    
    temp_df2<- rbind(temp_df2, temp)
  }
  saveRDS(temp_df2, paste0(file.path(import, year_ind), "/",  "caracteristicas_generales", year_ind, ".csv" ) )



########################### 
####  2021
###########################

  year_ind<- 2021
  #  Remove folder if it exists
  unlink(file.path(import, year_ind), recursive = TRUE, force = TRUE)
  # Create folder if it doesn't exist
  dir.create(file.path(import, year_ind), showWarnings = FALSE)
  
  temp_df <- df |> 
    filter(year== year_ind) |>
    filter( (str_detect(filename, "CSV") | str_detect(filename, "csv") ) & 
              str_detect(filename, "generales") & 
              str_detect(filename, "Cabecera") ) 
  
  temp_df2  <-  tibble()
  
  if (nrow(temp_df)!= 12) {
    print("==++++++++++++++++++++DESTROY EVERYTINHG ++++++++++++++++++++++++++++++++++++++++++++++")
    print(year_ind)
  }
  
  for (j in 1:nrow(temp_df)) {
    
    print(temp_df$archive[j])
    print(temp_df$filename[j])
    
    zip_file <- paste0(raw, "/", year_ind, "/", temp_df$archive[j] )
    
    ##
    temp <- read_delim(unz(zip_file, temp_df$filename[j]),  delim = ";") |>
      clean_names()
    
    temp_df2<- rbind(temp_df2, temp)
  }
  saveRDS(temp_df2, paste0(file.path(import, year_ind), "/",  "caracteristicas_generales", year_ind, ".csv" ) )
  
  
  ########################### 
  ####  2022
  ###########################
  
  year_ind<- 2022
  #  Remove folder if it exists
  unlink(file.path(import, year_ind), recursive = TRUE, force = TRUE)
  # Create folder if it doesn't exist
  dir.create(file.path(import, year_ind), showWarnings = FALSE)
  
  
  ### this is for ZIP files with zip files inside
  layer0 <-  df |> 
    filter(year== year_ind) |>
    filter(str_detect(filename, "CSV") &  str_detect(filename, ".zip") ) 

  
  temp_df2  <-  tibble()
  for (n in 1:3){
  
    
    zip::unzip(paste0(raw, "/", year_ind, "/", layer0$archive[n]),
             files = layer0$filename[n], 
             exdir = paste0(import, "/", year_ind ))
    files <- zip::zip_list( paste0(import, "/", year_ind, "/", layer0$filename[n] )  )
   
    
     temp_df <- tibble(files)
     temp_df <- temp_df |> 
       filter( str_detect(filename, "generales")  )
     
     if (nrow(temp_df)!= 1) {
       print("==++++++++++++++++++++DESTROY EVERYTINHG ++++++++++++++++++++++++++++++++++++++++++++++")
       print(year_ind)
     }
     
     for (j in 1:nrow(temp_df)) {
       
       print(temp_df$filename[1])
       
       zip_file <- paste0(import, "/", year_ind, "/", layer0$filename[n])
       
       ##
       temp <- read_delim(unz(zip_file, temp_df$filename[1]),  delim = ";") |>
         clean_names()
       
       temp_df2<- rbind(temp_df2, temp)
     
    
  }
}
  #### I don need to run temp_df2  <-  tibble() again because I have already create it
  
  temp_df <- df |> 
    filter(year== year_ind) |>
    filter((str_detect(filename, "CSV") | str_detect(filename, "csv") ) &
             str_detect(filename, "generales")  ) 

  if (nrow(temp_df)!= 9) {
    print("==++++++++++++++++++++DESTROY EVERYTINHG ++++++++++++++++++++++++++++++++++++++++++++++")
    print(year_ind)
  }
  
  for (j in 1:nrow(temp_df)) {
    
    print(temp_df$archive[j])
    print(temp_df$filename[j])
    
    zip_file <- paste0(raw, "/", year_ind, "/", temp_df$archive[j] )
    
    ##
    temp <- read_delim(unz(zip_file, temp_df$filename[j]),  delim = ";") |>
      clean_names()
    
    temp_df2<- rbind(temp_df2, temp)
  }
  saveRDS(temp_df2, paste0(file.path(import, year_ind), "/",  "caracteristicas_generales", year_ind, ".csv" ) )
  
  

  
  ########################### 
  ####  2023
  ###########################
  
  year_ind<- 2023
  #  Remove folder if it exists
  unlink(file.path(import, year_ind), recursive = TRUE, force = TRUE)
  # Create folder if it doesn't exist
  dir.create(file.path(import, year_ind), showWarnings = FALSE)
  
  
  ### this is for ZIP files with zip files inside
  layer0 <-  df |> 
    filter(year== year_ind) |>
    filter(str_detect(filename, "CSV") &  str_detect(filename, ".zip") ) 
  
  
  temp_df2  <-  tibble()
  for (n in 1:nrow(layer0)){
    
    
    zip::unzip(paste0(raw, "/", year_ind, "/", layer0$archive[n]),
               files = layer0$filename[n], 
               exdir = paste0(import, "/", year_ind ))
    files <- zip::zip_list( paste0(import, "/", year_ind, "/", layer0$filename[n] )  )
    
    
    temp_df <- tibble(files)
    temp_df <- temp_df |> 
      filter( str_detect(filename, "generales")  )
    
    if (nrow(temp_df)!= 1) {
      print("==++++++++++++++++++++DESTROY EVERYTINHG ++++++++++++++++++++++++++++++++++++++++++++++")
      print(year_ind)
    }
    
    for (j in 1:nrow(temp_df)) {
      
      print(temp_df$filename[j])
      
      zip_file <- paste0(import, "/", year_ind, "/", layer0$filename[n])
      
      ## 
      temp <- read_delim(unz(zip_file, temp_df$filename[j]),  delim = ";") |>
        clean_names()
      
      temp_df2<- rbind(temp_df2, temp)
      
      
    }
  }
  #### I don need to run temp_df2  <-  tibble() again because I have already create it
  
  temp_df <- df |> 
    filter(year== year_ind) |>
    filter((str_detect(filename, "CSV") | str_detect(filename, "csv") ) &
             str_detect(filename, "generales")  ) 
  
  cc= 12-nrow(layer0)
  
  if (nrow(temp_df)!= cc) {
    print("==++++++++++++++++++++DESTROY EVERYTINHG ++++++++++++++++++++++++++++++++++++++++++++++")
    print(year_ind)
  }
  
  for (j in 1:nrow(temp_df)) {
    
    print(temp_df$archive[j])
    print(temp_df$filename[j])
    
    zip_file <- paste0(raw, "/", year_ind, "/", temp_df$archive[j] )
    
    ##
    temp <- read_delim(unz(zip_file, temp_df$filename[j]),  delim = ";") |>
      clean_names()
    
    temp_df2<- rbind(temp_df2, temp)
  }
  saveRDS(temp_df2, paste0(file.path(import, year_ind), "/",  "caracteristicas_generales", year_ind, ".csv" ) )
  
  
  ########################### 
  ####  2024
  ###########################
  
  year_ind<- 2024
  #  Remove folder if it exists
  unlink(file.path(import, year_ind), recursive = TRUE, force = TRUE)
  # Create folder if it doesn't exist
  dir.create(file.path(import, year_ind), showWarnings = FALSE)
  
  
  ### this is for ZIP files with zip files inside
  layer0 <-  df |> 
    filter(year== year_ind) |>
    filter(str_detect(filename, "CSV") &  str_detect(filename, ".zip") ) 
  
  
  temp_df2  <-  tibble()
  for (n in 1:nrow(layer0)){
    
    
    zip::unzip(paste0(raw, "/", year_ind, "/", layer0$archive[n]),
               files = layer0$filename[n], 
               exdir = paste0(import, "/", year_ind ))
    files <- zip::zip_list( paste0(import, "/", year_ind, "/", layer0$filename[n] )  )
    
    
    temp_df <- tibble(files)
    temp_df <- temp_df |> 
      filter( str_detect(filename, "generales")  )
    
    if (nrow(temp_df)!= 1) {
      print("==++++++++++++++++++++DESTROY EVERYTINHG ++++++++++++++++++++++++++++++++++++++++++++++")
      print(year_ind)
    }
    
    for (j in 1:nrow(temp_df)) {
      
      print(temp_df$filename[j])
      
      zip_file <- paste0(import, "/", year_ind, "/", layer0$filename[n])
      
      ## 
      temp <- read_delim(unz(zip_file, temp_df$filename[j]),  delim = ";") |>
        clean_names()
      
      temp_df2<- rbind(temp_df2, temp)
      
      
    }
  }
  #### I don need to run temp_df2  <-  tibble() again because I have already create it
  
  temp_df <- df |> 
    filter(year== year_ind) |>
    filter((str_detect(filename, "CSV") | str_detect(filename, "csv") ) &
             str_detect(filename, "generales")  ) 
  
  cc= 12-nrow(layer0)
  
  if (nrow(temp_df)!= cc) {
    print("==++++++++++++++++++++DESTROY EVERYTINHG ++++++++++++++++++++++++++++++++++++++++++++++")
    print(year_ind)
  }
  
  for (j in 1:nrow(temp_df)) {
    
    print(temp_df$archive[j])
    print(temp_df$filename[j])
    
    zip_file <- paste0(raw, "/", year_ind, "/", temp_df$archive[j] )
    
    ##
    temp <- read_delim(unz(zip_file, temp_df$filename[j]),  delim = ";") |>
      clean_names()
    
    temp_df2<- rbind(temp_df2, temp)
  }
  saveRDS(temp_df2, paste0(file.path(import, year_ind), "/",  "caracteristicas_generales", year_ind, ".csv" ) )
  
  
  ########################### 
  ####  2025
  ###########################
  
  year_ind<- 2025
  #  Remove folder if it exists
  unlink(file.path(import, year_ind), recursive = TRUE, force = TRUE)
  # Create folder if it doesn't exist
  dir.create(file.path(import, year_ind), showWarnings = FALSE)
  
  
  ### this is for ZIP files with zip files inside
  layer0 <-  df |> 
    filter(year== year_ind) |>
    filter(str_detect(filename, "CSV") &  str_detect(filename, ".zip") ) 
  
  temp_df2  <-  tibble() 
  
  temp_df <- df |> 
    filter(year== year_ind) |>
    filter((str_detect(filename, "CSV") | str_detect(filename, "csv") ) &
             str_detect(filename, "generales")  ) 
  
  cc= 12-nrow(layer0)
  
  if (nrow(temp_df)!= cc) {
    print("==++++++++++++++++++++DESTROY EVERYTINHG ++++++++++++++++++++++++++++++++++++++++++++++")
    print(year_ind)
  }
  
  for (j in 1:nrow(temp_df)) {
    
    print(temp_df$archive[j])
    print(temp_df$filename[j])
    
    zip_file <- paste0(raw, "/", year_ind, "/", temp_df$archive[j] )
    
    ##
    temp <- read_delim(unz(zip_file, temp_df$filename[j]),  delim = ";") |>
      clean_names()
    
    temp_df2<- rbind(temp_df2, temp)
  }
  saveRDS(temp_df2, paste0(file.path(import, year_ind), "/",  "caracteristicas_generales", year_ind, ".csv" ) )
  
  