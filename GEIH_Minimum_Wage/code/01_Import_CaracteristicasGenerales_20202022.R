
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


#### define function to process the dataset


rename_if_exists <- function(df, new, olds) {
  old <- olds[olds %in% names(df)][1]
  if (!is.na(old)) {
    df <- dplyr::rename(df, !!new := !!rlang::sym(old))
  }
  df
}


read_geih_person <- function(zip_file, filename, guess_max = 10000) {
  
  temp <- readr::read_delim(
    unz(zip_file, filename),
    delim = NULL,
    guess_max = guess_max,
    show_col_types = FALSE
  ) |>
    janitor::clean_names() 
    
  
 if  ("p3271" %in% names(df)) {
   temp <- temp |> 
     mutate(new_version=1)
 } else {
   
   temp <- temp |> 
     mutate(new_version=0)
 }
  
  if  (!"esc" %in% names(df)) {
    temp <- temp |> 
      mutate(esc=NA)
  }
  
    # robust renaming (handles changing variable names)
    temp <- temp |>
      rename_if_exists("sex", c("p6020", "p3271")) |>
      rename_if_exists("age", c("p6040")) |>
      rename_if_exists("relation_with_head", c("p6050")) |>
      rename_if_exists("marital_status", c("p6070")) |>
      rename_if_exists("literacy", c("p6160")) |>
      rename_if_exists("student", c("p6170")) |>
      rename_if_exists("maxedulevel01", c("p6210", "p3042")) |>
      rename_if_exists("maxedulevel02", c("p6220", "p3043")) |>
      rename_if_exists("maxgrade", c("p6210s1", "p3042s1")) |>
      rename_if_exists("schyears", c("esc")) |>
      rename_if_exists("city", c("area")) |>
      rename_if_exists("month", c("mes")) |>
      
      # IDs for merging
      rename_if_exists("house", c("directorio")) |>
      rename_if_exists("household", c("secuencia_p")) |>
      rename_if_exists("person", c("orden")) |>
      # keep only renamed variables
    dplyr::select(
      sex, age, relation_with_head, marital_status, literacy, student,
      maxedulevel01, maxedulevel02, maxgrade, schyears,
      city, month, house, household, person, new_version
    )
  
    
    
  return(temp)
}

#########################
#### extract general characteristics




####################
#####     2020 
####################
  year_ind<- 2020
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
    
    temp <- read_geih_person(
      zip_file = zip_file,
      filename = temp_df$filename[j]
    )
    
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
    
    ##extract and proccess
    temp <- read_geih_person(
      zip_file = zip_file,
      filename = temp_df$filename[j]
    )
    
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
       
       ##extract and proccess
       temp <- read_geih_person(
         zip_file = zip_file,
         filename = temp_df$filename[j]
       )
       
       
       ##extract and proccess
       temp <- read_geih_person(
         zip_file = zip_file,
         filename = temp_df$filename[j]
       )
    
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
    
    ##extract and proccess
    temp <- read_geih_person(
      zip_file = zip_file,
      filename = temp_df$filename[j]
    )
    
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
      ##extract and proccess
      temp <- read_geih_person(
        zip_file = zip_file,
        filename = temp_df$filename[j]
      )
      
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
    
    ##extract and proccess
    temp <- read_geih_person(
      zip_file = zip_file,
      filename = temp_df$filename[j]
    )
    
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
      
      ##extract and proccess
      temp <- read_geih_person(
        zip_file = zip_file,
        filename = temp_df$filename[j]
      )
      
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
    
    ##extract and proccess
    temp <- read_geih_person(
      zip_file = zip_file,
      filename = temp_df$filename[j]
    )
    
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
    
    ##extract and proccess
    temp <- read_geih_person(
      zip_file = zip_file,
      filename = temp_df$filename[j]
    )
    
    temp_df2<- rbind(temp_df2, temp)
  }
  saveRDS(temp_df2, paste0(file.path(import, year_ind), "/",  "caracteristicas_generales", year_ind, ".csv" ) )
  
  