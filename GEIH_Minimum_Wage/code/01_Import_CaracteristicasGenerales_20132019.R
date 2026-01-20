
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

for (year_ind in 2013:2019) {
  
  #  Remove folder if it exists
  unlink(file.path(import, year_ind), recursive = TRUE, force = TRUE)
  # Create folder if it doesn't exist
  dir.create(file.path(import, year_ind), showWarnings = FALSE)
  
  temp_df <- df |> 
    filter(year== year_ind) |>
    filter(str_detect(filename, "Cabecera") &
             str_detect(filename, "generales")  )  
  
  
  temp_df2  <-  tibble()
  
  if (nrow(temp_df)!= 12) {
    print("==++++++++++++++++++++DESTROY EVERYTINHG ++++++++++++++++++++++++++++++++++++++++++++++")
    print("==++++++++++++++++++++DESTROY EVERYTINHG ++++++++++++++++++++++++++++++++++++++++++++++")
    print("==++++++++++++++++++++DESTROY EVERYTINHG ++++++++++++++++++++++++++++++++++++++++++++++")
    
  }
  
  for (j in 1:nrow(temp_df)) {
    
  
  print(temp_df$archive[j])
  print(temp_df$filename[j])
  
  
  zip_file <- paste0(raw, "/", year_ind, "/", temp_df$archive[j] )
  
  ##
  temp <- read_delim(
    unz(zip_file, temp_df$filename[j]),
    delim = NULL,
    guess_max = 10000,
    show_col_types = FALSE
  ) |>
    clean_names()

  
  temp<- temp %>%
    rename(sex=p6020, 
           age=p6040, 
           relation_with_head= p6050,
           marital_status= p6070, 
           literacy=p6160, 
           student=p6170, 
           maxedulevel01=p6210,
           maxedulevel02=p6220, 
           maxgrade= p6210s1,
           schyears= esc, 
           city= area, 
           month=mes)
  
  #### rename to merge
  temp <- temp %>%
    rename(house= directorio, 
           household=secuencia_p, 
           person=orden)
  
 
  temp <- temp %>%
    select(
      sex,age,relation_with_head,marital_status, literacy, student, maxedulevel01,
      maxedulevel02, maxgrade, schyears, city, month, house, household, person
    )
  
  
   
  
  
  temp_df2<- rbind(temp_df2, temp)
  }
  
  saveRDS(temp_df2, paste0(file.path(import, year_ind), "/",  "caracteristicas_generales", year_ind, ".csv" ) )
}





