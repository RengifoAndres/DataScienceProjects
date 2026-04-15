
library(pacman)
p_load(rvest, 
       httr, 
       stringr, 
       zip, 
       tidyverse, 
       stringr)

### step 0 clear all

rm(list= ls())

#### define paths



data_path <- "../data"
raw <- paste0(data_path, "/raw" )




###### define the years

# Define the catalog IDs mapping for GEIH
catalog_ids <- c(
  "2022" = 771,
  "2023" = 782,
  "2024" = 819,
  "2025" = 853
)


urls<- list()
count<- 1
for (year in 2022:2025) {
  # Implementation using your structure
  if (as.character(year) %in% names(catalog_ids)) {
    id <- catalog_ids[as.character(year)]
    url_base <- paste0("https://microdatos.dane.gov.co/index.php/catalog/", id, "/get-microdata")
    urls[count] <- url_base
    count<- 1 + count
  } else {
    stop("Year not found in the catalog mapping.")
  }
  
  
  # 1) Remove folder if it exists
  unlink(file.path(raw, year), recursive = TRUE, force = TRUE)
  
  # 2) Create folder if it doesn't exist
  dir.create(file.path(raw, year), showWarnings = FALSE)
  
  # 3) Leer la página HTML
  pagina <- read_html(url_base)
  
  # 4) Extraer enlaces de descarga (.zip)
  links <- pagina %>%
    html_nodes("input") %>%
    html_attr("onclick")
  
  
  zip_links <- str_extract(links, "https?://[^']+") |> str_trim()
  zip_links <- unique(zip_links)
  
  
  zip_name <- str_extract(links, "'[^']+\\.zip'") |> str_replace_all("'", "")
  zip_name <- unique(zip_name)
  
  
  if (length(zip_links)== length(zip_name)) {
    print("link and names have the same size")
  }
  
  name_links= tibble(
    names= zip_name, 
    links= zip_links
  )
  
  name_links <- name_links |> 
    filter(str_detect(names, fixed(".csv.")))
  
  ### THIS 
  if (nrow(name_links)!=0) {
    
    zip_name <- name_links$names
    zip_links <- name_links$links
    
  }
  
  for (i in 1:length(zip_name)) {
    
    if (is.na(zip_links[i])) {
      next
    }
    
    file_name <- basename(zip_links[i])
    destfile <- file.path(paste0(raw, "/", year ), zip_name[i])
    print(zip_name[i])
    message("Descargando: ", file_name)
    try(
      download.file(zip_links[i], destfile, mode = "wb", quiet = FALSE)
    )
    

    Sys.sleep(5) 
    print("Wait is over. Code execution continues.")
   # file.remove(destfile)
    
  }
  
}

##### End



