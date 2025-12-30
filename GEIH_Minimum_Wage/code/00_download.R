
library(pacman)
p_load(rvest, 
       httr, 
       stringr)


### step 0 clear all

rm(list= ls())

#### define paths

data_path <- "../data"
raw <- paste0(data_path, "/raw" )


# 1) Definir URL del GEIH 2020
url_base <- "https://microdatos.dane.gov.co/index.php/catalog/780/get-microdata"


# 2) Crear carpeta 2020 si no existe
dir.create( paste0(raw, "/2020" ), showWarnings = FALSE)

# 3) Leer la página HTML
pagina <- read_html(url_base)

# 4) Extraer enlaces de descarga (.zip)
links <- pagina %>%
  html_nodes("input") %>%
  html_attr("onclick")



zip_links <- str_extract(links, "https?://[^']+") |> str_trim()
zip_links <- unique(zip_links)
zip_links <- zip_links[1:12]


zip_name <- str_extract(links, "'[^']+\\.zip'") |> str_replace_all("'", "")
zip_name <- unique(zip_name)




file_name <- basename(zip_links[1])
destfile <- file.path(paste0(raw, "/2020" ), zip_name[1])

message("Descargando: ", file_name)
try(
  download.file(zip_links[1], destfile, mode = "wb", quiet = FALSE)
)


p_load(zip)

zip::unzip(destfile, exdir = paste0(raw, "2020" ))


##### did it up until here





# 6) Descargar todos los archivos .zip
for (link in zip_links) {
  file_name <- basename(link)
  destfile <- file.path("C:/Users/afrj1/Downloads/2020", file_name)
  
  message("Descargando: ", file_name)
  try(
    download.file(link, destfile, mode = "wb", quiet = FALSE)
  )
}

message("¡Descarga completada!")