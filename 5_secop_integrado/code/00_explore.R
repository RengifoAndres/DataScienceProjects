
library(pacman)
#p_load(gander)

p_load(tidyverse, 
       rio, 
       janitor,
       lubridate)

rm(list= ls())


df<- import("../data/SECOP_Integrado_20251025.csv") |>
  clean_names()

table(df$tipo_de_contrato)
table(df$tipo_documento_proveedor)
table(df$estado_del_proceso) 


### drop if there is not fecha_fin_ejecucion
df<- df |> 
  filter(fecha_fin_ejecucion !="" & fecha_de_firma_del_contrato!="") 

df<- df |> 
  mutate(fecha_fin_ejecucion= as.Date(fecha_fin_ejecucion, format = "%m/%d/%Y"), 
         fecha_inicio_ejecucion= as.Date(fecha_inicio_ejecucion, format = "%m/%d/%Y"),
         fecha_de_firma_del_contrato= as.Date(fecha_de_firma_del_contrato, format = "%m/%d/%Y")
 ) 


### que prestacion de servicios and only cedula de ciudadania y o extrangeria
df<- df |> 
  arrange( documento_proveedor, id_contrato ) |>
  filter(tipo_de_contrato=="Prestación de servicios" | tipo_de_contrato=="Prestación de Servicios" ) |>
  filter(tipo_documento_proveedor=="Cédula de Ciudadanía"| 
           tipo_documento_proveedor=="Cédula de Extranjería")


####  imputing the fecha inicion del contrato 
df<- df |> 
  mutate(fecha_inicio_ejecucion= if_else(is.na(fecha_inicio_ejecucion)== TRUE, 
                                         fecha_de_firma_del_contrato+ 1, fecha_inicio_ejecucion ) 
         ) 




####  calculate the day pay
df<- df |> 
  mutate(duration= fecha_fin_ejecucion- fecha_inicio_ejecucion) |>
  filter(duration> 0) |> 
  mutate(valor_contrato= as.numeric(gsub("[$,]", "", valor_contrato)),
         valor_diario= valor_contrato/as.numeric(duration)) 

ggplot(df|> filter(duration<= 1000), aes(x=duration))+
  geom_boxplot()


ggplot(df|> filter(valor_diario<=1000000), aes(x=valor_diario))+
  geom_histogram()


df<- df |> 
  mutate(flag= 1) |>
  group_by(id_contrato) |>
  mutate(n_entries_contrac= sum(flag)) |>
  select(-flag) |>
  ungroup()


table(df$n_entries_contrac)


View(df|> select(documento_proveedor, id_proceso, id_contrato, tipo_de_contrato, estado_del_proceso, 
                 fecha_de_firma_del_contrato, fecha_inicio_ejecucion,
                 fecha_fin_ejecucion, objeto_del_contrato, valor_contrato,n_entries_contrac ) |> filter(n_entries_contrac>5))


df2<- df |> 
  select(documento_proveedor, id_proceso, id_contrato, tipo_de_contrato, estado_del_proceso, 
            fecha_de_firma_del_contrato, fecha_inicio_ejecucion,
            fecha_fin_ejecucion, objeto_del_contrato, valor_contrato, n_entries_contrac) %>%
  filter(n_entries_contrac>5) 




View(df2 ) 




