
library(pacman)
#p_load(gander)

p_load(tidyverse, 
       rio, 
       janitor,
       lubridate)

rm(list= ls())


df<- import("../data/SECOP_Integrado_20251025.csv") |>
  clean_names()

#table(df$tipo_de_contrato)
#table(df$tipo_documento_proveedor)
#table(df$estado_del_proceso) 


### drop if there is not fecha_fin_ejecucion
df<- df |> 
  filter(fecha_fin_ejecucion !="" & fecha_de_firma_del_contrato!="") 

### format dates

df<- df |> 
  mutate(fecha_fin_ejecucion= as.Date(fecha_fin_ejecucion, format = "%m/%d/%Y"), 
         fecha_inicio_ejecucion= as.Date(fecha_inicio_ejecucion, format = "%m/%d/%Y"),
         fecha_de_firma_del_contrato= as.Date(fecha_de_firma_del_contrato, format = "%m/%d/%Y")
 )    |>
  mutate(
    year_fin      = year(fecha_fin_ejecucion),
    year_inicio   = year(fecha_inicio_ejecucion),
    year_firma    = year(fecha_de_firma_del_contrato)
  )



df<- df |> 
 filter(year_fin>=2025 & year_fin<=2027, 
        year_inicio==2025)


#table(df$year_fin)
#table(df$year_inicio)
#table(df$year_firma) 

### que prestacion de servicios and only cedula de ciudadania y o extrangeria
df<- df |> 
  arrange( documento_proveedor, id_contrato ) |>
  filter(tipo_de_contrato=="Prestación de servicios" | tipo_de_contrato=="Prestación de Servicios" ) |>
  filter(tipo_documento_proveedor=="Cédula de Ciudadanía"| 
           tipo_documento_proveedor=="Cédula de Extranjería")


####  imputing the fecha inicio del contrato 
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

########## drop terminados and cerradors

df<- df |> 
  filter(!estado_del_proceso %in% c("terminado", 
                                   "Convocado",
                                   "En aprobación", 
                                   "enviado Proveedor", 
                                   "Liquidado", 
                                    "Borrador", 
                                   "NO DEFINIDO"))
df<- df |> 
  mutate(
    flag = case_when(
    estado_del_proceso=="cedido" ~ 1,
    estado_del_proceso=="Suspendido" ~ 1,
    estado_del_proceso=="Cerrado" ~ 1,
    estado_del_proceso=="Terminado sin Liquidar" ~ 1,
    estado_del_proceso=="" ~ 1,
    TRUE ~ 0)) |>
  group_by(id_contrato) |>
  mutate(flag2= max(flag)) |>
  select(-flag) |>
  ungroup() 


df<- df |>
  filter(flag2==0)


ggplot(df,aes(x=estado_del_proceso)) +
  geom_bar()+
  coord_flip()




## create monthly salary
df<- df |> 
  mutate(valor_mensual= valor_diario*30 ) 

### keep last modification date, 
df <- df |> 
  group_by(id_contrato) |>
  mutate(flag2= max(fecha_fin_ejecucion)) |>
  ungroup()
  

# Just to Verify
#  view(df|> select(id_contrato, flag2, fecha_fin_ejecucion, fecha_inicio_ejecucion, fecha_de_firma_del_contrato, 
#                 valor_mensual) )
  
df <- df |>
  filter(flag2== fecha_fin_ejecucion)


########
### Clean 
#######


#### 1 Clean workers Id drop fake (prueba or test entries) 
df <- df |>
  mutate(documento_proveedor = str_replace_all(documento_proveedor, "[^0-9]", ""))


df <- df |>
  mutate( # drop IDs that become empty or only zeros of any length
          documento_proveedor = na_if(documento_proveedor, ""),            # empty → NA
          documento_proveedor = na_if(documento_proveedor, str_dup("0", nchar(documento_proveedor))),  # zeros → NA

          # compute length of the resulting string
          len_documento = nchar(documento_proveedor)
) 

#### filter document numbers with less than 5 digits

ggplot(df,aes(x=len_documento)) +
  geom_bar()+
  coord_flip()
# remove rows with missing IDs after cleaning

df <- df |>   
  filter(!is.na(documento_proveedor),
         len_documento>=5)

# most of IDs have 10 digits
#ggplot(df, aes(x=log(valor_diario)))+
#  geom_histogram() 
  

### drop contracts that last less than 15 days
## Probably these are errors and they increase the monthly salary
## and daily salary 

df <- df |> filter(duration>15) |>
  mutate(valor_contrato= if_else(valor_contrato>=1e9,
                                 valor_contrato/1000, ## probably they put 1.000.000.000 instead of 1.000.000
                                 valor_contrato))



#### drop if contract value is zero

df <- df |> filter(valor_mensual>0) 



##########
# Crate worker panel 
##########

df <- df |> 
  arrange(documento_proveedor)



View( df|> 
  filter(documento_proveedor=="1144098701" | documento_proveedor=="1144198124" ) )


#### impute high values of duration 
#a<- median(df$duration) 
#df <- df |>
#  mutate(duration= if_else(duration>=600,
#                           a, # duration of contract may have mistakes
#                           duration))




df  |>
  ggplot( aes(x = duration )) +
  geom_boxplot(fill = "tomato", alpha = 0.7) 


##### Graph
## view extreme values
view(df |> filter(valor_mensual<1e5))

p_load(scales)


df |> 
  ggplot( aes(x = valor_mensual / 1e6)) +
  geom_histogram(fill = "tomato", alpha = 0.7) +
  scale_x_log10(labels = label_number()) +
  theme_bw() +
  labs(x= "Salario Mensual (Millones de Pesos)") 




df |> 
  ggplot( aes(x = valor_mensual / 1e6)) +
  geom_boxplot(fill = "tomato", alpha = 0.7) +
  scale_x_log10(labels = label_number()) +
  theme_bw() +
  labs(x= "Salario Mensual (Millones de Pesos)") 


view(df |> filter(valor_mensual<=0))


##########
# Appendix
##########


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

ggplot(df,aes(x=estado_del_proceso)) +
  geom_bar()+
  coord_flip()

table(df$estado_del_proceso)





View(df |> filter(estado_del_proceso=="terminado"))

 
View(df |> 
       filter(url_contrato=="https://community.secop.gov.co/Public/Tendering/OpportunityDetail/Index?noticeUID=CO1.NTC.7320372&isFromPublicArea=True&isModal=true&asPopupView=true"))

View(df |> filter(nom_raz_social_contratista=="yorely Balbin Valencia"))




### it may be a good rule to 

table(df$n_entries_contrac)


View(df|> select(documento_proveedor, id_proceso, id_contrato, tipo_de_contrato, estado_del_proceso, 
                 fecha_de_firma_del_contrato, fecha_inicio_ejecucion,
                 fecha_fin_ejecucion, objeto_del_contrato, valor_contrato,n_entries_contrac, url_contrato, valor_mensual, valor_diario ) |> filter(n_entries_contrac>5))


df2<- df |> 
  select(documento_proveedor, id_proceso, id_contrato, tipo_de_contrato, estado_del_proceso, 
            fecha_de_firma_del_contrato, fecha_inicio_ejecucion,
            fecha_fin_ejecucion, objeto_del_contrato, valor_contrato, n_entries_contrac) %>%
  filter(n_entries_contrac>5) 




View(df2 ) 




