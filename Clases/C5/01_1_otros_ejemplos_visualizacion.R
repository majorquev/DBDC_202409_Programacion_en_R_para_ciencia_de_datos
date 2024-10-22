library(highcharter)
library(tidyverse)
library(purrr)
library(nycflights13)
library(ggplot2)
library(maps)


# carga de datos ----------------------------------------------------------
data(flights)
data(airports)


# Mapa con aeropuertos de destino -----------------------------------------
# Crear una tabla resumen  que contenga el promedio de arr_delay por cada 
# aeropuerto de destino, indicando la latitud y longitud de cada uno de estos últimos.

aux_data <- flights %>%
  group_by(dest) %>% 
  summarise(
    n_vuelos = n(),
    mean_arr_del = mean(arr_delay,na.rm = T)
  ) %>%
  left_join(airports,
            by = c("dest" = "faa")) %>%
  ungroup() %>%
  mutate(
    color = colorize(mean_arr_del),
    z = n_vuelos
    ) %>% 
  na.omit()


# visualización con ggplot ------------------------------------------------

ggplot(aux_data) +
  aes(x = lon, y = lat, color = mean_arr_del) + 
  geom_point() + 
  borders("state")
  


# visualización con highcharter -------------------------------------------

# Las variables "z" y "color" son necesarias para el tamaño (en caso de usar mapbubble)
#  mientras que el color lo lee automáticamente de la variable color.
hcmap("countries/us/us-all", showInLegend = F) %>% 
  hc_add_series(
    data = aux_data,
    type = "mapbubble",
    name = "Aeropuerto",
    dataLabels = list(enabled = F),
    tooltip = list(pointFormat = "{point.name}<br>Vuelos:{point.n_vuelos}<br>Retraseo medio (arr):{point.mean_arr_del}</b><br/>")
  ) %>%
  hc_mapNavigation(enabled = T) %>%
  hc_plotOptions(series = list(showInLegend = F)) %>% 
  hc_title(text = "Distribución de retrasos por aeropuerto de destino")



# Distribución de vuelos por destino/origen -------------------------------
# Buscamos generar un dataset donde cada fila represente 
# un aeropuerto de destino, y en cada columna se registre el conteo 
# de vuelos proveniente de los distintos aeropuertos de origen.


vuelos_ori_des <- flights %>% 
  pivot_wider(
    id_cols = c("origin","dest","arr_delay"),
    names_from = origin,
    values_from = arr_delay,
    values_fn = list(arr_delay = length),
    values_fill = list(arr_delay = 0)
    
  ) 


# Cantidad de vuelos por origen-destino -----------------------------------

# Mediante un gráfico de barras "estackeado", queremos representar para cada aeropuerto
# de destino, la cantidad de vuelos provenientes de los tres aeropuertos de origen en NY.

highchart() %>% 
  hc_chart(type = "column") %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_xAxis(categories = vuelos_ori_des$dest) %>%
  hc_add_series(name="Newark Liberty Intl",
                data = vuelos_ori_des$EWR,
                stack = "destino") %>%
  hc_add_series(name="La Guardia",
                data = vuelos_ori_des$LGA,
                stack = "destino") %>%
  hc_add_series(name="John F Kennedy Intl",
                data = vuelos_ori_des$JFK,
                stack = "destino") %>%
  hc_title(text="Cantidad de vuelos por origen/destino")


# Si usaramos ggplot ------------------------------------------------------

# 1. La data debe venir en formato "long" (por simplicidad)

aux <- vuelos_ori_des %>%
  pivot_longer(
    cols = c("EWR","LGA","JFK") , # columnas que se quieren despivotear.
    names_to = "origen", #nombres de las columnas, se van como categorías a una nueva variable (origen)
    values_to = "cantidad_vuelos", # los valores a rescatar se van a una nueva variable (cantidad_vuelos)
    values_drop_na = TRUE
  ) 
# notar que sólo se deben espicificar las columnas a trasponer. 
# No así los valores de las filas de la coolumna "dest" q es la de nuestro interés

aux %>% 
  ggplot() +
  aes(x = dest, y = cantidad_vuelos, fill = origen) +
  geom_bar(stat = "identity",
           position = "stack")



# Gráfico de series -------------------------------------------------------
data(weather)

# Se quiere mostrar el promedio de retraso por día y aeropuerto de origen, 
# junto con la velocidad media del viento.

df_ts <- flights %>%
  left_join(weather, by = c("origin","time_hour") ) %>%
  mutate(time_hour2 = as.Date(time_hour)) %>%  
  group_by(origin,time_hour2) %>%
  summarise(mean_dep_delay = mean(dep_delay,na.rm = T),
            mean_wind_gust =  mean(wind_gust,na.rm=TRUE))

# por simplicidad despivoteamos las columnas "mean_dep_delay" y "mean_wind_gust", 
# en una nueva columna llamada serie, mientras que sus valores a una llamada "valor"


df_ts <- df_ts %>% 
  pivot_longer(
  cols = c("mean_dep_delay" , "mean_wind_gust") , # columnas que se quieren despivotear.
  names_to = "serie", #nombres de las columnas, se van como categorías a una nueva variable (origen)
  values_to = "valor", # los valores a rescatar se van a una nueva variable (cantidad_vuelos)
  values_drop_na = TRUE
) 



df_ts %>% 
  filter(origin == "EWR") %>%
  hchart(type = "line",
         hcaes(y = valor,
               group = serie, x = time_hour2)
  )





