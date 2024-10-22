library(ggplot2)
library(dplyr)


# lectura de archivo ------------------------------------------------------

casos <- read.csv("../casos_diarios_covid.csv")

# Generación campo fecha --------------------------------------------------
casos2 <- casos %>% 
  mutate(mes = case_when(mes == "mar"~"03",
                         mes == "abr"~"04",
                         mes == "may"~"05",
                         mes == "jun"~"06",
                         mes == "jul"~"07",
                         mes == "ago"~"08",
                         mes == "sep"~"09",
                         mes == "oct"~"10",
                         mes == "nov"~"11",
                         mes == "dic"~"12"),
         dia = ifelse(nchar(dia_del_mes) == 1,
                      paste0("0",dia_del_mes),
                      dia_del_mes)) %>% 
  mutate(fecha = as.Date(paste("2020",mes,dia,sep = "-"))) 



# Gráfico 1 ---------------------------------------------------------------

#Sólo se grafican curvas para c("casos_activos","casos_totales","casos_recuperados")

casos2 %>% 
  filter(tipo %in% c("casos_activos","casos_totales","casos_recuperados")) %>% 
  ggplot() +
  aes(x = fecha, y = total, color = tipo) + 
  geom_line() +
  labs(
    x = "Fecha de actualización",
    y = "Casos totales",
    color = "Tipo de conteo",
    title = "Evolución casos COVID-19 Chile",
    subtitle = "Actualización al 08 de julio 2020",
    caption = "Gráfico 1: Casos acumulados"
  ) 


# Gráfico 2 ---------------------------------------------------------------
## Se grafican las curvas restantes

casos2 %>% 
  filter(tipo %in% c("casos_nuevos_con_sintomas","casos_nuevos_sin_sintomas","casos_nuevos_totales")) %>%
  ggplot() +
  aes(x = fecha, y = total, color = tipo) + 
  geom_line() +
  labs(
    x = "Fecha de actualización",
    y = "Casos totales",
    color = "Tipo de conteo",
    title = "Evolución casos COVID-19 Chile",
    subtitle = "Actualización al 08 de julio 2020",
    caption = "Gráfico 2: Casos diarios"
  ) + 
  scale_x_date(date_breaks = "14 day")

# elementos 
# labs para titulos, ejes y leyenda
# scale_x_date para modificar los cortes  

