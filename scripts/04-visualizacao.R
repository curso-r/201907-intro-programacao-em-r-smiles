# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(nycflights13)


# Bases de dados ----------------------------------------------------------

data(flights)
data(airlines)
data(airports)
data(planes)
data(weather)

flights <- sample_n(flights, 10000)

View(flights)

# Estrutura ---------------------------------------------------------------

# exemplo 1

flights %>% 
  ggplot()

# exemplo 2

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay))

# exemplo 3 

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay, color = origin))

# exercicio 1
# Faça um gráfico de dispersão do atraso na saída pelo tempo de vôo

# exercício 2
# Faça um gráfico de dispersão do atraso na chegada pela distância

# exemplo 4

flights %>%
  mutate(dia = lubridate::floor_date(time_hour, "day")) %>% 
  group_by(dia) %>% 
  summarise(
    arr_delay = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    n = n()
  ) %>% 
  ggplot() +
  geom_point(aes(x = n, y = qtd_atrasos))

# exercicio 3
# Usando a estrutura de código acima faça um gráfico de dispersão do
# número de vôos pela proporção de vôos atrasados.


# exercicio 4
# Também usando a estrutura do exemplo 4 coloque uma cor para cada uma
# das origens.


# Customização ------------------------------------------------------------

# exemplo 1

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay, color = origin)) + 
  labs(x = "Atraso na saída", y = "Atraso na chegada", color = "Origem")

# exemplo 2

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay, color = origin)) + 
  scale_color_manual(values = c("red", "blue", "green"))

# exemplo 3

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay, color = origin)) + 
  scale_color_manual(values = c(EWR = "grey", JFK = "red", LGA = "grey"))

# exemplo 4

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay), color = "red")

# exemplo 5

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay), color = "red") +
  gghighlight::gghighlight(origin == "JFK")

# exemplo 6

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay)) +
  theme_bw() 

# exemplo 7

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay)) +
  scale_y_continuous(breaks = c(0, 100, 500), minor_breaks = NULL)

# exemplo 8

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay), color = "darkorange") +
  labs(x = "Atraso na saída", y = "Atraso na chegada") +
  theme_minimal()

# exercício 1
# Crie um gráfico de dispersão do atraso na saida pelo tempo de vôo com uma cor para
# cada origem.
# Customize eixos, legendas e cores do gráfico.

# exercício 2
# Crie um gráfico de dispersão do número de vôos pelo atraso médio (por dia-hora).
# Customize o gráfico.

# Outros geoms ------------------------------------------------------------

# exemplo 1

flights %>% 
  group_by(dia = lubridate::floor_date(time_hour, "day")) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_line(aes(x = dia, y = n))

# exemplo 2

flights %>% 
  group_by(origin) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder(origin, n), y = n))

# exemplo 3 

flights %>% 
  ggplot() +
  geom_boxplot(aes(x = fct_reorder(carrier, arr_delay), y = arr_delay))

# exemplo 4

flights %>% 
  group_by(dia = lubridate::floor_date(time_hour, "day")) %>% 
  summarise(n = n()) %>% 
  ggplot() + 
  geom_area(aes(x = dia, y = n), alpha = 0.7)

# exemplo 5

flights %>% 
  group_by(dia = lubridate::floor_date(time_hour, "day")) %>% 
  summarise(
    atraso_max = max(arr_delay, na.rm = TRUE),
    atraso_min = min(arr_delay, na.rm = TRUE)
  ) %>% 
  ggplot() +
  geom_ribbon(aes(x = dia, ymin = atraso_min, ymax = atraso_max), alpha=0.4)

# exemplo 6

flights %>% 
  group_by(dia = lubridate::floor_date(time_hour, "day")) %>% 
  summarise(
    atraso_max = max(arr_delay, na.rm = TRUE),
    atraso_medio = mean(arr_delay, na.rm = TRUE),
    atraso_min = min(arr_delay, na.rm = TRUE)
  ) %>% 
  ggplot() +
  geom_ribbon(aes(x = dia, ymin = atraso_min, ymax = atraso_max), alpha=0.4) +
  geom_line(aes(x = dia, y = atraso_medio))

# exemplo 7

flights %>% 
  mutate(dest = fct_lump(dest, 15)) %>% 
  group_by(dest) %>% 
  summarise(n = n()) %>%
  filter(dest != "Other") %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder(dest, n), y = n)) +
  geom_text(aes(x = dest, y = n*0.9, label = n), color = "white")
  

# exercício 1
# Faça um gráfico de barras da proporção de atrasos por companhia aérea.

# exercício 2
# Faça um gráfico de linha do atraso mediano por dia.

# exercício 3
# Faça um gráfico de dispersão do atraso na saida e o tempo de voo e acrescente
# uma reta de regressão (dica: geom_smooth)

# exercício 4
# Faça um gráfico de dispersão do atraso pela velocidade do vento. (dica: join 
# com a tabela weather)

# exercício 5
# Faça um gráfico com 3 linhas (uma para cada origem) do dia pela velocidade média
# do vento.


# Facets ------------------------------------------------------------------

# exemplo 1
flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay, color = carrier)) +
  facet_wrap(~carrier) +
  guides(color = FALSE)

# exemplo 2
flights %>% 
  group_by(dia = lubridate::floor_date(time_hour, "month"), dest = fct_lump(dest, 9)) %>% 
  summarise(n = n()) %>% 
  filter(dest != "Other") %>% 
  ggplot() +
  geom_col(aes(x = dia, y = n)) +
  facet_wrap(~dest, ncol = 3)
    
# exemplo 3
flights %>% 
  mutate(dest = fct_lump(dest, 5), carrier = fct_lump(carrier, 5)) %>% 
  filter(dest != "Other") %>% 
  ggplot() +
  geom_point(aes( x = dep_delay, y = arr_delay)) +
  # facet_wrap(~dest + carrier)
  facet_grid(carrier ~ dest)

# Miscelânea --------------------------------------------------------------

# Mapas

# referência
# https://sjfox.github.io/post/world_map_flights/

flights_com_lat_lon <- flights %>%
  select(origin, dest) %>% 
  left_join(airports, by = c("origin" = "faa")) %>%
  rename(lat_origin = lat, lon_origin = lon) %>%
  left_join(airports %>% select(faa, lat, lon), by = c("dest" = "faa")) %>%
  rename(lat_dest = lat, lon_dest = lon) %>%
  mutate(ordered_pair = if_else(origin > dest, paste0(dest, origin), paste0(origin, dest))) %>%
  tidyr::drop_na() %>%
  dplyr::distinct()

mapa <- map_data("world") #poligonos do mapa

base_do_mapa <- mapa %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "black", color = "white", size=0.15) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"), legend.position = "none") +
  coord_equal(xlim = range(flights_com_lat_lon$lon_dest), ylim = range(flights_com_lat_lon$lat_dest)) 

base_do_mapa +
  geom_point(
    data = flights_com_lat_lon, 
    aes(x = lon_dest, y = lat_dest), 
    alpha=0.25, 
    size=1,
    color = "red", 
    inherit.aes = FALSE
  ) +
  geom_point(
    data = flights_com_lat_lon, 
    aes(x = lon_origin, y = lat_origin), 
    alpha=0.25, 
    size=3,
    color = "green", 
    inherit.aes = FALSE
  )

base_do_mapa +
  geom_curve(
    data = flights_com_lat_lon, 
    aes(x = lon_origin, xend = lon_dest, y = lat_origin,  yend = lat_dest), 
    alpha=0.25, 
    size=0.1,
    color = "purple", 
    inherit.aes = FALSE
  )

# plotly

library(plotly)

p <- flights %>% 
  ggplot(aes(x = dep_delay, y = arr_delay))+
  geom_point()

ggplotly(p)



