# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(nycflights13)

# Bases de dados ----------------------------------------------------------

data(flights)
data(airlines)
data(airports)
data(planes)
data(weather)

set.seed(11111)
flights <- flights %>% 
  sample_n(10000)


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
  geom_point(aes(x = dep_delay, y = arr_delay), size = 0.1)

# exercicio 1
# Faça um gráfico de dispersão do atraso na saída pelo tempo de vôo

flights %>% 
  ggplot() +
  geom_point(aes(x = air_time, y = dep_delay), size = 0.1)

# exercício 2
# Faça um gráfico de dispersão do atraso na chegada pela distância

flights %>% 
  ggplot() +
  geom_point(aes(x = distance, y = arr_delay), size = 0.1)

# exemplo 4

flights %>%
  mutate(dia = lubridate::floor_date(time_hour, "day")) %>%
  #select(time_hour, dia)
  group_by(dia) %>% 
  summarise(
    atraso_medio = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    n = n(),#sum(!is.na(arr_delay)),
    prop_atrasos = sum(arr_delay > 0, na.rm = TRUE)/n(),
  ) %>% 
  ggplot() +
  geom_point(aes(x = n, y = atraso_medio))

# exercicio 3
# Usando a estrutura de código acima faça um gráfico de dispersão do
# número de vôos pela proporção de vôos atrasados.

flights %>%
  mutate(dia = lubridate::floor_date(time_hour, "day")) %>%
  #select(time_hour, dia)
  group_by(dia) %>% 
  summarise(
    atraso_medio = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    n = n(),#sum(!is.na(arr_delay)),
    prop_atrasos = sum(arr_delay > 0, na.rm = TRUE)/n(),
  ) %>% 
  ggplot() +
  geom_point(aes(x = n, y = prop_atrasos))

# exercicio 4
# Também usando a estrutura do exemplo 4 coloque uma cor para cada uma
# das origens.

flights %>%
  mutate(dia = lubridate::floor_date(time_hour, "day")) %>%
  group_by(dia, origin) %>% 
  summarise(
    atraso_medio = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    n = n(),#sum(!is.na(arr_delay)),
    prop_atrasos = sum(arr_delay > 0, na.rm = TRUE)/n(),
  ) %>% 
  ggplot() +
  geom_point(aes(x = n, y = prop_atrasos, color = origin))

# Customização ------------------------------------------------------------

# exemplo 1

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay, color = origin)) + 
  labs(x = "Atraso na saída", y = "Atraso na chegada", color = "Origem") +
  theme(axis.title.x = element_text(size = 24, colour = "red"))

# exemplo 2

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay, color = origin)) +
  #scale_color_brewer()
  scale_color_manual(values = c("red", "blue", "green"))
  #scale_color_viridis_d()
  # scale_color_manual(
  #   values = wesanderson::wes_palette(
  #     "Zissou1", type = "discrete"
  #     )
  #   )


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

install.packages("gghighlight")

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay), color = "red") +
  gghighlight::gghighlight(
    dep_delay == max(dep_delay, na.rm = TRUE)
    )

# exemplo 6

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay))

# exemplo 7

flights %>% 
  ggplot() +
  geom_point(aes(x = dep_delay, y = arr_delay)) +
  scale_x_continuous(breaks = seq(0, 1200, by = 100))
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

rgb(red = 0, blue = 0, green = 1)

flights %>% 
  ggplot() + 
  geom_point(aes(x = dep_delay, y = air_time, color = origin), size = 0.1) +
  scale_color_manual(values = c("darkorange", "darkblue", "darkred")) + 
  labs(
    x = "Atraso na saída (em minutos)", 
    y = "Tempo de Vôo  (em minutos)", 
    color = "Origem"
  ) +
  theme_minimal()
  
# exercício 2
# Crie um gráfico de dispersão do número de vôos pelo atraso médio (por dia-hora).
# Customize o gráfico.

flights %>% 
  group_by(time_hour) %>% 
  summarise(
    numero_de_voos = n(),
    atraso_medio = mean(arr_delay[arr_delay > 0], na.rm = TRUE)
  ) %>% 
  ggplot() +
  geom_rect(xmin = 2, xmax = 4, ymin = 200, ymax = 300, fill = "yellow") + 
  geom_jitter(aes(x = numero_de_voos, y = atraso_medio), size = 0.1) +
  labs(x = "Número de Vôos", y = "Atraso médio") +
  theme_minimal() + 
  theme(
    panel.grid = element_blank()    
    # panel.grid.major = element_blank(), 
    # panel.grid.minor.x = element_blank()
  ) +
  geom_text(x = 2, y = 400, label = "Oi", size = 10)

# Outros geoms ------------------------------------------------------------

# exemplo 1

flights %>% 
  group_by(
    dia = lubridate::floor_date(time_hour, "week"),
    origin
    ) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_rect(
    xmin = as.POSIXct("2013-07-01"),
    xmax = as.POSIXct("2013-07-30"),
    ymin = -Inf,
    ymax = Inf,
    fill = "yellow",
    alpha = 0.01
  ) +
  geom_line(aes(x = dia, y = n, color = origin, linetype= origin)) +
  geom_vline(
    xintercept = as.POSIXct("2013-07-01"), 
    color = "red"
  ) +
  geom_text(x = as.POSIXct("2013-07-01"), y = 1000, label = "Campanha A")

# exemplo 2

flights %>% 
  group_by(origin) %>% 
  summarise(n = n()) %>% 
  # group_by(month) %>% 
  # mutate(n = n/sum(n)) %>% 
  ggplot() +
  geom_col(aes(x = "", y = n, fill = origin), position = "fill") +
  coord_polar("y", start = 0)

# exemplo 3 

flights %>% 
  ggplot() +
  geom_boxplot(
    aes(x = fct_reorder(carrier, arr_delay), y = arr_delay), 
    outlier.shape = NA
    ) +
  ylim(NA, 100)

# exemplo 4

flights %>% 
  group_by(
    dia = lubridate::floor_date(time_hour, "week"),
    origin
    ) %>% 
  summarise(n = n()) %>% 
  ggplot() + 
  geom_area(aes(x = dia, y = n, fill = origin, color = origin), alpha = 0.5)

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
  #filter(dest != "Other") %>% 
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
  sample_n(10000) %>% 
  ggplot() +
  geom_point(aes(x = distance, y = arr_delay, color = origin)) +
  facet_wrap(~carrier)

# exemplo 2
flights %>% 
  group_by(
    dia = lubridate::floor_date(time_hour, "month"), 
    dest = fct_lump(dest, 9)
    ) %>% 
  summarise(n = n()) %>% 
  filter(dest != "Other") %>% 
  ggplot() +
  geom_line(aes(x = dia, y = n)) +
  facet_wrap(~dest, ncol = 3)
    
# exemplo 3
flights %>% 
  mutate(
    dest = fct_lump(dest, 5), 
    carrier = fct_lump(carrier, 5)
    ) %>% 
  filter(dest != "Other") %>% 
  ggplot() +
  geom_boxplot(aes( x = as.factor(month), y = arr_delay), outlier.shape = NA) +
  # facet_wrap(~dest + carrier)
  facet_grid(carrier ~ dest, scales = "free") +
  ylim(NA, 200)

# Miscelânea --------------------------------------------------------------

# Mapas

# referência
# https://sjfox.github.io/post/world_map_flights/

flights_com_lat_lon <- flights %>%
  select(origin, dest) %>% 
  left_join(airports %>% select(faa, lat, lon) , by = c("origin" = "faa")) %>%
  rename(lat_origin = lat, lon_origin = lon) %>%
  left_join(airports %>% select(faa, lat, lon), by = c("dest" = "faa")) %>%
  rename(lat_dest = lat, lon_dest = lon) %>%
  mutate(ordered_pair = if_else(origin > dest, paste0(dest, origin), paste0(origin, dest))) %>%
  tidyr::drop_na() %>% 
  group_by_all() %>% 
  count()

mapa <- map_data("world") #poligonos do mapa

base_do_mapa <- mapa %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "black", color = "white", size=0.15) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"), legend.position = "none") + 
  coord_equal(
    xlim = range(flights_com_lat_lon$lon_dest), 
    ylim = range(flights_com_lat_lon$lat_dest)
    ) 

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
    aes(x = lon_origin, xend = lon_dest, y = lat_origin,  yend = lat_dest, 
        alpha = n, color = n), 
    # color = "purple", 
    inherit.aes = FALSE
  ) +
  scale_color_gradient(low = "red", high = "green")


leaflet::leaflet(flights_com_lat_lon) %>% 
  leaflet::addTiles() %>% 
  leaflet::addCircleMarkers(
    ~lon_dest, ~lat_dest, 
    clusterOptions = leaflet::markerClusterOptions()
  )


# plotly

library(plotly)
library(tidyverse)
library(nycflights13)

p <- flights %>% 
  sample_n(1000) %>% 
  ggplot(aes(x = dep_delay, y = arr_delay, label = tailnum))+
  geom_point()

ggplotly(p)



