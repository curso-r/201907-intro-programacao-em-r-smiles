# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(nycflights13)

# Base de dados -----------------------------------------------------------

data(flights)
data(airlines)
data(airports)
data(planes)
data(weather)

tail(flights)
View(flights)

flights %>% 
  filter(day == 1) %>% 
  view()

# filter ------------------------------------------------------------------

# exemplo 1
flights %>% filter(month == 1, day == 1)

# exemplo 2
jan1 <- flights %>% 
  filter(month == 1, day == 1)
jan1

# exemplo 3
jan1 <- jan1 %>% filter(origin == "JFK")
jan1

# exemplo 4 - Relembrando as comparações com o R

1 == 1
"a" == "b"

# Cuidado
sqrt(2) ^ 2 == 2
1 / 49 * 49 == 1

near(sqrt(2) ^ 2,  2)
near(1 / 49 * 49, 1)

# exercício 1
# Criar um objeto chamada `jan_mar` com os voos de janeiro a março.

jan_mar <- flights %>% filter(month <= 3)
jan_mar <- flights %>% filter(month == 1 | month == 2 | month == 3)
jan_mar <- flights %>% filter(month %in% 1:3)
jan_mar <- flights %>% filter(month %in% c(1, 2, 3))
jan_mar <- flights %>% filter(!(month != 1 & month != 2 & month != 3))

`%notin%` <- function(x, y) {
  !(x %in% y)
}

meses_que_eu_quero <- c(1,2,3)
jan_mar <- flights %>% filter(month %notin% meses_que_eu_quero)

# exemplo 5
# operadores lógicos

flights %>% filter(month == 1 & day == 1)
flights %>% filter(month == 1 & day <= 15)

flights %>% filter(month == 1 | month == 3)
flights %>% filter(arr_delay >= 180 & dep_delay <= 10)

flights %>% filter(!carrier == "UA")

# exercício 2
# crie um objeto chamado jan_atrasados com os vôos que atrasaram mais de 1h30
# em janeiro. 

jan_atrasado <- flights %>% filter(arr_delay > 90, month == 1)
jan_atrasado <- flights %>% filter(arr_delay > 90 & month == 1)

jan_atrasado %>% count(month)
summary(jan_atrasado$arr_delay)

# exercício 3
# crie um objeto chamado voos_aniversario com os vôos que acontteceram no dia
# do seu aniversario e no dia do aniversario um colega perto de você.

voos_aniversario <- flights %>% 
  filter((month == 1 & day == 18) | (month == 11 & day == 27))

flights %>% 
  filter(month ==1, day %in% c(18, 27))

# exercício 4
# crie um objeto que contenha os vôos que não ocorreram nem em janeiro nem em dezembro.

ex4 <- flights %>% filter(month != 1 & month != 12)
ex4 <- flights %>% filter(month %in% 2:11)
ex4 <- flights %>% filter(!month %in% c(1,12))

# exercício 5
# crie um objeto com os vôos das companhias UA e AA

ex5 <- flights %>% filter(carrier %in% c("UA", "AA"))
ex5 <- flights %>% filter(carrier == "UA" | carrier == "AA")

# exemplo 6
# %in%

california <- flights %>% filter(dest %in% c('SAN', "SFO", "SJC"))

# exercicio 6
# Refaça o exercício 5 usando o %in%.

ex5 <- flights %>% filter(carrier %in% c("UA", "AA"))

# exemplo 7
# Relembrando as operações com NA

NA > 5

10 == NA

NA + 10

NA / 2

NA == NA

# Seja x a idade de Maria. Não sabemos a idade de Maria:
x <- NA

# Seja y a idade de João. Não sabemos a idade de João:
y <- NA

# Maria e João têm a mesma idade?
x == y
#> [1] NA
# Não sabemos.

is.na(x)

df <- tibble(x = c(1, NA, 3))
df %>% filter(x > 1)
filter(df, is.na(x) | x > 1)

flights %>% filter(!is.na(dep_delay))

# exercício 7
# Tente descobrir porque a informação de atraso pode ser NA

flights %>% filter(is.na(arr_delay)) %>% nrow()
flights %>% filter(is.na(air_time)) %>% nrow()

# exemplo 8
# str_detect

LA <- airports %>% filter(str_detect(tzone, "Los_Angeles"))

flights %>% 
  filter(dest %in% LA$faa)

# exercício 8
# Encontre todas as companhias aéreas que possuem a palavra Airlines no nome.
# Use a tabela airlines

airlines %>% 
  filter(str_detect(name, "Airlines"))

# arrange -----------------------------------------------------------------

# exemplo 1

flights %>% arrange(arr_delay)

# exemplo 2

flights %>% arrange(desc(arr_delay))
flights %>% arrange(-arr_delay)

# exemplo 3

flights %>% arrange_at(vars(month, day), desc)

# exercício 1
# Ordene os vôos por ordem descrescente de atraso.

flights %>% 
  arrange(desc(arr_delay))

# exemplo 4
# NA

df <- tibble(x = c(NA, 2, 1), y = c(1, 2, 3))

df %>% arrange(desc(x))

# exemplo 5

x <- flights %>% 
  filter(month == 1) %>% 
  arrange(desc(arr_delay))

# exercício 2 
# ordene por ordem decrescente de atraso os vôos de um dia do ano à sua escolha

x <- flights %>% 
  filter(day == 18, month == 1) %>% 
  arrange(desc(arr_delay))

x %>% count(month, day)
flights %>% count(month, day) %>% View()
flights %>% distinct(month, day, .keep_all = TRUE)
flights %>% count(month, day, sort = T)

"
select * from flights where day = 1 and month = 1 order by arr_delay desc
"

# select ------------------------------------------------------------------

# exemplo 1

flights %>% select(year, month, day)

# exemplo 2 

flights %>% select(year, month, starts_with("arr"))
flights %>% select(year, month, contains("dep"))
x <- flights %>% select(dep_delay, everything())
flights %>% select_if(is.character)
flights %>% select_if(is.numeric)
flights %>% select_if(is.POSIXt)

is_numeric_or_char <- function(x) {
  is.numeric(x) | is.character(x)
}

function(.x) {
  length(unique(.x)) == 1
}

flights %>% select_if(~length(unique(.x)) == 1)



# exemplo 3

flights %>% select(-starts_with("arr"), -year)

# exercício 1
# crie uma tabela com as colunas year, month, day e air_time

flights %>% 
  select(year, month, day, air_time)

# exercício 2
# remova as colunas arr_delay, dep_delay, arr_time e dep_time de 3 formas 
# diferentes.

flights %>% 
  select(-arr_delay, -dep_delay, -arr_time, -dep_time)

flights %>% 
  select(-c(arr_delay, dep_delay, arr_time, dep_time))

flights %>% 
  select(-ends_with("_delay"), -ends_with("_time"), sched_dep_time, sched_arr_time, air_time)

flights %>% 
  select(-starts_with("arr_"), -starts_with("dep_"))

flights %>% 
  select(-matches("[^s]_delay"))


# exercício 3
# crie uma tabela com os vôos de dezembro, ordenada em ordem decrescente de 
# atraso e apenas com as colunas carrier, flight e arr_delay.

flights %>% 
  filter(month == 12) %>% 
  arrange(desc(dep_delay)) %>% 
  select(carrier, flight, dep_delay)
  
# mutate ------------------------------------------------------------------

# exemplo 1

library(lubridate)

flights %>% mutate(
  ganho = dep_delay - arr_delay,
  ganho_em_horas = ganho/60,
  ganho_acumulado = cumsum(ganho),
  lag_ganho = lag(ganho, 1, order_by = time_hour),
  media_movel = RcppRoll::roll_mean(ganho, n = 2, fill = 0),
  ganho_cat = case_when(
    ganho >= 0 ~ "recuperou",
    ganho < 0 ~ "atrasou mais ainda",
    TRUE ~ "outro"
  ),
  dia_da_semana = wday(time_hour, label = TRUE),
  safra = sprintf("%04d%02d", year(time_hour), month(time_hour)),
  ano_mes = floor_date(time_hour, "month"),
  n_linha = min_rank(dep_delay)
) %>% 
  View()

# exemplo 2

flights %>% mutate(velocidade = distance/air_time)

# exercício 1
# calcule o tempo do voo em horas

flights %>% 
  mutate(
    tempo_de_voo = round(air_time/60)
  ) %>% 
  View()

# exercicio 2
# crie uma coluna com a concatenacao da origem com o destino
# dica: função paste.

flights %>% 
  mutate(origem_destino = paste0(origin, dest)) %>% 
  select(origem_destino)

flights %>% 
  mutate(
    origem_destino = case_when(
      origin <= dest ~ paste0(origin, dest),
      origin > dest ~ paste0(dest, origin),
      TRUE ~ NA_character_
    )
  ) %>% 
  select(origem_destino, everything())

# exercício 3
# 1) encontre os vôos de um mês a sua escolha
# 2) calcule a velocidade média desses vôos
# 3) ordene por ordem decrescente da velocidade média
# 4) apresente apenas as colunas origem, destino e velocidade

flights %>% 
  filter(month == 7) %>% 
  mutate(velocidade = distance/(air_time/60)*1.6) %>% 
  arrange(desc(velocidade)) %>% 
  select(origin, dest, velocidade)

# exemplo 3
# datas

library(lubridate)
lubridate::today()
x <- lubridate::now()

floor_date(today() - months(18), "month") - days(1)


x <- flights %>% 
  #select(month, day, flight, time_hour) %>% 
  mutate(
    dia_da_semana = wday(time_hour, label = TRUE),
    data_mais_30_dias = time_hour + days(30),
    data_mais_3_meses = time_hour + month(3),
    tempo = difftime(now(), time_hour, units = "weeks"),
    arr_delay = ifelse(
      is.na(arr_delay), 
      yes = mean(arr_delay, na.rm = TRUE), 
      no = arr_delay
      )
    ) %>% 
  filter(dia_da_semana == "Mon")

summary(x$arr_delay)
summary(flights$arr_delay)



# summarise ---------------------------------------------------------------

# exemplo 1

"SELECT mean(arr_delay) FROM flights"
#lights %>% mutate(x = mean(arr_delay, na.rm = TRUE)) %>% select(x)

flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>% 
  summarise(
    media_atraso = mean(arr_delay, na.rm = TRUE),
    media_atraso2 = mean(dep_delay)
    )
flights %>% summarise(
  media_atraso = mean(ifelse(is.na(1000*arr_delay), 0,  1000*arr_delay))
  )

# exemplo 2

flights %>% summarise(
  media_atraso = mean(arr_delay, na.rm = TRUE),
  dp_atraso = sd(arr_delay, na.rm = TRUE)
)

# exemplo 3

flights %>% summarise(
  media_atraso = mean(arr_delay, na.rm = TRUE),
  dp_atraso = sd(arr_delay, na.rm = TRUE),
  qtd = n(),
  qtd_companhias = n_distinct(carrier),
  qtd_destinos = n_distinct(dest)
)

# exemplo 4

flights %>%
  summarise(n_UA = sum(carrier == "UA", na.rm = TRUE))

# exercício 1
# Use o `summarise` para calcular a proporção de vôos que foram feitos pela AA.

# exercício 2
# Calcule a duração média e mediana dos vôos.

# exercício 3
# calcule a média, mediana e desvio padrão da velocidade dos vôos

# group_by + summarise ----------------------------------------------------

# exemplo 1

flights %>% group_by(month)

# exemplo 2

flights %>% 
  group_by(month, day) %>% 
  summarise(qtd_voos = n())

# exemplo 3

flights %>% 
  group_by(month, day) %>% 
  summarise(
    qtd_voos = n(),
    qtd_comp = n_distinct(carrier)
    )

# exercício 1
# Crie uma tabela com a quantidade de voos por dia da semana
# dica: funcao wday

flights %>% 
  #mutate(dia_da_semana = wday(time_hour, label = TRUE)) %>% 
  group_by(dia_da_semana = wday(time_hour, label = TRUE, abbr = FALSE)) %>% 
  summarise(n = n())

# exercício 2
# Crie uma tabela com o atraso médio e mediano por companhia aérea

flights %>% 
  filter(arr_delay > 10) %>% 
  group_by(carrier) %>% 
  summarise(
    media = mean(arr_delay, na.rm = TRUE),
    mediana = median(arr_delay, na.rm = TRUE)
  ) %>% 
  arrange(desc(media))


# exercício 3
# Crie uma tabela com o atraso médio e mediano por hora do vôo

flights %>% 
  filter(arr_delay > 0) %>% 
  group_by(hour) %>% 
  summarise(
    media = mean(arr_delay, na.rm = TRUE),
    mediana = median(arr_delay, na.rm = TRUE)
  ) %>% 
  arrange(desc(media))

# exemplo 4

flights %>% 
  group_by(dest) %>% 
  summarise(atraso_medio = mean(arr_delay, na.rm = TRUE)) %>% 
  filter(atraso_medio > 30)

# exemplo 5

flights %>% 
  mutate(dia_da_semana = wday(time_hour)) %>% 
  filter(dia_da_semana == 2) %>% 
  group_by(dest) %>% 
  summarise(atraso_medio = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(desc(atraso_medio))

flights %>% 
  group_by(month) %>% 
  filter(row_number(desc(dep_delay)) %in% 1:10)

flights %>% 
  group_by(month) %>% 
  mutate(ordem = row_number(desc(dep_delay))) %>% 
  View()

flights %>% 
  group_by(month, day) %>% 
  filter(row_number(desc(time_hour)) == 1)

flights %>% 
  filter(time_hour > today() - days(30)) %>% 
  group_by(month, day, dest) %>% 
  filter(row_number(desc(time_hour)) == 1) %>% 
  ungroup()

flights %>% 
  group_by(dest) %>% 
  mutate(air_time = air_time/mean(air_time))

# left join ---------------------------------------------------------------

# exemplo 1

flights2 <-  flights %>% 
  left_join(
    airlines, 
    by = c("carrier" = "carrier")
    )

# exemplo 2

airports %>% 
  filter(str_detect(tzone, "Los_Angeles")) %>% 
  select(faa, name) %>% 
  inner_join(flights, by = c("faa" = "dest"))

# exemplo 3

weather %>% 
  group_by(month, day) %>% 
  summarise(
    temp = mean(temp, na.rm = TRUE),
    wind_speed = mean(wind_speed, na.rm = TRUE)
    ) %>% 
  right_join(flights, by = c("month", "day"))


"select * from flights left join airports on a.date <= b.date"

# exercicio 1
# descubra as companhias que fazem mais vôos com aviões da EMBRAER

# gather ------------------------------------------------------------------

# exemplo 1

flights %>% 
  group_by(origin, dest, hour) %>% 
  summarise(n = n()) %>% 
  spread(origin, n, fill = 0)

# spread ------------------------------------------------------------------

# exemplo 1

weather %>% 
  select(year, month, day, wind_dir, wind_speed, wind_gust) %>% 
  gather(wind_attr, value, starts_with("wind"))

