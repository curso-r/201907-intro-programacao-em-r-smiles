# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(nycflights13)

# Base de dados -----------------------------------------------------------

data(flights)
data(airlines)
data(airports)
data(planes)
data(weather)

# filter ------------------------------------------------------------------

# exemplo 1
flights %>% filter(month == 1, day == 1)

# exemplo 2
jan1 <- flights %>% filter(month == 1, day == 1)
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
# Criar uma variável chamada `jan_mar` com filmes com os voos de janeiro a março.


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


# exercício 3
# crie um objeto chamado voos_aniversario com os vôos que acontteceram no dia
# do seu aniversario e no dia do aniversario um colega perto de você.

# exercício 4
# crie um objeto que contenha os vôos que não ocorreram nem em janeiro nem em dezembro.

# exercício 5
# crie um objeto com os vôos das companhias UA e AA

# exemplo 6
# %in%

california <- flights %>% filter(dest %in% c('SAN', "SFO", "SJC"))

# exercicio 6
# Refaça o exercício 5 usando o %in%.

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
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

flights %>% filter(!is.na(dep_delay))

# exercício 7
# Tente descobrir porque a informação de atraso pode ser NA.

# exemplo 8
# str_detect

airports %>% filter(str_detect(tzone, "Los_Angeles"))

# exercício 8
# Encontre todas as companhias aéreas que possuem a palavra Airlines no nome.
# Use a tabela airlines

# arrange -----------------------------------------------------------------

# exemplo 1

flights %>% arrange(arr_delay)

# exemplo 2

flights %>% arrange(desc(arr_delay))

# exemplo 3

flights %>% arrange(desc(month), desc(day))

# exercício 1
# Ordene os vôos por ordem descrescente de atraso.

# exemplo 4
# NA

df <- tibble(x = c(NA, 2, 1), y = c(1, 2, 3))

df %>% arrange(x)

# exemplo 5

flights %>% filter(month == 1) %>% arrange(desc(arr_delay))

# exercício 2 
# ordene por ordem decrescente de atraso os vôos de um dia do ano à sua escolha

# select ------------------------------------------------------------------

# exemplo 1

flights %>% select(year, month, day)

# exemplo 2 

flights %>% select(year, month, starts_with("arr"))

# exemplo 3

flights %>% select(-starts_with("arr"), -year)

# exercício 1
# crie uma tabela com as colunas year, month, day e air_time

# exercício 2
# remova as colunas arr_delay, dep_delay, arr_time e dep_time de 3 formas 
# diferentes.

# exercício 3
# crie uma tabela com os vôos de dezembro, ordenada em ordem decrescente de 
# atraso e apenas com as colunas carrier, flight e arr_delay.


# mutate ------------------------------------------------------------------

# exemplo 1

flights %>% mutate(ganho = dep_delay - arr_delay)

# exemplo 2

flights %>% mutate(velocidade = distance/air_time)

# exercício 1
# calcule o tempo do voo em horas

# exercicio 2
# crie uma coluna com a concatenacao da origem com o destino
# dica: função paste.

# exercício 3
# 1) encontre os vôos de um mês a sua escolha que possuem 
# 2) calcule a velocidade média desses vôos
# 3) ordene por ordem decrescente da velocidade média
# 4) apresente apenas as colunas origem, destino e velocidade

# exemplo 3
# datas

library(lubridate)

flights %>% 
  select(month, day, flight, time_hour) %>% 
  mutate(dia_da_semana = wday(time_hour, label = TRUE)) %>% 
  filter(dia_da_semana == "Mon")

# summarise ---------------------------------------------------------------

# exemplo 1

flights %>% summarise(media_atraso = mean(arr_delay, na.rm = TRUE))

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
  group_by(month) %>% 
  summarise(qtd_voos = n())

# exemplo 3

flights %>% 
  group_by(month, day) %>% 
  summarise(qtd_voos = n())

# exercício 1
# Crie uma tabela com a quantidade de voos por dia da semana

# exercício 2
# Crie uma tabela com o atraso médio e mediano por companhia aérea

# exercício 3
# Crie uma tabela com o atraso médio e mediano por hora do vôo

# exemplo 4

flights %>% 
  group_by(dest) %>% 
  summarise(atraso_medio = mean(arr_delay, na.rm = TRUE)) %>% 
  filter(atraso_medio > 30)

# exemplo 5

flights %>% 
  mutate(dia_da_semana = wday(time_hour, label = TRUE)) %>% 
  filter(dia_da_semana == "Mon") %>% 
  group_by(dest) %>% 
  summarise(atraso_medio = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(desc(atraso_medio))

# left join ---------------------------------------------------------------

# exemplo 1

flights2 <-  flights %>% 
  left_join(airlines, by = c("carrier" = "carrier"))

# exemplo 2

airports %>% 
  filter(str_detect(tzone, "Los_Angeles")) %>% 
  select(faa) %>% 
  inner_join(flights, by = c("faa" = "dest"))

# exemplo 3

weather %>% 
  group_by(month, day) %>% 
  summarise(
    temp = mean(temp, na.rm = TRUE),
    wind_speed = mean(wind_speed, na.rm = TRUE)
    ) %>% 
  right_join(flights, by = c("month", "day"))


# exercicio 1
# descubra as companhias que fazem mais vôos com aviões da EMBRAER

# gather ------------------------------------------------------------------

# exemplo 1

flights %>% 
  group_by(origin, dest) %>% 
  summarise(n = n()) %>% 
  spread(origin, n, fill = 0)

# spread ------------------------------------------------------------------

# exemplo 1

weather %>% 
  select(year, month, day, wind_dir, wind_speed, wind_gust) %>% 
  gather(wind_attr, value, starts_with("wind"))

