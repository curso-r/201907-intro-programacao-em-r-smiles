
library(tidyverse)

# Caminhos até o arquivo --------------------------------------------------

# 1. Podem ser absolutos
"~/Documents/"

# 2. Podem ser relativos ao diretório de trabalho
getwd()

# Principais formatos -----------------------------------------------------

# Arquivos de texto
library(readr)
flights <- read_csv(file = "dados/flights.csv")
flights2 <- read_delim("dados/flights2.csv", delim = ";")

# Excel
library(readxl)
flights_excel <- read_excel("dados/flights.xlsx")

# SQL ---------------------------------------------------------------------

conexao <- src_sqlite("dados/flights.sqlite")
conexao

flights_sqlite <- tbl(conexao, "flights")

x <- flights_sqlite %>% 
  filter(month == 1, day == 31)
show_query(x)

flights_select <- tbl(conexao, sql("SELECT year, month, day, dep_time FROM flights"))

# trazer para a memória
collect(flights_sqlite)
collect(flights_select)

# db.rstudio.com

# Outros formatos ---------------------------------------------------------

library(jsonlite)
flights_json <- read_json("dados/flights.json")

library(haven)

flights_sas <- read_sas("dados/flights.sasb7dat")
flights_spss <- read_spss("dados/flights.sav")

# data.table
library(data.table)
flights_dt <- fread("dados/flights.csv")

# vroom
library(vroom)
flights_vroom <- vroom("dados/flights.csv")


