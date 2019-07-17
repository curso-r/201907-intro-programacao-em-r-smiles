library(tidyverse)
library(nycflights13)

amostra <- sample_n(flights, 2000)

write_csv(amostra, "dados/flights.csv")
write_csv2(amostra, "dados/flights2.csv")
write_delim(amostra, delim = "|", path = "dados/flights.txt")
writexl::write_xlsx(amostra, "dados/flights.xlsx")
haven::write_sav(amostra, "dados/flights.sav")
haven::write_sas(amostra, "dados/flights.sasb7dat")
jsonlite::write_json(amostra, "dados/flights.json")
saveRDS(amostra, "dados/flights.rds")


con <- src_sqlite("dados/flights.sqlite", create = TRUE)
copy_to(con, flights, temporary = FALSE)
copy_to(con, airlines, temporary = FALSE)
copy_to(con, airports, temporary = FALSE)
copy_to(con, planes, temporary = FALSE)
copy_to(con, weather, temporary = FALSE)



