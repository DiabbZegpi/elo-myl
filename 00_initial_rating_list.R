# Initial list
# DO NOT RUN!
library(googlesheets4)
library(tidyverse)

data_path <- "https://docs.google.com/spreadsheets/d/1dj8J1QKNHAEHMJP-TwFtFk-Gh0JXQ7Bayam6eXv2ai8/edit?gid=0#gid=0"
rating_list_path <- "https://docs.google.com/spreadsheets/d/1Wh3PTzHAhoY7VKp-WfJgEF7IjWMYSdecgzIlwFnDA3E/edit?gid=0#gid=0"
jugadores <- read_sheet(data_path, "jugadores")
elo_inicial <- 1400
version_inicial <- "20240801"

lista_inicial <-
  jugadores |>
  filter(tor != "bye") |>
  distinct(tor) |>
  mutate(
    elo = elo_inicial,
    version = version_inicial
  ) |>
  select(
    version,
    jugador = tor,
    elo
  )

write_sheet(lista_inicial, rating_list_path, "ranking")
