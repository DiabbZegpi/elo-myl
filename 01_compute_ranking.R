library(googlesheets4)
library(tidyverse)

# Only one time to set the cache directory
# library(gargle)
# options(gargle_oauth_cache = ".gargle")

gs4_auth(
  email = "diabbluis@gmail.com",
  cache = ".gargle"
)
# Importar data -----------------------------------------------------------

data_path <- "https://docs.google.com/spreadsheets/d/1dj8J1QKNHAEHMJP-TwFtFk-Gh0JXQ7Bayam6eXv2ai8/edit?gid=0#gid=0"
rating_list_path <- "https://docs.google.com/spreadsheets/d/1Wh3PTzHAhoY7VKp-WfJgEF7IjWMYSdecgzIlwFnDA3E/edit?gid=0#gid=0"

resultados <- read_sheet(data_path, "resultados", col_types = "ccDiccii")
torneos <- read_sheet(data_path, "torneos", col_types = "cccDcc")
jugadores <- read_sheet(data_path, "jugadores", col_types = "ccc")
ranking <- read_sheet(rating_list_path, "ranking", col_types = "icd")

# Parámetros --------------------------------------------------------------

elo_inicial <- 1400
k_coef <- 40
version_actual <- today() |> format("%Y%m%d") |> as.integer()
ultima_version <- ranking$version |> max()

# Filtros -----------------------------------------------------------------

torneos_por_computar <-
  torneos |>
  filter(fecha > ymd(ultima_version)) |>
  distinct(id) |>
  pull(id)

torneos_computados <-
  torneos |>
  filter(fecha <= ymd(ultima_version)) |>
  distinct(id) |>
  pull(id)

ultimo_ranking <-
  ranking |>
  filter(version == ultima_version) |>
  select(jugador, elo)

jugadores_por_rankear <-
  torneos |>
  filter(id %in% torneos_por_computar) |>
  distinct(jugador) |>
  left_join(ultimo_ranking, join_by(jugador)) |>
  mutate(elo = replace_na(elo, elo_inicial))

rondas_por_computar <-
  resultados |>
  filter(torneo %in% torneos_por_computar) |>
  select(jugador1, jugador2, puntos1, puntos2)

rondas_computadas <-
  resultados |>
  filter(torneo %in% torneos_computados)


# Cálculo de Elo ----------------------------------------------------------

resultado_global <- tibble(
  version = integer(),
  jugador = character(),
  elo = numeric()
)

for (jugador in jugadores_por_rankear$jugador) {

  puntos_jugador <-
    rondas_por_computar |>
    filter(jugador1 == jugador | jugador2 == jugador) |>
    transmute(
      oponente = if_else(jugador1 == jugador, jugador2, jugador1),
      puntos_a_favor = if_else(jugador1 == jugador, puntos1, puntos2),
      puntos_en_contra = if_else(jugador1 != jugador, puntos1, puntos2),
      total = puntos_a_favor + puntos_en_contra
    ) |>
    filter(oponente != "bye")

  elo_propio <-
    jugadores_por_rankear |>
    filter(jugador == !!jugador) |>
    pull(elo)

  resultado_jugador <-
    puntos_jugador |>
    left_join(jugadores_por_rankear, by = join_by(oponente == jugador)) |>
    mutate(
      elo_jugador = !!elo_propio,
      puntaje_esperado = 1 / (1 + 10 ^ ((elo - elo_propio) / 400)),
      puntaje_total_esperado = puntaje_esperado * total,
      variacion = puntos_a_favor - puntaje_total_esperado
    ) |>
    summarize(variacion_total = sum(variacion) * !!k_coef) |>
    transmute(
      version = !!version_actual,
      jugador = !!jugador,
      elo = !!elo_propio + variacion_total
    )

  resultado_global <- bind_rows(resultado_global, resultado_jugador)

}

nuevo_ranking <-
  ranking |>
  bind_rows(resultado_global) |>
  slice_max(version, by = jugador) |>
  mutate(version = !!version_actual) |>
  bind_rows(ranking)


# Exportar ranking --------------------------------------------------------

write_sheet(nuevo_ranking, rating_list_path, "ranking")
