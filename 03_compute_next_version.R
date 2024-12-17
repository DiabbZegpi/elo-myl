library(tidyverse)
library(here)
library(googlesheets4)

gs4_auth(email = "diabbluis@gmail.com", cache = ".gargle")
data_path <- "https://docs.google.com/spreadsheets/d/1ZErNDZVSbIP8ekj-mmTpdmJ_Gbpo8yQaN5HeLBlqblE/edit?gid=1605468956#gid=1605468956"

ranking_db <- read_sheet(data_path, "ranking", col_types = "Dcd")
resultados_db <- read_sheet(data_path, "resultados", col_types = "ciccii")
torneos_db <- read_sheet(data_path, "torneos", col_types = "ccDDcccccc")
jugadores_db <- read_sheet(data_path, "jugadores", col_types = "ccc")
torneos_computados_db <- read_sheet(data_path, "torneos computados", col_types = "cD")

df_raw <- read_csv(here("data", "PB_resultados_24_12_17.csv"))

df_filtered <-
  df_raw |>
  mutate(
    fecha = ymd(str_extract(id_torneo, "(\\d+)", 1L)),
    semana = as_date(floor_date(fecha, "weeks", week_start = 1))
  ) |>
  drop_na(tor_a, tor_b, resultado_a, resultado_b) |>
  filter(
    estado == "Reportado",
    juego != "MyL Primer Bloque - Lanzamiento",
    formato %in% c("Racial Edición", "Racial EdiciónModalidad")
  ) |>
  anti_join(torneos_db, by = join_by(id_torneo))

elo_inicial <- 1400
criterio_min_jugador_antiguo <- 40
k_coef_jugador_nuevo <- 40
k_coef_jugador_antiguo <- 20
elo_dispersion <- 1135.77
version_actual <- today()


# Tabla `jugadores` -------------------------------------------------------

jugadores <-
  df_filtered |>
  select(starts_with("jugador"), starts_with("tor")) |>
  pivot_longer(jugador_a:jugador_b, names_to = "player_col", values_to = "nombre") |>
  pivot_longer(tor_a:tor_b, names_to = "tor_col", values_to = "tor") |>
  filter(str_sub(player_col, start = -1L) == str_sub(tor_col, start = -1L)) |>
  select(tor, nombre) |>
  distinct()


# Tabla `torneos` ---------------------------------------------------------

torneos <-
  df_filtered |>
  pivot_longer(c(tor_a, tor_b), names_to = "tor_col", values_to = "tor") |>
  pivot_longer(c(raza_a, raza_b), names_to = "raza_col", values_to = "raza") |>
  filter(str_sub(tor_col, start = -1L) == str_sub(raza_col, start = -1L)) |>
  select(
    id_torneo,
    nombre_torneo,
    fecha,
    semana,
    tienda,
    estado,
    juego,
    formato,
    tor,
    raza
  ) |>
  distinct(id_torneo, tor, .keep_all = TRUE) |>
  arrange(fecha)


# Tabla `rondas` ----------------------------------------------------------

resultados <-
  df_filtered |>
  select(
    id_torneo,
    ronda,
    tor_a,
    tor_b,
    resultado_a,
    resultado_b
  )


# Tabla `ranking` ---------------------------------------------------------

ultimo_ranking <- ranking_db |> slice_max(version, by = tor)

ranking <-
  jugadores |>
  left_join(
    ultimo_ranking,
    by = join_by(tor),
    relationship = "one-to-one"
  ) |>
  mutate(
    elo = replace_na(elo, elo_inicial)
  ) |>
  select(
    version,
    tor,
    elo
  )


# Tabla `antiguedad_jugadores` --------------------------------------------

antiguedad_jugadores <-
  resultados_db |>
  pivot_longer(cols = c(tor_a, tor_b), values_to = "tor") |>
  mutate(
    partidas_jugadas = resultado_a + resultado_b,
    partidas_jugadas = if_else(partidas_jugadas == 0, 1, partidas_jugadas)
  ) |>
  count(tor, wt = partidas_jugadas, name = "partidas_jugadas") |>
  right_join(jugadores, by = join_by(tor)) |>
  mutate(jugador_nuevo = case_when(
    is.na(partidas_jugadas) ~ TRUE,
    partidas_jugadas < !!criterio_min_jugador_antiguo ~ TRUE,
    .default = FALSE
  )) |>
  select(
    tor,
    jugador_nuevo
  )


# Cálculo de elo ----------------------------------------------------------

computo_jugadores <- tibble(
  version = ymd(),
  tor = character(),
  elo = numeric()
)

for (jugador in jugadores$tor) {

  puntos_jugador <-
    resultados |>
    filter(
      tor_a == jugador | tor_b == jugador
    ) |>
    transmute(
      oponente = if_else(tor_a == jugador, tor_b, tor_a),
      puntos_a_favor = if_else(tor_a == jugador, resultado_a, resultado_b),
      puntos_en_contra = if_else(tor_a != jugador, resultado_a, resultado_b),
      total = puntos_a_favor + puntos_en_contra
    )

  elo_propio <-
    ranking |>
    filter(tor == !!jugador) |>
    pull(elo)

  jugador_nuevo <-
    antiguedad_jugadores |>
    filter(tor == !!jugador) |>
    pull(jugador_nuevo)

  resultado_jugador <-
    puntos_jugador |>
    left_join(ranking, by = join_by(oponente == tor)) |>
    mutate(
      elo_jugador = !!elo_propio,
      puntaje_esperado = 1 / (1 + 10 ^ ((elo - elo_propio) / !!elo_dispersion)),
      resultado_match = case_when(
        puntos_a_favor > puntos_en_contra ~ 1,
        puntos_a_favor == puntos_en_contra ~ 0.5,
        puntos_a_favor < puntos_en_contra ~ 0
      ),
      pd = puntos_a_favor - puntos_en_contra,
      margen_coef = if_else(abs(pd) == 1 & (puntos_a_favor > 0 & puntos_en_contra > 0), 0.75, 1),
      diff = resultado_match - puntaje_esperado,
      variacion = diff * if_else(jugador_nuevo, !!k_coef_jugador_nuevo, !!k_coef_jugador_antiguo) * margen_coef
    )  |>
    summarize(variacion_total = sum(variacion)) |>
    transmute(
      version = !!version_actual,
      tor = !!jugador,
      elo = !!elo_propio + variacion_total
    )

  computo_jugadores <- bind_rows(computo_jugadores, resultado_jugador)

  if ((which(jugadores$tor == jugador) %% 100 == 0) | which(jugadores$tor == jugador) == length(jugadores$tor)) {
    cat(paste0(which(jugadores$tor == jugador), "/", length(jugadores$tor)), "jugadores computados\n")
  }
}


# Exportar ----------------------------------------------------------------
# tw: to write

ranking_tw <-
  ranking_db |>
  bind_rows(computo_jugadores) |>
  slice_max(version, by = tor, with_ties = FALSE) |>
  mutate(version = version_actual) |>
  bind_rows(ranking_db) |>
  arrange(desc(version), desc(elo))

jugadores_tw <-
  jugadores |>
  anti_join(jugadores_db, by = join_by(tor)) |>
  bind_rows(jugadores_db) |>
  arrange(nombre)

torneos_tw <-
  bind_rows(torneos, torneos_db) |>
  arrange(desc(fecha))

resultados_tw <-
  bind_rows(resultados, resultados_db) |>
  arrange(id_torneo)

torneos_computados_tw <-
  torneos |>
  distinct(id_torneo) |>
  mutate(computado_en = version_actual) |>
  select(id_torneo, computado_en) |>
  bind_rows(torneos_computados_db) |>
  arrange(desc(computado_en))

# write_csv(ranking_tw, here("data", "ranking.csv"))
# write_csv(jugadores_tw, here("data", "jugadores.csv"))
# write_csv(torneos_tw, here("data", "torneos.csv"))
# write_csv(resultados_tw, here("data", "resultados.csv"))
# write_csv(torneos_computados_tw, here("data", "torneos_computados.csv"))

write_sheet(ranking_tw, data_path, "ranking")
write_sheet(torneos_tw, data_path, "torneos")
write_sheet(resultados_tw, data_path, "resultados")
write_sheet(jugadores_tw, data_path, "jugadores")
write_sheet(torneos_computados_tw, data_path, "torneos computados")

posiciones_actuales <-
  ranking_tw |>
  filter(version == version_actual) |>
  mutate(rank = min_rank(-elo)) |>
  left_join(jugadores_tw, by = join_by(tor)) |>
  mutate(
    team = replace_na(team, ""),
    team = str_to_title(team),
    nombre = str_to_title(nombre)
  ) |>
  select(rank, nombre, tor, team, elo)

path_lista_completa <- "https://docs.google.com/spreadsheets/d/1kR45qa9CH3yiZpRMyTan1Hvmi3DgDkV-Uc2wrSGn1C4/edit?gid=1129691467#gid=1129691467"
write_sheet(posiciones_actuales, path_lista_completa, "elo-myl-lista-completa")
