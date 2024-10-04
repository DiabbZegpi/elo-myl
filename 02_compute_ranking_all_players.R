library(tidyverse)
library(here)
library(googlesheets4)

df_raw <- read_csv(here("data", "PB_RE_24_09_24_SR.csv"))

df_filtered <-
  df_raw |>
  mutate(
    fecha = ymd( str_extract(id_torneo, "(\\d+)", 1L)),
    semana = as_date(floor_date(fecha, "weeks", week_start = 1))
  ) |>
  drop_na(tor_a, tor_b, resultado_a, resultado_b) |>
  filter(
    estado == "Reportado",
    juego != "MyL Primer Bloque - Lanzamiento"
  )

elo_inicial <- 1400
k_coef <- 40
elo_dispersion <- 1135.77


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

rondas <-
  df_filtered |>
  select(
    id_torneo,
    tor_a,
    tor_b,
    resultado_a,
    resultado_b
  )


# Tabla `ranking` ---------------------------------------------------------

ranking <-
  jugadores |>
  mutate(
    elo = !!elo_inicial,
    version = min(torneos$semana) %m-% weeks(1)
  ) |>
  select(
    version,
    tor,
    elo
  )


# Cálculo de elo ----------------------------------------------------------

todas_semanas <- as.character(unique(torneos$semana))

for (semana_actual in todas_semanas) {

  resultado_semanal <- tibble(
    version = ymd(),
    tor = character(),
    elo = numeric()
  )

  torneos_semana <-
    torneos |>
    filter(semana == semana_actual)

  rondas_semana <-
    rondas |>
    filter(id_torneo %in% torneos_semana$id_torneo)

  jugadores_semana <-
    torneos_semana |>
    distinct(tor)

  for (jugador in jugadores_semana$tor) {

    ranking_actual <-
      ranking |>
      slice_max(version, by = tor, with_ties = FALSE)

    puntos_jugador <-
      rondas_semana |>
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
      ranking_actual |>
      filter(tor == !!jugador) |>
      pull(elo)

    resultado_jugador <-
      puntos_jugador |>
      left_join(ranking_actual, by = join_by(oponente == tor)) |>
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
        variacion = diff * !!k_coef * margen_coef
      )  |>
      summarize(variacion_total = sum(variacion)) |>
      transmute(
        version = as_date(semana_actual),
        tor = !!jugador,
        elo = !!elo_propio + variacion_total
      )

    resultado_semanal <- bind_rows(resultado_semanal, resultado_jugador)

  }

  ranking <-
    ranking |>
    bind_rows(resultado_semanal) |>
    slice_max(version, by = tor, with_ties = FALSE) |>
    mutate(version = as_date(semana_actual)) |>
    bind_rows(ranking)

  indice_semanal <- which(todas_semanas == semana_actual)
  semanas_completadas <- paste0("(", indice_semanal, "/", length(todas_semanas), ")")
  cat(
    "Semana", semana_actual, "rankeada", semanas_completadas, "|",
    nrow(resultado_semanal), "jugadores rankeados esta semana\n"
  )

}

torneos_computados <-
  torneos |>
  distinct(id_torneo, semana) |>
  mutate(computado_en = semana %m+% weeks(1)) |>
  select(id_torneo, computado_en)

write_csv(ranking, here("data", "ranking.csv"))
write_csv(jugadores, here("data", "jugadores.csv"))
write_csv(torneos, here("data", "torneos.csv"))
write_csv(rondas, here("data", "rondas.csv"))
write_csv(torneos_computados, here("data", "torneos_computados.csv"))


gs4_auth(email = "diabbluis@gmail.com", cache = ".gargle")

gs_db_url <- "https://docs.google.com/spreadsheets/d/1ZErNDZVSbIP8ekj-mmTpdmJ_Gbpo8yQaN5HeLBlqblE/edit?gid=0#gid=0"
write_sheet(ranking, gs_db_url, "ranking")
write_sheet(torneos, gs_db_url, "torneos")
write_sheet(rondas, gs_db_url, "rondas")
write_sheet(jugadores, gs_db_url, "jugadores")
write_sheet(torneos_computados, gs_db_url, "torneos computados")





















