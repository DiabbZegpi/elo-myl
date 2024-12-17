library(tidyverse)
library(googlesheets4)
library(here)
tidymodels_prefer()

gs4_auth(email = "diabbluis@gmail.com", cache = ".gargle")
data_path <- "https://docs.google.com/spreadsheets/d/1ZErNDZVSbIP8ekj-mmTpdmJ_Gbpo8yQaN5HeLBlqblE/edit?gid=1605468956#gid=1605468956"

ranking_db <- read_sheet(data_path, "ranking", col_types = "Dcd")
resultados_db <- read_sheet(data_path, "resultados", col_types = "ciccii")
torneos_db <- read_sheet(data_path, "torneos", col_types = "ccDDcccccc")
jugadores_db <- read_sheet(data_path, "jugadores", col_types = "ccc")
torneos_computados_db <- read_sheet(data_path, "torneos computados", col_types = "cD")
tiendas_db <- read_sheet(data_path, "tiendas", col_types = "ccccccccc")

# Set August as the first month with reliable ranking
ranking <- filter(ranking_db, month(version) >= 8)

# Create full df
full_df_missing_elo <-
  resultados_db |>
  distinct() |>
  left_join(
    torneos_db |> select(id_torneo, tor, raza),
    by = join_by(id_torneo, tor_a == tor),
    relationship = "many-to-one"
  ) |>
  left_join(
    torneos_db,
    by = join_by(id_torneo, tor_b == tor),
    relationship = "many-to-one",
    suffix = c("_a", "_b")
  ) |>
  left_join(
    ranking,
    by = join_by(tor_a == tor, closest(fecha >= version)),
    relationship = "many-to-one"
  ) |>
  left_join(
    ranking,
    by = join_by(tor_b == tor, closest(fecha >= version)),
    relationship = "many-to-one",
    suffix = c("_a", "_b")
  ) |>
  select(id_torneo, ronda, tor_a, tor_b, resultado_a, resultado_b, raza_a, raza_b, elo_a, elo_b, fecha:formato) |>
  filter(str_detect(juego, "Liga"))

complete_elo <-
  full_df_missing_elo |>
  filter(is.na(elo_a) | is.na(elo_b)) |>
  left_join(
    ranking,
    by = join_by(tor_a == tor, closest(fecha <= version)),
    relationship = "many-to-one"
  ) |>
  left_join(
    ranking,
    by = join_by(tor_b == tor, closest(fecha <= version)),
    relationship = "many-to-one",
    suffix = c("_replace_a", "_replace_b")
  ) |>
  mutate(
    elo_a = if_else(is.na(elo_a), elo_replace_a, elo_a),
    elo_b = if_else(is.na(elo_b), elo_replace_b, elo_b)
  ) |>
  select(-contains("replace")) |>
  bind_rows(full_df_missing_elo |> filter(!is.na(elo_a), !is.na(elo_b))) |>
  select(id_torneo, ronda, tor_a, tor_b, resultado_a, resultado_b, raza_a, raza_b, elo_a, elo_b, fecha:formato) |>
  select(-c(juego, formato, semana)) |>
  left_join(
    tiendas_db |> select(nombre, region, ciudad),
    by = join_by(tienda == nombre),
    relationship = "many-to-one",
    multiple = "first"
  )

full_df <-
  complete_elo |>
  relocate(1:2, 4, 3, 6, 5, 8, 7, 10, 9) |>
  rename_with(\(x) colnames(complete_elo)) |>
  bind_rows(complete_elo) |>
  mutate(target = case_when(
    resultado_a > resultado_b ~ "win",
    resultado_a == resultado_b ~ "draw",
    resultado_a < resultado_b ~ "lose"
  )) |>
  select(-contains("_b"), -resultado_a) |>
  rename_with(~ str_remove(.x, "_a"))

tiendas_razas <-
  full_df |>
  count(tienda, raza) |>
  complete(tienda, raza, fill = list(n = 0)) |>
  group_by(tienda) |>
  mutate(pct = n / sum(n)) |>
  ungroup() |>
  select(tienda, raza, pct)

ciudad_razas <-
  full_df |>
  count(ciudad, raza) |>
  complete(ciudad, raza, fill = list(n = 0)) |>
  group_by(ciudad) |>
  mutate(pct = n / sum(n)) |>
  ungroup() |>
  select(ciudad, raza, pct)

write_rds(full_df, here("neural myl", "complete_data.rds"), compress = "gz")
