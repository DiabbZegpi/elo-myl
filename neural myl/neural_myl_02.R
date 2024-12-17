library(tidyverse)
library(todymodels)
library(here)
tidymodels_prefer()


# Import data -------------------------------------------------------------

df <- read_rds(here("neural myl", "complete_data.rds"))
df <- distinct(df)


# Data partition ----------------------------------------------------------

set.seed(2024)
split <- initial_split(df, strata = target)
df_train <- training(split)
df_test <- testing(split)
set.seed(2025)
folds <- vfold_cv(df_train, v = 5, strata = target)

