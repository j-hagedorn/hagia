library(tidyverse)
library(purrr)

# Prepare translated source-language datasets
prepare_translation <- function(df) {
  if (any(df$translation_status != "translated", na.rm = TRUE)) {
    stop("One or more translations are incomplete.")
  }
  
  df |>
    mutate(
      source_text = translation,
      source_language = "English"
    )
}

# Load and prepare all datasets
datasets <- 
  list(
    aelfric = arrow::read_parquet("derived/df_aelfric.parquet"),
    doaks = arrow::read_parquet("derived/df_doaks.parquet"),
    eusebius_martyrs = arrow::read_parquet("derived/df_eusebius_martyrs.parquet"),
    golden_legend = arrow::read_parquet("derived/df_golden_legend.parquet"),
    npnf = arrow::read_parquet("derived/df_npnf.parquet"),
    sozomen = arrow::read_parquet("derived/df_sozomen.parquet"),
    latin = readRDS("derived/df_latin_translation_checkpoint.rds") |> prepare_translation(),
    greek = readRDS("derived/df_greek_translation_checkpoint.rds") |> prepare_translation()
  ) |>
  imap(
    ~ if ("dataset" %in% names(.x)) {
      .x
    } else {
      mutate(.x, dataset = .y)
    }
  )

# Retain only fields common to every dataset
common_cols <- Reduce(intersect,lapply(datasets, names))

hagia <- 
  datasets |>
  map(~ select(.x, all_of(common_cols))) |>
  bind_rows()

rm(datasets, common_cols, prepare_translation)

arrow::write_parquet(hagia,"derived/hagia.parquet")
