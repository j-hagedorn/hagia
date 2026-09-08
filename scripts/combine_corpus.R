library(dplyr)
library(purrr)

# Load all scraped dataset files
df_ado_pl123 <- arrow::read_parquet("derived/df_ado_pl123.parquet")
df_aelfric <- arrow::read_parquet("derived/df_aelfric.parquet")
df_doaks <- arrow::read_parquet("derived/df_doaks.parquet")
df_eusebius_martyrs <- arrow::read_parquet("derived/df_eusebius_martyrs.parquet")
df_golden_legend <- arrow::read_parquet("derived/df_golden_legend.parquet")
df_historia_tripartita <- arrow::read_parquet("derived/df_historia_tripartita.parquet")
df_jean_de_mailly <- arrow::read_parquet("derived/df_jean_de_mailly.parquet")
df_sozomen <- arrow::read_parquet("derived/df_sozomen.parquet")
df_theodoret_eh <- arrow::read_parquet("derived/df_theodoret_eh.parquet")
df_theodoret <- arrow::read_parquet("derived/df_theodoret.parquet")
df_npnf <- arrow::read_parquet("derived/df_npnf.parquet")


datasets <- list(
  aelfric = df_aelfric,
  doaks = df_doaks,
  golden_legend = df_golden_legend,
  npnf = df_npnf,
  theodoret = df_theodoret,
  eusebius_martyrs = df_eusebius_martyrs,
  jean_de_mailly = df_jean_de_mailly,
  sozomen = df_sozomen,
  theodoret_eh = df_theodoret_eh,
  ado_pl123 = df_ado_pl123,
  historia_tripartita = df_historia_tripartita
) |>
  imap(
    ~ mutate(
      .x,
      dataset = .y
    )
  )

common_cols <- Reduce(
  intersect,
  lapply(datasets, names)
)

df_oehc <- datasets |>
  map(
    ~ select(
      .x,
      all_of(common_cols)
    )
  ) |>
  bind_rows()

rm(datasets); rm(common_cols); rm(manifest)

## Scrape and save

# arrow::write_parquet(df_oehc, "derived/df_oehc.parquet")

# df_oehc <- arrow::read_parquet("derived/df_oehc.parquet")
