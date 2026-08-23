library(dplyr)
library(purrr)

datasets <- list(
  aelfric = df_aelfric,
  doaks = df_doaks,
  golden_legend = df_golden_legend,
  npnf = df_npnf
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
