# [FUNCTIONS] -------------------------------------------------------------
# - Data for tax report ---------------------------------------------------
fun_b3_tax_data <- function(
    df_position,
    df_position_now,
    df_daytrolha_year,
    df_dividends_year,
    int_year = year(Sys.Date()) - 1) {
  # arguments validated in main function

  # position at the closing of the year
  df_position |>
    anti_join(
      df_position_now
    ) |>
    group_by(ticker) |>
    filter(
      last(position) != 0
    ) |>
    slice(n()) |>
    ungroup() |>
    mutate(
      .before = 1,
      year = as.integer(int_year - 1)
    ) |>
    bind_rows(
      df_position_now |>
        mutate(
          .before = 1,
          year = as.integer(int_year)
        )
    ) |>
    group_by(ticker) |>
    arrange(
      date,
      .by_group = T
    ) |>
    ungroup() ->
  df_position_tax

  # total dividends in the year
  df_dividends_year %>%
    filter(
      year == int_year
    ) -> df_dividends_tax

  # total daytrolha in the year
  df_daytrolha_year %>%
    filter(
      year == int_year
    ) -> df_daytrolha_tax

  # add data frame subclasses?

  # output
  return(list(
    "position_tax" = df_position_tax,
    "dividends_tax" = df_dividends_tax,
    "daytrolha_tax" = df_daytrolha_tax
  ))
}
