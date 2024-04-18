# [FUNCTIONS] -------------------------------------------------------------
# - Data for tax report ---------------------------------------------------
fun_b3_tax_data <- function(
    df_position,
    df_daytrolha_year,
    df_dividends_year,
    int_year = year(Sys.Date()) - 1
){

  # arguments validated in main function

  # position at the closing of the year
  df_position %>%
    mutate(
      .before = 1
      , year =
        year(date)
    ) %>%
    select(
      -date
    ) %>%
    filter(
      year == c(
        int_year - 1,
        int_year
      )
    ) %>%
    group_by(
      year,
      ticker
    ) %>%
    slice(n()) %>%
    ungroup() %>%
    filter(
      position > 0
    ) %>%
    arrange(desc(
      value
    )) ->
    df_position_tax

  # total dividends in the year
  list_dividends$
    dividends_year %>%
    filter(
      year == int_year
    ) -> df_dividends_tax

  # total daytrolha in the year
  list_daytrolha$
    daytrolha_year %>%
    filter(
      year == int_year
    ) -> df_daytrolha_tax

  # add data frame subclasses?

  # output
  return(list(
    'position_tax' = df_position_tax,
    'dividends_tax' = df_dividends_tax,
    'daytrolha_tax' = df_daytrolha_tax
  ))

}
