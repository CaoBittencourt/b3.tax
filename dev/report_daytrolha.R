# [FUNCTIONS] -------------------------------------------------------------
# - Dynamic tax report ----------------------------------------------------
fun_b3_tax_report <- function(list_tax_data){

  # arguments validation

  # position report
  list_tax_data$
    position_tax %>%
    inner_join(
      df_position_cat
    ) -> df_position_report

  # dynamic description
  df_position_report %>%
    mutate(
      text = paste0(
        'Ao final do ano de '
        , year
        , ', eu tinha '
        , position
        , ' '
        , ticker
        , ', com preço médio de '
        , dollar(
          mean_price
          , prefix = 'R$'
          , big.mark = '.'
          , decimal.mark = ','
          , suffix = ', '
        )
        , 'totalizando '
        , dollar(
          value
          , prefix = 'R$'
          , big.mark = '.'
          , decimal.mark = ','
          , suffix = '.'
        )
      )
    ) %>%
    arrange(
      year
    ) %>%
    group_by(
      ticker
    ) %>%
    mutate(
      text = paste(
        text
        , collapse = ' '
      )
    ) %>%
    ungroup() %>%
    mutate(
      text = if_else(
        year == max(year)
        , text
        , ''
      )
    ) %>%
    arrange(
      ticker,
      year
    ) -> df_position_report

  # dividends report
  list_tax_data$
    dividends_tax %>%
    inner_join(
      df_dividends_cat
    ) -> df_dividends_report

  # daytrolha report
  # df_daytrolha_tax

  # output
  return(list(
    'position_report' = df_position_report,
    'dividends_report' = df_dividends_report
    # 'daytrolha_report' = df_daytrolha_report
  ))

}

# daytrolha
