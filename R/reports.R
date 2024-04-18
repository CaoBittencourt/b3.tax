# [FUNCTIONS] -------------------------------------------------------------
# - Dynamic tax report (assets) ----------------------------------------------------
fun_b3_tax_report_assets <- function(list_tax_data){

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

  # output
  return(df_position_report)

}

# - Dynamic tax report (dividends) ----------------------------------------------------
fun_b3_tax_report_dividends <- function(list_tax_data){

  # arguments validation

  # dividends report
  list_tax_data$
    dividends_tax %>%
    inner_join(
      df_dividends_cat
    ) -> df_dividends_report

  # output
  return(df_dividends_report)

}

# - Tax report main function ---------------------------------------------------
fun_b3_tax_report <- function(
    df_position,
    df_daytrolha_year,
    df_dividends_year,
    int_year = year(Sys.Date()) - 1
){

  # arguments validation
  stopifnot(
    "'df_position' must be a data frame with the 'df_position' subclass." =
      all(
        is.data.frame(df_position)
        , any(class(df_position) == 'df_position')
      )
  )

  stopifnot(
    "'df_daytrolha_year' must be a data frame with the 'df_daytrolha_year' subclass." =
      all(
        is.data.frame(df_daytrolha_year)
        , any(class(df_daytrolha_year) == 'df_daytrolha_year')
      )
  )

  stopifnot(
    "'df_dividends_year' must be a data frame with the 'df_dividends_year' subclass." =
      all(
        is.data.frame(df_dividends_year)
        , any(class(df_dividends_year) == 'df_dividends_year')
      )
  )

  stopifnot(
    "'int_year' must be an integer between 0 and the current year." =
      all(
        round(int_year[[1]]) >= 0,
        round(int_year[[1]]) <= year(Sys.Date())
      )
  )

  # data wrangling
  round(int_year[[1]]) -> int_year

  # tax data
  call_list <- match.call()

  call_list[[1]] <- fun_b3_tax_data

  list_tax_data <- eval.parent(call_list)

  rm(call_list)

  # apply tax report functions
  list(
    assets = fun_b3_tax_report_assets,
    dividends = fun_b3_tax_report_dividends
    #,daytrolha = fun_b3_tax_report_daytrolha
  ) %>%
    lapply(
      function(fun_report){
        tryCatch(
          expr = {
            do.call(
              fun_report
              , list(list_tax_data)
            )
          }, error = function(e){NULL}
        )
      }
    ) -> list_tax_report

  # output
  return(list(
    'data' = list_tax_data,
    'report' = list_tax_report
  ))

}
