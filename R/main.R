# [FUNCTIONS] -------------------------------------------------------------
# - Tax report main function ---------------------------------------------------
fun_b3_tax_main <- function(
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
        expr = {do.call(b3_fun, list_tax_data)}
        , error = function(e){NULL}
      )
    }
  ) -> list_tax_report

  # output
  return(list(
    'data' = list_tax_data,
    'report' = list_tax_report
  ))

}
