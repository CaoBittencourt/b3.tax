# [FUNCTIONS] -------------------------------------------------------------
# - Data for tax report ---------------------------------------------------
fun_b3_tax_data <- function(
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
  
  # position at the closing of the year
  df_position %>% 
    filter(
      year(date) ==
        int_year
    ) %>% 
    group_by(
      ticker
    ) %>% 
    slice(n()) %>% 
    mutate(
      date = as_date(
        paste0(
          int_year,
          '-12-31'
        )
      )
    ) %>% 
    ungroup() %>% 
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

# - Dynamic tax report ---------------------------------------------------


