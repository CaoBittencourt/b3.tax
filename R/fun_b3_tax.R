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

# - Categories data frame -------------------------------------------------
# Dividends categories
tibble(
  event = c('dividendo', 'juros sobre capital próprio', 'rendimento'),
  account = c('Rendimentos Isentos e Não Tributáveis', 'Rendimentos Sujeitos à Tributação Exclusiva/Definitiva', 'Rendimentos Sujeitos à Tributação Exclusiva/Definitiva'),
  category = as.integer(c(9, 10, 6)),
  category_desc = c('09 - Lucros e dividendos recebidos', '10 - Juros sobre capital próprio', '06 - Rendimento de aplicações financeiras'),
) -> df_dividends_cat

# Assets categories
tibble(
  stock = c(T, F),
  account = c('Bens e Direitos', 'Bens e Direitos'),
  category = as.integer(c(3, 4)),
  subcategory = as.integer(c(1, 2)),
  category_desc = c('03 - Participações Societárias', '04 - Aplicações e Investimentos'),
  subcategory_desc = c('01 - Ações (inclusive as listadas em bolsa)', '02 - Títulos públicos e privados sujeitos à tributação (Tesouro Direto, CDB, RDB e Outros)')
) -> df_position_cat

# - Dynamic tax report ----------------------------------------------------
fun_b3_tax_report <- function(list_tax_data){

  # arguments validation

  # position report
  list_tax_data$
    position_tax %>%
    inner_join(
      df_position_cat
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

  # tax report data frame
  fun_b3_tax_report(
    list_tax_data
  ) -> df_tax_report

  # output
  return(list(
    'data' = list_tax_data,
    'report' = df_tax_report
  ))

}


# # - dsds ------------------------------------------------------------------
# fun_b3_tax_report(
#   df_position =
#     list_position$
#     position
#   , df_daytrolha_year =
#     list_daytrolha$
#     daytrolha_year
#   , df_dividends_year =
#     list_dividends$
#     dividends_year
# )
