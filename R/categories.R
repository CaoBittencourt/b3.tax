# [DATA] -------------------------------------------------------------
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
