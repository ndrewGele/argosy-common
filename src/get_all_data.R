get_all_data <- function(
    db.con,
    symbols = 'all',
    start.date = as.Date('01/01/1900', '%m/%d/%Y'),
    end.date = as.Date('01/01/2100', '%m/%d/%Y')
) {
  
  require(dplyr)
  require(dbplyr)
  
  if(start.date > end.date) stop('start.date must be an earlier date than end.date')
  
  ohlc_df <- db.con %>% 
    tbl('daily_ohlc') %>%
    filter(
      'all' %in% !!symbols | symbol %in% !!symbols,
      timestamp >= !!start.date,
      timestamp <= !!end.date
    ) %>% 
    transmute(
      symbol, 
      date = date_trunc('day', timestamp), 
      open, high, low, close, vwap
    ) %>% 
    collect()
  
  indicators_df <- db.con %>% 
    tbl('indicators') %>% 
    filter(
      timestamp >= !!start.date,
      timestamp <= !!end.date
    ) %>% 
    transmute(
      symbol, 
      date = date_trunc('day', timestamp), 
      indicator, value
    ) %>% 
    collect() %>% 
    tidyr::pivot_wider(
      names_from = indicator,
      values_from = value
    )
  
  macro_indicators_df <- db.con %>% 
    tbl('macro_indicators') %>% 
    filter(
      date >= !!start.date,
      date <= !!end.date
    ) %>% 
    transmute(
      date = date_trunc('day', date), 
      name, value
    ) %>% 
    collect() %>% 
    tidyr::pivot_wider(
      names_from = name,
      values_from = value
    )
  
  df <- left_join(
    ohlc_df,
    indicators_df,
    by = c('symbol', 'date')
  ) %>% 
    left_join(
      macro_indicators_df,
      by = 'date'
    ) %>% 
    arrange(symbol, date)
  
}

# # Test
# db_con <- DBI::dbConnect(
#   drv = RPostgres::Postgres(),
#   dbname = Sys.getenv('POSTGRES_DB'),
#   host = Sys.getenv('POSTGRES_HOST'),
#   port = Sys.getenv('POSTGRES_PORT'),
#   user = Sys.getenv('POSTGRES_USER'),
#   password = Sys.getenv('POSTGRES_PASSWORD')
# )
# 
# test_all_data <- get_all_data(db.con = db_con)
# test_msft <- get_all_data(db.con = db_con, symbols = 'MSFT')
# test_one_month <- get_all_data(
#   db.con = db_con,
#   start.date = as.Date('08/01/2022', '%m/%d/%Y'),
#   end.date = as.Date('08/31/2022', '%m/%d/%Y')
# )
# 
# DBI::dbDisconnect(db_con)
