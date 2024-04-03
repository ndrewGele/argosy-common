plan_next_day <- function(
  portfolio = NULL, 
  strategy.decisions
) {

  require(dplyr)

  orders <- strategy.decisions %>% 
    filter(!is.na(decision))

  return(orders)

}

# Test

library(dplyr)
library(dbplyr)
library(TTR)

fake_portfolio <- data.frame(
  symbol = c('cash.USD', 'TSLA', 'AAPL'),
  shares = c(50000, 20, 10)
)

test_plan <- plan_next_day(
  portfolio = data.frame(cashmoney = 5000),
  strategy.decisions = filter(
    decisions_df,
    strategy
  )
)

fake_portfolio <- 
test_plan2 <- plan_next_day(
  portfolio = data.frame(cashmoney = 5000),
  strategy.decisions = decisions_df
)

DBI::dbDisconnect(db_con)