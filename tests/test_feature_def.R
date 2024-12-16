list(
  list(
    fun = 'TTR::ADX',
    amount = c(0,2),
    cols = c('high', 'low', 'close'),
    args = list(
      n = sample(1:100, size = 2, replace = FALSE)
    ),
    result = 'ADX'
  ),
  list(
    fun = 'TTR::BBands',
    amount = c(0,2),
    cols = c('high', 'low', 'close'),
    args = list(
      n = sample(1:100, size = 2, replace = FALSE)
    ),
    result = 'pctB'
  )
)