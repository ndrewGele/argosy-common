generate_feature_spec <- function(definition) {
  
  res <- purrr::map(
    .x = definition,
    .f = function(x) {
      
      single_instruction <- x
      amount <- sample(x$amount[1]:x$amount[2], size = 1)
      print(amount)
      if(amount > 0) {
        
        purrr::map(
          .x = 1:amount,
          .f = function(x) {
            
            arg_names <- names(single_instruction$args)
            args <- purrr::map(
              .x = single_instruction$args,
              .f = function(x) eval(parse(text = x))
            )
            names(args) <- arg_names
            
            return(
              list(
                num = x,
                fun = single_instruction$fun,
                cols = single_instruction$cols,
                args = args,
                results = single_instruction$results
              )
            )
            
          }
        ) # end of inner map
        
      } else {
        # What to return if amount is 0
        return(list('None'))
      }
      
    }
  ) # end of outer map
  
  return(unname(unlist(res, recursive = FALSE)))
  
}

create_features <- function(data, feature.spec) {
  
  if(all(feature.spec == 'None')) return(data)
  
  df <- dplyr::rename_with(data, tolower)
  
  res <- purrr::map_dfc(
    .x = feature.spec,
    .f = function(x) {
      
      df <- rlang::exec(
        x$fun, 
        dplyr::select(df, x$cols), 
        unlist(x$args)
      )
      
      df <- dplyr::as_tibble(df)
      
      df <- dplyr::select(df, x$results)
      
      df <- dplyr::rename_with(
        .data = df, 
        .fn = paste0, 
        .cols = dplyr::everything(), 
        '_', 
        x$num
      ) 
      
      return(df)
      
    }
  )
  
  return(dplyr::bind_cols(data, res))
  
}

# # Test Case
# 
# test_in <- list(
#   adx = list(
#     name = 'adx',
#     amount = c(0,2),
#     fun = TTR::ADX,
#     cols = c('high', 'low', 'close'),
#     args = list(
#       n = 'sample(1:100, size = 1)'
#     ),
#     results = c('DX', 'ADX')
#   ),
#   bb = list(
#     name = 'bb',
#     amount = c(0,2),
#     fun = TTR::BBands,
#     cols = c('high', 'low', 'close'),
#     args = list(
#       n = 'sample(1:100, size = 1)'
#     ),
#     results = c('pctB')
#   )
# )
# 
# library(TTR)
# data(ttrc)
# 
# test_mid <- generate_feature_spec(test_in)
# test_out <- create_features(ttrc, test_mid)
