generate_feature_spec <- function(feature.def) {
  
  res <- purrr::map(
    .x = feature.def,
    .f = function(x) {
      
      single_def <- x
      amount <- sample(x$amount[1]:x$amount[2], size = 1)
      
      if(amount == 0) return(NULL)
      if(amount > 0) {
        
        purrr::map(
          .x = 1:amount,
          .f = function(x) {
            
            num <- x
            args <- purrr::map(
              .x = single_def$args,
              .f = function(x) {
                purrr::pluck(x, num)
              }
            )
            
            return(
              list(
                name = paste0(
                  single_def$result, '.',
                  unlist(args, recursive = TRUE) |>
                    paste(collapse = '.')
                ),
                fun = single_def$fun,
                cols = single_def$cols,
                args = args,
                result = single_def$result
              )
            )
            
          } # end of map fn
        ) # end of inner map
        
      } # end of if(amount > 0) 
      
    } # end of map fn
  ) # end of outer map
  
  res <- unname(unlist(res, recursive = FALSE))
  res <- res[order(purrr::map_chr(.x = res, .f = purrr::pluck, 'name'))]
  return(res)
  
}

create_features <- function(data, feature.spec) {
  
  if(is.null(feature.spec)) return(NULL)
  if(length(feature.spec) == 0) return(NULL)
  
  df <- dplyr::rename_with(data, tolower)
  
  res <- purrr::map_dfc(
    .x = feature.spec,
    .f = function(x) {
      
      if(!is.list(x)) return(data.frame(drop = 1:nrow(df)))
      
      print(glue::glue('Generating feature: {x$name}'))
      df <- rlang::exec(
        eval(parse(text = x$fun)), 
        dplyr::select(df, x$cols), 
        unlist(x$args)
      )
      
      df <- dplyr::as_tibble(df)
      
      df <- dplyr::select(df, x$result)
      
      names(df) <- x$name
      
      return(df)
      
    }
  )
  
  return(res)
  
}

tweak_feature_spec <- function(feature.spec, feature.def) {
  
  cat('Tweaking feature spec \n')
  print(glue::glue('Length of current spec: {length(feature.spec)}'))

  # Generate a fresh spec to combine in some way with given spec
  fresh_spec <- generate_feature_spec(feature.def)
  
  # We want to prevent the possibility of duplicating features, 
  # so we'll make sure that none of the new features match the old features
  existing_names <- feature.spec |> purrr::map_chr(purrr::pluck, 'name')
  fresh_names <- fresh_spec |> purrr::map_chr(purrr::pluck, 'name')
  
  while(any(fresh_names %in% existing_names)) {
    fresh_spec <- generate_feature_spec(feature.def)
    fresh_names <- fresh_spec |> purrr::map_chr(purrr::pluck, 'name')
  }
  
  print(glue::glue('Length of fresh spec: {length(fresh_spec)}'))
  
  # Create table of all valid operations
  valid_ops_df <- purrr::map_dfr(
    .x = feature.def,
    .f = \(x) data.frame(
      stem = x$result,
      max = max(x$amount)
    )
  ) |> 
    dplyr::mutate(
      current = purrr::map(
        .x = stem,
        .f = \(x) {
          all_feats = purrr::map_chr(feature.spec, purrr::pluck, 'name')
          all_feats[grepl(pattern = x, x = all_feats)]
        }
      ),
      cur.len = purrr::map_dbl(.x = current, .f = length),
      available = purrr::map(
        .x = stem,
        .f = \(x) {
          all_feats = purrr::map_chr(fresh_spec, purrr::pluck, 'name')
          all_feats[grepl(pattern = x, x = all_feats)]
        }
      ),
      ava.len = purrr::map_dbl(.x = available, .f = length),
      # cur.gt.zero = cur.len > 0,
      # cur.lt.max = cur.len < max,
      # ava.gt.zero = ava.len > 0,
      valid_ops = purrr::pmap(
        .l = list(cur.len, max, ava.len), 
        .f = \(cur.len, max, ava.len) {
          if(cur.len == 0 & ava.len == 0) return('nothing')
          ops_res = c()
          if(cur.len < max & ava.len > 0) {
            ops_res <- append(ops_res, 'add')
          }
          if(cur.len > 0 & ava.len > 0) {
            ops_res <- append(ops_res, 'replace')
          }
          if(cur.len > 0) {
            ops_res <- append(ops_res, 'remove')
          }
          return(ops_res)
        }
        
      )
    )
  
  # Return early if there is nothing to tweak
  if(all(unique(unlist(valid_ops_df$valid_ops)) == 'nothing')) {
    cat('No valid tweak operations. Returning input feature spec.')
    return(feature.spec)
  }
  
  # Pick an operation and pick a feature to perform it on
  all_valid_ops <- unlist(valid_ops_df$valid_ops)
  all_valid_ops <- all_valid_ops[all_valid_ops != 'nothing']
  picked_operation <- sample(all_valid_ops, 1)
  print(glue::glue('Picked operation: {picked_operation}'))
  
  valid_ops_df <- valid_ops_df |> 
    dplyr::mutate(
      pickable = purrr::map_lgl(
        .x = valid_ops,
        .f = \(x) {
          any(grepl(pattern = picked_operation, x = x))
        }
      )
    )
  
  picked_row <- valid_ops_df |> 
    dplyr::filter(pickable) |> 
    dplyr::slice_sample(n = 1)
  
  # Add and/or remove features
  res <- feature.spec

  if(picked_operation %in% c('replace', 'remove')) {
    
    remove_feat <- sample(picked_row$current[[1]], 1)
    print(glue::glue('Removing feature: {remove_feat}'))
    remove_index <- which(
      purrr::map_lgl(
        .x = res,
        .f = \(x) {
          purrr::pluck(x, 'name') == remove_feat
        }
      )
    )
    res[remove_index] <- NULL
    
  }
  
  if(picked_operation %in% c('add', 'replace')) {
    
    add_feat <- sample(picked_row$available[[1]], 1)
    print(glue::glue('Adding feature: {add_feat}'))
    add_index <- which(
      purrr::map_lgl(
        .x = fresh_spec,
        .f = \(x) {
          purrr::pluck(x, 'name') == add_feat
        }
      )
    )
    res <- append(res, fresh_spec[add_index])
    
  }
  
  res <- res[order(purrr::map_chr(.x = res, .f = purrr::pluck, 'name'))]

  return(res)
  
}
