create_model_set <- function(model.reqs, db.con, s3.con) {
  
  require(dplyr)
  require(dbplyr)
  require(paws.common)
  require(paws.storage)
  
  model_options <- db.con %>%
    tbl('models') %>%
    inner_join(
      db.con %>%
        tbl('models') %>%
        filter(name %in% !!model.reqs) %>%
        group_by(name, hash) %>%
        summarise(update_timestamp = max(update_timestamp, na.rm = TRUE)),
      by = c('name', 'hash', 'update_timestamp')
    ) %>%
    filter(status %in% c('champion', 'challenger')) %>%
    collect()
  
  model_set <- purrr::map(
    .x = model.reqs,
    .f = \(x) {
      model <- model_options %>%
        filter(name == x) %>%
        sample_n(1)
      
      tmp <- tempfile(pattern = model$hash)
      
      s3.con$download_file(
        Bucket = Sys.getenv('SEAWEED_MODEL_BUCKET'),
        Key = model$file_name,
        Filename = tmp
      )
      
      list(
        hash = model$hash,
        model = readRDS(tmp)
      )
    }
  )
  
  names(model_set) <- model.reqs
  
  return(model_set)
  
}