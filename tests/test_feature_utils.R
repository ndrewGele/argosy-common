library(TTR)
data(ttrc)

ttrc <- ttrc |> 
  dplyr::filter(lubridate::year(Date) >= 2000)


# Run functions in sequence -----------------------------------------------

source('./src/feature_utils.R')

test_spec <- generate_feature_spec(
  feature.def = source('./tests/test_feature_def.R')$value
)

test_features <- create_features(
  data = ttrc,
  feature.spec = test_spec
)

test_tweaked_spec <- tweak_feature_spec(
  feature.spec = test_spec,
  feature.def = source('./tests/test_feature_def.R')$value
)

test_tweaked_features <- create_features(
  data = ttrc,
  feature.spec = test_tweaked_spec
)

# Demonstrate how to serialize and unserialize as well
spec_char_json <- test_spec |> 
  as.character() |> 
  jsonlite::toJSON() |> 
  as.character()

read_spec <- spec_char_json |> 
  jsonlite::fromJSON() |> 
  purrr::map(\(x) eval(parse(text = x)))

test_read_features <- create_features(
  data = ttrc,
  feature.spec = read_spec
)

test_tweaked_read_spec <- tweak_feature_spec(
  feature.spec = read_spec,
  feature.def = source('./tests/test_feature_def.R')$value
)


# Test Outputs and Alert on Failure ---------------------------------------

# Verify that there are no duplicates when generating features
spec_names <- test_spec |> 
  purrr::map_chr(purrr::pluck, 'name') |> 
  length()
spec_names_unique <- test_spec |> 
  purrr::map_chr(purrr::pluck, 'name') |> 
  unique() |> 
  length()
if(spec_names_unique < spec_names) print('UNIQUE NAMES TEST FAILED')

# Verify that there are no duplicates as a result of tweaking
spec_names <- test_tweaked_spec |> 
  purrr::map_chr(purrr::pluck, 'name') |> 
  length()
spec_names_unique <- test_tweaked_spec |> 
  purrr::map_chr(purrr::pluck, 'name') |> 
  unique() |> 
  length()
if(spec_names_unique < spec_names) print('UNIQUE TWEAK TEST FAILED')

# Verify that data is the same after serializing and unserializing spec
if(!identical(test_read_features, test_features)) {
  print('SERIALIZE TEST FAILED')
}
