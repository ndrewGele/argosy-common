library(TTR)
data(ttrc)

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
  jsonlite::toJSON()

read_spec <- spec_char_json |> 
  jsonlite::fromJSON() |> 
  purrr::map(\(x) eval(parse(text = x)))

test_tweaked_read_spec <- tweak_feature_spec(
  feature.spec = read_spec,
  feature.def = source('./tests/test_feature_def.R')$value
)

test_read_features <- create_features(
  data = ttrc,
  feature.spec = test_tweaked_spec
)

# Verify that all feature data created from specs contain data
# (2 and 3 should always be equal)
complete.cases(test_features) |> sum()
complete.cases(test_tweaked_features) |> sum()
complete.cases(test_read_features) |> sum()
