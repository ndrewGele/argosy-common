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