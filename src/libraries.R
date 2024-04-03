# Renv scans project for libraries to add to renv.lock.

# Because we source functions from common-code (another directory), 
# they aren't found by renv.

# Placing these here will allow renv to pick them up without 
# unnecessarily loading the libraries before they're needed.

library(TTR)

library(recipes)
library(workflows)
library(glmnet)
library(ranger)
library(xgboost)