require(tidyverse)
library(bayesplot)
library(rstan)

source("scripts/stan_helpers.R")

bilingual_item_data_clean <- readRDS("data/bilingual_item_data_clean.Rds")

stan_data <- prepare_stan_data(bilingual_item_data_clean, #n_items=300, # can do 300 with 64gb RAM
                               original_dataset_name = "Marchman Dallas Bilingual")

rstan_options(auto_write = TRUE) 
options(mc.cores = parallel::detectCores())


fit_stan_by_lexical_category(
  data = bilingual_item_data_clean,
  categories = c("nouns"),
  n_items = 200, # takes ~2 weeks if we do all 462 nouns..
  model_file = "models/model5-quadratic-exposure.stan",
  model_name = "quadratic",
  output_dir = "models",
  original_dataset_name = "Marchman Dallas Bilingual",
  seed = 123
)


fit_stan_by_lexical_category(
  data = bilingual_item_data_clean,
  categories = c("predicates","function_words"), #c("nouns"),
  #n_items = 200, # takes ~2 weeks if we do all 462 nouns..
  model_file = "models/model6-cubic-exposure.stan",
  model_name = "cubic",
  output_dir = "models",
  original_dataset_name = "Marchman Dallas Bilingual",
  seed = 123
)

fit_stan_by_lexical_category(
  data = bilingual_item_data_clean,
  categories = c("nouns"),
  n_items = 200, # takes ~2 weeks if we do all 462 nouns..
  model_file = "models/model6-cubic-exposure.stan",
  model_name = "cubic",
  output_dir = "models",
  original_dataset_name = "Marchman Dallas Bilingual",
  seed = 123
)

fit_stan_by_lexical_category(
  data = bilingual_item_data_clean,
  model_file = "models/model5-quadratic-exposure.stan",
  model_name = "quadratic",
  output_dir = "models",
  original_dataset_name = "Marchman Dallas Bilingual",
  seed = 123
)



fit_stan_by_lexical_category(
  data = bilingual_item_data_clean,
  categories = c("nouns"),
  n_items = 200, # takes ~2 weeks if we do all 462 nouns..
  model_file = "models/model2.stan",
  model_name = "linear",
  output_dir = "models",
  original_dataset_name = "Marchman Dallas Bilingual",
  seed = 123
)

fit_stan_by_lexical_category(
  data = bilingual_item_data_clean,
  categories = c("nouns"),
  n_items = 200, # takes ~2 weeks if we do all 462 nouns..
  model_file = "models/model4-exposure-spline.stan",
  model_name = "spline",
  output_dir = "models",
  original_dataset_name = "Marchman Dallas Bilingual",
  seed = 123
)