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
  model_file = "models/model5-quadratic-exposure.stan",
  model_name = "quadratic",
  output_dir = "models",
  original_dataset_name = "Marchman Dallas Bilingual",
  seed = 123
)

