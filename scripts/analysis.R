estimate_partials <- function(gam_mod, term = "exposure_proportion") {
  dat <- gam_mod$mu.coefSmo[[3]]$data
  
  if (term == "age") {
    pred_params <- expand_grid(age = seq(min(dat$age), max(dat$age), by = 0.5), 
                               exposure_proportion = 50,
                               child_id = factor(0))
  } else if (term == "exposure_proportion") {
    pred_params <- expand_grid(age = median(dat$age), 
                               exposure_proportion = seq(0, 100, by = 1),
                               child_id = factor(0))
  }
  
  preds <- predict(gam_mod,
                   newdata = pred_params,
                   type = "terms") |> 
    as_tibble() |> 
    `colnames<-`(value = c("part_age", "part_exp", "part_re", "part_age_exp"))
  pred_data <- pred_params |> cbind(preds)
  
  if (term == "age") {
    pred_data |> select(age, part_age)
  } else if (term == "exposure_proportion") {
    pred_data |> select(exposure_proportion, part_exp)
  }
}
