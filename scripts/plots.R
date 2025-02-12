plot_preds <- function(gam_mod, x = "exposure_proportion") {
  dat <- gam_mod$mu.coefSmo[[3]]$data # found by searching through `str()`
  pred_params <- expand_grid(age = seq(min(dat$age), max(dat$age)), 
                             exposure_proportion = seq(0, 100, length.out = 20),
                             child_id = factor(0))
  preds <- predict(gam_mod,
                   newdata = pred_params,
                   type = "response")
  preds_plot <- pred_params |> cbind(preds)
  
  if (x == "age") {
    ggplot(preds_plot,
           aes(x = age, y = preds, 
               col = exposure_proportion, 
               group = exposure_proportion)) +
      geom_line() +
      labs(x = "Age", 
           y = "Proportion produced", 
           col = "Exposure proportion")
  } else if (x == "exposure_proportion") {
    ggplot(preds_plot,
           aes(x = exposure_proportion, y = preds, 
               col = age, 
               group = age)) +
      geom_line() +
      labs(x = "Exposure proportion", 
           y = "Proportion produced", 
           col = "Age")
  } else {
    preds_plot
  }
}

plot_resids <- function(gam_mod, type = "raw") {
  dat <- gam_mod$mu.coefSmo[[3]]$data
  plot_dat <- dat |> 
    mutate(resid = resid(gam_mod),
           fitted = fitted(gam_mod))
  
  if (type == "raw") {
    ggplot(plot_dat,
           aes(x = fitted,
               y = resid)) +
      geom_hline(yintercept = 0,
                 lty = "dashed") +
      geom_point(alpha = .5) +
      geom_smooth() +
      labs(x = "Fitted values",
           y = "Residuals")
  } else if (type == "qq") {
    ggplot(plot_dat,
           aes(sample = resid)) +
      geom_qq_line() +
      geom_qq(alpha = .5) +
      labs(x = "Theoretical quantiles",
           y = "Sample quantiles")
  } else if (type == "wp") {
    ggplot(plot_dat,
           aes(sample = resid)) +
      geom_hline(yintercept = 0, lty = "dashed") +
      stat_qq_band(detrend = TRUE) +
      stat_qq_point(detrend = TRUE, alpha = .5) +
      # coord_cartesian(ylim = c(-1, 1)) +
      labs(x = "Theoretical quantiles",
           y = "Detrended sample quantiles")
  } else {
    plot_dat
  }
}

plot_qq <- partial(plot_resids, type = "qq")
plot_wp <- partial(plot_resids, type = "wp")

plot_partial <- function(gam_mod, x = "exposure_proportion") {
  dat <- gam_mod$mu.coefSmo[[3]]$data
  
  preds <- lpred(gam_mod, type = "terms", se = TRUE)
  plot_data <- dat |> 
    cbind(preds$fit |> as_tibble() |> 
            `colnames<-`(value = c("part_age", "part_exp", "part_re", "part_age_exp"))) |> 
    cbind(preds$se.fit |> as_tibble() |> 
            `colnames<-`(value = c("se_age", "se_exp", "se_re", "se_age_exp")))
  
  if (x == "age") {
    ggplot(plot_data |> arrange(age), 
           aes(x = age)) +
      geom_ribbon(aes(ymin = part_age - 1.96 * se_age,
                      ymax = part_age + 1.96 * se_age),
                  alpha = .25) +
      geom_line(aes(y = part_age), col = "darkorange") +
      labs(x = "Age",
           y = "Partial effect of age")
  } else if (x == "exposure_proportion") {
    ggplot(plot_data |> arrange(exposure_proportion), 
           aes(x = exposure_proportion)) +
      geom_ribbon(aes(ymin = part_exp - 1.96 * se_exp,
                      ymax = part_exp + 1.96 * se_exp),
                  alpha = .25) +
      geom_line(aes(y = part_exp), col = "darkorange") +
      labs(x = "Exposure proportion",
           y = "Partial effect of exposure proportion")
  } else {
    plot_data
  }
}
