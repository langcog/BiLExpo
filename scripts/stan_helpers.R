prepare_stan_data <- function(data_item, desired_lexical_category = NULL, 
                              n_items = NULL, seed = 123, 
                              original_dataset_name = NULL) {
  set.seed(seed)
  
  # Optionally filter to dataset
  if (!is.null(original_dataset_name)) {
    data_item <- data_item |>
      filter(dataset_origin_name == original_dataset_name)
  }
  
  # Optionally filter by lexical category
  if (!is.null(desired_lexical_category)) {
    data_item <- data_item |>
      filter(lexical_category %in% desired_lexical_category)
  }
  
  # Optionally sample a subset of items
  if (!is.null(n_items)) {
    sampled_items <- sample(unique(data_item$item_id), n_items, replace = F)
    data_item <- data_item |>
      filter(item_id %in% sampled_items)
  }
  
  # Keep only needed columns and remove missing values
  data_item <- data_item |>
    select(produces, age, exposure_proportion, child_id, uni_lemma, item_id, language) |>
    na.omit()
  
  # Prepare data list for Stan
  stan_data <- list(
    N = nrow(data_item),
    I = length(unique(data_item$child_id)),
    W = length(unique(data_item$uni_lemma)),
    J = length(unique(data_item$item_id)),
    L = length(unique(data_item$language)),
    y = data_item$produces,
    age = data_item$age,
    exposure = data_item$exposure_proportion,
    child = as.integer(factor(data_item$child_id)),
    word = as.integer(factor(data_item$uni_lemma)),
    item = as.integer(factor(data_item$item_id)),
    lang = as.integer(factor(data_item$language))
  )
  
  return(stan_data)
}

fit_stan_by_lexical_category <- function(data, model_file, model_name = NULL,
                                         output_dir = "models",
                                         categories = NULL, n_items = NULL,
                                         original_dataset_name = NULL,
                                         seed = 123, iter = 4000,
                                         warmup = 1000, chains = 4,
                                         control = list(max_treedepth = 15)) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Get unique lexical categories if not supplied
  if (is.null(categories)) {
    categories <- unique(data$lexical_category)
  }
  
  for (cat in categories) {
    message("Fitting model for lexical category: ", cat)
    
    # Prepare Stan data
    stan_data <- prepare_stan_data(
      data = data,
      desired_lexical_category = cat,
      n_items = n_items,
      original_dataset_name = original_dataset_name,
      seed = seed
    )
    
    # Fit Stan model
    fit <- stan(
      file = model_file,
      data = stan_data,
      model_name = if (is.null(model_name)) paste0("model_", cat) else model_name,
      control = control,
      iter = iter,
      warmup = warmup,
      chains = chains,
      seed = seed
    )
    
    # Save model
    out_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(model_file)), "_fit_", cat, ".rds"))
    saveRDS(fit, file = out_file)
    message("Saved model fit to: ", out_file)
    
    # Clean up
    rm(fit)
    gc()
  }
}


plot_children <- function(posterior_samples) {
  u_child <- posterior_samples$u_child
  mean_u_child <- apply(u_child, 2, mean)
  
  child_df <- data.frame(
    child_id = 1:length(mean_u_child),
    mean_u_child = mean_u_child,
    ci_lower = apply(u_child, 2, quantile, probs = 0.025),
    ci_upper = apply(u_child, 2, quantile, probs = 0.975)
  )
  
  child_df |> mutate(child_id = reorder(child_id, mean_u_child)) |>
    ggplot(aes(x = child_id, y = mean_u_child)) +
    geom_point(color = "blue") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "blue") +
    labs(x = "Child ID", y = "Child Ability", title = "Random Effects for Children") +
    theme_minimal() + coord_flip()
}

plot_items <- function(posterior_samples) {
  u_item <- posterior_samples$u_item
  mean_u_item <- apply(u_item, 2, mean)
  
  item_df <- data.frame(
    item_id = 1:length(mean_u_item),
    mean_u_item = mean_u_item,
    ci_lower = apply(u_item, 2, quantile, probs = 0.025),
    ci_upper = apply(u_item, 2, quantile, probs = 0.975)
  )
  
  item_df |> mutate(item_id = reorder(item_id, mean_u_item)) |>
    ggplot(aes(x = item_id, y = mean_u_item)) +
    geom_point(color = "red") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "red") +
    labs(x = "Item ID", y = "Item Difficulty", title = "Random Effects for Items") +
    theme_minimal() + coord_flip()
}

summarize_exposure_by_age <- function(fit, exposure_param = "beta_exposure", 
                                      age_param = "beta_age",
                                      interaction_param = "beta_age_exposure",
                                      age_range = seq(0.5, 3.0, by = 0.1),
                                      plot = TRUE) {
  # Extract posterior samples
  beta_exp <- rstan::extract(fit, pars = exposure_param)[[1]]
  beta_age <- rstan::extract(fit, pars = age_param)[[1]]
  beta_exp_age <- rstan::extract(fit, pars = interaction_param)[[1]]
  
  if (is.null(beta_exp) || is.null(beta_age) || is.null(beta_exp_age)) {
    stop("One or more specified parameters not found in the model.")
  }
  
  n_draws <- nrow(beta_exp)
  age_effects <- lapply(age_range, function(a) {
    exposure_effect <- beta_exp + a * beta_exp_age
    tibble(
      age = a,
      mean = mean(exposure_effect),
      lower = quantile(exposure_effect, 0.025),
      upper = quantile(exposure_effect, 0.975)
    )
  }) |> bind_rows()
  
  if (plot) {
    p <- ggplot(age_effects, aes(x = age, y = mean)) +
      geom_line(linewidth = 1.2, color = "#3366CC") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#3366CC") +
      labs(
        title = "Estimated Exposure Effect as a Function of Age",
        x = "Age (years)",
        y = "Exposure Effect (posterior mean ± 95% CI)"
      ) +
      theme_minimal(base_size = 14)
    print(p)
  }
  
  return(age_effects)
}


# FixMe
summarize_and_plot_by_lexcat <- function(model_base = "model2_fit",
                                         output_dir = "models",
                                         exposure_param = "beta_exposure", 
                                         item_param = "u_item", 
                                         plot = TRUE) {
  # List all matching .rds files
  files <- list.files(output_dir, pattern = paste0("^", model_base, "_fit_.*\\.rds$"), full.names = TRUE)
  
  # Extract category names from filenames
  categories <- str_extract(files, "(?<=_fit_)[^/]+(?=\\.rds$)")
  
  extract_summary <- function(fit, param, level = NULL) {
    samples <- rstan::extract(fit, pars = param)[[1]]
    if (!is.null(level)) {
      # Multi-dimensional parameter (e.g., vector)
      df <- as.data.frame(samples)
      names(df) <- paste0(param, "[", seq_len(ncol(df)), "]")
      df_long <- df |>
        pivot_longer(everything(), names_to = "param", values_to = "value") |>
        group_by(param) |>
        summarize(mean = mean(value),
                  lower = quantile(value, 0.025),
                  upper = quantile(value, 0.975),
                  .groups = "drop") |>
        mutate(level = seq_along(mean))
    } else {
      df_long <- tibble(
        param = param,
        mean = mean(samples),
        lower = quantile(samples, 0.025),
        upper = quantile(samples, 0.975)
      )
    }
    return(df_long)
  }
  
  all_exposure <- list()
  all_items <- list()
  
  for (i in seq_along(files)) {
    fit <- readRDS(files[i])
    lexcat <- categories[i]
    
    exposure_df <- extract_summary(fit, exposure_param)
    exposure_df$lexical_category <- lexcat
    
    item_df <- extract_summary(fit, item_param, level = "item")
    item_df$lexical_category <- lexcat
    
    all_exposure[[i]] <- exposure_df
    all_items[[i]] <- item_df
    
    rm(fit)
    gc()
  }
  
  exposure_summary <- bind_rows(all_exposure)
  item_summary <- bind_rows(all_items)
  
  if (plot) {
    p1 <- ggplot(exposure_summary, aes(x = lexical_category, y = mean)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
      labs(title = "Exposure Effect by Lexical Category",
           y = "Posterior Mean ± 95% CI", x = "Lexical Category") +
      theme_minimal()
    
    p2 <- ggplot(item_summary, aes(x = lexical_category, y = mean)) +
      stat_summary(fun.data = mean_cl_normal, geom = "pointrange") +
      labs(title = "Average Item Difficulty by Lexical Category",
           y = "Item Difficulty (mean ± CI)", x = "Lexical Category") +
      theme_minimal()
    
    print(p1)
    print(p2)
  }
  
  return(list(
    exposure_summary = exposure_summary,
    item_summary = item_summary
  ))
}