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
