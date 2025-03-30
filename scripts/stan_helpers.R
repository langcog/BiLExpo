
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
