plot_trial_rtdist <- function(detailed, dataset_id, rt_cutoff = 2){
  conditions = detailed$trial_data |>
    dplyr::filter(dataset_id == {{dataset_id}}) |>
    distinct(condition_id)
  
  for (condition_id in conditions$condition_id){
    conditions$mean_acc[conditions$condition_id == condition_id] = round(mean(detailed$trial_data$accuracy[which(detailed$trial_data$condition_id == condition_id)], na.rm = TRUE), 3)
  }
  
  mean_accuracy_labels = paste0(conditions$condition_id, "; mean_acc = ", conditions$mean_acc)
  
  
  detailed$trial_data |> 
    dplyr::filter(rt < rt_cutoff) |> 
    dplyr::filter(dataset_id == {{dataset_id}}) |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = rt,
        fill = factor(condition_id)
      )
    )+
    ggplot2::geom_density(alpha = 0.5)+
    ggplot2::labs(
      x = "Response Time (in s)",
      y = "density",
      fill = "condition_id",
      title = paste0(
        "Response Time Distribuion for dataset_id: ",
        dataset_id
      ),
      caption = "Find more information about the condition_id in the respective table."
    )+
    ggplot2::scale_fill_hue(labels = mean_accuracy_labels)+
    ggplot2::theme_classic()
}
