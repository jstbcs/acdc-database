plot_dataset_histograms <- function(dataset, order_by = "Dataset ID"){
  acc_plot <- dataset |> 
    dplyr::mutate(dataset_id = factor(`Dataset ID`)) |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = forcats::fct_reorder(dataset_id, !!sym(paste(rlang::get_expr(rlang::enquo(order_by)))), .desc = TRUE),
        y = `Mean accuracy (dataset)`
      )
    ) +
    ggplot2::geom_point(
      size = 3
    ) +
    ggplot2::labs(
      title = "Mean Accuracy",
      x = "dataset_id",
      caption = "Values are based on the 'mean_dataset_acc' column."
    )+
    ggplot2::theme_classic()
  
  rt_plot <- dataset |> 
    dplyr::mutate(dataset_id = factor(`Dataset ID`)) |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = forcats::fct_reorder(dataset_id, !!sym(paste(rlang::get_expr(rlang::enquo(order_by)))), .desc = TRUE),
        y = `Mean reaction time (dataset)`
      )
    ) +
    ggplot2::geom_point(
      size = 3
    ) +
    ggplot2::labs(
      title = "Mean Response Time (in s)",
      x = "dataset_id",
      caption = "Values are based on the 'mean_dataset_rt' column."
    )+
    ggplot2::theme_classic()
  
  ggpubr::ggarrange(acc_plot, rt_plot, ncol = 1, nrow = 2)
}

