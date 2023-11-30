plot_dataset_histograms <- function(dataset, order_by = "Dataset ID"){
  dataset |> 
    dplyr::mutate(dataset_id = factor(`Dataset ID`)) |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = forcats::fct_reorder(dataset_id, {{order_by}}, .desc = TRUE),
        y = `Mean reaction time (dataset)`
      )
    ) +
    ggplot2::geom_point(
      size = 3
    ) +
    ggplot2::geom_bar(
      mapping = ggplot2::aes(
        y = `Mean accuracy (dataset)`
      ),
      color = "black",
      stat = "identity",
      fill = "transparent",
      width = 0.6
    )+
    ggplot2::scale_y_continuous(
      name = "Response Time (in s)",
      sec.axis = ggplot2::sec_axis(~., name = "Accuracy"),
      breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
      labels = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
    ) +
    ggplot2::labs(
      title = "Mean Response Time and Accuracy",
      x = "dataset_id",
      caption = "Dots represent response time, bars indicate accuracy.\nValues are based on the 'mean_dataset_rt' and 'mean_dataset_acc' column."
    )+
    ggplot2::theme_classic()
}

