plot_dataset_histograms <- function(dataset){
  dataset |> 
    dplyr::mutate(dataset_id = factor(dataset_id)) |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = forcats::fct_reorder(dataset_id, mean_dataset_rt, .desc = TRUE),
        y = mean_dataset_rt
      )
    ) +
    ggplot2::geom_point(
      size = 3
    ) +
    ggplot2::geom_bar(
      mapping = ggplot2::aes(
        y = mean_dataset_acc
      ),
      color = "black",
      stat = "identity",
      fill = "transparent",
      width = 0.6
    )+
    ggplot2::scale_y_continuous(
      name = "Reaction Time",
      sec.axis = ggplot2::sec_axis(~., name = "Accuracy"),
      breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
      labels = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
    ) +
    ggplot2::labs(
      title = "",
      x = "dataset_id"
    )+
    ggplot2::theme_classic()
}

