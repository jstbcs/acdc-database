plot_condition_histograms <- function(dataset, rt_cutoff = 2){
  dataset |> 
    dplyr::filter(rt < rt_cutoff) |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = rt,
        fill = factor(condition_id),
        group = factor(condition_id)
      )
    ) +
    ggplot2::geom_histogram(
      
    ) +
    ggplot2::scale_color_brewer(palette = "Set1")
}

