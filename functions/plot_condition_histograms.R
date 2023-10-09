plot_condition_histograms <- function(plot_list){
  n_plots = length(plot_list)

  colors = RColorBrewer::brewer.pal(n_plots, "Set1")
  for (i in 1:n_plots){
    plot(x = plot_list[[i]]$mids,
         y = plot_list[[i]]$density,
         type = "l",
         main = paste("RT - distribution of different conditions"),
         xlab = "response time (in s)",
         ylab = "density",
         xlim = c(0, 2),
         col = colors[i])
  }

  legend("topright",
         legend = paste("condition", 1:n_plots),
         col = c(colors))
}

