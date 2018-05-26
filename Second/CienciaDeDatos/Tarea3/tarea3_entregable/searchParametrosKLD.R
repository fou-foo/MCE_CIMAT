library(plotly)
#saveRDS(u, 'C:\\Users\\fou-f\\Desktop\\w_KLD.Rdd')
#u <- readRDS('C:\\Users\\fou-f\\Desktop\\w_KLD.Rdd')
p <- plot_ly(
  u, x = ~train, y = ~test,
  # Hover text:
  text = ~paste("mu: ", mu, '<br> sigma:', sigma),
  color = ~sigma, size = ~mu
)
p


