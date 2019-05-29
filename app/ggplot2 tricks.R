#Discovering ggplot2 default palette
df <- data.frame(x = 1:2, y = 1:2, color = c("a", "b"))
df
p <- ggplot(df, aes(x = x, y = y, color = color)) +
  geom_point(size = 4)
p

ggplot_build(p)$data
