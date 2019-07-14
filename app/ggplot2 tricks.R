#Discovering ggplot2 default palette
library(ggplot2)
df <- data.frame(x = 1:2, y = 1:2, color = c("a", "b"))
df
p <- ggplot(df, aes(x = x, y = y, color = color)) +
  geom_point(size = 4)
p

ggplot_build(p)$data


ip <- as.data.frame(installed.packages()[,c(1,3:4)])
rownames(ip) <- NULL
ip <- ip[is.na(ip$Priority),1:2,drop=FALSE]
print(ip, row.names=FALSE)
