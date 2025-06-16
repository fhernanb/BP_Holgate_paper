library(MultivDists)

# Figure 1
X1 <- 0:10
X2 <- 0:10
data <- expand.grid(X1=X1, X2=X2)
data$Prob <- dBP_HOL(x=data, l1=3, l2=4, l0=1)
data$X1 <- factor(data$X1)
data$X2 <- factor(data$X2)

library(ggplot2)
p1 <- ggplot(data, aes(X1, X2, fill=Prob)) +
  geom_tile() +
  scale_fill_gradient(low="darkgreen", high="pink")

p1

ggsave(filename="Figures/heat_map_1.pdf", plot=p1,
       height=5, width=6)

# Figure 2
X1 <- 0:10
X2 <- 0:10
data <- expand.grid(X1=X1, X2=X2)
data$Prob <- dZIBP_HOL(x=data, l1=3, l2=4, l0=1, psi=0.15)
data$X1 <- factor(data$X1)
data$X2 <- factor(data$X2)

library(ggplot2)
p2 <- ggplot(data, aes(X1, X2, fill=Prob)) +
  geom_tile() +
  scale_fill_gradient(low="darkgreen", high="yellow")

p2

ggsave(filename="Figures/heat_map_2.pdf", plot=p2,
       height=5, width=6)


