
# Function to simulate datasets -------------------------------------------
library(MultivDists)

gen_data_ZIBP_HOL <- function(n=100) {
  # To generate the covariates
  x1 <- runif(n=n)
  x2 <- runif(n=n)
  # To generate the lambdas
  l1 <- exp(1 + 1.3 * x1)
  l2 <- exp(1 + 2.1 * x2)
  # The third lambda
  l0 <- 1
  # To generate the psi
  logit_inv <- function(x) exp(x) / (1+exp(x))
  psi <- logit_inv(-2.4 + 2.1 * x2)
  # To generate Y1 and Y2
  y <- NULL
  for (i in 1:n)
    y <- rbind(y, rZIBP_HOL(n=1, l1=l1[i], l2=l2[i],
                            l0=l0, psi=psi[i]))
  # To create the dataset
  dataset <- data.frame(y1=y[,1], y2=y[,2],
                        x1=x1, x2=x2,
                        l0=l0, psi=psi,
                        l1=l1, l2=l2)
  return(dataset)
}


# To perform the simulation -----------------------------------------------

library("parSim")

parSim(
  ### SIMULATION CONDITIONS
  n = seq(from=10, to=100, by=10),
  
  reps = 10,                 # Number of repetitions
  write = TRUE,             # Writing to a file
  name = "Simulation/Results_sim1_05",  # Name of the file
  nCores = 1,               # Number of cores to use
  
  expression = {

    # Generate data:
    dat <- gen_data_ZIBP_HOL(n=n)
    
    # Run analysis:
    mod <- ZIBP_HOL(l1.fo=y1~x1,
                    l2.fo=y2~x2,
                    psi.fo=~x2,
                    data=dat)
    
    # Store coefficients:
    b0_hat <- mod$par[1]
    b1_hat <- mod$par[2]
    g0_hat <- mod$par[3]
    g1_hat <- mod$par[4]
    d0_hat <- mod$par[5]
    d1_hat <- mod$par[6]
    l0_hat <- mod$par[7]
    
    # Results list:
    Results <- list(
      b0_hat = b0_hat,
      b1_hat = b1_hat,
      g0_hat = g0_hat,
      g1_hat = g1_hat,
      d0_hat = d0_hat,
      d1_hat = d1_hat,
      l0_hat = l0_hat
    )
    
    # Return:
    Results
  }
)

# To load the results -----------------------------------------------------

archivos <- list.files(pattern = "^Results_sim1.*\\.txt$", 
                       path="Simulation",
                       full.names = TRUE)
lista_datos <- lapply(archivos, read.table, header = TRUE, 
                      sep = "", stringsAsFactors = FALSE)
datos <- do.call(rbind, lista_datos)

# Percentage of errors
prop.table(table(datos$error))

# Filter only row without error messages
datos <- datos[!datos$error, ]

# To analize the results --------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

trim <- 0.10

# True parameter values
b0 <- 1
b1 <- 1.3
g0 <- 1
g1 <- 2.1
d0 <- -2.4
d1 <- 2.1
l0 <- 1

datos %>% group_by(n) %>% 
  summarise(nobs = n())

dat <- datos %>% group_by(n) %>% 
  summarise(nobs = n(),
            mean_b0 = mean(b0_hat, na.rm=TRUE, trim=trim),
            mean_b1 = mean(b1_hat, na.rm=TRUE, trim=trim),
            mean_g0 = mean(g0_hat, na.rm=TRUE, trim=trim),
            mean_g1 = mean(g1_hat, na.rm=TRUE, trim=trim),
            mean_d0 = mean(d0_hat, na.rm=TRUE, trim=trim),
            mean_d1 = mean(d1_hat, na.rm=TRUE, trim=trim),
            mean_l0 = mean(l0_hat, na.rm=TRUE, trim=trim),
            mse_b0 = mean((b0_hat - b0)^2),
            mse_b1 = mean((b1_hat - b1)^2),
            mse_g0 = mean((g0_hat - g0)^2),
            mse_g1 = mean((g1_hat - g1)^2),
            mse_d0 = mean((d0_hat - d0)^2),
            mse_d1 = mean((d1_hat - d1)^2),
            mse_l0 = mean((l0_hat - l0)^2)            
  )

dat

# Mean -----------------------------------------------------
p1 <- ggplot(data=dat, aes(x=n, y=mean_b0)) + 
  geom_line() + 
  labs(x="n", y=expression(hat(beta)[0])) +
  geom_line(y=b0, col='red', lty='dashed') + 
  theme(panel.background = element_rect(fill = "white", colour = "black"))

p2 <- ggplot(data=dat, aes(x=n, y=mean_b1)) + 
  geom_line() +
  labs(x="n", y=expression(hat(beta)[1])) +
  geom_line(y=b1, col='red', lty='dashed') + 
  theme(panel.background = element_rect(fill = "white", colour = "black"))

p3 <- ggplot(data=dat, aes(x=n, y=mean_g0)) + 
  geom_line() + 
  labs(x="n", y=expression(hat(gamma)[0])) +
  geom_line(y=g0, col='red', lty='dashed') + 
  theme(panel.background = element_rect(fill = "white", colour = "black"))

p4 <- ggplot(data=dat, aes(x=n, y=mean_g1)) + 
  geom_line() + 
  labs(x="n", y=expression(hat(gamma)[1])) +
  geom_line(y=g1, col='red', lty='dashed') + 
  theme(panel.background = element_rect(fill = "white", colour = "black"))

p5 <- ggplot(data=dat, aes(x=n, y=mean_d0)) + 
  geom_line() + 
  labs(x="n", y=expression(hat(delta)[0])) +
  geom_line(y=d0, col='red', lty='dashed') + 
  theme(panel.background = element_rect(fill = "white", colour = "black"))

p6 <- ggplot(data=dat, aes(x=n, y=mean_d1)) + 
  geom_line() + 
  labs(x="n", y=expression(hat(delta)[1])) +
  geom_line(y=d1, col='red', lty='dashed') + 
  theme(panel.background = element_rect(fill = "white", colour = "black"))

p7 <- ggplot(data=dat, aes(x=n, y=mean_l0)) + 
  geom_line() + 
  labs(x="n", y=expression(hat(lambda)[0])) +
  geom_line(y=l0, col='red', lty='dashed') + 
  theme(panel.background = element_rect(fill = "white", colour = "black"))

mean1 <- grid.arrange(p1, p2, p3, p4, p5, p6, p7,
                      nrow=4, ncol=2)
mean1

ggsave(filename="Figures/mean1.pdf", plot=mean1,
       width=8, height=10)

# MSE -----------------------------------------------------
q1 <- ggplot(data=dat, aes(x=n, y=mse_b0)) + 
  geom_line() + 
  labs(x = "n", y = expression("MSE " * hat(beta[0]))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))

q2 <- ggplot(data=dat, aes(x=n, y=mse_b1)) + 
  geom_line() +
  labs(x = "n", y = expression("MSE " * hat(beta[1]))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))

q3 <- ggplot(data=dat, aes(x=n, y=mse_g0)) + 
  geom_line() + 
  labs(x = "n", y = expression("MSE " * hat(gamma[0]))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))

q4 <- ggplot(data=dat, aes(x=n, y=mse_g1)) + 
  geom_line() + 
  labs(x = "n", y = expression("MSE " * hat(gamma[1]))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))

q5 <- ggplot(data=dat, aes(x=n, y=mse_d0)) + 
  geom_line() + 
  labs(x = "n", y = expression("MSE " * hat(delta[0]))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))

q6 <- ggplot(data=dat, aes(x=n, y=mse_d1)) + 
  geom_line() + 
  labs(x = "n", y = expression("MSE " * hat(delta[1]))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))

q7 <- ggplot(data=dat, aes(x=n, y=mse_l0)) + 
  geom_line() + 
  labs(x = "n", y = expression("MSE " * hat(lambda[0]))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"))

mse1 <- grid.arrange(q1, q2, q3, q4, q5, q6, q7,
                      nrow=4, ncol=2)
mse1

ggsave(filename="Figures/mse1.pdf", plot=mse1,
       width=8, height=10)
