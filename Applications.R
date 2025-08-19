library(MultivDists)

library(AER)
data("NMES1988")

# To obtain some statistics as in the paper
y1 <- NMES1988$nvisits
y2 <- NMES1988$novisits

cor(y1, y2)
cor.test(y1, y2)
cov(y1, y2)
mean(y1==0 & y2==0)

# 
tbl <- table(y1, y2)
df <- as.data.frame(tbl)
df$Freq <- df$Freq / nrow(NMES1988)
ind <- df$Freq > 0.001
datos <- df[ind, ]

library(ggplot2)
p1 <- ggplot(datos, aes(y1, y2, fill=Freq)) +
  geom_tile() +
  scale_fill_gradient(low="darkgreen", high="pink")

p1


# Model as in section 5.1 from Geoffroy et. al (2021).
mod3 <- NULL
mod3 <- marZIBP_HOL(mu1.fo=nvisits ~health+chronic+age+gender+
                      married+school+income+medicaid,
                    mu2.fo=novisits~health+chronic+age+gender+
                        married+school+income+medicaid,
                    psi.fo=~chronic+gender+school+medicaid,
                    data=NMES1988,
                    optimizer="optim")

mod3 <- ZIBP_HOL(l1.fo=nvisits ~health+chronic+age+gender+
                      married+school+income+medicaid,
                 l2.fo=novisits~health+chronic+age+gender+
                      married+school+income+medicaid,
                 psi.fo=~chronic+gender+school+medicaid,
                 data=NMES1988)

summary(mod3)

