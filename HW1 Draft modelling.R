set.seed(450) 
n <- 200
x1 <- runif(n)
x2 <- runif(n)
d <- data.frame(x1,x2)
d
error <- rnorm(n, mean = 0, sd = 0.5)
y <- x1 + x1*x1 + x2 +x2*x2 + error
summary(y)
hist(y)

y_new <- exp(y)/(1+exp(y))
hist(y_new)
y_new_d <- data.frame(y_new)
y_new_d
success = 0.5
y_new_d$success <- y_new <= 0.5 
y_new_d
y_new_d$id <- seq(200)
y_new_d

library(ggplot2)
ggplot(data = y_new_d) + 
  geom_point(mapping = aes(y = y_new, x = id, color = success))