dnorm(0)
dnorm(1)
dnorm(-1)

vec <- seq(-3, 3, by = .1)
vec
dnorm(vec)
plot(vec, dnorm(vec))

iq <- seq(50, 150, by = .1)
plot(iq, dnorm(iq, mean = 100, sd = 15))

pnorm(0)
pnorm(1)
pnorm(2)

pnorm(100, mean = 100, sd = 15)
1 - pnorm(130, mean = 100, sd = 15)
pnorm(90, mean = 100, sd = 15)

plot(iq, pnorm(iq, mean = 100, sd = 15))
p <- seq(0, 1, by = .01)
plot(p, qnorm(p, mean = 100, sd = 15))

set.seed(42)
samp <- rnorm(100, mean = 100, sd = 15)
samp
hist(samp)
hist(rnorm(15, mean = 100, sd = 15))

mean(samp)
sqrt(sum((samp - mean(samp))^2)/(length(samp)-1) )
sd(samp)

#CI95%     
set.seed(42)
many_means <- replicate(10000000, mean(rnorm(100, mean = 100, sd = 15)))
many_means
hist(many_means)
mean(many_means)

sd(many_means)

samp
se_est <- sd(samp)/sqrt(length(samp))
mean(samp) - qnorm(0.975) * se_est
mean(samp) + qnorm(0.975) * se_est

hist(rlnorm(100000), breaks = 100)

hist(rlnorm(100))
many_means_lognorm <- replicate(1000000, mean(rlnorm(100)))
hist(many_means_lognorm, breaks = 100)

size <- 200
plot(0:size, dbinom(0:size, size, prob = 0.5), type = "l")
 
hist(rnorm(1000000))

mu <- 100
sigma <- 15
z_emp <- (mean(samp) - mu)/(sigma/sqrt(length(samp)))
z_emp

1 - pnorm(z_emp)
(1 - pnorm(z_emp)) * 2

# t-test ------------------------------------------------------------------

t_emp <- (mean(samp) - mu)/(sd(samp)/sqrt(length(samp)))

t.test(samp, mu = 100)

t_emp
(1 - pt(t_emp, df = length(samp) - 1)) * 2


library(tidyverse)
heroes <- read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/refs/heads/master/data/heroes_information.csv",
                   na = c("NA", "-", "-99", " "))

t.test(heroes$Weight, mu = 100)
t.test(heroes$Height, mu = 185)

#H0: mu1 = mu2 (mu1 - mu2 = 0)
#H1: mu1 != mu2 (mu1 - mu2 != 0)


diet <- readr::read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/master/data/stcp-Rdataset-Diet.csv")
diet1 <- diet %>%
  filter(Diet == 1)

mean(diet1$weight6weeks - diet1$pre.weight)

options(scipen = 999)
t.test(diet1$pre.weight, diet1$weight6weeks, paired = TRUE)
#t.test(diet1$weight6weeks - diet1$pre.weight, mu = 0)

diet2 <- diet %>%
  filter(Diet == 2)

# aov(back$backpack_kg ~ back$BackProblems) %>% summary()
# t.test(back$backpack_kg ~ back$BackProblems, var.equal = TRUE) 

