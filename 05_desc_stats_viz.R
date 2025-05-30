library(tidyverse)
heroes <- read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/refs/heads/master/data/heroes_information.csv",
                   na = c("NA", "-", "-99", " "))
options(scipen = 999)
mean(c(rep(50000, 10), 2000000))
v <- c(2, 3, 5, 8, 17)
median(c(v, 100))

weight <- heroes %>%
  drop_na(Weight) %>%
  pull(Weight)

mean(weight)
median(weight)

mean(c(weight, 100500))
median(c(weight, 100500))

table(heroes$Gender)
heroes %>%
  count(Race, sort = TRUE)

?mean()

mean(weight, trim = .1)
mean(weight, trim = 0.5)
median(weight)
mean(weight)


# Меры рассеяния ----------------------------------------------------------

diff(range(weight))
IQR(weight)

median(abs(weight - median(weight))) * 1.4826
mad(weight)

sum((weight - mean(weight))^2)/(length(weight) - 1)
var(weight)

sqrt(sum((weight - mean(weight))^2)/(length(weight) - 1))

(weight - mean(weight))/sd(weight)
c(scale(weight))

install.packages("psych")
psych::skew(weight)
psych::kurtosi(weight)
quantile(weight)

summary(weight)
summary(heroes)
psych::describe(weight)

heroes %>%
  group_by(Gender) %>%
  summarise(psych::describe(Weight))

install.packages("skimr")
skimr::skim(weight)
heroes %>%
  skimr::skim()

heroes %>%
  group_by(Gender) %>%
  skimr::skim(ends_with("color"))


# xxx ---------------------------------------------------------------------

xxx <- read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/refs/heads/master/data/d.csv")
xxx
skimr::skim(xxx)
var(xxx$x)
var(xxx$y)
psych::skew(xxx$x)
psych::skew(xxx$y)
psych::kurtosi(xxx$x)
psych::kurtosi(xxx$y)
IQR(xxx$x)
IQR(xxx$y)
mad(xxx$x)
mad(xxx$y)
psych::describe(xxx)
cor(xxx$x, xxx$y)
xxx
plot(xxx)

# Визуализация в base r ---------------------------------------------------

plot(xxx$x, xxx$y)
plot(weight)
plot(iris %>% select(!Species))
plot(iris)
length(weight)
hist(weight, breaks = 100)
boxplot(Weight ~ Gender, data = heroes)


# ggplot2 -----------------------------------------------------------------

ggplot(data = heroes, aes(x = Height, y = Weight)) +
  geom_point(aes(colour = Weight, size = Weight), alpha = .3) +
  geom_smooth(method = "lm",
              colour = "#177756") +
  #scale_y_log10() +
  scale_colour_viridis_c() +
  facet_wrap(~Gender, ncol = 1) +
  labs(title = "Weight vs. Height of Superheroes",
       subtitle = "Superheroes are heavy",
       x = "Height, cm",
       y = "Weight, kg") +
  hrbrthemes::theme_ipsum()

ggplot() +
  geom_histogram(data = heroes, aes(x = Weight)) +
  theme_minimal()

ggplot() +
  geom_density(data = heroes, aes(x = Weight), fill = "#33BB66")

heroes %>%
  drop_na(Gender) %>%
  ggplot(aes(x = Gender, y = Weight, fill = Gender)) +
  geom_violin() +
  geom_boxplot(fill = "white", 
               width = .1,
               outlier.shape = NA) +
  facet_wrap(~Alignment)

ggplot() +
  geom_point(data = heroes, aes(x = Gender, y = Weight),
             position = position_jitter(height = 0, width = .25), alpha = .75) +
  geom_boxplot(data = heroes, aes(x = Gender, y = Weight), 
               width = .45, alpha = .7,
               outlier.shape = NA)


heroes_gender_count <- heroes %>%
  count(Gender)

ggplot() +
  geom_col(data = heroes_gender_count, aes(x = Gender, y = n, fill = Gender)) +
  coord_flip()

ggplot() +
  geom_bar(data = heroes, aes(x = "", fill = Alignment),
           position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  coord_polar(theta = "y")


gg_bar_figure <- ggplot() +
  geom_bar(data = heroes, aes(x = Gender, fill = Alignment),
           position = "dodge")

ggsave("gg_bar_figure.png", gg_bar_figure, scale = 2.3)
class(gg_bar_figure)
