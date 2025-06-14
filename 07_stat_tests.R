library(tidyverse)


# t-тест (продолжение) ----------------------------------------------------


heroes <- read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/refs/heads/master/data/heroes_information.csv",
                   na = c("NA", "-", "-99", " "))
diet <- read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/master/data/stcp-Rdataset-Diet.csv")

diet2 <- diet %>%
  filter(Diet == 2)

t.test(diet2$pre.weight, diet2$weight6weeks, paired = TRUE)
mean(diet2$pre.weight)
mean(diet2$weight6weeks)

diet3 <- diet %>%
  filter(Diet == 3)

t.test(diet3$pre.weight, diet3$weight6weeks, paired = TRUE)

diet12 <- diet %>%
  filter(Diet %in% 1:2)

t.test(diet12$weight6weeks ~ diet12$Diet)
t.test(weight6weeks ~ Diet, data = diet12)
t.test(weight6weeks ~ Diet, data = diet12, var.equal = TRUE)

diet13 <- diet %>%
  filter(Diet %in% c(1,3))
t.test(weight6weeks ~ Diet, data = diet13)

heroes_white_black <- heroes %>%
  filter(`Eye color` %in% c("white", "black"))
heroes_white_black
t.test(Weight ~ `Eye color`, data = heroes_white_black)

# нормальность ------------------------------------------------------------


set.seed(42)
samp <- rnorm(100, mean = 100, sd = 15)
samp
shapiro.test(samp)

diet2 <- diet %>%
  filter(Diet == 2)
hist(diet2$weight6weeks, breaks = 10)
hist(rnorm(length(diet2$weight6weeks)), breaks = 10)

qqnorm(diet2$weight6weeks)
qqnorm(rnorm(length(diet2$weight6weeks)))
weight <- heroes %>% drop_na(Weight) %>% pull(Weight)
hist(weight, breaks = 30)
hist(rnorm(length(weight)))

qqnorm(weight)
qqnorm(rnorm(length(weight)))

diet %>%
  group_by(Diet) %>%
  summarise(m = mean(weight6weeks),
            sd = sd(weight6weeks))


# непараметрические аналоги t-теста ---------------------------------------


t.test(diet1$pre.weight, diet1$weight6weeks, paired = TRUE)
wilcox.test(diet1$pre.weight, diet1$weight6weeks, paired = TRUE)

diet12 <- diet %>%
  filter(Diet %in% 1:2)

t.test(weight6weeks ~ Diet, data = diet12)
wilcox.test(weight6weeks ~ Diet, data = diet12)

wilcox.test(diet2$pre.weight, diet2$weight6weeks, paired = TRUE)
wilcox.test(diet3$pre.weight, diet3$weight6weeks, paired = TRUE)

wilcox.test(weight6weeks ~ Diet, data = diet13)


new_diet <- tribble(
  ~student, ~before_r, ~after_r,
  "Alex", 82, 74,
  "Ben", 90, 84,
  "ГЕННАДИЙ", 73, 71
)
t.test(new_diet$before_r, new_diet$after_r, paired = TRUE, alternative = "greater")
wilcox.test(new_diet$before_r, new_diet$after_r, paired = TRUE, alternative = "greater")
wilcox.test(new_diet$before_r, new_diet$after_r, paired = TRUE)

# тест хи-квадрат на независимость ----------------------------------------


sum(rnorm(4)^2)
?dchisq

gender_publisher <- heroes %>%
  select(Gender, Publisher) %>%
  drop_na() %>%
  filter(Publisher %in% c("Marvel Comics", "DC Comics"))

gender_publisher
gender_publisher %>%
  count(Gender, Publisher)
str(table(gender_publisher))

table(gender_publisher)
61/(61 + 153)
111/(111 + 252)

61/(61 + 111)
153/(153 + 252)

summary(table(gender_publisher))
gender_publisher %>%
  table() %>%
  summary()

chisq.test(table(gender_publisher))
gender_publisher %>%
  table() %>%
  chisq.test(correct = FALSE)
gender_publisher %>%
  table() %>%
  fisher.test()

mosaicplot(table(gender_publisher), shade = TRUE, color = TRUE)

pub_good <- heroes %>%
  filter(Alignment %in% c("good", "bad")) %>%
  select(Alignment, Gender) %>%
  drop_na()

pub_good
chisq.test(table(pub_good))
mosaicplot(table(pub_good), shade = TRUE, color = TRUE)

heroes %>%
  mutate(is_human = Race == "Human") %>%
  drop_na(Gender, is_human) %>%
  group_by(is_human) %>%
  summarise(mean(Gender == "Female"))
  
heroes %>%
  mutate(is_human = Race == "Human") %>%
  select(Gender, is_human) %>%
  drop_na() %>%
  table() %>%
  fisher.test()
  
# Ковариация и корреляция -------------------------------------------------

install.packages("Stat2Data")
library(Stat2Data)
data(Backpack)

back <- Backpack %>%
  mutate(backpack_kg = BackpackWeight * 0.453592,
         body_kg = BodyWeight * 0.453592)

back %>%
  select(backpack_kg, body_kg) %>%
  cov()

cov(back$backpack_kg, back$body_kg)

back %>%
  select(backpack_kg, body_kg) %>%
  var()

back %>%
  select(backpack_kg, body_kg) %>%
  cor()

6.601943/(sqrt(6.838322) * sqrt(177.807410))
cor_result <- cor.test(back$backpack_kg, back$body_kg)
str(cor_result)
class(cor_result)
cor_result$p.value

cor.test(back$backpack_kg, back$body_kg, method = "spearman")
cor.test(back$backpack_kg, back$body_kg, method = "kendall")

plot(back$backpack_kg, back$body_kg)

back %>%
  select(backpack_kg, body_kg) %>%
  cov()

back %>%
  select(BackpackWeight, BodyWeight) %>%
  cov()

back %>%
  select(backpack_kg, body_kg) %>%
  cor()

back %>%
  select(BackpackWeight, BodyWeight) %>%
  cor()

cor.test(heroes$Weight, heroes$Height)
cor.test(heroes$Weight, heroes$Height, method = "spearman")
cor.test(heroes$Weight, heroes$Height, method = "kendall")
plot(heroes$Weight, heroes$Height)

heroes %>%
  filter(Height > 600) %>% View()

heroes_clean <- heroes %>%
  filter(Height < 600)

cor.test(heroes_clean$Weight, heroes_clean$Height)
cor.test(heroes_clean$Weight, heroes_clean$Height, method = "spearman")
cor.test(heroes_clean$Weight, heroes_clean$Height, method = "kendall")

