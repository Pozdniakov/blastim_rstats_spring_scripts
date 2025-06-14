library(tidyverse)
library(Stat2Data)
data(Backpack)

back <- Backpack %>%
  mutate(backpack_kg = BackpackWeight * 0.453592,
         body_kg = BodyWeight * 0.453592)

cor.test(back$backpack_kg, back$BackProblems)
t.test(back$backpack_kg ~ back$BackProblems, var.equal = TRUE)


# Множественные корреляции ------------------------------------------------


back %>%
  select(body_kg, backpack_kg, Units, Year) %>%
  cor()

back %>%
  select(body_kg, backpack_kg, Units, Year) %>%
  psych::corr.test()

rnorm(1000) %>%
  matrix(ncol = 10) %>% 
  as.data.frame() %>%
  psych::corr.test()

(10 ^ 2 - 10)/2 * 0.05

back %>%
  select(body_kg, backpack_kg, Units, Year) %>%
  psych::corr.test(adjust = "bonferroni")

back %>%
  select(body_kg, backpack_kg, Units, Year) %>%
  psych::corr.test()
p_vec <- seq(0.0001, 0.06, length.out = 10)
p.adjust(p_vec)
p.adjust(p_vec, method = "bonferroni")

back %>%
  select(body_kg, backpack_kg, Units, Year) %>%
  psych::corr.test(adjust = "BH")
p.adjust(p_vec, method = "BH")

install.packages("corrplot")
mtcars
library(corrplot)
corrplot(cor(mtcars), method = "color", order = "hclust")

mtcars_cors <- psych::corr.test(mtcars)
str(mtcars_cors)
class(mtcars_cors$r)
mtcars_cors$p

corrplot(corr = mtcars_cors$r, p.mat = mtcars_cors$p,
         method = "color", order = "hclust")
names(mtcars_cors)


# Линейная регрессия ------------------------------------------------------

cor.test(back$body_kg, back$backpack_kg)

model <- lm(backpack_kg ~ body_kg, data = back)
model
str(model)
model$coefficients

ggplot(data = back, aes(x = body_kg, y = backpack_kg)) +
  geom_point() +
  geom_abline(intercept = model$coefficients[1],
              slope = model$coefficients[2])

predict(model, newdata = data.frame(body_kg = 0))
model
summary(model)

back$fitted <- fitted(model)
back$residuals <- residuals(model)
options(scipen = 999)
mean(back$residuals)

summary(model)
plot(model)
tss <- sum((back$backpack_kg - mean(back$backpack_kg))^2)
rss <- sum(back$residual ^ 2)
1 - rss/tss

cor.test(back$body_kg, back$backpack_kg)$estimate ^ 2

multi_model <- lm(backpack_kg ~ body_kg + Units, data = back)
summary(multi_model)
install.packages("car")
car::vif(multi_model)

multi_model2 <- lm(backpack_kg ~ body_kg + Units + Year + BackProblems, data = back)
car::vif(multi_model2)
summary(model)
summary(multi_model)
summary(multi_model2)
lm(backpack_kg ~ body_kg + Units + Year, data = back) %>%
  summary()

# Дисперсионный анализ ----------------------------------------------------

# ANalysis Of VAriance (ANOVA)

diet <- read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/master/data/stcp-Rdataset-Diet.csv")
diet <- diet %>%
  mutate(weight.loss = weight6weeks - pre.weight,
         Dietf = factor(Diet, labels = LETTERS[1:3]),
         Person = factor(Person),
         age_group = if_else(Age >= 40, "старшие", "младшие"))

aov_model <- aov(weight.loss ~ Dietf, diet)
aov_model
summary(aov_model)

lm(weight.loss ~ Dietf, diet) %>% summary()

summary(aov_model)
pairwise.t.test(diet$weight.loss, diet$Dietf)
# pairwise.t.test(diet$weight.loss, diet$Dietf,
#                 p.adjust.method = "none")

pairwise.t.test(diet$weight.loss, 
                diet$Dietf, 
                pool.sd = FALSE)

TukeyHSD(aov_model)

hist(residuals(aov_model))
hist(rnorm(length(residuals(aov_model))))
diet %>%
  group_by(Dietf) %>%
  summarise(m = mean(weight.loss), 
            sd = sd(weight.loss))

#car::leveneTest(diet$weight.loss, diet$Dietf)

kruskal.test(weight.loss ~ Dietf, data = diet)

#3x2 ANOVA: 2 factors
#1 factor: 3 levels
#2 factor: 2 levels

aov(weight.loss ~ Dietf * gender, data = diet) %>%
  summary()

diet %>%
  drop_na(gender) %>%
  mutate(gender = factor(gender, labels = c("ж", "м"))) %>%
  group_by(gender, Dietf) %>%
  summarise(m_loss = mean(weight.loss),
            sd_loss = sd(weight.loss),
            se = sd_loss/sqrt(n())) %>%
  ggplot(aes(x = Dietf, colour = gender, y = m_loss)) +
  geom_line(aes(group = gender), linewidth = 1.6) +
  geom_pointrange(aes(ymin = m_loss - se, ymax = m_loss + se)) +
  theme_minimal()

aov(weight.loss ~ Dietf * gender * age_group, data = diet) %>%
  summary()
#3x2x2 ANOVA

install.packages("ez")
library(ez)
diet_long <- diet %>%
  pivot_longer(cols = c(pre.weight, weight6weeks), 
               names_to = "time",
               values_to = "weight")

diet_long_c <- diet_long %>%
  filter(Dietf == "C")

ezANOVA(data = diet_long_c,
        dv = weight,
        wid = Person,
        within = time)

ezANOVA(data = diet_long,
        dv = weight,
        wid = Person,
        within = time,
        between = Dietf)

ezANOVA(data = diet_long,
        dv = weight,
        wid = Person,
        within = time,
        between = .c(Dietf, gender))


# Generalized linear model ------------------------------------------------

heroes <- read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/refs/heads/master/data/heroes_information.csv",
                   na = c("NA", "-", "-99", " "))
heroes$good <- heroes$Alignment == "good"

heroes_good_glm <- glm(good ~ Weight + Gender, heroes, family = binomial())
summary(heroes_good_glm)

heroes_good_glm_noweight <- glm(good ~ Gender, heroes, family = binomial())
summary(heroes_good_glm_noweight)

predict(heroes_good_glm, type = "response")


# LMEM --------------------------------------------------------------------

install.packages("lme4")
library(lme4)
data("sleepstudy")
?sleepstudy

sleepstudy
sleepstudy %>%
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point() +
  scale_x_continuous(breaks = 0:9) +
  facet_wrap(~Subject) +
  theme_minimal()

sleep_lme0 <- lmer(Reaction ~ Days + (1|Subject), data = sleepstudy)
sleep_lme1 <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)
sleepstudy$predicted_lme0 <- predict(sleep_lme0)
sleepstudy$predicted_lme1 <- predict(sleep_lme1)

sleepstudy %>%
  rename(observed_RT = Reaction) %>%
  pivot_longer(cols = c(observed_RT, predicted_lme0, predicted_lme1),
               names_to = "model", values_to = "RT_ms") %>%
  ggplot(aes(x = Days, y = RT_ms, colour = model)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:9) +
  facet_wrap(~Subject) +
  theme_minimal()
  
anova(sleep_lme0, sleep_lme1)  

library(report)
report(sleep_lme1)

summary(sleep_lme1)
library(lmerTest)
sleep_lme0 <- lmer(Reaction ~ Days + (1|Subject), data = sleepstudy)
sleep_lme1 <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)
summary(sleep_lme1)

report(heroes_good_glm)
