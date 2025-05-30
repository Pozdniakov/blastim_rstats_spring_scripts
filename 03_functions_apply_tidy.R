
# Создание функций --------------------------------------------------------



pow <- function(x, p) {
  power <- x ^ p
  return(power)
}

pow(3, 2)

pow <- function(x, p) {
  x ^ p
}
pow(4, 2)

pow <- function(x, p) x ^ p
pow(5, 3)

pow <- function(x, p = 2) x ^ p
pow(10, 4)

power <- 5
pow <- function(x) x ^ power
pow(2)

pow <- function(x, p = a - 1) {
  a <- x / 2
  x ^ p
}
pow(6)

80/(1.7)^2

w <- 78
h <- 1.83

w/h^2

imt <- function(weight, height) weight/height^2
imt(70, 1.6)
imt(500, 4)

numb <- 9
which(numb %% 1:numb == 0)
factors <- function(x) which(x %% 1:x == 0)
factors(2025)

is_prime <- function(x) length(factors(x)) == 2
is_prime(10)
is_prime(2017)
factors(2017)
is_prime <- function(x) sum(x %% seq_len(x) == 0) == 2
is_prime(2017)
#is_prime(1:10) #не работает
mean(1:10)

factors[4]
list(mean, factors, `+`)

# Семейство функций apply() и пакет {purrr} -------------------------------

A <- matrix(1:12, 3, 4)
A
rowSums(A)
colSums(A)
rowMeans(A)
colMeans(A)

apply(A, 1, sum)
apply(A, 2, sum)

A[2, 2] <- NA
A
apply(A, 1, mean, na.rm = TRUE)

neo <- c("Welcome", "to", "the", "matrix", "Neo", "!")
B <- matrix(neo, 3, 2)
sum(nchar(neo))
sum_nchar <- function(string) sum(nchar(string))
sum_nchar(month.name)
apply(B, 1, sum_nchar)
apply(B, 1, function(x) sum(nchar(x)))
apply(B, 1, \(x) sum(nchar(x)))
apply(B, 2, \(x) sum(nchar(x)))

some_list <- list(some = 1:10, list = letters)
some_list
length(some_list)
lapply(some_list, length)

lapply(iris, typeof)
sapply(some_list, length)
sapply(iris, typeof)

unclass(iris$Species)
as.integer(iris$Species)
as.character(iris$Species)

install.packages("purrr")
library(purrr)
map(iris, typeof)
map_chr(iris, typeof)

lapply(some_list, length)
sapply(some_list, length)
map_int(some_list, length)
map(some_list, 2)


# sapply(seq_len(100000000), sqrt)
# sqrt(seq_len(100000000))

is_prime_vectorized <- Vectorize(is_prime)
which(is_prime_vectorized(1:20))

#is_prime(2:20) #не работает
sapply(2:20, is_prime)

many_means <- replicate(10000, mean(rlnorm(30)) )
hist(many_means)


# {data.table} vs. tidyverse ----------------------------------------------

install.packages("data.table")
library(data.table)
heroes_dt <- fread("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/refs/heads/master/data/heroes_information.csv",
      na = c("NA", "-", "-99"))
heroes_dt
class(heroes_dt)
print
methods(print)
attributes(heroes_dt)
heroes_dt[Alignment == "good",
          .(height_mean = mean(Height, na.rm = TRUE)),
          by = Gender][order(-height_mean),]

#install.packages("tidyverse")
library(tidyverse)
heroes_tbl <- read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/refs/heads/master/data/heroes_information.csv",
         na = c("NA", "-", "-99"))
class(heroes_tbl)
heroes_tbl %>%
  filter(Alignment == "good") %>%
  group_by(Gender) %>%
  summarise(height_mean = mean(Height, na.rm = TRUE)) %>%
  arrange(desc(height_mean))

# Пайпы -------------------------------------------------------------------

sum(log(abs(sin(1:22))))

1:22 %>% 
  sin() %>% 
  abs() %>% 
  log(2, base = .) %>% 
  sum()

1:22 |>
  sin() |>
  abs() |>
  log(2, base = _) |>
  sum()

B <- matrix(10:39, nrow = 5)
apply(B, 1, mean)
10:39 %>%
  matrix(nrow = 5) %>%
  apply(1, mean)
some_list <- list(some = 1:10, list = letters)
map(some_list, length)
some_list %>%
  map(length)


# Работа с колонками ------------------------------------------------------

heroes <- heroes_tbl

heroes %>%
  select(1, 5)

heroes %>%
  select(name, Race, Publisher, `Hair color`)

heroes_selected_cols <- heroes %>%
  select(name:Publisher)
heroes_selected_cols
#select(heroes, name:Publisher)
heroes %>%
  select(name:Publisher)
heroes %>%
  select(name:`Eye color`, Publisher:Weight)
heroes %>%
  select(!...1)
heroes %>%
  select(!(Gender:Height))
heroes %>%
  select(!c(Gender, Height))
heroes %>%
  select(name:last_col())
#last_col(heroes) не работает!

heroes %>%
  select(everything())
heroes %>%
  select(Gender, Race, Height, everything())
heroes %>%
  select(ends_with("color"))

heroes %>%
  select(starts_with("H"))

heroes %>%
  select(starts_with("H") & ends_with("color"))

heroes %>%
  select(contains("eigh"))

heroes %>%
  select(where(is.numeric))
heroes %>%
  select(where(is.character))
heroes %>%
  select(where(function(x) !any(is.na(x)) ))

heroes %>%
  select(where(function(x) mean(is.na(x)) < 0.1 ))

heroes %>%
  select(id = ...1)

heroes %>%
  rename(id = ...1)
heroes %>%
  rename_with()

names(heroes)
names(heroes) %>% str_to_lower()
names(heroes) %>% str_to_upper()
#names(heroes) <- names(heroes) %>% make.names()

heroes %>%
  rename_with(make.names)

heroes %>%
  relocate(Publisher)
heroes %>%
  relocate(Publisher, .after = name)
heroes %>%
  relocate(Publisher, .before = name)
heroes %>%
  relocate(Universe = Publisher, .after = name)
heroes %>%
  relocate(Publisher, where(is.numeric), .after = name)

heroes %>%
  select(Weight) %>%
  pull()

heroes %>%
  pull(Weight)

heroes %>%
  pull(Weight, name)

# Работа со строками ------------------------------------------------------

heroes %>%
  slice(220:229)
#1
heroes %>%
  filter(Publisher == "DC Comics") %>%
  filter(Weight > 200)
#2
heroes %>%
  filter(Publisher == "DC Comics" & Weight > 200)
#3
heroes %>%
  filter(Publisher == "DC Comics", Weight > 200)

# heroes %>%
#   slice_head()
# 
# heroes %>%
#   slice_tail()

heroes %>%
  slice_max(Weight, n = 10)

heroes %>%
  slice_max(Weight, n = 10, with_ties = FALSE)

heroes %>%
  slice_min(Weight, n = 3)

heroes %>%
  slice_sample(n = 10)

heroes %>%
  slice_sample(prop = .01)

heroes %>%
  slice_sample(prop = 1)

heroes %>%
  drop_na()

heroes %>%
  drop_na(Weight, Height)

heroes %>%
  drop_na(where(is.numeric))
