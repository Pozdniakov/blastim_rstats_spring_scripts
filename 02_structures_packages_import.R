as.integer(c("1", "2", "three"))
n <- 1:10
n[1:30]

missed <- NA
missed == "NA"
missed == ""
missed == 0
missed == NA
n[5] <- NA
n
mean(n)
n != NA
NA == NA
Joe <- NA
Mary <- NA
Joe == Mary
n
is.na(n)
mean(n[!is.na(n)])
#Посчитать среднее вектора n без NA (найти второй способ)
#mean(n[which(n>0)])
#Найти еще один способ
mean(n, na.rm = TRUE)

typeof(NA)
as.character(n)
NA
NA_integer_
NA_character_
NA_real_

NA ^ 0
NA | TRUE
NA & FALSE

1 / 0
-1 / 0
0 / 0
mean(c(NA, NA, NA), na.rm = TRUE)
is.nan(0/0)
is.nan(NA)
is.na(NA)
is.na(0/0)

NULL
seq_len(0)
character(0)
length("")
length(character(0))
length(NULL)
c()

# Матрица -----------------------------------------------------------------

matrix(1:12, nrow = 4, ncol = 3)
A <- matrix(1:12, nrow = 4)
A[3, 2]
A[1:2, 2:3]
A > 10
A[1:4, 2:3]
A[, 2:3]
A[1:2, ]
A[1:2, 1:2] <- 100
A
attributes(A)
attr(A, "dim") <- NULL
A
attr(A, "dim") <- c(2, 6)
A
attr(A, "dim") <- c(2, 2, 3)
A
attr(A, "dim") <- NULL
A

matrix(rep(1:9, 9) * rep(1:9, each = 9), nrow = 9)
rep(1:9, 9) * rep(1:9, each = 9)

outer(1:9, 1:9)
1:9 %o% 1:9

# Списки ------------------------------------------------------------------

simple_list <- list(42, "Hey", TRUE)
simple_list
complex_list <- list(1:10, letters, simple_list, A, mean, `[`)
complex_list
str(complex_list)

named_list <- list(name = "Veronika", age = 26, student = FALSE)
named_list

named_list["age"]
named_list$age
named_list[1]
class(named_list$age)
class(named_list[1])
named_list[1]
named_list$name
named_list[[1]]
is.vector(1:10)
is.vector(1)
is.vector(named_list)
is.atomic(1)
is.atomic(named_list)
is.recursive(named_list)
is.recursive(1:10)

list1 <- list(numbers = 1:5, letters = letters, logic = TRUE)
list1
list2 <- list(pupa = list1, lupa = list1)
list2
list2[[1]][[2]][3]


df <- data.frame(name = c("Вероника", "Эдуард", "Анна", "Маша", "Виктория"),
     age = c(25, 24, 27, 35, 41),
     student = c(FALSE, TRUE, TRUE, FALSE, FALSE))
df[1:2, 1:2]
df[df$age > 30, c("name", "age")]
df$lovesR <- TRUE
df
df$lovesR <- NULL
df
df[df$age > mean(df$age), "name"]

df$name[df$age > mean(df$age)]
df[!df$student, ]
sum(df$age <= 25)

row.names(mtcars)

B <- matrix(1:12, nrow = 3)
mtcars[1,1]
mtcars[7]
mtcars[, 7]

B[, 3]
B[3, ]
B[, 3, drop = FALSE]
B[3, , drop = FALSE]

# Пакеты ------------------------------------------------------------------

installed.packages(priority = "base")
#install.packages("beepr")

library(beepr)
beep()
beepr::beep(11)

if(!require(devtools)) {install.packages("devtools")}
devtools::install_github("brooke-watson/BRRR")
BRRR::skrrrahh(7)

# Импорт данных -----------------------------------------------------------

read.csv("/Users/ivan/R/blastim_rstats_spring_scripts/data/heroes_information.csv")
getwd()
read.csv("heroes_information.csv")
heroes <- read.csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/refs/heads/master/data/heroes_information.csv",
                   na.strings = c("NA", "-", "-99", " "))
heroes
?read.csv

str(heroes)
install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2025-03-11')
pixar_films <- tuesdata$pixar_films
pixar_films <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/pixar_films.csv")
