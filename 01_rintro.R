# R как калькулятор: основные операторы. Функции --------------------------

2 + 2
3 - 2
2 * 3
15 / 2
15 %/% 2 #это деление с остатком
15 %% 2
(2 + 2) * 2
16 ^ (1/2)
16 ^ 0.5
sqrt(16)
sqrt(16)
log(8)
?log
log(x = 8, base = 2)
log(8, 2)
log(8, sqrt(4))
# ?`+`
# `+`(3, 4)


pi
sin(pi)
options(scipen = 999)
sin(pi)
typeof(pi)
typeof(2/3)

# Присвоение переменных ---------------------------------------------------


a = log(4, 2) #у нас тут так не принято
#print(a) #мы не используем функцию print() в R!
a
a * 2
b <- 3
# 3 -> b #тоже не очень принято, но не криминал
many_words_variable <- 2 * 2
rm(many_words_variable)
# many_words_variable #а нету больше!

# Операторы сравнения -----------------------------------------------------

a * b
a ^ b
a == b
a != b
factorial(5)
# 5!=120
a > b
a < b  
a >= b
a >= a
a <= b
b <= b


# Типы данных -------------------------------------------------------------

typeof(a)
is.double(a)
typeof(a)
is.integer(as.integer(a))
d <- 4L
is.integer(d)
typeof(3 + 2i)

comparison <- a > b
typeof(comparison)
t1 <- TRUE
f1 <- FALSE
!t1
!f1
t1 & t1 #TRUE
t1 & f1 #FALSE
f1 & t1 #FALSE
f1 & f1 #FALSE

t1 | t1 #TRUE
t1 | f1 #TRUE
f1 | t1 #TRUE
f1 | f1 #FALSE

#xor(t1, t1)

age <- 25
age >= 18 & age <= 30 #[18, 30]
age < 18 | age > 30 #(-Inf, 18) | (30, +Inf)

s <- "hey"
s <- 'hey'
s
typeof(s)
paste("welcome", "to", "the", "R", "course")
?paste
paste("welcome", "to", "the", "R", "course", sep = "_<3*&^%__")
paste("welcome", "to", "the", "R", "course", sep = "")
paste0("welcome", "to", "the", "R", "course")

# Векторы -----------------------------------------------------------------

sum(c(4, 8, 15, 16, 23, 42))
c("welcome", "to", "the", "vector", "Neo")
c(TRUE, FALSE)
c(2, 3)
1:10
5:-20

#0:2:10
seq(0, 10, by = 2)
seq(0, 100, by = 10)
seq(1, 13, length.out = 4)
seq(1, 13, length.out = 7)
seq_len(20)
1:20
seq_len(0)
seq_along(5:-20)
rep(1, 5)
rep(1:3, 5)
rep(1:3, c(10, 3, 5))
rep("Hey", 10)
c(1:10, seq(1, 13, length.out = 7), 1000)
sum(1:10)
mean(1:10)
rep(1:9, 1:9)
c(1:20, 19:1)

# Приведение типов --------------------------------------------------------

c(FALSE, 2)
2 + TRUE
c(TRUE, 2, "три")
c(c(TRUE, 2), "три")

as.character(as.integer(c(FALSE, TRUE, FALSE)))

as.integer(c("1", "2", "три"))

# Векторизация ------------------------------------------------------------

n <- 1:4
m <- c(10, 20, 30, 40)
n
m
n + m
m - n
n * m
n / m
n * m / (n - m) ^ n
sqrt(1:10)
sqrt(n)
log(1:10)
log(x = 2, base = 1:10)
1/0
Inf > 10000000000000000
-1/0
k <- c(10, 100)
n
k
n * k
n * 10
t <- c(10, 100, 1000)
n * t
1:20 * rep(0:1, 10)
1:20 * 0:1
sum(1/2^(0:20))
sum(seq(1, 28, by = 3)/3^(0:9))
seq(1, 28, by = 3)/3^(0:9) > 0.5

# Индексирование векторов -------------------------------------------------
options(scipen = 999)
n <- c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
n[1]
length(n)
n[length(n)]
head(n, 1)
tail(n, 1)
n[3] <- 20
n
n[4:7]
n[length(n):1]
n
rev(n)
n
n[4:7] <- 0
n
n[-1]
n[-5]
n[-1:-5]
# n[2, 5, 10] #не работает!
n[c(2, 5, 10)]
n[c(TRUE, FALSE, TRUE, FALSE, TRUE,
    FALSE, TRUE, FALSE, TRUE, FALSE)]
n[c(TRUE, FALSE)]

n[n > 10]
n[n >= 18 & n <= 30]
n[n > mean(n)]
named_vector <- c(first = 1, second = 2, third = 3)
attributes(named_vector)
names(named_vector)
letters[1:3]
LETTERS
pi
month.name
month.abb
names(named_vector) <- letters[1:3]
named_vector
named_vector["b"]
named_vector[c("a", "c")]

# 2 4 6 8 10 12 14 16 18 20 -----------------------------------------------

#1
seq(2, 20, by = 2)
#2
2 * 1:10
#3
c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
#4
(1:20)[1:20 %% 2 == 0]
#5
vv <- 1:20
vv[c(FALSE, TRUE)]
#6
(1:20)[-seq(1,20, by = 2)]
#7
(1:20)[1:20 %% 2 != 1]
#8
vec4 <- (1:20) * (0:1) 
vec4[vec4>0]
#9 
2 * seq_len(10)
#10
2 + 2 * (0:9)
#11
rep(2,10) * 1:10


# Работа с логическими векторами ------------------------------------------

eyes <- c("green", "blue", "blue", "brown", "green", "blue")
eyes == "blue"
sum(eyes == "blue")/length(eyes == "blue")
mean(eyes == "blue") 
paste0(mean(eyes == "blue") * 100, "%")
all(eyes == "blue")
any(eyes == "blue")
all(!eyes == "blue")

which(eyes == "blue")
#seq_along(eyes == "blue")[eyes == "blue"]

eyes[eyes == "blue"]
eyes[eyes == "blue" | eyes == "green"]
eyes[eyes == c("green", "blue")]

eyes %in% c("green", "blue")

respondents_cities <- c("Балашиха", "Москва", "Санкт-Петербург", "Сочи", "Урюпинск",
                        "Воронеж", "Москва", "Новосибирск", "Москва")
big_cities <- c("Москва", "Санкт-Петербург", "Новосибирск")

respondents_cities %in% big_cities
respondents_cities[respondents_cities %in% big_cities]

#big_cities %in% respondents_cities
