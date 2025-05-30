library(tidyverse)
heroes <- read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/refs/heads/master/data/heroes_information.csv",
                       na = c("NA", "-", "-99", " "))

abs(sin(1:20))
1:20 %>% sin() %>% abs()

heroes %>%
  select(name, Publisher)

heroes %>%
  filter(Gender == "Female" & Height > 200)

# Сортировка строк --------------------------------------------------------

heroes %>%
  arrange(Weight)

heroes %>%
  arrange(desc(Weight))

heroes %>%
  arrange(Gender, desc(Weight))

# Создание колонок --------------------------------------------------------


heroes %>%
  mutate(imt = Weight/(Height/100) ^ 2, .after = name)

heroes %>%
  transmute(imt = Weight/(Height/100) ^ 2, name, Publisher)

heroes %>%
  mutate(mean_weight = mean(Weight, na.rm = TRUE))

n <- -2:2 
if (n > 0) {
  "positive number"
} else if (n < 0) {
  "negative number"
} else {
  "zero"
}

if_else(n > 0, "positive number", "negative number or zero")
if_else(n > 0, 
        "positive number", 
        if_else(n < 0, 
                "negative number", 
                "zero"))

case_when(
  n > 0 ~ "Positive number", #if
  n < 0 ~ "Negative number", #else if
  .default = "zero" #else
)


heroes %>%
  mutate(imt = Weight/(Height/100) ^ 2, .after = name) %>%
  mutate(imt_group = case_when(
    imt >= 40 ~ "Ожирение", #if
    imt >= 25 ~ "Выше нормы", #else if
    imt <= 16 ~ "Ниже нормы", #else if
    .default = "Норма" #else
  ), .after = imt)

# Агрегация ---------------------------------------------------------------

heroes %>%
  summarise(mean_weight = mean(Weight, na.rm = TRUE),
            first_weight = first(Weight),
            tenth_weight = nth(Weight, 10),
            last_weight = last(Weight))

heroes %>%
  group_by(Gender) %>%
  summarise(mean_weight = mean(Weight, na.rm = TRUE),
            first_weight = first(Weight),
            tenth_weight = nth(Weight, 10),
            last_weight = last(Weight))

range(1:10)

heroes %>%
  group_by(Gender) %>%
  reframe(range_weight = range(Weight, na.rm = TRUE))

heroes %>%
  group_by(Gender, Alignment) %>%
  summarise(mean_weight = mean(Weight, na.rm = TRUE),
            first_weight = first(Weight),
            tenth_weight = nth(Weight, 10),
            last_weight = last(Weight))

heroes %>%
  group_by(Gender) %>%
  summarise(n = n())

heroes %>%
  count(Gender)

heroes %>%
  count(Race, sort = TRUE)

heroes %>%
  summarise(mean(Weight, na.rm = TRUE), .by = Gender)

heroes %>%
  group_by(Race) %>%
  filter(n() > 10)
heroes %>%
  group_by(Race) %>%
  filter(n() == 1)

heroes %>%
  select(name, Weight, Gender, Race) %>%
  group_by(Gender) %>%
  mutate(mean_weight_by_gender = mean(Weight, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(diff_weight_to_gender = Weight - mean_weight_by_gender)

install.packages("janitor")
janitor::get_dupes(heroes)

heroes %>%
  janitor::get_dupes(name)

heroes %>%
  group_by(name) %>%
  slice(1) %>%
  ungroup() %>%
  janitor::get_dupes(name)


# Объединение датафреймов -------------------------------------------------

band_members
band_instruments

bind_rows(band_members, band_instruments)
bind_cols(band_members, band_instruments)

short_names <- dir("data/data_tcr")
full_names <- dir("data/data_tcr", full.names = TRUE)
read_tsv(full_names, id = "name") 

tcr <- full_names %>%
  map(read_tsv) %>%
  set_names(short_names) %>%
  bind_rows(.id = "name")

tcr %>%
  group_by(name) %>%
  summarise(max_count = max(count))


band_members
band_instruments

left_join(band_members, band_instruments)

band_members %>%
  left_join(band_instruments)

intersect(names(band_members), names(band_instruments))

band_members %>%
  left_join(band_instruments, by = "name")

band_members
band_instruments2
band_members %>%
  left_join(band_instruments2, by = c("name" = "artist"))

band_members %>%
  right_join(band_instruments)
band_instruments %>%
  left_join(band_members)

band_members %>%
  full_join(band_instruments)

band_members %>%
  inner_join(band_instruments)

# band_members %>%
#   filter(name %in% band_instruments$name)
band_members %>%
  semi_join(band_instruments)

band_members %>%
  filter(!name %in% band_instruments$name)
band_members %>%
  anti_join(band_instruments)
band_instruments %>%
  anti_join(band_members)

powers <- read_csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/master/data/super_hero_powers.csv")
powers
heroes

powers %>%
  filter(`Web Creation`)

semi_join(heroes, powers %>% filter(`Web Creation`), by = c("name" = "hero_names"))

anti_join(heroes, powers, by = c("name" = "hero_names")) %>%
  pull(name)
anti_join(powers, heroes, by = c("hero_names" = "name")) %>%
  pull(hero_names)


# Широкие и длинные данные ------------------------------------------------

new_diet <- tribble(
  ~student, ~before_r, ~after_r,
  "Alex", 82, 74,
  "Ben", 90, 84,
  "ГЕННАДИЙ", 73, 71
)

new_diet %>%
  pivot_longer(cols = before_r:after_r,
               names_to = "time_point",
               values_to = "weight") %>%
  pivot_wider(names_from = time_point,
              values_from = weight)

# across() ----------------------------------------------------------------


heroes %>%
  group_by(Gender) %>%
  summarise(height_mean = mean(Height, na.rm = TRUE),
            weight_mean = mean(Weight, na.rm = TRUE))

heroes %>%
  #drop_na(Height, Weight) %>%
  group_by(Gender) %>%
  summarise(across(c(Height, Weight), mean, na.rm = TRUE ))

heroes %>%
  group_by(Gender) %>%
  summarise(across(c(Height, Weight), function(x) mean(x, na.rm = TRUE)))

heroes %>%
  drop_na(Height, Weight) %>%
  group_by(Gender) %>%
  summarise(across(where(is.numeric), median ))

heroes %>%
  group_by(Gender) %>%
  summarise(across(where(is.character),
                   function(x) mean(nchar(x), na.rm = TRUE)  
                   )
            )

heroes %>%
  drop_na(Height, Weight) %>%
  group_by(Gender) %>%
  summarise(across(where(is.character),
                   function(x) mean(nchar(x), na.rm = TRUE)),
            across(where(is.numeric), mean))

heroes %>%
  drop_na(Height, Weight) %>%
  group_by(Gender) %>%
  summarise(across(c(Height, Weight), list(minimum = min,
                                           average = mean,
                                           maximum = max) ))

heroes %>%
  group_by(Alignment, Gender) %>%
  summarise(across(everything(), function(x) mean(is.na(x)) ))

iris %>%
  mutate(across(where(is.numeric), function(x) x / max(x) * 100 ))

heroes %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.character))

# nesting and list-columnns -----------------------------------------------

heroes %>%
  group_by(Gender) %>%
  nest()

nested_heroes <- heroes %>%
  nest(!Gender)
nested_heroes
nested_heroes %>%
  mutate(dim = map(data, dim)) %>%
  select(!data) %>%
  unnest(cols = dim)

nested_heroes %>%
  mutate(dim = map(data, dim)) %>%
  select(!data) %>%
  unnest_wider(dim, names_sep = "_")

films <- tribble(
  ~film, ~genre,
  "Lord of the Rings", "fantasy, advantures",
  "Die Hard", "action, advantures",
  "Green Elephant", "drama, horror, psychological",
  "Breaking Bad", "crime, drama, psychological"
)

# films %>%
#   filter(str_detect(genre, "advantures"))

str_split(films$genre, pattern = ", ")

films %>%
  mutate(genre = str_split(genre, pattern = ", ")) %>%
  unnest(cols = c(genre)) %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = genre, values_from = value, values_fill = FALSE)

films %>%
  separate_longer_delim(cols = genre, delim = ", ") %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = genre, values_from = value, values_fill = FALSE)
