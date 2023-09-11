install.packages("tidyr")
install.packages("flextable")
library(dplyr)
library(ggplot2)
library(readr)
library(tibble)
library(psych)
library(tidyr)
library(tibble)
library(flextable)
install.packages("gdtools")
install.packages("Cairo")

data_1 <- read_tsv("Data/data_tsv.tsv")
x <- data_1
data_info_1 <- summary(data_1)
data_info_2 <- describe(data_1, na.rm = TRUE, skew = FALSE, ranges = TRUE)
#Таблица с абсолютными частотами групп крови у пациентов в двух разных группах
data_info_3 <- table(data_1$Группа, data_1$`Группа крови`) 

#Таблица с относительными частотами групп крови у пациентов в двух разных группах
data_info_4 <- prop.table(table(data_1$Группа, data_1$`Группа крови`) )


??list
#Стат таблица
statistics <- list(
  `Количество субъектов` = ~length(.x),
  `Количество (есть данные)` = ~sum(!is.na(.x)),
  `Нет данных` = ~sum(is.na(.x)),
  `Ср. знач.` = ~ifelse(sum(!is.na(.x)) == 0, "Н/П*", mean(.x, na.rm = TRUE) %>% round(2) %>% as.character()),
  `Станд. отклон.` = ~ifelse(sum(!is.na(.x)) < 3, "Н/П*", sd(.x, na.rm = TRUE) %>% round(2) %>% as.character()),
  `95% ДИ для среднего` = ~sd(.x, na.rm = TRUE) %>% round(2) %>% as.character(),
  `мин. - макс.` = ~ifelse(sum(!is.na(.x)) == 0, "Н/П*", paste0(min(.x, na.rm = TRUE) %>% round(2), " - ", max(.x, na.rm = TRUE) %>% round(2))),
  `Медиана` = ~ifelse(sum(!is.na(.x)) == 0, "Н/П*", median(.x, na.rm = TRUE) %>% round(2) %>% as.character()),
  `Q1 - Q3` = ~ifelse(sum(!is.na(.x)) == 0, "Н/П*", paste0(quantile(.x, 0.25, na.rm = TRUE) %>% round(2), " - ", quantile(.x, 0.75, na.rm = TRUE) %>% round(2)))
)


x %>% 
  select(`Группа`, where(is.numeric)) %>%
  group_by(`Группа`) %>%
  summarise(across(where(is.numeric), statistics)) %>%
  pivot_longer(! `Группа`) %>%
  separate(name, into = c("Переменная", "Статистика"), sep = "_") %>%
  rename(`Значение` = value) %>%
  flextable()
