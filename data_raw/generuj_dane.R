# data_raw/generuj_dane.R

set.seed(123)

library(readr)
library(dplyr)

# 1. Wczytanie danych Kaggle (po pobraniu CSV)
data_raw <- read_csv("data_raw/Students_Social_Media_Addiction.csv")

# 2. Czyszczenie i selekcja
data_raw <- data_raw %>%
  select(
    "Student_ID",
    "Most_Used_Platform",
    "Avg_Daily_Usage_Hours",
    "Addicted_Score",
    "Mental_Health_Score",
    "Sleep_Hours_Per_Night",
    "Conflicts_Over_Social_Media"
  )

# 3. Destymulanty → stymulanty
# (mniej snu / gorsza psychika = większy potencjał uzależniający)

max_sleep <- max(data_raw$Sleep_Hours_Per_Night, na.rm = TRUE)
data_raw$Sleep_Hours_Per_Night <- max_sleep - data_raw$Sleep_Hours_Per_Night

max_mental <- max(data_raw$Mental_Health_Score, na.rm = TRUE)
data_raw$Mental_Health_Score <- max_mental - data_raw$Mental_Health_Score

# 4. Zapis do użycia w pakiecie

# KROK KLUCZOWY: Zapisanie danych do folderu pakietu /data

# Funkcja use_data automatycznie kompresuje dane do formatu .rda
usethis::use_data(data_raw, overwrite = TRUE)

