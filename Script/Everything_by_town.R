library(readxl)
library(readr)
library(dplyr)
library(tidyverse)

#Town density loaded
Dense <- read_csv(here::here("data_start/Final/villes_france.csv"), col_names = FALSE)
View(Dense)

Density_town <- Dense |>
  select(c(4, 11, 15, 19)) |>
  rename(Town_name = `X4`, 
         Town_code = `X11`,
         Pop_2012 = `X15`,
         Size = `X19`) |>
  filter(!str_detect(Town_code, "^97")) |>
  mutate(Density_2019 = Pop_2012/Size) |>
  select(Town_code, Town_name, Density_2019)
view(Density_town)

#Load crime pertown
CrimeParVille <- read_csv(here::here("data_end/CrimeParVille.csv"))
View(CrimeParVille)

View(Type_per_year)

Crime_2019_town <- Type_per_year |>
  filter(Year == 19) |>
  group_by(Departement, Town) |>
  summarize(Rate_per_1k = mean(Rate_per_1k)) |>
  rename(Town_code = Town, Dep_number = Departement)

view(Crime_2019_town)


#population 2019
Pop_2019 <- read_delim(here::here("data_start/Final/Pop_2019.csv"), 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Pop_2019)

Pop_by_town <- Pop_2019 |>
  group_by(CODGEO) |>
  summarise(Total = sum(NB)) |>
  rename(Town_code = CODGEO, Total_pop = Total)
view(Pop_by_town)

#Load the pop without diploma
Pop_16_no_diploma <- read_excel(here::here("data_start/Final/pop-16ans-dipl6819_v2.xls"), 
                                    sheet = "COM_2019", skip = 15)
View(Pop_16_no_diploma)
write_csv(Pop_16_no_diploma, "Pop_16_no_diploma.csv")

Pop_no_diploma <- Pop_16_no_diploma |>
  select(c(2, 3, 6, 8, 10)) |>
  slice(-1) |>
  rename(Dep_number = `Département\nen géographie courante`,
         Town_number = `Commune\nen géographie courante`,
         Town_name = `Libellé de commune`,
         no_diploma_M = `Aucun diplôme\nHommes\n25 ans ou plus\nRP2019`,
         no_diploma_F = `Aucun diplôme\nFemmes\n25 ans ou plus\nRP2019`) |>
  group_by(Dep_number, Town_number) |>
  summarize(no_diploma = sum(as.numeric(no_diploma_F), as.numeric(no_diploma_M), na.rm = TRUE)) |>
  mutate(Town_code = paste0(Dep_number, Town_number)) |>
  select(Dep_number, Town_code, no_diploma) |>
  filter(!str_detect(Dep_number, "^97")) |>
  left_join(Pop_by_town, join_by(Town_code)) |>
  na.omit() |>
  mutate(No_diploma_rate1k = (no_diploma/Total_pop)*1000)

Paris_tot <- Pop_no_diploma |>
  filter(Dep_number == "75") |>
  summarize(No_diploma_rate1k = mean(No_diploma_rate1k)) |>
  rename(Town_code = Dep_number) |>
  mutate(Town_code = "75056")

Pop_no_diploma <- Pop_no_diploma |>
  group_by(Town_code, No_diploma_rate1k) |>
  select(Town_code, No_diploma_rate1k)

Pop_no_diploma <- rbind(Pop_no_diploma, Paris_tot)



#Load lepen
Vote_2017 <- read_csv(here::here("data_end/Vote_2017.csv"))
View(Vote_2017)



#Poverty 
Pov_2019 <- read_delim(here::here("data_start/Final/FILO2019_DEC_Pauvres_COM.csv"), 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Pov_2019)

Povr_2019 <- Pov_2019 |>
  select(c(1, 4, 6)) |>
  na.omit() |>
  rename(Town_code = CODGEO,
         Povrety_2019 = TP6019,
         Intensity_povrety = TP60IP19)

view(Povr_2019)


#Lets join them all and see what happens
#we will do a full join and see how many missing values we get

Everything_by_town <- Crime_2019_town |>
  full_join(Pop_by_town, join_by(Town_code)) |> 
  full_join(Density_town, join_by(Town_code)) |>
  full_join(Vote_2017, join_by(Town_code)) |>
  full_join(Povr_2019, join_by(Town_code)) |>
  full_join(Pop_no_diploma, join_by(Town_code))

summary(Everything_by_town)

Everything_by_town_clean <- Everything_by_town |>
  na.omit() |>
  select(Dep_number, 
         Town_code, 
         Town_name, 
         Total_pop, Rate_per_1k, Density_2019, Lepen, Win_Lepen, Povrety_2019, Intensity_povrety, No_diploma_rate1k)

view(Everything_by_town_clean)

view(filter(Everything_by_town, Dep_number == "75"))









