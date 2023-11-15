library(readxl)
library(readr)
library(dplyr)
library(tidyverse)

Crimes <- read.csv(here::here("data_start/Final/try.csv"), sep = ";")

Crimes_short <- head(Crimes, n=10)

write_csv(Crimes_short, "Crimes_short.csv")
View(Crimes_short)

#Get a feeling of the way Towns are encoded
Towncode <- Crimes |> 
  filter(annee == 21) |>
  filter(valeur.publiée == "diff") |>
  filter(!grepl("^97", CODGEO_2023)) |>
  select(CODGEO_2023, classe) |> 
  group_by(CODGEO_2023) |> 
  summarize(classe = n_distinct(classe)) |>
  select(CODGEO_2023)
    
View(Towncode)

#Find the rate at which towns publish data in a departement
Missing_in_dep <- Crimes |>
  filter(!grepl("^97", CODGEO_2023)) |>
  select(CODGEO_2023, valeur.publiée)

Missing_in_dep <- cbind(Missing_in_dep$CODGEO_2023, Missing_in_dep)
colnames(Missing_in_dep) <- c("Departement", "Town", "Published")
  
Missing_in_dep <- Missing_in_dep |> 
  mutate(Departement = substr(Departement, 1, 2)) |>
  mutate(Town = substr(Town, 3, 5)) |> 
  group_by(Departement) |>
  summarize(Publishing_rate = sum(Published=="diff")/n())

View(Missing_in_dep)



Type_per_year <- Crimes |> 
  filter(valeur.publiée == "diff") |>
  filter(!grepl("^97", CODGEO_2023)) |>
  select(CODGEO_2023, annee, classe, faits, tauxpourmille)

Type_per_year <- cbind(Type_per_year[1], Type_per_year)
colnames(Type_per_year) <- c("Departement", "Town", "Year", "Type", "Number", "Rate_per_1k")

Type_per_year <- Type_per_year |> 
  mutate(Departement = substr(Departement, 1, 2)) |>
  mutate(Rate_per_1k = str_replace_all(Rate_per_1k, ",", ".")) |>
  mutate(Rate_per_1k = as.numeric(Rate_per_1k)) |>
  group_by(Departement, Town, Year, Number, Type) |>
  summarize(Rate_per_1k = mean(Rate_per_1k, na.rm = TRUE), ) |>
  group_by(Departement, Town, Year, Type, Rate_per_1k) |>
  summarize(Number = sum(Number))
  
View(Type_per_year)
write.csv(Type_per_year, "Type_per_year.csv")


########SOME DATA MISSING IN 55 for some towns. BUT NOT WRITTEN AS Not Shared

#Most common types of crime in

Common_crimes <- Type_per_year |>
  group_by(Type) |>
  summarize(Rate_per_1k = mean(Rate_per_1k, na.rm = TRUE)) |>
  arrange(desc(Rate_per_1k))

View(Common_crimes)
#We find that the 5 most common crimes are 
#Destructions et dégradations volontaires
#Vols sans violence contre des personnes 
#Coups et blessures volontaires
#Cambriolages de logement 
#Usage de stupéfiants


#Create a Dataset with only the 5 most common crimes

Five_common <- Type_per_year|>
  filter(Type %in% c("Destructions et dégradations volontaires",
                  "Vols sans violence contre des personnes",
                  "Coups et blessures volontaires",
                  "Cambriolages de logement",
                  "Usage de stupéfiants")) 

View(Five_common)


##Create a dataset of crime rate per department per crime for PCA analysis.

Type_per_year <- read_csv(here::here("data_end/Type_per_year.csv"))
View(Type_per_year)

PCA_rate <- Type_per_year |>
  filter(Year == 19) |>
  group_by(Departement, Type) |>
  summarize(Rate_per_1k = mean(Rate_per_1k, na.rm = TRUE)) |>
  pivot_wider(names_from = Type, values_from = Rate_per_1k)

view(PCA_rate)

