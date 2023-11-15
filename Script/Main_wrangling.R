#### This file will be the main wrangling file for R

####################################################################################

#These are our needed packages for the wrangling
library(readxl)
library(readr)
library(dplyr)
library(tidyverse)

####################################################################################
####################################################################################


#Create a list of metropolitan departements
List_depart <- 
  read_excel(here::here("Raw_data/population par département.xls"), sheet = "2023")

#List of departments 
List_depart <- List_depart[5:100, 1:2]
colnames(List_depart) <- c("Dep_number", "Dep_name")



####################################################################################

#We need to have the population by department

years_pop <- as.character(1975:2023) #every year we need to extract a sheet
Pop_by_year <- List_depart[1] #list with depart numbers 
Col_name <-  paste0("pop_", years_pop) #List to rename the columns

for (i in 1:length(years_pop)) {
  pop <- 
    read_excel(here::here("Raw_data/population par département.xls"), 
               sheet = years_pop[i]) 
  Pop_by_year <- cbind(Pop_by_year, pop[5:100, 8])#add them to the df
  colnames(Pop_by_year)[i+1] <- Col_name[i] #rename cols
}

Pop_2019 <- Pop_by_year |> 
  select(Dep_number, pop_2019)

####################################################################################

#Now onto crime
years_crime <- 1996:2021
Crime_total <- t(data.frame(years_crime))
colnames(Crime_total) <- Crime_total[1,]

for (i in 1:nrow(List_depart)) {
  crimes_depart <- read_excel(here::here("Raw_data/crimes_depart.xlsx"), 
                              sheet = List_depart$Dep_number[i]) 
  #Load each datasheet with the department list
  
  crimes_depart <- crimes_depart |>
    select(-(1:10)) |> 
    summarise_all(sum) |>
    pivot_longer(cols = everything(), names_to = "Year", values_to = "Dep_number") |>
    mutate(Year = substr(Year, 2, 5)) |>
    group_by(Year) |>
    summarize(Dep_number = sum(Dep_number)) |> t()
  
  Crime_total <- rbind(Crime_total, crimes_depart[2,])
}

Crime_total <-  Crime_total[-1, ]
Crime_total <- cbind(List_depart[1], Crime_total)

Crime_2019 <- Crime_total |>
  select(Dep_number, `2019`) |>
  rename(Crime_tot_2019 = `2019`)

Crime_2019 <- full_join(Crime_2019, Pop_2019, join_by("Dep_number"))

Crime_2019 <- Crime_2019 |>
  mutate(Crime_rate_1k = (as.numeric(Crime_tot_2019)/as.numeric(pop_2019))*1000)

####################################################################################

#For unemployment 
Unemployment <- 
  read.csv(here::here("Raw_data/valeurs_trimestrielles.csv"), 
           sep = ";")


dummy <- seq_len(nrow(Unemployment)) %% 2 #Dummy variable equal to 1 if the row is an odd number
Unemployment <- cbind(dummy, Unemployment)

Unemployment <- Unemployment |>
  filter(dummy == 1) |>
  select(-c(1, 3,4,5))
Unemployment <-  Unemployment[-(1:15),]

Unemployment <- Unemployment |> 
  separate(Libellé, into = c("X", "Dep_name") ,sep=" - ") |>
  select(-1) 

Unemployment <- Unemployment[-(97:100), ]

Unemployment_quarterly <- Unemployment |>
  pivot_longer(cols = -Dep_name, names_to = "Year", values_to = "Total") |>
  mutate(Total = as.numeric(Total)) |>
  mutate(Year = substr(Year, 2, 8)) |>
  group_by(Dep_name, Year) |>
  summarize(Total= mean(Total)) |>
  pivot_wider(names_from = Year, values_from = Total)

Unemployment_quarterly <- full_join(List_depart, Unemployment_quarterly, join_by(Dep_name))

Unemployment_year <- Unemployment_quarterly |>
  pivot_longer(cols = -(Dep_number:Dep_name), names_to = "Year", values_to = "Total") |>
  mutate(Year = substr(Year, 1, 4)) |>
  group_by(Dep_number, Dep_name, Year) |>
  summarize(Total = mean(Total)) |>
  pivot_wider(names_from = Year, values_from = Total)

Unemp <- Unemployment_year |> 
  select(Dep_number, `2019`) |>
  rename(Unemp_2019 = `2019`)


####################################################################################

Middle_results <- read.csv(here::here("Raw_data/fr-en-dnb-par-etablissement.csv"), 
                           sep = ";")

Middle <- Middle_results |>
  filter(Session == 2019) |>
  select(Code.département, Admis, Inscrits) |>
  mutate(Pass_rate = (Admis/Inscrits)) |>
  group_by(Code.département) |>
  summarize(Pass_rate_2019 = mean(Pass_rate)) |>
  rename(Dep_number = Code.département) |>
  mutate(Dep_number = substr(Dep_number, 2, 3))

Middle <- Middle[(2:97), ]

####################################################################################

Election_dep <- read_excel("Raw_data/Election_dep.xls", 
                           sheet = "Départements Tour 2", skip = 2)

Election_by_dep <- Election_dep |>
  select(c(1,28)) |>
  rename(Dep_number = `Code du département`,
         Lepen_score = `% Voix/Exp...28`) |>
  mutate(Win_lepen = ifelse(Lepen_score>50, 1, 0))


Election_by_dep <- Election_by_dep[1:96, ] #only take metropolitan departments

#Need to change the dep notation
for (i in 1:nrow(Election_by_dep)) {
  if(nchar(Election_by_dep$Dep_number[i]) == 1) {
    Election_by_dep$Dep_number[i] <- paste0("0",  Election_by_dep$Dep_number[i])
  }
}

####################################################################################

#Immigration
Immigration_2019 <- read_excel(here::here("Raw_data/Immigration_2019.xlsx"), 
                               sheet = "Pop0_D", skip = 1)

Immig_2019 <- Immigration_2019 |>
  slice(1:96) |>
  select(c(1, 2, 4)) |>
  rename(Dep_name = `...1`,
         Immig_tot = Immigrés,
         pop_2019 = `Ensemble...4`) |>
  mutate(Immig_rate = Immig_tot/pop_2019) |>
  select(Dep_name, Immig_rate)

####################################################################################

#Density
population_2019 <- read_excel(here::here("Raw_data/population_2019.xlsx"), 
                              sheet = "Figure 3", skip = 2)

Dens_2019 <- population_2019 |>
  rename(Dep_number = Département,
         Density_2019 = Densité) |>
  select(Dep_number, Density_2019) |>
  slice(1:96)

#####################################################################################

#Join all of them together

Full_data_dep <- Unemp |>
  full_join(Crime_2019, join_by("Dep_number")) |>
  full_join(Middle, join_by("Dep_number")) |>
  full_join(Election_by_dep, join_by("Dep_number")) |>
  full_join(Immig_2019, join_by("Dep_name")) |>
  full_join(Dens_2019, join_by("Dep_number"))

view(Full_data_dep)

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################




