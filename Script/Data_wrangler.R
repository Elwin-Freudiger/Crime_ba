#Load datasets
library(readxl)
library(readr)
library(dplyr)
library(tidyverse)

#Create a list of metropolitan departements
List_depart <- 
  read_excel(here::here("data_start/Final/population par département.xls"), sheet = "2023")
View(List_depart)

List_depart <- List_depart[5:100, 1:2]
colnames(List_depart) <- c("Dep_number", "Dep_name")

view(List_depart)
#We need to have the population by department
#We will load the datasheet from excel, clean it and add it

years_pop <- as.character(1975:2023) #every year we need to extract a sheet
Pop_by_year <- List_depart[1] #list with depart numbers 
Col_name <-  paste0("pop_", years_pop) #List to rename the columns


for (i in 1:length(years_pop)) {
  pop <- 
    read_excel(here::here("data_start/Final/population par département.xls"), 
               sheet = years_pop[i]) 
  Pop_by_year <- cbind(Pop_by_year, pop[5:100, 8])#add them to the df
  colnames(Pop_by_year)[i+1] <- Col_name[i] #rename cols
}

write.csv(Pop_by_year, "Pop_by_year.csv")

View(Pop_by_year)

#Now onto crime
years_crime <- 1996:2021
Crime_total <- t(data.frame(years_crime))
colnames(Crime_total) <- Crime_total[1,]

view(Crime_total)

for (i in 1:nrow(List_depart)) {
  crimes_depart <- read_excel(here::here("data_start/Final/crimes_depart.xlsx"), 
                              sheet = List_depart$Dep_number[i]) 
  #Load each datasheet with the departement list
  
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

#rownames(Crime_total) <- Crime_total$Dep_number
#Crime_total <- Crime_total[, -1]

View(Crime_total)

write.csv(Crime_total, "Crime_to_use.csv") #export it into a csv file


#For unemployement 
Unemployement <- 
  read.csv(here::here("data_start/Final/valeurs_trimestrielles.csv"), 
                                     sep = ";")

view(Unemployement)

dummy <- seq_len(nrow(Unemployement)) %% 2 #Dummy variable equal to 1 if the row is an odd number
Unemployement <- cbind(dummy, Unemployement)

Unemployement <- Unemployement |>
  filter(dummy == 1) |>
  select(-c(1, 3,4,5))
Unemployement <-  Unemployement[-(1:15),]

#start <- "Taux de chômage localisé par département - "
Unemployement <- Unemployement |> 
  separate(Libellé, into = c("X", "Dep_name") ,sep=" - ") |>
  select(-1) 

Unemployement <- Unemployement[-(97:100), ]

Unemployment_quarterly <- Unemployement |>
  pivot_longer(cols = -Dep_name, names_to = "Year", values_to = "Total") |>
  mutate(Total = as.numeric(Total)) |>
  mutate(Year = substr(Year, 2, 8)) |>
  group_by(Dep_name, Year) |>
  summarize(Total= mean(Total)) |>
  pivot_wider(names_from = Year, values_from = Total)

Unemployment_quarterly <- full_join(List_depart, Unemployment_quarterly, join_by(Dep_name))


View(Unemployment_quarterly)
write_csv(Unemployment_year, "Unemployment_year.csv")



Unemployment_year <- Unemployment_quarterly |>
  pivot_longer(cols = -(Dep_number:Dep_name), names_to = "Year", values_to = "Total") |>
  mutate(Year = substr(Year, 1, 4)) |>
  group_by(Dep_number, Dep_name, Year) |>
  summarize(Total = mean(Total)) |>
  pivot_wider(names_from = Year, values_from = Total)

view(Unemployment_year)

