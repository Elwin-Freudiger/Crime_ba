library(readxl)
library(readr)
library(dplyr)
library(tidyverse)

#Load the data
Presidentielle_2017_Resultats_Communes_Tour_2_c <- read_excel(here::here("data_start/Final/Presidentielle_2017_Resultats_Communes_Tour_2_c.xls"), 
                                                              sheet = "Feuil1", skip =3)
View(Presidentielle_2017_Resultats_Communes_Tour_2_c)
#Dataset of 2017 election results by town

Vote_2017 <- Presidentielle_2017_Resultats_Communes_Tour_2_c |>
  select(c(1, 2, 3, 25, 32)) #select ony the values important to us in that case, the department, the towncode and the result
Vote_2017 <- Vote_2017[1:35281, ]
colnames(Vote_2017) <- c("Department", "Dep_name", "Town_num", "Macron", "Lepen")

#Need to add the Corse departement number(2A/2B)
Vote_2017$Department <- as.character(Vote_2017$Department)

for (i in 1:nrow(Vote_2017)) {
  if (Vote_2017$Dep_name[i] == "Corse-du-Sud") {
    Vote_2017$Department[i] <-  "2A"
  }
  if (Vote_2017$Dep_name[i] == "Haute-Corse") {
    Vote_2017$Department[i] <-  "2B"
  }  
}

#Now let's standardize the notation of departments and town codes by adding zeroes before.
for (i in 1:nrow(Vote_2017)) {
  if (nchar(Vote_2017$Department[i]) == 1) {
    Vote_2017$Department[i] <- paste0("0", Vote_2017$Department[i])
  }
  if (nchar(Vote_2017$Town_num[i])<=2) {
    if (nchar(Vote_2017$Town_num[i]) <= 1) { 
           Vote_2017$Town_num[i] <- paste0("00", Vote_2017$Town_num[i])
    }
           else {Vote_2017$Town_num[i] <- paste0("0", Vote_2017$Town_num[i])
           }
  }
}

#Create the town key
Vote_2017 <- Vote_2017 |> 
  mutate(Town_code = paste0(Department, Town_num)) |>
  mutate(Win_Lepen = ifelse(Lepen>50, 1, 0)) |>
  select(Department, Town_code, Macron, Lepen, Win_Lepen)#Create a dummy variable that equals 1 if LePen "won" the town (scored more than 50%)

view(Vote_2017)
write_csv(Vote_2017, "Vote_2017.csv")


