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

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################

#Crimes 
Crimes <- read.csv(here::here("Raw_data/try.csv"), sep = ";")

#Get a feeling of the way Towns are encoded
Towncode <- Crimes |> 
  filter(annee == 21) |>
  filter(valeur.publiée == "diff") |>
  filter(!grepl("^97", CODGEO_2023)) |>
  select(CODGEO_2023, classe) |> 
  group_by(CODGEO_2023) |> 
  summarize(classe = n_distinct(classe)) |>
  select(CODGEO_2023)

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

Common_crimes <- Type_per_year |>
  group_by(Type) |>
  summarize(Rate_per_1k = mean(Rate_per_1k, na.rm = TRUE)) |>
  arrange(desc(Rate_per_1k))
#We find that the 5 most common crimes are 
#Destructions et dégradations volontaires
#Vols sans violence contre des personnes 
#Coups et blessures volontaires
#Cambriolages de logement 
#Usage de stupéfiants

Five_common <- Type_per_year|>
  filter(Type %in% c("Destructions et dégradations volontaires",
                     "Vols sans violence contre des personnes",
                     "Coups et blessures volontaires",
                     "Cambriolages de logement",
                     "Usage de stupéfiants")) 


##Create a dataset of crime rate per department per crime for PCA analysis.

PCA_rate <- Type_per_year |>
  filter(Year == 19) |>
  group_by(Departement, Type) |>
  summarize(Rate_per_1k = mean(Rate_per_1k, na.rm = TRUE)) |>
  pivot_wider(names_from = Type, values_from = Rate_per_1k)


#Load in 2019 only
Crime_2019_town <- Type_per_year |>
  filter(Year == 19) |>
  group_by(Departement, Town) |>
  summarize(Rate_per_1k = mean(Rate_per_1k)) |>
  rename(Town_code = Town, Dep_number = Departement)


#####################################################################################

#Dataset of 2017 election results by town
Presidentielle_2017 <- read_excel(here::here("Raw_data/Presidentielle_2017_Resultats_Communes_Tour_2_c.xls"), 
                                                              sheet = "Feuil1", skip =3)

Vote_2017 <- Presidentielle_2017 |>
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

#Now let's standardize the notation of departments and town codes by adding leading zeroes.
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

#####################################################################################

#Density
Dense <- read_csv(here::here("Raw_data/villes_france.csv"), col_names = FALSE)

Density_town <- Dense |>
  select(c(4, 11, 15, 19)) |>
  rename(Town_name = `X4`, 
         Town_code = `X11`,
         Pop_2012 = `X15`,
         Size = `X19`) |>
  filter(!str_detect(Town_code, "^97")) |>
  mutate(Density_2019 = Pop_2012/Size) |>
  select(Town_code, Town_name, Density_2019)

#####################################################################################

#Population 2019
Pop_2019 <- read_delim(here::here("Raw_data/Pop_2019.csv"), 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

Pop_by_town <- Pop_2019 |>
  group_by(CODGEO) |>
  summarise(Total = sum(NB)) |>
  rename(Town_code = CODGEO, Total_pop = Total)

#####################################################################################

#No diploma
Pop_16_no_diploma <- read_excel(here::here("Raw_data/pop-16ans-dipl6819_v2.xls"), 
                                sheet = "COM_2019", skip = 15)

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

#We add the total for only paris
Paris_tot <- Pop_no_diploma |> 
  filter(Dep_number == "75") |>
  summarize(No_diploma_rate1k = mean(No_diploma_rate1k)) |>
  rename(Town_code = Dep_number) |>
  mutate(Town_code = "75056") #This is paris town code

#Add our Paris total at the end
Pop_no_diploma <- Pop_no_diploma |>
  group_by(Town_code, No_diploma_rate1k) |>
  select(Town_code, No_diploma_rate1k)

Pop_no_diploma <- rbind(Pop_no_diploma, Paris_tot)

#####################################################################################

Pov_2019 <- read_delim(here::here("Raw_data/FILO2019_DEC_Pauvres_COM.csv"), 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

Povr_2019 <- Pov_2019 |>
  select(c(1, 4, 6)) |>
  na.omit() |>
  rename(Town_code = CODGEO,
         Povrety_2019 = TP6019,
         Intensity_povrety = TP60IP19)

#####################################################################################

#Add them all together
Everything_by_town <- Crime_2019_town |>
  full_join(Pop_by_town, join_by(Town_code)) |> 
  full_join(Density_town, join_by(Town_code)) |>
  full_join(Vote_2017, join_by(Town_code)) |>
  full_join(Povr_2019, join_by(Town_code)) |>
  full_join(Pop_no_diploma, join_by(Town_code))

#remove the Na's, and only select important values
Everything_by_town_clean <- Everything_by_town |>
  na.omit() |>
  select(Dep_number, 
         Town_code, 
         Town_name, 
         Total_pop, Rate_per_1k, Density_2019, Lepen, Win_Lepen, Povrety_2019, Intensity_povrety, No_diploma_rate1k)


