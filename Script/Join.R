library(readxl)
library(readr)
library(dplyr)
library(tidyverse)

#Put all the department data into a single dataset

#Open crimes and population
Pop_by_year <- read_csv(here::here("data_end/Pop_by_year.csv"))
View(Pop_by_year)

Pop_2019 <- Pop_by_year |> 
  select(Dep_number, pop_2019)
View(Pop_2019)

Crime <- read_csv(here::here("data_end/Crime_to_use.csv"))
View(Crime)

Crime_2019 <- Crime |>
  select(Dep_number, `2019`) |>
  rename(Crime_tot_2019 = `2019`)

Crime_2019 <- full_join(Crime_2019, Pop_2019, join_by("Dep_number"))
View(Crime_2019)

Crime_2019 <- Crime_2019 |>
  mutate(Crime_rate_1k = (Crime_tot_2019/pop_2019)*1000)


#Open list of names department
List_depart <- read_csv(here::here("data_end/List_depart.csv"))
View(List_depart)
List_depart <- List_depart[-1]

#Open Unemployment and only take 2019
Unemp <- read_csv("data_end/Unemployment_year.csv")
View(Unemp)

Unemp <- Unemp |> 
  select(Departement, `2019`) |>
  rename(Dep_name = Departement,
         Unemp_2019 = `2019`)


#take the sucess rate at the middle school final exam
Middle_results <- read.csv(here::here("data_start/Final/fr-en-dnb-par-etablissement.csv"), 
                                          sep = ";")
View(Middle_results)


Middle <- Middle_results |>
  filter(Session == 2019) |>
  select(Code.département, Admis, Inscrits) |>
  mutate(Pass_rate = (Admis/Inscrits)) |>
  group_by(Code.département) |>
  summarize(Pass_rate_2019 = mean(Pass_rate)) |>
  rename(Dep_number = Code.département) |>
  mutate(Dep_number = substr(Dep_number, 2, 3))

Middle <- Middle[(2:97), ]
view(Middle)


#Now for election results by department
Election_dep <- read_excel("data_start/Final/Election_dep.xls", 
                           sheet = "Départements Tour 2", skip = 2)
View(Election_dep)

Election_by_dep <- Election_dep |>
  select(c(1,28)) |>
  rename(Dep_number = `Code du département`,
         Lepen_score = `% Voix/Exp...28`) |>
  mutate(Win_lepen = ifelse(Lepen_score>50, 1, 0))


Election_by_dep <- Election_by_dep[1:96, ]

#Need to change the dep notation
for (i in 1:nrow(Election_by_dep)) {
  if(nchar(Election_by_dep$Dep_number[i]) == 1) {
    Election_by_dep$Dep_number[i] <- paste0("0",  Election_by_dep$Dep_number[i])
  }
}

View(Election_by_dep)

#Add immigration by department
Immigration_2019 <- read_excel(here::here("data_start/Final/Immigration_2019.xlsx"), 
                               sheet = "Pop0_D", skip = 1)
View(Immigration_2019)

Immig_2019 <- Immigration_2019 |>
  slice(1:96) |>
  select(c(1, 2, 4)) |>
  rename(Dep_name = `...1`,
         Immig_tot = Immigrés,
         pop_2019 = `Ensemble...4`) |>
  mutate(Immig_rate = Immig_tot/pop_2019) |>
  select(Dep_name, Immig_rate)


view(Immig_2019)


#Population density 2019
population_2019 <- read_excel(here::here("data_start/Final/population_2019.xlsx"), 
                              sheet = "Figure 3", skip = 2)
View(population_2019)

Dens_2019 <- population_2019 |>
  rename(Dep_number = Département,
         Density_2019 = Densité) |>
  select(Dep_number, Density_2019) |>
  slice(1:96)

view(Dens_2019)


#Join them all together
Full_data_dep <- full_join(List_depart, Unemp, join_by("Dep_name")) |>
  full_join(Crime_2019, join_by("Dep_number")) |>
  full_join(Middle, join_by("Dep_number")) |>
  full_join(Election_by_dep, join_by("Dep_number")) |>
  full_join(Immig_2019, join_by("Dep_name")) |>
  full_join(Dens_2019, join_by("Dep_number"))

view(Full_data_dep)
  
    
    
