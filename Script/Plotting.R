library(readxl)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(gganimate)
install.packages("gifski")
library(gifski)



#Histogram of the evolution of crimes per year
Five_hist <- Five_common |>
  group_by(Year, Type) |>
  summarize(Rate_per_1k = mean(Rate_per_1k, na.rm = TRUE))


View(Five_hist)

evol5 <- ggplot(Five_hist) +
  geom_col(aes(x = as.factor(Year), y = Rate_per_1k, fill = Type), position = "dodge") +
  labs(Title= "Evolution of types of crime by year", y = "Rate per thousand", x = "Years")
evol5 <- ggplotly(evol5)
evol5

#Comparison of Paris and a rural departement
Dep75 <- Five_common %>%
  group_by(Departement, Type, Year) %>%
  summarize(Rate_per_1k= mean(Rate_per_1k, na.rm=TRUE)) %>%
  filter(Departement=="75")
ggplot(Dep18, aes(x=Year, y=Rate_per_1k, color=Type)) + 
  geom_line() 



#Evolution of unemployement by year
Unemployment_T <- read_csv(here::here("data_end/Unemployment_T.csv"))

Unemployment_fr <- Unemployment_T[-(1)] |> 
  pivot_longer(-Departement, names_to = "Year", values_to = "Rate") |>
  group_by(Year) |>
  summarize(Rate = mean(Rate))
view(Unemployment_fr)


ggplot(Unemployment_fr, aes(x = Year, y = Rate, group = 1)) + 
  geom_rect(xmin="1990.T3", xmax="1991.T1", ymin=-Inf, ymax = Inf, fill= "gray", alpha = 0.5)+
  geom_text(x="1990.T3", y = 10.2, nudge_x = -0.4, label= "First Gulf War")+
  geom_rect(xmin="2008.T3", xmax="2009.T3", ymin=-Inf, ymax = Inf, fill= "gray", alpha = 0.5)+
  geom_text(x="2007.T3", y = 10.2, label= "2008 crisis")+
  geom_rect(xmin="2020.T1", xmax="2021.T4", ymin=-Inf, ymax = Inf, fill= "gray", alpha = 0.5)+
  geom_text(x="2020.T1", y = 10.1, label= "COVID-19 \n Pandemic")+
  geom_line() +
  labs(title = "Unemployment rate trough the years", y = "Unemployement rate in %") +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(color = "black", fill = "white")) +
  scale_x_discrete("Years", 
                   breaks = Unemployment_fr$Year[seq(1, nrow(Unemployment_fr), 8)], 
                   labels=seq(1982, 2023, 2))




#Unemployment of every departement in 2022 interactive graph
Unemployment_2022 <- select(Unemployment, Departement, "2022")
colnames(Unemployment_2022) <- c("Departement", "Rate")
Unemp_2022 <- arrange(Unemployment_2022, desc(Rate))



Bars <- ggplot(Unemp_2022, aes(x = reorder(Departement, Rate), y = Rate, fill = Departement)) +
  geom_col() + 
  theme(axis.text.y=element_blank(), legend.position = "none")+
  labs(Title = "Unemployment rate in 2022 by Departement", x = "Rate(%)") +
  theme_minimal() +
  coord_flip()

Bars <- ggplotly(Bars)
Bars




#Plot the evolution of crime in the least criminal states and in the most criminal states
Plus_minus <- read_csv(here::here("data_end/Crime_to_use.csv"))
view(Plus_minus)

Most <- Plus_minus[-(1)] |> 
  filter(Dep_number %in% c("92","69","13","93","75")) |>
  pivot_longer(as.character(1996:2021), names_to = "Year", values_to = "Tot_crime")


Least <- Plus_minus[-(1)] |> 
  filter(Dep_number %in% c("12","15","50","70","48")) |>
  pivot_longer(as.character(1996:2021), names_to = "Year", values_to = "Tot_crime")


Most_plot <- ggplot(Most, aes(x = as.numeric(Year), y= Tot_crime, color =Dep_number,)) +
  geom_line() +
  labs(Title= "Evolution of crime in the most criminal departments", x= "Year", y= "Total Crime")+
  theme_minimal()

Least_plot <- ggplot(Least, aes(x = as.numeric(Year), y= Tot_crime, color =Dep_number,)) +
  geom_line() +
  labs(Title= "Evolution of crime in the least criminal departments", x= "Year", y= "Total Crime")+
  theme_minimal()

Most_plot <- ggplotly(Most_plot)
Most_plot

Least_plot <- ggplotly(Least_plot)
Least_plot
