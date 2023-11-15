install.packages("ggplot2")
install.packages("sf")
install.packages("rgeos")
install.packages("dplyr")
install.packages("ggspatial")
library(readr)
library(tidyverse)
library(plotly)
library(ggplot2)
library(sf)
library(dplyr)
library(ggspatial)

#lets make a map of crime, as well as unemployment by departement

border <- st_read(here::here("Departement_geoson_carte.geojson"))
colnames(border) <- c("Dep_number", "Dep_name", "geometry")
Everything_by_dep <- read_csv(here::here("data_end/Everything_by_dep.csv"))
View(Everything_by_dep)
both <- left_join(border, Everything_by_dep, join_by("Dep_number"))
view(both)

summary(scale(both$Density_2019))



plot_map <- ggplot() +
  geom_sf(data = both, aes(fill = Unemp_2019)) + 
  scale_fill_gradient(low = "green", high = "darkgreen")+
  theme(axis.text.x  = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank()) +
  labs(title = "Unemployment rate in France")

plot_map <- ggplotly(plot_map)
plot_map






interactive <- ggplotly(plot_map)


interactive <- interactive %>%
  layout(
    updatemenus = list(
      list(
        type = "buttons",
        x = 1.05,
        y = 0.8,
        buttons = list(
          list(method = "relayout",
               args = list("mapbox.layers[0].sourcetype", "Unemp_2019"),
               label = "Crime"),
          list(method = "relayout",
               args = list("mapbox.layers[0].sourcetype", "Crime_tot_2019"),
               label = "Unemp")
        )
      )
    )
  )


interactive

