#' BGR to lat long
library(ggplot2)
library(ggmap)
library(gganimate)
library(gifski)
library(av)
library(e1071)
library(rnrfa)
library(readxl)
library(tidyverse)
require(scales) 

# load data 
morts <- read_excel("Scottish salmon mortality/morts.xlsx")

#restrict to salmon
morts <- subset(morts, Species == "SAL")

# convert OS NGR to lat/long
morts$latitude <- osg_parse(grid_refs = c(morts$`OS Grid Reference`), coord_system = c("WGS84"))$lat
morts$longitude <- osg_parse(grid_refs = c(morts$`OS Grid Reference`), coord_system = c("WGS84"))$lon

# remove NAs
morts <- drop_na(morts, `Weight (kg)`)
morts <- drop_na(morts, latitude)

# create year
morts$year <- substr(morts$`Start Date`, 1, 4)
morts$group <- seq(1, nrow(morts), 1)

key <- '890cbc42-c1c1-4f8e-a575-ac0a17536104'
register_stadiamaps(key, write = TRUE)
# create plot
p <- qmplot(x = longitude, y = latitude, data = morts, 
            group = group, #removes linkages/transitions between data
            size = `Weight (kg)`, 
            color = `Weight (kg)`, 
            alpha = .8,
            maptype = 'stamen_terrain_background',
            darken = .12,
            zoom = 6) + 
  transition_states(year, transition_length = 1, state_length = 4, wrap = FALSE) + #adds annual transitions
  labs(
    title = "Scottish salmon mortality by location",
    subtitle = "Year: {closest_state}",
    caption = "Point size and colour both represent reported mortality weight (kg)",
    x = NULL,
    y = NULL
  ) + 
  enter_fade() + enter_grow() + # smooth fade/scale in
  exit_fade() + exit_shrink() + # smooth fade/scale out
  shadow_wake(wake_length = 0.08) +
  guides(alpha = "none", size = guide_legend(title = "Weight (kg)"),
         colour = guide_colorbar(title = "Weight (kg)")) +
  scale_size_continuous(
    trans = "sqrt",
    labels = label_number(big.mark = ",", accuracy = 1),
    range = c(1.5, 11)
  ) + # add thousand separator to numbers
  scale_colour_viridis_c(
    option = "magma",
    direction = -1,
    labels = label_number(big.mark = ",", accuracy = 1)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey20")
  ) +
  ease_aes("sine-in-out")
# animate plot
myAnimation <- animate(p, 
        duration = 20, 
        fps = 25, 
        res = 200, height = 1200, width =1200)
myAnimation
anim_save("Scottish salmon mortality/test.gif", animation = myAnimation)


# map of locations moving over space
data <- read.csv("animated trade flows data.csv")

p <- qmplot(x = long, y = lat, data = data, 
            group = group, #removes linkages/transitions between data
            color = group, 
            maptype = 'stamen_terrain_background',
            size = 1.5) + 
  transition_time(as.numeric(time)) + #adds annual transitions
  labs(title = "Time: {round(frame_time, 0)}") + 
  ease_aes("sine-out") + 
  shadow_wake(wake_length = 1)
animate(p, 
        duration = 20, 
        fps = 25, 
        res = 200
        , height = 1200, width =1200
        )
