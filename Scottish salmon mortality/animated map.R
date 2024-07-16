#' BGR to lat long
library(ggplot2)
library(ggmap)
library(gganimate)
library(gifski)
library(av)
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
            color = "red", 
            alpha = .7,
            maptype = 'stamen_terrain_background') + 
  transition_time(as.numeric(year)) + #adds annual transitions
  labs(title = "Year: {round(frame_time, 0)}") + 
  enter_fade() + # fade in 
  exit_fade() + # fade out
  guides(colour = "none", alpha = "none", size=guide_legend(title="Weight (kg)")) +
  scale_size_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                        range = c(1, 10)) #add thousand separator to numbers
# animate plot
myAnimation <- animate(p, 
        duration = 20, 
        fps = 25, 
        res = 200, height = 1200, width =1200)
myAnimation
anim_save("test.gif", animation = myAnimation)
