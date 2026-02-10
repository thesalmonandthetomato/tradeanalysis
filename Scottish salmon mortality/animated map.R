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
morts <- read_excel("Scottish salmon mortality/morts 2026.xlsx")

#restrict to salmon
morts <- subset(morts, Species == "SAL")

# convert OS NGR to lat/long
morts$latitude <- osg_parse(grid_refs = c(morts$`OS Grid Reference`), coord_system = c("WGS84"))$lat
morts$longitude <- osg_parse(grid_refs = c(morts$`OS Grid Reference`), coord_system = c("WGS84"))$lon

# remove NAs
morts <- drop_na(morts, `Weight (kg)`)
morts <- drop_na(morts, latitude)

# create event date from reporting date
morts$event_date <- as.Date(
  morts$`Date reported`,
  tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y")
)
# fallback for Excel-style numeric dates in any still-missing rows
reported_numeric <- suppressWarnings(as.numeric(morts$`Date reported`))
excel_date_rows <- is.na(morts$event_date) & !is.na(reported_numeric)
morts$event_date[excel_date_rows] <- as.Date(reported_numeric[excel_date_rows], origin = "1899-12-30")
morts <- drop_na(morts, event_date)
morts$group <- seq(1, nrow(morts), 1)

# keep bubbles visible for ~3-6 months (default 4 months) in the timeline
persist_months <- 4
persist_days <- round(30.44 * persist_months)
persist_days <- max(90, min(183, persist_days))

# expand each event across daily timesteps so visibility is continuous (~3-6 months)
morts_anim <- morts %>%
  tidyr::uncount(weights = persist_days, .id = "day_index") %>%
  mutate(display_date = event_date + (day_index - 1))

key <- '890cbc42-c1c1-4f8e-a575-ac0a17536104'
register_stadiamaps(key, write = TRUE)
# create plot
p <- qmplot(x = longitude, y = latitude, data = morts_anim, 
            group = group, #removes linkages/transitions between data
            size = `Weight (kg)`, 
            color = `Total mortality during event`, 
            alpha = .8,
            maptype = 'stamen_terrain_background',
            darken = .12,
            zoom = 6) + 
  transition_time(display_date) + # animate continuously by reported event date
  labs(
    title = "Scottish salmon mortality by location",
    subtitle = "Date reported: {format(frame_time, '%Y-%m-%d')}",
    caption = "Mortality weight estimated from av. weight x mortalities",
    x = NULL,
    y = NULL
  ) + 
  shadow_wake(wake_length = 0.04, alpha = FALSE) +
  guides(alpha = "none", size = guide_legend(title = "Weight (kg)"),
         colour = guide_colorbar(title = "Total mortality during event")) +
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
                       duration = 60, 
                       fps = 25, 
                       res = 200, height = 1200, width =1200)
myAnimation
anim_save("Scottish salmon mortality/morts.gif", animation = myAnimation)


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
