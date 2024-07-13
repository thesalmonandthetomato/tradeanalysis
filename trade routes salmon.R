library(leaflet)
library(geosphere)
library(tidyverse)
library(maps)

# create country lat long centers based on capitals
country_locations <- read.csv("countries_codes_and_coordinates.csv")
country_locations <- data.frame(code = country_locations$Alpha.3.code, 
                                latitude = country_locations$Latitude..average., 
                                longitude = country_locations$Longitude..average.)
country_locations$code <- trimws(country_locations$code)

colnames(data)[2] <- 'ExporterISO3'
colnames(data)[6] <- 'ImporterISO3'
data2 <- dplyr::left_join(data, country_locations, by = c("ExporterISO3" = "code"))
colnames(data2)[23] <- 'ExporterLat'
colnames(data2)[24] <- 'ExporterLong'
data2 <- dplyr::left_join(data2, country_locations, by = c("ImporterISO3" = "code"))
colnames(data2)[25] <- 'ImporterLat'
colnames(data2)[26] <- 'ImporterLong'

data2_2022 <- subset(data2, Year == 2022)
data2_2022 <- data2_2022[complete.cases(data2_2022[ , 13]),]
data2_summary_2022 <- data2_2022 %>%
  group_by(ExporterISO3, Exporter, ImporterISO3, Importer, ExporterLat, ExporterLong, ImporterLat, ImporterLong) %>%
  summarise(weight = sum(`Weight (1000kg)`)) 

# order and select top 10 exporters
data2_summary_2022 <- data2_summary_2022[order(-data2_summary_2022$weight),]
top10_exporters_2022 <- subset(data2_summary_2022, 
                               Exporter == "Norway" | Exporter == "Iceland" | Exporter == "Faeroe Islands" | Exporter == "Canada" | Exporter == "Australia" | Exporter == "United Kingdom" | Exporter == "Chile")
top10_exporters_2022 <- top10_exporters_2022[complete.cases(top10_exporters_2022[ , 3]),]

# Background map
maps::map('world',
    col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,
    mar=rep(0,4),border=0, ylim=c(-80,80) 
)
# add Importer points
points(x=top10_exporters_2022$ImporterLong, y=top10_exporters_2022$ImporterLat, col="yellow", cex=1, pch=20)
# add Exporter points
points(x=top10_exporters_2022$ExporterLong, y=top10_exporters_2022$ExporterLat, col="slateblue", cex=1, pch=20)

# plot lines function
plot_my_connection=function( dep_lon, dep_lat, arr_lon, arr_lat, ...){
  inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
  inter=data.frame(inter)
  diff_of_lon=abs(dep_lon) + abs(arr_lon)
  if(diff_of_lon > 180){
    lines(subset(inter, lon>=0), ...)
    lines(subset(inter, lon<0), ...)
  }else{
    lines(inter, ...)
  }
}

# add every connections:
for(i in 1:nrow(top10_exporters_2022)){
  plot_my_connection(top10_exporters_2022$ExporterLong[i], top10_exporters_2022$ExporterLat[i], top10_exporters_2022$ImporterLong[i], top10_exporters_2022$ImporterLat[i], col="skyblue", lwd=1)
}
