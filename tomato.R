library(readxl)
library(tidyverse)
library(scales)
library(plotly)
library(viridis)
library(hrbrthemes)

options("scipen"=100, "digits"=4) #set not to use scientific display of integers

# import data
data <- read_excel("2007-2022_tomatoes_allExporters_allImporters.xlsx", 
                   sheet = "Trades")
data <- data[complete.cases(data[ , 13]),]

# UK tomato suppliers over time
# select top 15 exporters in 2022 and combine all others
UK <- subset(data, Importer == "United Kingdom")
UK_summary <- UK %>%
  group_by(`Exporter ISO3`, Exporter, Year) %>%
  summarise(weight = sum(`Weight (1000kg)`))
UK_summary <- UK_summary[order(UK_summary$weight, decreasing = TRUE),]
# combine all non-top-15 countries
UK_top15 <- unique(UK_summary$Exporter)[1:15]
UK_summary_top15 <- subset(UK_summary, Exporter %in% UK_top15)
UK_summary_rest <- subset(UK_summary, !Exporter %in% UK_top15)
UK_summary_rest <- UK_summary_rest %>%
  group_by(Year) %>%
  summarise(weight = sum(weight))
UK_summary_rest$Exporter <- "Other countries"
UK_summary <- rbind(UK_summary_top15, UK_summary_rest)
UK_summary$Exporter <- factor(UK_summary$Exporter, levels=c(UK_top15, "Other countries"))
# line plot
UK_p1 <- ggplot(UK_summary, aes(y=weight, x=Year, group=Exporter, text=paste0(Exporter,": ",format(round(weight,0),big.mark=","), " tonnes"))) +
  geom_line(aes(color=Exporter)) +
  geom_point(aes(color=Exporter)) + 
  #theme(legend.position="none") +
  labs(y="Weight (tonnes)", 
       x="Year",
       title="United Kingdom") +
  scale_y_continuous(label=comma)
ggplotly(UK_p1, tooltip = c("text"))
# stacked area
UK_p1 <- ggplot(UK_summary, aes(y=weight, 
                       x=Year, 
                       fill=Exporter)) + 
  geom_area(alpha=0.6 , linewidth=.5, colour="white") +
  scale_fill_viridis(discrete = T, direction = -1) +
  theme_ipsum() +
  #theme(legend.position="none") +
  labs(y="Weight (tonnes)", 
       x="Year",
       title="United Kingdom") +
  scale_y_continuous(label=comma)
ggplotly(UK_p1, tooltip = c("text"))

# Germany tomato suppliers over time
DE <- subset(data, Importer == "Germany")
DE_summary <- DE %>%
  group_by(`Exporter ISO3`, Exporter, Year) %>%
  summarise(weight = sum(`Weight (1000kg)`))
DE_summary <- DE_summary[order(DE_summary$weight, decreasing = TRUE),]
# combine all non-top-15 countries
DE_top15 <- unique(DE_summary$Exporter)[1:15]
DE_summary_top15 <- subset(DE_summary, Exporter %in% DE_top15)
DE_summary_rest <- subset(DE_summary, !Exporter %in% DE_top15)
DE_summary_rest <- DE_summary_rest %>%
  group_by(Year) %>%
  summarise(weight = sum(weight))
DE_summary_rest$Exporter <- "Other countries"
DE_summary <- rbind(DE_summary_top15, DE_summary_rest)
DE_summary$Exporter <- factor(DE_summary$Exporter, levels=c(DE_top15, "Other countries"))
# line plot
DE_p1 <- ggplot(DE_summary, aes(y=weight, x=Year, group=Exporter, text=paste0(Exporter,": ",format(round(weight,0),big.mark=","), " tonnes"))) +
  geom_line(aes(color=Exporter)) +
  geom_point(aes(color=Exporter)) + 
  #theme(legend.position="none") +
  labs(y="Weight (tonnes)", 
       x="Year",
       title="Germany") +
  scale_y_continuous(label=comma)
ggplotly(DE_p1, tooltip = c("text"))
# stacked area
DE_p2 <- ggplot(DE_summary, aes(y=weight, 
                       x=Year, 
                       fill=Exporter)) + 
  geom_area(alpha=0.6 , linewidth=.5, colour="white") +
  scale_fill_viridis(discrete = T, direction = -1) +
  theme_ipsum() +
  #theme(legend.position="none") +
  labs(y="Weight (tonnes)", 
       x="Year",
       title="United Kingdom") +
  scale_y_continuous(label=comma)
ggplotly(DE_p2, tooltip = c("text"))

# Germany tomato suppliers over time
FR <- subset(data, Importer == "France")
FR_summary <- FR %>%
  group_by(`Exporter ISO3`, Exporter, Year) %>%
  summarise(weight = sum(`Weight (1000kg)`))
FR_summary <- FR_summary[order(FR_summary$weight, decreasing = TRUE),]
# combine all non-top-15 countries
FR_top15 <- unique(FR_summary$Exporter)[1:15]
FR_summary_top15 <- subset(FR_summary, Exporter %in% FR_top15)
FR_summary_rest <- subset(FR_summary, !Exporter %in% FR_top15)
FR_summary_rest <- FR_summary_rest %>%
  group_by(Year) %>%
  summarise(weight = sum(weight))
FR_summary_rest$Exporter <- "Other countries"
FR_summary <- rbind(FR_summary_top15, FR_summary_rest)
FR_summary$Exporter <- factor(FR_summary$Exporter, levels=c(FR_top15, "Other countries"))
# line plot
FR_p1 <- ggplot(FR_summary, aes(y=weight, x=Year, group=Exporter, text=paste0(Exporter,": ",format(round(weight,0),big.mark=","), " tonnes"))) +
  geom_line(aes(color=Exporter)) +
  geom_point(aes(color=Exporter)) + 
  #theme(legend.position="none") +
  labs(y="Weight (tonnes)", 
       x="Year",
       title="France") +
  scale_y_continuous(label=comma)
ggplotly(FR_p1, tooltip = c("text"))
# stacked area
FR_p2 <- ggplot(FR_summary, aes(y=weight, 
                                x=Year, 
                                fill=Exporter)) + 
  geom_area(alpha=0.6 , linewidth=.5, colour="white") +
  scale_fill_viridis(discrete = T, direction = -1) +
  theme_ipsum() +
  #theme(legend.position="none") +
  labs(y="Weight (tonnes)", 
       x="Year",
       title="France") +
  scale_y_continuous(label=comma)
ggplotly(FR_p2, tooltip = c("text"))





# Spanish exports
ES <- subset(data, Exporter == "Spain")
ES_summary <- ES %>%
  group_by(`Importer ISO3`, Importer, Year) %>%
  summarise(weight = sum(`Weight (1000kg)`))
ES_summary <- ES_summary[order(ES_summary$weight, decreasing = TRUE),]
# combine all non-top-15 countries
ES_top15 <- unique(ES_summary$Importer)[1:15]
ES_summary_top15 <- subset(ES_summary, Importer %in% ES_top15)
ES_summary_rest <- subset(ES_summary, !Importer %in% ES_top15)
ES_summary_rest <- ES_summary_rest %>%
  group_by(Year) %>%
  summarise(weight = sum(weight))
ES_summary_rest$Importer <- "Other countries"
ES_summary <- rbind(ES_summary_top15, ES_summary_rest)
ES_summary$Importer <- factor(ES_summary$Importer, levels=c(ES_top15, "Other countries"))
# line plot
ES_p1 <- ggplot(ES_summary, aes(y=weight, x=Year, group=Importer, text=paste0(Importer,": ",format(round(weight,0),big.mark=","), " tonnes"))) +
  geom_line(aes(color=Importer)) +
  geom_point(aes(color=Importer)) + 
  #theme(legend.position="none") +
  labs(y="Weight (tonnes)", 
       x="Year",
       title="Spanish tomato exports") +
  scale_y_continuous(label=comma)
ggplotly(ES_p1, tooltip = c("text"))
# stacked area
ES_p2 <- ggplot(ES_summary, aes(y=weight, 
                                x=Year, 
                                fill=Importer)) + 
  geom_area(alpha=0.6 , linewidth=.5, colour="white") +
  scale_fill_viridis(discrete = T, direction = -1) +
  theme_ipsum() +
  #theme(legend.position="none") +
  labs(y="Weight (tonnes)", 
       x="Year",
       title="Cumulative Spanish tomato exports") +
  scale_y_continuous(label=comma)
ggplotly(ES_p2, tooltip = c("text"))



