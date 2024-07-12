library(readxl)
library(tidyverse)
library(scales)
library(plotly)

options("scipen"=100, "digits"=4) #set not to use scientific display of integers

# import data
data <- read_excel("2007-2022_freshSalmon_allExporters_allImporters.xlsx", 
                   sheet = "Trades")

UK <- subset(big_exporters_time, Exporter == "United Kingdom")

#summarise EXPORTERS by country for 2022
data_2022 <- subset(data, Year == 2022)
data_2022 <- data_2022[complete.cases(data_2022[ , 13]),]
data_summary_2022_exporters <- data_2022 %>%
  group_by(`Exporter ISO3`, Exporter) %>%
  summarise(weight = sum(`Weight (1000kg)`)) 
write.csv(data_summary_2022_exporters, '2022 salmon importer data.csv', row.names = FALSE)

#summarise IMPORTERS by country for 2022
data_summary_2022_importers <- data_2022 %>%
  group_by(`Importer ISO3`, Importer) %>%
  summarise(weight = sum(`Weight (1000kg)`)) 
write.csv(data_summary_2022_importers, '2022 salmon exporter data.csv', row.names = FALSE)

# top 10 exporters in 2022
data_summary_2022_exporters <- data_summary_2022_exporters[order(-data_summary_2022_exporters$weight),]
top10_exporters_2022 <- data_summary_2022_exporters$Exporter[1:10]

# top 10 exporters in 2022 over time
big_exporters <- data.frame(Exporter = top10_exporters_2022)
# set up source data
data2 <- data[complete.cases(data[ , 13]),]
source_data <- data2 %>%
  group_by(`Exporter ISO3`, Exporter, Year) %>%
  summarise(weight = sum(`Weight (1000kg)`)) 
source_data <- pivot_wider(source_data, names_from = Year, values_from = weight)
# merge dfs
big_exporters_time <- dplyr::left_join(big_exporters, source_data, by = "Exporter")
big_exporters_time <- cbind(big_exporters_time[,1:2], big_exporters_time[ , sort(colnames(big_exporters_time)[3:22])])
big_exporters_time <- big_exporters_time %>%
  pivot_longer(cols = -c(Exporter, `Exporter ISO3`), names_to = "Year", values_to = "weight")
big_exporters_time <- merge(big_exporters_time, alpha, by.x = "Exporter", 
                            by.y = "Exporter", all.x = TRUE, all.y = FALSE)
big_exporters_time$labels <- paste0(big_exporters_time$Year, ": ", big_exporters_time$Exporter, " - ", label_comma(accuracy = 1)(big_exporters_time$weight), " tonnes")
# restrict to cage producers
big_exporters_time <- subset(big_exporters_time, Exporter == "Australia" | Exporter == "Canada" | Exporter == "Chile" | Exporter == "Faeroe Islands" | Exporter == "Iceland" | Exporter == "Norway" | Exporter == "United Kingdom")
big_exporters_time <- big_exporters_time[!is.na(big_exporters_time$weight),]

# plot
p <- ggplot(data=big_exporters_time, aes(x=Year, y=weight, text = labels, group=Exporter, colour=Exporter)) +
  geom_line() + geom_point() +
  scale_color_brewer(palette="Paired") +
  scale_alpha(guide = 'none') +
  scale_y_continuous(label=comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Year") + ylab("Weight (tonnes)")

ggplotly(p, tooltip = c("labels"))



# declare `city` as the SQL 'query by' column
tx <- highlight_key(big_exporters_time, key=~Exporter)

# initiate a plotly object
base <- plot_ly(tx, x=~Year, y=~weight, color = ~Exporter)

# create a time series of median house price
plot <- base %>%
  group_by(Exporter) %>%
  add_lines(x = ~Year, y = ~weight,
            hovertemplate = paste('<b>%{x}</b>: %{y:,.0f} tonnes'),) %>%
  layout(title = "Top salmon exporters in 2022",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Weight (tonnes)"))
plot 
