library(readxl)
library(tidyverse)
library(scales)
library(plotly)


#' comtrade codes for salmon
#' 030212 - Fresh Or Chilled Pacific Salmon Oncorhynchus Nerka, Oncorhynchus Gorbuscha, Oncor
#' 0302140003 - Atlantic and Danube salmon, fresh or chilled, excluding fillets, other meat portions, livers and roes - Farmed
#' 030214 - Fresh or chilled Atlantic salmon "Salmo salar" and Danube salmon "Hucho hucho"
#' 030313 - Frozen, Atlantic salmon "Salmo salar" and Danube salmon "Hucho hucho"
#' 030541 - Smoked Pacific salmon "Oncorhynchus nerka, Oncorhynchus gorbuscha, Oncorhynchus keta, Oncorhynchus tschawytscha, Oncorhynchus kisutch, Oncorhynchus masou and Oncorhynchus rhodurus", Atlantic salmon "Salmo salar" and Danube salmon "Hucho hucho", incl. fillets (excl. offal)
#' 030441 - Fresh or chilled fillets of Pacific salmon "Oncorhynchus nerka, Oncorhynchus gorbuscha, Oncorhynchus keta, Oncorhynchus tschawytscha, Oncorhynchus kisutch, Oncorhynchus masou and Oncorhynchus rhodurus", Atlantic salmon "Salmo salar" and Danube salmon "Hucho hucho"

options("scipen"=100, "digits"=4) #set not to use scientific display of integers

# import data
data <- read_excel("2007-2022_freshSalmon_allExporters_allImporters.xlsx", 
                   sheet = "Trades")

# UK data
UK <- subset(big_exporters_time, Exporter == "United Kingdom")

# subset data for 2022
data_2022 <- subset(data, Year == 2022)
data_2022 <- data_2022[complete.cases(data_2022[ , 13]),]

#summarise EXPORTERS by country for 2022
data_summary_2022_exporters <- data_2022 %>%
  group_by(`Exporter ISO3`, Exporter) %>%
  summarise(weight = sum(`Weight (1000kg)`)) 
write.csv(data_summary_2022_exporters, '2022 salmon importer data.csv', row.names = FALSE)

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




#summarise IMPORTERS by country for 2022
data_summary_2022_importers <- data_2022 %>%
  group_by(`Importer ISO3`, Importer) %>%
  summarise(weight = sum(`Weight (1000kg)`)) 
write.csv(data_summary_2022_importers, '2022 salmon importer data.csv', row.names = FALSE)

# top 10 importers in 2022
data_summary_2022_importers <- data_summary_2022_importers[order(-data_summary_2022_importers$weight),]
top20_importers_2022 <- data_summary_2022_importers$Importer[1:20]

# top 10 exporters in 2022 over time
big_importers <- data.frame(Importer = top20_importers_2022)
# set up source data
data2 <- data[complete.cases(data[ , 13]),]
source_data <- data2 %>%
  group_by(`Importer ISO3`, Importer, Year) %>%
  summarise(weight = sum(`Weight (1000kg)`)) 
source_data <- pivot_wider(source_data, names_from = Year, values_from = weight)
# merge dfs
big_importers_time <- dplyr::left_join(big_importers, source_data, by = "Importer")
big_importers_time <- cbind(big_importers_time[,1:2], big_importers_time[ , sort(colnames(big_importers_time)[3:22])])
big_importers_time <- big_importers_time %>%
  pivot_longer(cols = -c(Importer, `Importer ISO3`), names_to = "Year", values_to = "weight")
big_importers_time <- merge(big_importers_time, alpha, by.x = "Importer", 
                            by.y = "Importer", all.x = TRUE, all.y = FALSE)
big_importers_time$labels <- paste0(big_importers_time$Year, ": ", big_importers_time$Importer, " - ", label_comma(accuracy = 1)(big_importers_time$weight), " tonnes")
# restrict to cage producers
big_importers_time <- big_importers_time[!is.na(big_importers_time$weight),]
big_importers_time <- big_importers_time[!is.na(big_importers_time$`Importer ISO3`),]

# plot
p <- ggplot(data=big_importers_time, aes(x=Year, y=weight, group=Importer, colour=Importer,
                                         text = paste0("Year: ", Year, "\nImporter: ", Importer, "\n", comma(round(weight,0)), " tonnes"))) +
  geom_line() + geom_point() +
  scale_alpha(guide = 'none') +
  scale_y_continuous(label=comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Year") + ylab("Weight (tonnes)")

ggplotly(p, tooltip = c("text"))
