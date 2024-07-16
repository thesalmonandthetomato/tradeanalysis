library(readxl)
library(tidyverse)
library(scales)
library(plotly)

morts <- read_excel("Scottish salmon mortality/morts.xlsx")

#restrict to salmon
morts <- subset(morts, Species == "SAL")

morts$year <- substr(morts$`Start Date`, 1, 4)

morts_noNA <- drop_na(morts, `Weight (kg)`)

morts_summ <- morts_noNA %>% 
  group_by(year) %>% 
  summarise(n = sum(`Weight (kg)`))

ggplot(data=morts_summ, aes(x=year, y=n/1000)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  xlab('Year') +
  ylab('Mortality weight (tonnes)') +
  scale_y_continuous(label=comma)
