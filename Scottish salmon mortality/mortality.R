library(readxl)
library(tidyverse)
library(scales)
library(plotly)
library(gganimate)

morts <- read_excel("Scottish salmon mortality/morts.xlsx")

#restrict to salmon
morts <- subset(morts, Species == "SAL")

morts$year <- substr(morts$`Start Date`, 1, 4)

morts_noNA <- drop_na(morts, `Weight (kg)`)

#summarise by year
morts_summ <- morts_noNA %>% 
  group_by(year) %>% 
  summarise(n = sum(`Weight (kg)`))
#plot
ggplot(data=morts_summ, aes(x=year, y=n/1000)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  xlab('Year') +
  ylab('Mortality weight (tonnes)') +
  scale_y_continuous(label=comma)

#summarise by business
morts_noNA$group <- seq(1, nrow(morts_noNA), 1)
morts_noNA$`Reporting Business Name` <- tolower(morts_noNA$`Reporting Business Name`)
morts_noNA$`Reporting Business Name` <- str_to_title(morts_noNA$`Reporting Business Name`)
business_summ <- morts_noNA %>% 
  group_by(year, `Reporting Business Name`) %>% 
  summarise(n = sum(`Weight (kg)`))
#plot
p <- ggplot(data=business_summ, 
            aes(y=`Reporting Business Name`, x=n/1000),
            group = group) + #removes linkages/transitions between data) 
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  ylab('Business') +
  xlab('Mortality weight (tonnes)') +
  scale_x_continuous(label=comma) +
  scale_y_discrete(limits=rev) +
  transition_time(as.numeric(year)) +
  labs(caption = 'Year: {round(frame_time,0)}') +
  enter_fade() +
  exit_fade() +
  ease_aes("exponential-in-out") + 
  shadow_mark(fill = "red", alpha = 0.1)
# animate plot
myAnimation <- animate(p, 
                       duration = 20, 
                       fps = 25, 
                       res = 200, height = 1200, width =1200)
myAnimation
anim_save("Scottish salmon mortality/business_morts_perYear.gif", animation = myAnimation)


#summarise by site
morts_noNA$`Site Name` <- tolower(morts_noNA$`Site Name`)
morts_noNA$`Site Name` <- str_to_title(morts_noNA$`Site Name`)
site_summ <- morts_noNA %>% 
  group_by(year, `Site Name`) %>% 
  summarise(n = sum(`Weight (kg)`))
#plot
p <- ggplot(data=site_summ, 
            aes(y=`Site Name`, x=n/1000),
            group = group) + #removes linkages/transitions between data) 
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  theme(text=element_text(size = 5)) +
  ylab('Site') +
  xlab('Mortality weight (tonnes)') +
  scale_x_continuous(label=comma) +
  scale_y_discrete(limits=rev) +
  transition_time(as.numeric(year)) +
  labs(caption = 'Year: {round(frame_time,0)}') +
  enter_fade() +
  exit_fade() +
  ease_aes("exponential-in-out") + 
  shadow_mark(fill = "red", alpha = 0.1)
# animate plot
myAnimation <- animate(p, 
                       duration = 20, 
                       fps = 25, 
                       res = 200, height = 4000, width =1000)
myAnimation
anim_save("Scottish salmon mortality/site_morts_perYear.gif", animation = myAnimation)
