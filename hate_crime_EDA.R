#Hate Crime EDA
library(tidyverse)
library(urbnmapr)
library(fivethirtyeight)

hc <- fivethirtyeight::hate_crimes


#Joining fivethirtyeight data with mapping data
state <- urbnmapr::states
names(state)[names(state) == "state_name"] <- "state" #Changing state_name to state to match hc data

hc_state <- left_join(state, hc, by = "state")

#Midwest states
midwest <- c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", 
             "Minnesota", "Missouri", "Nebraska", "North Dakota", 
             "Ohio", "South Dakota", "Wisconsin")

hc_viz <- hc_state %>%
  filter(state == midwest) %>%     #Filter down to Midwest States
  ggplot(aes(long, lat, 
             group = group,
             fill = avg_hatecrimes_per_100k_fbi)) +
  scale_fill_continuous(high = "#ff0000", low = "#ffdab9")+
  geom_polygon(color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black"),
        plot.title = element_text(face = "bold", hjust = .5),
        plot.caption = element_text(face = "italic", hjust = 1.45)) +
  labs(fill = "Average Hate Crimes per 100,000 people",
       caption = "Based on data from the FBI aggregated by fivethirtyeight") +
  ggtitle("Average Hate Crimes in the Midwest 2010-2015")

hc_viz

