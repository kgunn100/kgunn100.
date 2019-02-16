library(dplyr)
library(ggplot2)
library(plotly)
library(knitr)
library(leaflet)
library(ggrepel)

# Reads the data of the shooting data of 2018
shootings_data<- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)
shootings_data <- data.frame(shootings_data)

# How many shooting occurred?
how_many_shootings <- nrow(shootings_data)

# How many people have died
lives_lost <- summarize(shootings_data,
                        lives_lost = sum(num_killed))

# The cities with the number of people injuried and num_killed exceeded the mean
most_impact_city <- shootings_data %>% 
  mutate(affect = num_injured + num_killed) %>% 
  filter(affect == max(affect)) %>% 
  select(city)


# Which state has the most people injuried and num_killed
most_impacted_state <-  shootings_data %>% 
  mutate(affect = num_injured + num_killed) %>% 
  group_by(state) %>% 
  summarise(total_impact = sum(affect)) %>% 
  filter(total_impact == max(total_impact)) %>% 
  select('state')

# My incident
parkland <- shootings_data %>% 
  filter(city == "Pompano Beach (Parkland)")


# The Summary Table consisting of the mean, median, and sum of the total people affect, injury and num_killed, by state                    
summary_table <- shootings_data %>% 
  group_by(state) %>% 
  mutate(affected = num_injured + num_killed) %>% 
  summarise(Median = median(affected),
            Mean = round(mean(affected),1),
            Sum = sum(affected))

colnames(summary_table)[1] <- "States"

# total amount of shooting by state
top_ten <- shootings_data %>%
  group_by(state) %>% 
  summarise(killed = sum(num_killed)) %>% 
  top_n( n = 10, wt = killed) %>% 
  arrange(-killed)
  
  

# Top ten states with the highest death counts 
p <- plot_ly(
  data = top_ten,      
  x = ~state, 
  y = ~killed, 
  color = ~killed, 
  type = "scatter", 
  mode = "markers"  
)

# Set up of the interactive map of the shootings in the United States of 2018
map <- leaflet(data= shootings_data) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -99.7129, lat = 39.0902, zoom =5) %>% 
  addCircles(
    lat = ~lat,
    lng = ~long,
    stroke = FALSE,
    popup = ~paste(city, "<br>", 
                   date, "<br>", 
                   "Killed: ", num_killed, 
                   "<br> Injured: ", num_injured),
    radius = 30000,
    fillOpacity = 0.5
  )





  
