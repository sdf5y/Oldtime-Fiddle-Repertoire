setwd("C:/Users/18045/Documents/R/fiddle")
#install.packages("xtable", "dplyr",  "readxl", "gridExtra","xtable", "readr", "car", "maps", "mapdata", "usmap", "sf", "tigris")
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)
library(gridExtra)
library(xtable)
library(car)
library(writexl)
library(maps)
library(mapdata)
library(usmap)
library(sf)
library(tigris)
#install.packages('plotly')
library(plotly)

#LoadData----

fiddle_ink <- read_excel("Collection of Fiddle Tunes.xlsx", 
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "skip"))

artist_df <- read_csv("Artist Location.csv") #google county level by hand

#Tally by Key ----

fiddle_copy <- fiddle_ink
i <- 0
fiddle_copy$Other <- NA
for (i in 1:length(fiddle_copy$Key)) {
  row <- fiddle_copy$Key[i]
  
  if (grepl(";", fiddle_copy$Key[i]) == TRUE) {
    fiddle_copy$Key[i] <- substr(row, 1, 1)
  }
  else if (grepl("/", fiddle_copy$`Key`[i])) {
    fiddle_copy$Other[i] <- "Cotillion"
    fiddle_copy$Key[i] <- substr(row, 1, 1)
  }
  else if (grepl("modal", fiddle_copy$`Key`[i])) {
      fiddle_copy$Other[i] <- "Modal"
      fiddle_copy$Key[i] <- substr(row, 1, 1)
  }
  else if (grepl("minor", fiddle_copy$`Key`[i])) {
      fiddle_copy$Other[i] <- "Minor"
      fiddle_copy$Key[i] <- substr(row, 1, 1)
  }
  else if (grepl(" ", fiddle_copy$`Key`[i])) {
    fiddle_copy$Key[i] <- substr(row, 1, 1)
  }
}

big_tab <- 0
big_tab <- fiddle_copy %>%
  group_by(`Tuning Notes`) %>%
  count(substr(fiddle_ink$Key, 1, 1), `Tuning Notes`) %>%
  rename("Key" = `substr(fiddle_ink$Key, 1, 1)`, 
         "Count" = `n`) 

big_tab$Key <- ifelse(big_tab$Key == "B", "Bb", big_tab$Key)

big_key <- fiddle_copy %>%
  group_by(`Tuning Notes`) %>%
  count(Key, Other)

colnames(big_key) <- c("Tuning Notes","Key",  "Other", "Count")

#-----Plots ----

#Plots of tuning and key----

big_tab %>%
  arrange(`Tuning Notes`) %>%
  mutate(`Tuning Notes` = factor(`Tuning Notes`, levels = c("GDAE",
                                                        "ADAE",
                                                        "AEAE",
                                                        "DDAD",
                                                        "AEAC#",
                                                        "GDAD",
                                                        "AEAD",
                                                        "AEF#C#",
                                                        "EDAE")))%>%
  ggplot(aes(x = `Tuning Notes`,
           y = `Count`,
           fill = `Key`,
           color = `Key`
           ) )+
  geom_histogram(stat = "identity")+
  labs(title = "Fiddle Tunings by Key",
       y = "Count",
       x = "Tuning")+
  theme_minimal()+
  scale_alpha(guide = 'none')+ 
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,  margin = margin(r=0)))

#Plot of Tonal Characteristics----
big_key %>%
  arrange(`Other`) %>%
  filter(!is.na(`Other`)) %>%
  mutate(`Tuning Notes` = factor(`Tuning Notes`, levels = c("GDAE",
                                                            "AEAE",
                                                            "ADAE",
                                                            "EDAE")))%>%
  ggplot(aes(x = `Tuning Notes`,
             y = `Count`,
             fill = `Other`))+
  geom_histogram(stat = "identity")+
  labs(title = "Tonal Characteristics by Tuning",
       y = "Count",
       x = "Tuning")+
  theme_minimal()+
  theme(text = element_text(size = 30),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, margin = margin(r=0)))

#Plot of Crooked Characteristics----

fiddle_ink %>%
  arrange(`Tuning Notes`) %>%
  filter(!is.na(`Crooked [-/+]`)) %>%
  count(`Tuning Notes`, `Crooked [-/+]`) %>%
  mutate(`Tuning Notes` = factor(`Tuning Notes`, levels = c("GDAE",
                                                            "AEAE",
                                                            "ADAE",
                                                            'AEF#C#')))%>%
  ggplot(aes(x = `Tuning Notes`,
             y = `n`,
             fill = `Crooked [-/+]`,
             color = `Crooked [-/+]`))+
  geom_histogram(stat = "identity")+
  labs(title = "Crooked Characteristics by Tuning",
       y = "Count",
       x = "Tuning")+
  ylim(0, 15)+
  theme_minimal()+
  theme(text = element_text(size = 30),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, margin = margin(r=0)))

#Tally by Artist----
source_df <-fiddle_copy %>%
  group_by(gsub("\\ /.*","", fiddle_copy$`Source of Tune`)) %>%
  count(`Tuning Notes`)

colnames(source_df) <- c("Name", "Tuning Notes", "Count")

source_2_df <- fiddle_copy %>%
  group_by(gsub("\\ /.*","", fiddle_copy$`Source of Tune`)) %>%
  summarise(count= n()) 
  
colnames(source_2_df) <- c("Name", "Count")

source_df <- fiddle_copy %>%
  group_by(gsub("\\ /.*","", fiddle_copy$`Source of Tune`)) %>%
  count(`Key`)

colnames(source_df) <- c("Name", "Key", "Count")

#write.csv(source_2_df, file = "artistlocation0.csv")

artist_locations <- na.omit(merge(source_2_df, artist_df, by.x = 'Name', by.y = 'Name', all.x = T))

artist_locations <- artist_locations %>%
  arrange(-`Count`)

xtable(artist_locations, digits = 0)

fiddle_copy <- fiddle_copy %>%
  mutate(`Source of Tune` = gsub("\\ /.*","", fiddle_copy$`Source of Tune`))

#-----Maps ----

usa <- map_data("usa")

states <- map_data("state")

counties <- map_data('county')

states$region <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    
     states$region,
     perl = TRUE)

counties$region <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    
                      counties$region,
                      perl = TRUE)

counties$subregion <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    
                        counties$subregion,
                        perl = TRUE)

#Unique states then merge dataframes----

centroids <- tigris::counties(resolution = "20m") %>% 
  st_centroid()

state_boundaries <- tigris::states(class = "sf")

centroids <- st_join(centroids, state_boundaries)

state_names <- data.frame(state_boundaries$STATEFP, state_boundaries$NAME)
colnames(state_names) <- c("STATEFP", "STATE")

centroids[18] <- colnames("STATE")

centroids <- centroids %>% 
  filter(!grepl("(?i)city", NAMELSAD))

matched_data <- centroids %>%
  left_join(artist_locations, by = c("NAME.x" = "County", "NAME.y" = "State")) %>%
  distinct(Name, .keep_all = TRUE)

matched_data <- subset(matched_data, !is.na(matched_data$Name))

coords <- st_coordinates(matched_data)

new_df <- matched_data %>%
  mutate(Latitude = coords[, 2],
         Longitude = coords[, 1]) %>%
  st_set_geometry(NULL) %>% 
  select(NAME.x, NAME.y, Name, Count, Latitude, Longitude)

colnames(new_df) <- c("County", "State", "Name", "Count", "Latitude", "Longitude")

final_df <- merge(fiddle_copy, new_df, by.x = 'Source of Tune', by.y = 'Name', all.x = T)

#Plot prepwork----

list_states_df <- c("New Jersey", "Pennsylvania", "Ohio", 'Indiana', 'Illinois', 'Iowa', 'Missouri', 
                    'Kansas', 'Nebraska', 'Oklahoma', 'Texas', 'Louisiana', 'Mississippi', 'Alabama', 'Georgia', 'Florida', 
                    'South Carolina', 'North Carolina', 'Virginia', 'West Virginia', 'Tennessee', "Kentucky", "Arkansas", 
                    'Delaware', "Maryland")

filtered_states <- states %>%
  filter(region %in% list_states_df) 

filtered_counties <- counties %>%
  filter(region %in% list_states_df)

new_df$'Count Bin' <- cut(new_df$Count, breaks = c(0, 1, 5, 9, 15, Inf),
                        labels = c("1", "2-5", "6-10", "11-15", "16+"),
                        include.lowest = TRUE)

color_scale <- c("1" = "blue", "2-5" = "green", "6-10" = "orange", "11-15" = "purple", "16+" = "red")
tuning <- c("GDAE", "AEAE", "DDAD", "GDAD", "ADAE", "AEAC#", "AEAD", "EDAE", "AEF#C#")
tuning_colorscale <- c("GDAE" = "blue",
                       "ADAE" = "green",
                       "AEAE" = "red",
                       "DDAD" = "purple",
                       "AEAC#" = "orange",
                       "AEAD" = "black",
                       "GDAD" = "cyan",
                       "AEF#C#" = "yellow",
                       "EDAE" = "brown")

unique((sort(new_df$Count)))

#Histogram of Tune Counts and States-----

sum_counts_per_state <- new_df %>%
  group_by(State) %>%
  summarise(Total_Count = sum(Count))

ggplot(sum_counts_per_state, 
       aes(x = reorder(State, -Total_Count), 
           y = Total_Count)) +
  geom_bar(stat = "identity", 
           fill = "lightblue",
           position = 'Dodge') +
  labs(title = "Total Counts per State",
       x = "State", 
       y = "Count") +
  theme_minimal()+
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 55, 
                                   margin = margin(r=0), 
                                   hjust = 1))

#Histogram of Key Counts and States-----

i <- 0
for (i in 1:length(final_df$Key)) {
  row <- final_df$Key[i]
  
  if (grepl("|;", final_df$Key[i]) == TRUE) {
    final_df$Key[i] <- substr(row, 1, 1)
  }
  else if (grepl("/", final_df$`Key`[i])) {
    final_df$Other[i] <- "Cotillion"
    final_df$Key[i] <- substr(row, 1, 1)
  }
  else if (grepl("modal", final_df$`Key`[i])) {
    final_df$Other[i] <- "Modal"
    final_df$Key[i] <- substr(row, 1, 1)
  }
  else if (grepl("minor", final_df$`Key`[i])) {
    final_df$Other[i] <- "Minor"
    final_df$Key[i] <- substr(row, 1, 1)
  }
  else if (grepl(" ", final_df$`Key`[i])) {
    final_df$Key[i] <- substr(row, 1, 1)
  }
}

big_key_3 <- final_df %>%
  group_by(`Tuning Notes`) %>%
  count(Key)

test  <- final_df %>%
  group_by(State, Key) %>%
  summarise(Total_Key_Count = n()) 

test <- na.omit(test)

test$Key <- ifelse(test$Key == "B", "Bb", test$Key)

test %>%
  arrange(`State`) %>%
  mutate(`State` = factor(`State`, levels = c("Virginia",
                                              "North Carolina",
                                              "Kentucky",
                                              "West Virginia",
                                              "Tennessee",
                                              "Georgia",
                                              "Alabama",
                                              "Mississippi",
                                              "Arkansas",
                                              "Nebraska",
                                              "Texas",
                                              "Florida",
                                              "Indiana",
                                              "Louisiana")))%>%
  ggplot(aes(x = State,
           y = Total_Key_Count,
           fill = Key)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Key Counts by State",
       x = "State", 
       y = "Count") +
  theme_minimal()+
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 55, 
                                   margin = margin(r=0), 
                                   hjust = 1))

#Histogram of Tuning counts by State ----

test4 <- final_df %>%
  group_by(`State`) %>%
  count(`Tuning Notes`) %>%
  na.omit()

test4 %>%
  arrange(`State`) %>%
  mutate(`State` = factor(`State`, levels = c("Virginia",
                                              "North Carolina",
                                              "Kentucky",
                                              "West Virginia",
                                              "Tennessee",
                                              "Georgia",
                                              "Alabama",
                                              "Mississippi",
                                              "Arkansas",
                                              "Nebraska",
                                              "Texas",
                                              "Florida",
                                              "Indiana",
                                              "Louisiana")))%>%
  ggplot(aes(x = State,
             y = n,
             fill = `Tuning Notes`,
             alpha = 0.75)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Tuning Counts per State",
       x = "State", 
       y = "Count") +
  theme_minimal()+
  labs(color = "Tuning Notes") + 
  scale_fill_manual(values = c("GDAE" = "blue",
                                "ADAE" = "green",
                                "AEAE" = "red",
                                "DDAD" = "purple",
                                "AEAC#" = "orange",
                                "AEAD" = "black",
                                "GDAD" = "cyan",
                                "AEF#C#" = "yellow",
                                "EDAE" = "brown"))+
  scale_alpha(guide = 'none')+ 
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 55, 
                                   margin = margin(r=0), 
                                   hjust = 1)) 

#Chi-Squared Independence test----
test2 <- sum_counts_per_state %>%
  filter(Total_Count >= 5)

chisq.test(test2$Total_Count)

#USA Map Artist Tune Count ----

artist_n <- ggplot() +
  geom_polygon(data = counties, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = new_df,
             aes(text= paste(Name, 
                             '\n', County, ',', State, 
                             '\n', "Tune Count: ", `Count Bin`),
               x = Longitude, 
               y = Latitude, 
               color = `Count Bin`),
             size = 2,
             position = position_jitter(width = 0.15, height = 0.15)) +
  labs(color = "Count",
       title = "Tune Counts By County") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  #coord_cartesian(xlim = c(-102.3, -74.6), ylim = c(25.5, 42.5))+ #comment out for full view
  theme(text = element_text(size = 20),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = color_scale)+
  theme(legend.position = "right",   
        legend.justification = "left")

#USA Map Tuning Count ----

test3 <- final_df %>%
  group_by(`Tuning Notes`, `Latitude`, `Longitude`, `County`, `State`) %>%
  summarise(count = n()) %>%
  filter(`Tuning Notes` %in% tuning) %>%
  na.omit()

tuning_n <- ggplot() +
  geom_polygon(data = counties, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = test3,
             aes(text= paste(County, ",", State, 
                             '\n', "Tune Count: ", count, 
                             '\n', 'Tuning: ', `Tuning Notes`),
               x = Longitude, 
               y = Latitude, 
               color = `Tuning Notes`, 
               size = count
               ),
             alpha = 0.5,
             position = position_jitter(width = 0.1, height = 0.1)) +
  labs(color = "Tuning Notes",
       title = "Tuning Distribution By County") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  coord_cartesian(xlim = c(-102.3, -74.6), ylim = c(25.5, 42.5))+ #comment out for full view
  theme(text = element_text(size = 20),
        plot.margin = margin(5, 20, 50, 50)) +
  scale_color_manual(values = c("GDAE" = "blue",
                                "ADAE" = "green",
                                "AEAE" = "red",
                                "DDAD" = "purple",
                                "AEAC#" = "orange",
                                "AEAD" = "black",
                                "GDAD" = "cyan",
                                "AEF#C#" = "yellow",
                                "EDAE" = "brown"))+
  scale_size_continuous(range = c(2, 15), 
                        guide = 'none') + 
  theme(legend.position = "right",   
        legend.justification = "left")

#FUNCTION State plotting ----

state_plot <- function(temp, w, h){
  
  counties_v <- counties[counties$region == temp,  ]
  states_v <- states[states$region == temp, ]
  merged_data_v <- new_df[new_df$State  == temp, ]
  
  ggplot() +
    geom_polygon(data = counties_v, 
                 aes(x = long, 
                     y = lat, 
                     group = group),
                 fill = "white", 
                 color = "gray",
                 linejoin = "round",
                 lineend = "butt") +  
    geom_polygon(data = states_v, 
                 aes(x = long, 
                     y = lat, 
                     group = group),
                 fill = NA,
                 color = "black",
                 linejoin = "round",
                 lineend = "round") +
    geom_point(data = merged_data_v,
               aes(x = Longitude, 
                   y = Latitude, 
                   color = `Count Bin`),
               size = 2.5,
               position = position_jitter(width = w, height = h)) +
    labs(color = "Count") +
    theme_void() +
    coord_fixed(ratio = 1.3)+
    theme(text = element_text(size = 20),
          plot.margin = margin(5, 20, 5, 5)) +
    scale_color_manual(values = color_scale)+
    theme(legend.position = "right",   
          legend.justification = "left")
}

state_plot_tuning <- function(temp, temp1){
  
  counties_v <- counties[counties$region == temp,  ]
  states_v <- states[states$region == temp, ]
  
  test3 <- final_df %>%
    filter(`State` == temp) %>%
    group_by(`Tuning Notes`, `Latitude`, `Longitude`) %>%
    summarise(count = n()) %>%
    filter(`Tuning Notes` %in% temp1)
  
  ggplot() +
    geom_polygon(data = counties_v, 
                 aes(x = long, 
                     y = lat, 
                     group = group),
                 fill = "white", 
                 color = "gray",
                 linejoin = "round",
                 lineend = "butt") +  
    geom_polygon(data = states_v, 
                 aes(x = long, 
                     y = lat, 
                     group = group),
                 fill = NA,
                 color = "black",
                 linejoin = "round",
                 lineend = "round") +
    geom_point(data = test3,
               aes(x = Longitude,  
                   y = Latitude, 
                   color = `Tuning Notes`,
                   size = count),
               alpha = 0.5,
               position = position_jitter(width = 0.1, height = 0.1)) +
    labs(color = "Tuning Notes") +
    theme_void() +
    coord_fixed(ratio = 1.3)+
    theme(text = element_text(size = 20),
          plot.margin = margin(5, 20, 50, 50)) +
    scale_color_manual(values = c("GDAE" = "blue", "ADAE" = "green", "AEAE" = "red", 
                                  "DDAD" = "purple", "AEAC#" = "orange", "AEAD" =  'grey',
                                  "GDAD" = "cyan", "AEF#C#" = 'yellow', "EDAE" = 'darkgreen'))+
    scale_size_continuous(range = c(5, 15)) + 
    theme(legend.position = "right",   
          legend.justification = "left")
}
  
#PLOTTING States ----  
state_plot("Virginia", 0.15, 0.06)
state_plot("West Virginia", 0.1, 0.1)
state_plot("North Carolina", 0.1, 0.1)
state_plot("Tennessee", 0.05, 0.05)
state_plot("Kentucky", 0.05, 0.05)
state_plot("Georgia", 0.01, 0.05)
state_plot("Alabama", 0.05, 0.05)
state_plot("Mississippi", 0.05, 0.05)
state_plot("Texas", 0.05, 0.05)
state_plot("Florida", 0.05, 0.05)
state_plot("Louisiana", 0.05, 0.05)
state_plot("Arkansas", 0.05, 0.05)
state_plot("Nebraska", 0.05, 0.05)
state_plot("Indiana", 0.05, 0.05)

state_plot_tuning("Virginia", tuning) 
state_plot_tuning("West Virginia", tuning) 
state_plot_tuning("North Carolina", tuning) 
state_plot_tuning("Tennessee", tuning) 
state_plot_tuning("Kentucky", tuning) 
state_plot_tuning("Georigia", tuning) 
state_plot_tuning("Alabama", tuning) 
state_plot_tuning("Mississippi", tuning) 
state_plot_tuning("Texas", tuning) 
state_plot_tuning("Florida", tuning) 
state_plot_tuning("Louisiana", tuning) 
state_plot_tuning("Arkansas", tuning) 
state_plot_tuning("Nebraska", tuning) 
state_plot_tuning("Indiana", tuning) 

#PLOTLY of some map -----

artist_n_plotly <- ggplotly(artist_n, tooltip = "text") 
tuning_n_plotly <- ggplotly(tuning_n, tooltip = "text")  

#write.csv(final_df, file = "final_df.csv")
