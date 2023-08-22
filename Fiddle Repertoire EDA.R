setwd("C:/Users/18045/Documents/R/fiddle")# Sean's WD
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

#LoadData----

fiddle_ink <- read_excel("Collection of Fiddle Tunes.xlsx")
artist_df <- read_csv("Artist Location.csv")

#Tally by Key ----

big_tab <- 0
big_tab <- fiddle_ink %>%
  group_by(`Tuning Notes`) %>%
  count(substr(fiddle_ink$Key, 1, 1), `Tuning Notes`) %>%
  rename("Key" = `substr(fiddle_ink$Key, 1, 1)`, 
         "Count" = `n`) 

big_tab <- big_tab %>% 
  arrange(pick(`Count`), .by_group = TRUE) %>%
  reorder(big_tab$`Tuning Notes`, big_tab$Count)
  
big_tab$Key <- ifelse(big_tab$Key == "B", "Bb", big_tab$Key)

fiddle_copy <- fiddle_ink
i <- 0
fiddle_copy$Other <- NA
for (i in 1:length(fiddle_copy$Key)) {
  row <- fiddle_copy$Key[i]
  
  if (grepl("/|;", fiddle_copy$Key[i]) == TRUE) {
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

big_key <- fiddle_copy %>%
  group_by(`Tuning Notes`) %>%
  count(Key, Other)

colnames(big_key) <- c("Tuning Notes","Key",  "Other", "Count")

#Plots ----

#Plots of tuning and key
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
                                                        "EADE")))%>%
ggplot(aes(x = `Tuning Notes`,
           y = `Count`,
           fill = `Key`,
           color = `Key`))+
  geom_histogram(stat = "identity")+
  labs(title = "Fiddle Tunings by Keys",
       y = "Count",
       x = "Tuning")+
  theme_minimal()+
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,  margin = margin(r=0)))

#plot of modal and minor tunes 
big_key %>%
  arrange(`Other`) %>%
  filter(!is.na(`Other`)) %>%
  mutate(`Tuning Notes` = factor(`Tuning Notes`, levels = c("GDAE",
                                                            "AEAE",
                                                            "ADAE",
                                                            "EADE")))%>%
  ggplot(aes(x = `Tuning Notes`,
             y = `Count`,
             fill = `Other`,
             color = `Other`))+
  geom_histogram(stat = "identity")+
  labs(title = "Fiddle Tunings by Modality",
       y = "Count",
       x = "Tuning")+
  theme_minimal()+
  theme(text = element_text(size = 30),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, margin = margin(r=0)))

#plot of crooked tunes 

fiddle_copy$`Cooked [-/+]`

fiddle_ink %>%
  arrange(`Tuning Notes`) %>%
  filter(!is.na(`Cooked [-/+]`)) %>%
  count(`Tuning Notes`, `Cooked [-/+]`) %>%
  mutate(`Tuning Notes` = factor(`Tuning Notes`, levels = c("GDAE",
                                                            "AEAE",
                                                            "ADAE")))%>%
  ggplot(aes(x = `Tuning Notes`,
             y = `n`,
             fill = `Cooked [-/+]`,
             color = `Cooked [-/+]`))+
  geom_histogram(stat = "identity")+
  labs(title = "Crooked Fiddle Tunes by Tuning",
       y = "Count",
       x = "Tuning")+
  ylim(0, 15)+
  theme_minimal()+
  theme(text = element_text(size = 30),
        plot.title = element_text(hjust = .5, margin = margin(b = 100)),
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

source_df <-fiddle_copy %>%
  group_by(gsub("\\ /.*","", fiddle_copy$`Source of Tune`)) %>%
  count(`Key`)

colnames(source_df) <- c("Name", "Key", "Count")

source_2_df <- source_2_df %>%
  arrange(-`Count`)

#write.csv(source_2_df, file = "C:/Users/18045/Documents/R/fiddle/artistlocation.csv")

#xtable(source_2_df)
#xtable(artist_df[2:5], digits = 0)

#Maps ----

usa <- map_data("usa")

states <- map_data("state")

counties <- map_data('county')

states$region <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
     states$region,
     perl = TRUE)

counties$region <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
                      counties$region,
                      perl = TRUE)

counties$subregion <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
                        counties$subregion,
                        perl = TRUE)

#Convert state acronym to full name----
i <- 0
df <- data.frame()
matching_state <- data.frame()
for (i in seq_along(artist_df$State)) {
  
  cell <- artist_df$State[i]
  matching_state <- state.name[grep(cell, state.abb)]  # Find matching state abbreviation(s)
  
  if (length(matching_state) > 0) {
    df <- c(df, matching_state)
  } 
}

df <- as.data.frame(df)
df <- t(df)
df <- data.frame(df)

colnames(df) <- c("Region")

#remove non-USA artists from the artist dataframe
artist_df <- subset(artist_df, `State` != "NA")

df <- subset(df, `Region` != "NA")

#identify unique states, and merge dataframes----

centroids <- tigris::counties(resolution = "20m") %>% 
  st_centroid()

state_boundaries <- tigris::states(class = "sf")

centroids <- st_join(centroids, state_boundaries)

state_names <- data.frame(state_boundaries$STATEFP, state_boundaries$NAME)
colnames(state_names) <- c("STATEFP", "STATE")

centroids[18] <- colnames("STATE")

matched_data <- centroids %>%
  left_join(artist_df, by = c("NAME.x" = "County", "NAME.y" = "State"))

matched_data <- matched_data %>%
  distinct(Name, .keep_all = TRUE)

matched_data <- subset(matched_data, is.na(matched_data$Name)!=T)

#Erroneous row ----
replacememt_row <- subset(centroids, COUNTYFP == '067' & STATEFP.x == "51") 

matched_data[matched_data$Name == "N.H. Mills", colnames(centroids)] <- replacememt_row[colnames(centroids)]

#Extract Lat and Long
matched_data <- matched_data %>%
  mutate(coordinates = st_coordinates(geometry)) %>%
  mutate(Latitude = coordinates[, "Y"],
         Longitude = coordinates[, "X"])

new_df <- matched_data %>%
  st_set_geometry(NULL) %>% 
  select(NAME.x, NAME.y, Rank, Name, Count, Latitude, Longitude) 

colnames(new_df) <- c("County", "State", "Rank", "Name", "Count", "Lat", "Long")

#Final Plots prepwork----

list_states_df <- c("New Jersey", "Pennsylvania", "Ohio", 'Indiana', 'Illinois', 'Iowa', 'Missouri', 
                    'Kansas', 'Nebraska', 'Oklahoma', 'Texas', 'Louisiana', 'Mississippi', 'Alabama', 'Georgia', 'Florida', 
                    'South Carolina', 'North Carolina', 'Virginia', 'West Virginia', 'Tennessee', "Kentucky", "Arkansas", 
                    'Delaware', "Maryland")

filtered_states <- states %>%
  filter(region %in% list_states_df) 

filtered_counties <- counties %>%
  filter(region %in% list_states_df)

count_colors <- c("blue", "green", "red", "purple", "orange")
new_df$Count_Bin <- cut(new_df$Count, breaks = c(0, 1, 5, 10, 14, Inf), 
                        labels = c("1", "2-5", "6-10", "11-14", "15+"), 
                        include.lowest = TRUE)

#Histogram of counts and states-----

sum_counts_per_state <- new_df %>%
  group_by(State) %>%
  summarise(Total_Count = sum(Count))

# Create a bar plot
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

#Chi-Squared Independence test----
test <- sum_counts_per_state %>%
  filter(Total_Count >= 5)

chisq.test(test)

#zoomed in ----

ggplot() +
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
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 3,
             position = position_jitter(width = 0.11, height = 0.11)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  coord_cartesian(xlim = c(-102.3, -74.6), ylim = c(25.5, 42.5))+
  theme(text = element_text(size = 25),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(legend.position = "right",   
        legend.justification = "left")

#USA plot ----

ggplot() +
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
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2,
             position = position_jitter(width = 0.2, height = 0.2)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(text = element_text(size = 20),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(legend.position = "right",   
        legend.justification = "left")

#Virginia plot ----

counties_va <- counties[counties$region == "Virginia",  ]
states_va <- states[states$region == "Virginia", ]
merged_data_va <- new_df[new_df$State  == "Virginia", ]

ggplot() +
  geom_polygon(data = counties_va, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states_va, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = merged_data_va,
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2.5,
position = position_jitter(width = 0.08, height = 0.08)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(text = element_text(size = 20),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(legend.position = "right",   
        legend.justification = "left")


#West Virginia plot ----

counties_wva <- counties[counties$region == "West Virginia", ]
states_wva <- states[states$region == "West Virginia", ]
merged_data_wva <- new_df[new_df$State  == "West Virginia",]

ggplot() +
  geom_polygon(data = counties_wva, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states_wva, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = merged_data_wva,
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2.5,
             position = position_jitter(width = 0.08, height = 0.08)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(text = element_text(size = 30),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(legend.position = "right",   
        legend.justification = "left")


#North Carolina plot ----

counties_nc <- counties[counties$region == "North Carolina", ]
states_nc <- states[states$region == "North Carolina", ]
merged_data_nc <- new_df[new_df$State  == "North Carolina",]

ggplot() +
  geom_polygon(data = counties_nc, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states_nc, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = merged_data_nc,
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2.5,
             position = position_jitter(width = 0.11, height = 0.09)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(text = element_text(size = 20),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(legend.position = "right",   
        legend.justification = "left")

#Tennessee plot ----

counties_tn <- counties[counties$region == "Tennessee", ]
states_tn <- states[states$region == "Tennessee", ]
merged_data_tn <- new_df[new_df$State  == "Tennessee",]

ggplot() +
  geom_polygon(data = counties_tn, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states_tn, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = merged_data_tn,
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2.5,
             position = position_jitter(width = 0.11, height = 0.095)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(text = element_text(size = 20),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(legend.position = "right",   
        legend.justification = "left")

#Kentucky plot ----

counties_ky <- counties[counties$region == "Kentucky", ]
states_ky <- states[states$region == "Kentucky", ]
merged_data_ky <- new_df[new_df$State  == "Kentucky",]

ggplot() +
  geom_polygon(data = counties_ky, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states_ky, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = merged_data_ky,
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2.5,
             position = position_jitter(width = 0.1, height = 0.1)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(text = element_text(size = 20),
        legend.position = "right",   
        legend.justification = "left")

#Georgia plot ----

counties_ga <- counties[counties$region == "Georgia", ]
states_ga <- states[states$region == "Georgia", ]
merged_data_ga <- new_df[new_df$State  == "Georgia",]

ggplot() +
  geom_polygon(data = counties_ga, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states_ga, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = merged_data_ga,
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2.5,
             position = position_jitter(width = 0.1, height = 0.09)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(text = element_text(size = 20),
        legend.position = "right",   
        legend.justification = "left")

#Alabama plot ----

counties_al <- counties[counties$region == "Alabama", ]
states_al <- states[states$region == "Alabama", ]
merged_data_al <- new_df[new_df$State  == "Alabama",]

ggplot() +
  geom_polygon(data = counties_al, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states_al, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = merged_data_al,
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2.5,
             position = position_jitter(width = 0.1, height = 0.09)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(text = element_text(size = 20),
        legend.position = "right",   
        legend.justification = "left")

#Mississippi plot ----

counties_ms <- counties[counties$region == "Mississippi", ]
states_ms <- states[states$region == "Mississippi", ]
merged_data_ms <- new_df[new_df$State  == "Mississippi",]

ggplot() +
  geom_polygon(data = counties_ms, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states_ms, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = merged_data_ms,
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2.5,
             position = position_jitter(width = 0.1, height = 0.09)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(text = element_text(size = 20),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(legend.position = "right",   
        legend.justification = "left")

#Texas plot ----

counties_tx <- counties[counties$region == "Texas", ]
states_tx <- states[states$region == "Texas", ]
merged_data_tx <- new_df[new_df$State  == "Texas",]

ggplot() +
  geom_polygon(data = counties_tx, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states_tx, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = merged_data_tx,
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2.5,
             position = position_jitter(width = 0.1, height = 0.09)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(text = element_text(size = 20),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(legend.position = "right",   
        legend.justification = "left")


#Florida plot ----

counties_fl <- counties[counties$region == "Florida", ]
states_fl <- states[states$region == "Florida", ]
merged_data_fl <- new_df[new_df$State  == "Florida",]

ggplot() +
  geom_polygon(data = counties_fl, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states_fl, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = merged_data_fl,
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2.5,
             position = position_jitter(width = 0.1, height = 0.09)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(text = element_text(size = 25),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(legend.position = "right",   
        legend.justification = "left")

#Louisiana plot ----

counties_la <- counties[counties$region == "Louisiana", ]
states_la <- states[states$region == "Louisiana", ]
merged_data_la <- new_df[new_df$State  == "Louisiana",]

ggplot() +
  geom_polygon(data = counties_la, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states_la, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = merged_data_la,
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2.5,
             position = position_jitter(width = 0.1, height = 0.09)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(text = element_text(size = 20),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(legend.position = "right",   
        legend.justification = "left")

#Arkansas plot ----

counties_ar <- counties[counties$region == "Arkansas", ]
states_ar <- states[states$region == "Arkansas", ]
merged_data_ar <- new_df[new_df$State  == "Arkansas",]

ggplot() +
  geom_polygon(data = counties_ar, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states_ar, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = merged_data_ar,
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2.5,
             position = position_jitter(width = 0.1, height = 0.09)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(text = element_text(size = 25),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(legend.position = "right",   
        legend.justification = "left")

#Nebraska plot ----

counties_nb <- counties[counties$region == "Nebraska", ]
states_nb <- states[states$region == "Nebraska", ]
merged_data_nb <- new_df[new_df$State  == "Nebraska",]

ggplot() +
  geom_polygon(data = counties_nb, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states_nb, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = merged_data_nb,
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2.5,
             position = position_jitter(width = 0.1, height = 0.09)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(text = element_text(size = 20),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(legend.position = "right",   
        legend.justification = "left")

#Indiana plot ----

counties_in <- counties[counties$region == "Indiana", ]
states_in <- states[states$region == "Indiana", ]
merged_data_in <- new_df[new_df$State  == "Indiana",]

ggplot() +
  geom_polygon(data = counties_in, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = "white", 
               color = "gray",
               linejoin = "round",
               lineend = "butt") +  
  geom_polygon(data = states_in, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black",
               linejoin = "round",
               lineend = "round") +
  geom_point(data = merged_data_in,
             aes(x = Long, 
                 y = Lat, 
                 color = Count_Bin),
             size = 2.5,
             position = position_jitter(width = 0.1, height = 0.09)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(text = element_text(size = 20),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1" = "blue", "2-5" = "green", "6-10" = "orange", 
                                "11-14" = "purple", "15+" = "red"))+
  theme(legend.position = "right",   
        legend.justification = "left")
