setwd("C:/Users/18045/Documents/Python/fiddletune")  #R/fiddle
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
fiddle_ink <- fiddle_ink[,-7]

#artist_df <- read_csv("Artist Location.csv")

large_df <- read_csv("test.csv")

artist_df <- read.csv("artistlocation.csv")
artist_df <- artist_df[,-1]

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
                                                        "EDAE")))%>%
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
                                                            "EDAE")))%>%
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

fiddle_copy$`Crooked [-/+]`

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

#write.csv(source_2_df, file = "C:/Users/18045/Documents/Python/fiddletune/artistlocation0.csv")

xtable(source_2_df)
xtable(artist_df[2:5], digits = 0)

#Maps ----

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

artist_df <- subset(artist_df, `State` != "NA")

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

new_df <- matched_data %>%
  st_set_geometry(NULL) %>% 
  select(NAME.x, NAME.y, Rank, Name, Count, LAT_jittered, LONG_jittered)

colnames(new_df) <- c("County", "State", "Rank", "Name", "Count", "Latitude", "Longitude")

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
new_df$Count_Bin <- cut(new_df$Count, breaks = c(0, 3, 8, 12, 18, Inf), #c(0, 1, 5, 10, 14, Inf)
                        labels = c("1-3", "4-8", "9-12", "13-18", "19+"), #c("1", "2-5", "6-10", "11-14", "15+")
                        include.lowest = TRUE)

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
large_df$Other <- NA
for (i in 1:length(large_df$Key)) {
  row <- large_df$Key[i]
  
  if (grepl("/|;", large_df$Key[i]) == TRUE) {
    large_df$Key[i] <- substr(row, 1, 1)
  }
  else if (grepl("modal", large_df$`Key`[i])) {
    large_df$Other[i] <- "Modal"
    large_df$Key[i] <- substr(row, 1, 1)
  }
  else if (grepl("minor", large_df$`Key`[i])) {
    large_df$Other[i] <- "Minor"
    large_df$Key[i] <- substr(row, 1, 1)
  }
  else if (grepl(" ", large_df$`Key`[i])) {
    large_df$Key[i] <- substr(row, 1, 1)
  }
}

big_key_3 <- large_df %>%
  group_by(`Tuning Notes`) %>%
  count(Key, Other)

test  <- large_df %>%
  group_by(State, Key) %>%
  summarise(Total_Key_Count = n())

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
  labs(title = "Total Key Counts per State",
       x = "State", 
       y = "Count") +
  theme_minimal()+
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 55, 
                                   margin = margin(r=0), 
                                   hjust = 1))

#Chi-Squared Independence test----
test2 <- sum_counts_per_state %>%
  filter(Total_Count >= 5)

chisq.test(test2$Total_Count)
plot(table(large_df$Count))

tunejenktbl <- table(large_df$Count)
print(tunejenktbl )
getJenksBreaks((large_df$Count), 5, subset = NULL)
getJenksBreaks(large_df$Count, 5)
?getJenksBreaks
install.packages('BAMMtools')
library(BAMMtools)

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
             aes(x = Longitude, 
                 y = Latitude, 
                 color = Count_Bin),
             size = 3,
             position = position_jitter(width = 0.11, height = 0.11)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  coord_cartesian(xlim = c(-102.3, -74.6), ylim = c(25.5, 42.5))+
  theme(text = element_text(size = 25),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1-3" = "blue", "4-8" = "green", "9-12" = "orange", #c("1" = "blue", "2-5" = "green", "6-10" = "orange", "11-14" = "purple", "15+" = "red")
                                "13-18" = "purple", "19+" = "red"))+
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
             aes(x = Longitude, 
                 y = Latitude, 
                 color = Count_Bin),
             size = 2,
             position = position_jitter(width = 0.2, height = 0.2)) +
  labs(color = "Count") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(text = element_text(size = 20),
        plot.margin = margin(5, 20, 5, 5)) +
  scale_color_manual(values = c("1-3" = "blue", "4-8" = "green", "9-12" = "orange", #c("1" = "blue", "2-5" = "green", "6-10" = "orange", "11-14" = "purple", "15+" = "red")
                                "13-18" = "purple", "19+" = "red"))+
  theme(legend.position = "right",   
        legend.justification = "left")

#USA Other way plot ----

large_df %>%
  group_by(`Source of Tune` ) %>%
  summarise(count = n(`Tuning Notes`))

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
  geom_point(data = large_df,
             aes(x = LONG_jittered,  
                 y = LAT_jittered, 
                 color = `Tuning Notes`),
             size = 2,
             position = position_jitter(width = 0.2, height = 0.2)) +
  labs(color = "Tuning Notes") +
  theme_void() +
  coord_fixed(ratio = 1.3)+
  theme(text = element_text(size = 20),
        plot.margin = margin(5, 20, 50, 50)) +
  scale_color_manual(values = c("GDAE" = "blue", "ADAE" = "green", "AEAE" = "red", 
                                "DDAD" = "purple", "AEAC#" = "orange", "AEAD" =  'grey',
                                "GDAD" = "cyan", "AEF#C#" = 'yellow', "EDAE" = 'darkgreen'))+
  theme(legend.position = "right",   
        legend.justification = "left")

#FUNCTION State plotting ----

state_plot <- function(temp){
  
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
                   color = Count_Bin),
               size = 2.5,
               position = position_jitter(width = 0.08, height = 0.08)) +
    labs(color = "Count") +
    theme_void() +
    coord_fixed(ratio = 1.3)+
    theme(text = element_text(size = 20),
          plot.margin = margin(5, 20, 5, 5)) +
    scale_color_manual(values = c("1-3" = "blue", "4-8" = "green", "9-12" = "orange", #c("1" = "blue", "2-5" = "green", "6-10" = "orange", "11-14" = "purple", "15+" = "red")
                                  "13-18" = "purple", "19+" = "red"))+
    theme(legend.position = "right",   
          legend.justification = "left")
}

#PLOTTING States ----  
state_plot("Virginia")
state_plot("West Virginia")
state_plot("North Carolina")
state_plot("Tennessee")
state_plot("Kentucky")
state_plot("Georgia")
state_plot("Alabama")
state_plot("Mississippi")
state_plot("Texas")
state_plot("Florida")
state_plot("Louisiana")
state_plot("Arkansas")
state_plot("Nebraska")
state_plot("Indiana")


#Erroneous row ---- skiped
replacememt_row <- subset(centroids, COUNTYFP == '067' & STATEFP.x == "51") 

matched_data[matched_data$Name == "N.H. Mills", colnames(centroids)] <- replacememt_row[colnames(centroids)]

#Extract Lat and Long
matched_data <- matched_data %>%
  mutate(coordinates = st_coordinates(geometry)) %>%
  mutate(Latitude = coordinates[, "Y"],
         Longitude = coordinates[, "X"])