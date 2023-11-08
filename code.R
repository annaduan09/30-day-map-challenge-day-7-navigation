library(sf)
library(tidyverse)
library(mapview)
library(tigris)
library(lubridate)
library(forcats)
library(gganimate)

jan <- read.csv("/Users/annaduan/Desktop/GitHub/30-day-map-challenge-2023/7\ navigation/data/1\ jan.csv")  %>%
  mutate(count = 1,
         flight = paste(ORIGIN_CITY_NAME, DEST_CITY_NAME, sep = " ")) %>%
  group_by(FL_DATE, flight) %>%
  summarize(trips = sum(count),
            ORIGIN_CITY_NAME = first(ORIGIN_CITY_NAME),
            DEST_CITY_NAME = first(DEST_CITY_NAME)) %>%
  ungroup() %>%
  mutate(date = mdy(word(FL_DATE, 1)),
         day = yday(date),
         month = month(date)) %>%
  dplyr::select(day, month, date, ORIGIN_CITY_NAME, DEST_CITY_NAME, trips)

feb <- read.csv("/Users/annaduan/Desktop/GitHub/30-day-map-challenge-2023/7\ navigation/data/2\ feb.csv") %>%
  mutate(count = 1,
         flight = paste(ORIGIN_CITY_NAME, DEST_CITY_NAME, sep = " ")) %>%
  group_by(FL_DATE, flight) %>%
  summarize(trips = sum(count),
            ORIGIN_CITY_NAME = first(ORIGIN_CITY_NAME),
            DEST_CITY_NAME = first(DEST_CITY_NAME)) %>%
  ungroup() %>%
  mutate(date = mdy(word(FL_DATE, 1)),
         day = yday(date),
         month = month(date)) %>%
  dplyr::select(day, month, date, ORIGIN_CITY_NAME, DEST_CITY_NAME, trips)

mar <- read.csv("/Users/annaduan/Desktop/GitHub/30-day-map-challenge-2023/7\ navigation/data/3\ mar.csv") %>%
  mutate(count = 1,
         flight = paste(ORIGIN_CITY_NAME, DEST_CITY_NAME, sep = " ")) %>%
  group_by(FL_DATE, flight) %>%
  summarize(trips = sum(count),
            ORIGIN_CITY_NAME = first(ORIGIN_CITY_NAME),
            DEST_CITY_NAME = first(DEST_CITY_NAME)) %>%
  ungroup() %>%
  mutate(date = mdy(word(FL_DATE, 1)),
         day = yday(date),
         month = month(date)) %>%
  dplyr::select(day, month, date, ORIGIN_CITY_NAME, DEST_CITY_NAME, trips)

apr <- read.csv("/Users/annaduan/Desktop/GitHub/30-day-map-challenge-2023/7\ navigation/data/4\ apr.csv") %>%
  mutate(count = 1,
         flight = paste(ORIGIN_CITY_NAME, DEST_CITY_NAME, sep = " ")) %>%
  group_by(FL_DATE, flight) %>%
  summarize(trips = sum(count),
            ORIGIN_CITY_NAME = first(ORIGIN_CITY_NAME),
            DEST_CITY_NAME = first(DEST_CITY_NAME)) %>%
  ungroup() %>%
  mutate(date = mdy(word(FL_DATE, 1)),
         day = yday(date),
         month = month(date)) %>%
  dplyr::select(day, month, date, ORIGIN_CITY_NAME, DEST_CITY_NAME, trips)

may <- read.csv("/Users/annaduan/Desktop/GitHub/30-day-map-challenge-2023/7\ navigation/data/5 may.csv") %>%
  mutate(count = 1,
         flight = paste(ORIGIN_CITY_NAME, DEST_CITY_NAME, sep = " ")) %>%
  group_by(FL_DATE, flight) %>%
  summarize(trips = sum(count),
            ORIGIN_CITY_NAME = first(ORIGIN_CITY_NAME),
            DEST_CITY_NAME = first(DEST_CITY_NAME)) %>%
  ungroup() %>%
  mutate(date = mdy(word(FL_DATE, 1)),
         day = yday(date),
         month = month(date)) %>%
  dplyr::select(day, month, date, ORIGIN_CITY_NAME, DEST_CITY_NAME, trips)

jun <- read.csv("/Users/annaduan/Desktop/GitHub/30-day-map-challenge-2023/7\ navigation/data/6 jun.csv") %>%
  mutate(count = 1,
         flight = paste(ORIGIN_CITY_NAME, DEST_CITY_NAME, sep = " ")) %>%
  group_by(FL_DATE, flight) %>%
  summarize(trips = sum(count),
            ORIGIN_CITY_NAME = first(ORIGIN_CITY_NAME),
            DEST_CITY_NAME = first(DEST_CITY_NAME)) %>%
  ungroup() %>%
  mutate(date = mdy(word(FL_DATE, 1)),
         day = yday(date),
         month = month(date)) %>%
  dplyr::select(day, month, date, ORIGIN_CITY_NAME, DEST_CITY_NAME, trips)

jul <- read.csv("/Users/annaduan/Desktop/GitHub/30-day-map-challenge-2023/7\ navigation/data/7 jul.csv") %>%
  mutate(count = 1,
         flight = paste(ORIGIN_CITY_NAME, DEST_CITY_NAME, sep = " ")) %>%
  group_by(FL_DATE, flight) %>%
  summarize(trips = sum(count),
            ORIGIN_CITY_NAME = first(ORIGIN_CITY_NAME),
            DEST_CITY_NAME = first(DEST_CITY_NAME)) %>%
  ungroup() %>%
  mutate(date = mdy(word(FL_DATE, 1)),
         day = yday(date),
         month = month(date)) %>%
  dplyr::select(day, month, date, ORIGIN_CITY_NAME, DEST_CITY_NAME, trips)

aug <- read.csv("/Users/annaduan/Desktop/GitHub/30-day-map-challenge-2023/7\ navigation/data/8 aug.csv") %>%
  mutate(count = 1,
         flight = paste(ORIGIN_CITY_NAME, DEST_CITY_NAME, sep = " ")) %>%
  group_by(FL_DATE, flight) %>%
  summarize(trips = sum(count),
            ORIGIN_CITY_NAME = first(ORIGIN_CITY_NAME),
            DEST_CITY_NAME = first(DEST_CITY_NAME)) %>%
  ungroup() %>%
  mutate(date = mdy(word(FL_DATE, 1)),
         day = yday(date),
         month = month(date)) %>%
  dplyr::select(day, month, date, ORIGIN_CITY_NAME, DEST_CITY_NAME, trips)

sep <- read.csv("/Users/annaduan/Desktop/GitHub/30-day-map-challenge-2023/7\ navigation/data/9 sep.csv") %>%
  mutate(count = 1,
         flight = paste(ORIGIN_CITY_NAME, DEST_CITY_NAME, sep = " ")) %>%
  group_by(FL_DATE, flight) %>%
  summarize(trips = sum(count),
            ORIGIN_CITY_NAME = first(ORIGIN_CITY_NAME),
            DEST_CITY_NAME = first(DEST_CITY_NAME)) %>%
  ungroup() %>%
  mutate(date = mdy(word(FL_DATE, 1)),
         day = yday(date),
         month = month(date)) %>%
  dplyr::select(day, month, date, ORIGIN_CITY_NAME, DEST_CITY_NAME, trips)

oct <- read.csv("/Users/annaduan/Desktop/GitHub/30-day-map-challenge-2023/7\ navigation/data/10 oct.csv") %>%
  mutate(count = 1,
         flight = paste(ORIGIN_CITY_NAME, DEST_CITY_NAME, sep = " ")) %>%
  group_by(FL_DATE, flight) %>%
  summarize(trips = sum(count),
            ORIGIN_CITY_NAME = first(ORIGIN_CITY_NAME),
            DEST_CITY_NAME = first(DEST_CITY_NAME)) %>%
  ungroup() %>%
  mutate(date = mdy(word(FL_DATE, 1)),
         day = yday(date),
         month = month(date)) %>%
  dplyr::select(day, month, date, ORIGIN_CITY_NAME, DEST_CITY_NAME, trips)

nov <- read.csv("/Users/annaduan/Desktop/GitHub/30-day-map-challenge-2023/7\ navigation/data/11 nov.csv") %>%
  mutate(count = 1,
         flight = paste(ORIGIN_CITY_NAME, DEST_CITY_NAME, sep = " ")) %>%
  group_by(FL_DATE, flight) %>%
  summarize(trips = sum(count),
            ORIGIN_CITY_NAME = first(ORIGIN_CITY_NAME),
            DEST_CITY_NAME = first(DEST_CITY_NAME)) %>%
  ungroup() %>%
  mutate(date = mdy(word(FL_DATE, 1)),
         day = yday(date),
         month = month(date)) %>%
  dplyr::select(day, month, date, ORIGIN_CITY_NAME, DEST_CITY_NAME, trips)

dec <- read.csv("/Users/annaduan/Desktop/GitHub/30-day-map-challenge-2023/7\ navigation/data/12 dec.csv") %>%
  mutate(count = 1,
         flight = paste(ORIGIN_CITY_NAME, DEST_CITY_NAME, sep = " ")) %>%
  group_by(FL_DATE, flight) %>%
  summarize(trips = sum(count),
            ORIGIN_CITY_NAME = first(ORIGIN_CITY_NAME),
            DEST_CITY_NAME = first(DEST_CITY_NAME)) %>%
  ungroup() %>%
  mutate(date = mdy(word(FL_DATE, 1)),
         day = yday(date),
         month = month(date)) %>%
  dplyr::select(day, month, date, ORIGIN_CITY_NAME, DEST_CITY_NAME, trips)

flights <- rbind(jan, feb, mar, apr,
                 may, jun, jul, aug,
                 sep, oct, nov, dec)

rm(jan, feb, mar, apr,
   may, jun, jul, aug,
   sep, oct, nov, dec)

flights.trip <- flights %>%
 mutate(city_1 = ORIGIN_CITY_NAME,
        city_2 = DEST_CITY_NAME,         
        trip = paste(city_1, city_2, sep = "; ")) %>%
  dplyr::select(date, day, month, city_1, city_2, trip, trips)

#### GEO WORK ####

states <- states(resolution = "20m") %>%
  shift_geometry() %>%
  st_transform("ESRI:102003") %>%
  filter(NAME %in% c('Commonwealth of the Northern Mariana Islands', "Guam", "American Samoa", 'United States Virgin Islands') == FALSE)


places <- places(state = NULL, cb = TRUE) %>%
  shift_geometry() %>%
  mutate(NAME = ifelse(GEOID == "0281920", "Barrow", NAME),
         city = paste(NAME, STUSPS, sep = ", ")) %>%
  st_centroid() %>%
  dplyr::select(GEOID, NAME, STUSPS, city) %>%
  st_transform(src = states) %>%
  st_transform("ESRI:102003") 

coords_1 <- places %>%
  rename(city_1 = city) %>%
  mutate(geometry_1 = geometry) %>%
  extract(geometry, c('lon_1', 'lat_1'), '\\((.*), (.*)\\)', convert = TRUE) %>%
  as.data.frame() %>%
  dplyr::select("city_1", "lon_1", "lat_1", "geometry_1")

coords_2 <- places %>%
  rename(city_2 = city) %>%
  mutate(geometry_2 = geometry) %>%
  extract(geometry, c('lon_2', 'lat_2'), '\\((.*), (.*)\\)', convert = TRUE) %>%
  as.data.frame() %>%
  dplyr::select("city_2", "lon_2", "lat_2", "geometry_2")


#### TRIPS DF #### 
flights.sf <- flights.trip %>%
  mutate(city_1 = str_replace(city_1, "Dallas/Fort Worth, TX", "Dallas, TX"),
         city_2 = str_replace(city_2, "Dallas/Fort Worth, TX", "Dallas, TX"),
         
         city_1 = str_replace(city_1, "Nashville, TN", "Nashville-Davidson metropolitan government (balance), TN"),
         city_2 = str_replace(city_2, "Nashville, TN", "Nashville-Davidson metropolitan government (balance), TN"),
         
         city_1 = str_replace(city_1, "Indianapolis, IN", "Indianapolis city (balance), IN"),
         city_2 = str_replace(city_2, "Indianapolis, IN", "Indianapolis city (balance), IN"),
         
         city_1 = str_replace(city_1, "Raleigh/Durham, NC", "Raleigh, NC"),
         city_2 = str_replace(city_2, "Raleigh/Durham, NC", "Raleigh, NC"),
         
         city_1 = str_replace(city_1, "Honolulu, HI", "Urban Honolulu, HI"),
         city_2 = str_replace(city_2, "Honolulu, HI", "Urban Honolulu, HI")) %>%
  left_join(coords_1, by = "city_1") %>%
  left_join(coords_2, by = "city_2") %>%
  filter(grepl("TT", trip) == FALSE & grepl("VI", trip) == FALSE) %>%
  st_as_sf(sf_column_name = "geometry_1") 

#### MAP ANIMATION ####
flight_sample <- flights.sf %>%
  sample_n(size = 600000) %>%
  filter(!is.na(lat_1) & !is.na(lat_2) & !is.na(lon_1) & !is.na(lon_2)) %>%
  mutate(
    d_name = as.numeric(substr(date, start = 9, stop = 10)),
    m_name = case_when(
      month == 1 ~ "January",
      month == 2 ~ "February",
      month == 3 ~ "March",
      month == 4 ~ "April",
      month == 5 ~ "May",
      month == 6 ~ "June",
      month == 7 ~ "July",
      month == 8 ~ "August",
      month == 9 ~ "September",
      month == 10 ~ "October",
      month == 11 ~ "November",
      month == 12 ~ "December",
      TRUE ~ NA_character_  # Catch-all in case none of the above conditions are true
    ),
    day_label = paste(m_name, d_name, sep = " "),
    day_label_factor = factor(day_label, levels = unique(day_label[order(day)]))
  )

city_list <- c("Dallas, TX",         "Denver, CO",         "Chicago, IL",       
               "Atlanta, GA",        "Charlotte, NC",      "Houston, TX",       
               "Phoenix, AZ",        "Detroit, MI",        "Minneapolis, MN",   
               "Salt Lake City, UT", "Las Vegas, NV",      "Washington, DC",    
               "Seattle, WA",        "Los Angeles, CA",    "San Francisco, CA")


rm(city1, city2, coords_1, coords_2, flights.trip)

anim_flight <- ggplot() + 
  geom_sf(data = states, fill = "gray15", colour = "gray20") +
  geom_curve(
    data = flight_sample, 
    aes(x = lon_2, y = lat_2, xend = lon_1, yend = lat_1, group = day, alpha = trips), 
    lineend = "round", color = "cyan", curvature = 0.3, linewidth = 0.3) +
  scale_alpha(range = c(0.01, 0.25), guide = "none") +
  geom_sf(data = places %>% filter(city %in% city_list), color = "orchid", size = 4) +
  theme_void() + 
  theme(panel.background = element_rect(fill = "gray10"),
        plot.title = element_text( color = "cyan4", face = "bold", size = 70, hjust = 0.06, vjust = -10)) +
  transition_states(day_label_factor) +  # Use transition_manual to animate based on 'day_label'
  ease_aes('sine-in-out') +
  labs(title = '{closest_state}')

# animate(anim_flight, end_pause = 5, height = 1600, width = 2000, fps = 8, renderer = gifski_renderer())

anim_save("flight_animation.gif", animation = anim_flight, end_pause = 4, height = 1500, width = 2500, fps = 8, renderer = gifski_renderer())

#### LINE GRAPH ANIMATION ####
anim_plot <- flight_sample %>%
  st_drop_geometry() %>%
  ungroup() %>%
  group_by(day_label_factor) %>%
  summarize(trips = sum(trips)) %>%
  ungroup() %>%
  ggplot(aes(x = day_label_factor, y = trips)) +
  geom_point(color = "cyan4", size = 8) +
  transition_states(day_label_factor) +  # Use transition_manual to animate based on 'day_label'
  ease_aes('cubic-in-out') +
  exit_disappear() +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "gray10"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

animate(anim_plot, end_pause = 3, height = 400, width = 600, fps = 8, renderer = gifski_renderer())

anim_save("flight_animation.gif", animation = anim_plot)

# check for most frequented cities
city1 <- flights.sf %>%
  st_drop_geometry() %>%
  dplyr::select(city_1) %>%
  rename(city = city_1)

city2 <- flights.sf %>%
  st_drop_geometry() %>%
  dplyr::select(city_2) %>%
  rename(city = city_2)

cities <- rbind(city1, city2) %>%
  mutate(trips = 1) %>%
  group_by(city) %>%
  summarize(trips = sum(trips)) %>%
  arrange(desc(trips)) %>%
  head(15) %>%
  left_join(places, by = "city") %>%
  st_as_sf()


# to know which cities to label and where they are
ggplot() +
  geom_sf(data = states, color = "gray", fill = "gray80") +
  geom_sf(data = cities, color = "orchid") +
  geom_sf_text(data = cities, aes(label = substr(city, start = 1, stop = 10), size = 2), nudge_y = 10) +
  scale_size(guide = "none") +
  theme_void()
  
