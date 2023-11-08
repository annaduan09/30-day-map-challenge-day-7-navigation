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

# old_names_1 <- flights.trip %>%
#   dplyr::mutate(city = city_1) %>% 
#   ungroup() %>%
#   left_join(places, by = "city") %>%
#   filter(is.na(NAME) == TRUE) %>%
#   dplyr::select(city) %>%
#   unique()
# 
# old_names_2 <- flights.trip %>%
#   mutate(city = city_2) %>% 
#   ungroup() %>%
#   left_join(places, by = "city") %>%
#   filter(is.na(NAME) == TRUE) %>%
#   dplyr::select(city) %>%
#   unique()

# old_names <- rbind(old_names_2, old_names_1) %>%
#   unique()
# old_names <- old_names[order(old_names$city),]

#old_names <- list(old_names)


# new_names <- c("Adak, AK", "Allentown, PA", "Arcata, CA","Ashland, VA", "Augusta-Richmond County consolidated government (balance), GA",
#                "Beaumont, TX", "Bend, OR",  "Bismarck, ND", "Bloomington, IL","Boise City, ID",
#                "Bristol, TN","Butte-Silver Bow (balance), MT", "Cedar Rapids, IA","Champaign, IL",  "Charleston, WV",
#                "Clarksburg, WV", "College Station, TX","Dallas, TX", "Prudhoe Bay, AK",  "Elmira, NY",
#                "Greensboro, NC",  "Adacao, GU", "Gulfport, MS","Hancock, MI","Harlingen, TX", 
#                "Hattiesburg, MS", "Hilton Head Island, SC","Urban Honolulu, HI","Indianapolis city (balance), IN","Iron Mountain, MI",
#                "Ithaca, NY", "Jackson, MS","Jacksonville, NC",  "Kailua, HI",  "Lawton, OK",
#                "Lexington-Fayette, KY",  "Manhattan, KS", "Midland, TX", "Mission, TX", "Montrose, CO",
#                "Nashville-Davidson metropolitan government (balance), TN", "New Bern, NC", "Newburgh, NY", "Newport News, VA", "North Bend, OR",  
#                "Pago Pago, AS", "Pasco, WA",  "Raleigh, NC", "Riverton, WY", "Saginaw, MI", 
#                "Kagman, MP",  "Sarasota, FL",  "Scranton, PA",     "Sun Valley, ID", "Palm Beach, FL") 

# 
# missing_names_1 <- data.frame(old_names, new_names) %>%
#   rename(new_names_1 = new_names,
#          city_1 = city)
# 
# missing_names_2 <- data.frame(old_names, new_names) %>%
#   rename(city_2 = city,
#          new_names_2 = new_names)

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
  # left_join(missing_names_1, by = "city_1") %>%
  # left_join(missing_names_2, by = "city_2") %>%
  # mutate(city_1 = ifelse(is.na(new_names_1) == FALSE, new_names_1, city_1),
  #        city_2 = ifelse(is.na(new_names_2) == FALSE, new_names_2, city_2)) %>%
  left_join(coords_1, by = "city_1") %>%
  left_join(coords_2, by = "city_2") %>%
 # dplyr::select(-c(new_names_1, new_names_2)) %>%
  filter(grepl("TT", trip) == FALSE & grepl("VI", trip) == FALSE) %>%
  st_as_sf(sf_column_name = "geometry_1") 

#### MAP ####
# 
# flight_sample <- flights.sf %>%
#  # sample_n(size = 500000) %>%
#   filter(is.na(lat_1) == FALSE &
#            is.na(lat_2) == FALSE &
#            is.na(lon_1) == FALSE &
#            is.na(lon_2) == FALSE) %>%
#   mutate(d_name = substr(date, start = 6, stop = 7) %>% as.numeric(),
#            m_name = ifelse(month == 1, "January",
#                          ifelse(month == 2, "February",
#                                 ifelse(month == 3, "March",
#                                        ifelse(month == 4, "April",
#                                               ifelse(month == 5, "May",
#                                                      ifelse(month == 6, "June",
#                                                             ifelse(month == 7, "July",
#                                                                    ifelse(month == 8, "August",
#                                                                           ifelse(month == 9, "September",
#                                                                                  ifelse(month == 10, "October",
#                                                                                         ifelse(month == 11, "November", "December"))))))))))),
#          day_label = paste(m_name, d_name, sep = " "))


flight_sample <- flights.sf %>%
 # sample_n(size = 10000) %>%
  filter(!is.na(lat_1) & !is.na(lat_2) & !is.na(lon_1) & !is.na(lon_2)) %>%
  mutate(
    d_name = as.numeric(substr(date, start = 6, stop = 7)),
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


anim <- ggplot() + 
  geom_sf(data = states, fill = "gray15", colour = "gray20") +
  geom_curve(
    data = flight_sample, 
    aes(x = lon_2, y = lat_2, xend = lon_1, yend = lat_1, group = day, alpha = trips), 
    lineend = "round", color = "cyan", curvature = 0.3, linewidth = 0.2) +
  scale_alpha(range = c(0.01, 0.35)) +
  theme_void() + 
  theme(panel.background = element_rect(fill = "gray10"),
        plot.title = element_text( color = "cyan4", face = "bold", size = 30, hjust = 0.06, vjust = -10)) +
  transition_states(date) +  # Use transition_manual to animate based on 'day_label'
  ease_aes('sine-in-out') +
  #exit_fade(alpha = 0.01) + 
  labs(title = 'Date: {closest_state}')

animate(anim, end_pause = 5, height = 800, width = 1000, fps = 4, renderer = gifski_renderer())

anim_save("flight_animation.gif", animation = anim)
# 
# 
# ggplot() + 
#   geom_sf(data = states, fill = "gray15", colour = "gray20") +
#   geom_curve(data = flight_sample, lineend = "round", color = "cyan", curvature = 0.3, linewidth = 0.1,
#              aes(xend = lon_1, yend = lat_1, x = lon_2, y = lat_2), alpha = 0.2) +
#   theme_void() + 
#   theme(panel.background = element_rect(fill = "gray10")) +
#   transition_manual(frames = day) +
#   labs(title = 'Day: {current_frame}') +
#   ease_aes('linear') 
# 
# anim <- last_animation()
# anim_save("flight_animation.gif", animation = anim)
