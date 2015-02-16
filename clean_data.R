# Import the results of data processing ruby scripts, and reshape into a form
# usable for GIS plotting

library(dplyr)
library(tidyr)
library(stringr)

orig_landscape_data <- read.csv("data/object_data.csv")
name_coordinates <- read.csv("data/location_coordinates.csv")

# Only use those named coordinates with exact values
name_coordinates <- name_coordinates %>% filter(!(is.na(longitude) & is.na(latitude)))

drop_types <- c("avondmaalskelk", "beeld", "beeldhouwwerk", "bouwfragment", "document", "kan", "balkon", "schaal (objectnaam)", "drinkschaal", "tazza", "troffel")

# Use non-location columns as the id variables. This results in a table with one
# row per location; artworks with multiple locations will be represented with
# multiple rows.
multi_row_data <- orig_landscape_data %>%
  gather(
  key=numPlace,
  value=place,
  classification.places.0:classification.places.8,
  na.rm=TRUE) %>%
  filter(!(objectTypes.0 %in% drop_types))

# Join coordinates to each artwork/location row, keeping only those rows with
# places specified in name_coordinates
location_data <- name_coordinates %>% inner_join(multi_row_data, by="place") %>% select(-description) %>% filter(type != "")

# Categorize artworks into 20-year periods
per <- seq(1540, 1750, by = 20)
labelize <- function(x) {
  label <- paste(x, "-", x + 19, sep="")
  return(label)
}
per_labs <- vapply(per, labelize, FUN.VALUE = "character")
location_data$period <- cut(location_data$dating.year, breaks = per)
levels(location_data$period) <- per_labs

place_stats <- location_data %>%
  group_by(place) %>%
  summarize(
    count = n(),
    longitude = min(longitude),
    latitude = min(latitude),
    min_start = min(dating.yearEarly),
    max_start = max(dating.yearEarly),
    min_end = (min(dating.yearLate)),
    max_end = max(dating.yearLate),
    date_sd = sd(dating.yearEarly),
    date_var = var(dating.yearEarly),
    date_mean = mean(dating.yearEarly)) %>%
  mutate(spread = max_start - min_start)

period_place_stats <- location_data %>%
  group_by(place, period) %>%
  summarize(
    count = n(),
    longitude = min(longitude),
    latitude = min(latitude),
    min_start = min(dating.yearEarly),
    max_start = max(dating.yearEarly),
    min_end = (min(dating.yearLate)),
    max_end = max(dating.yearLate),
    date_sd = sd(dating.yearEarly),
    date_var = var(dating.yearEarly),
    date_mean = mean(dating.yearEarly)) %>%
  mutate(spread = max_start - min_start)

location_data$short_place <- str_match(location_data$place, "(.*) \\(Amsterdam\\)")[,2]
place_stats$short_place <- str_match(place_stats$place, "(.*) \\(Amsterdam\\)")[,2]
period_place_stats$short_place <- str_match(period_place_stats$place, "(.*) \\(Amsterdam\\)")[,2]

save(location_data, period_place_stats, file = "data/location_data.RData")
