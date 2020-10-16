library(tidyverse)
library(ggplot2)
library(tidyr)
library(janitor)
library(lubridate)


age <- read_csv("Data/boroughs-by-age.csv", col_types = 
                  cols(.default = col_double(),
                       group = col_character() ))

# It was difficult to work this data because the intial dataset was very wide.
# Therefore, I decided to make the data longer, whcih was difficult!

age_long <- pivot_longer(data = age, 
                         cols = -group, 
                         names_to = "county",
                         values_to = "count") %>% 
  separate(col = county, into = c("location", "type"), sep = "_", extra = "drop") %>%
  slice(1:150) %>%
  group_by(group, type, location) %>%
  summarize(av = mean(count), .groups = "drop") %>%
  arrange(location)

# I worked with Ishan to understand how to split the column that had both the
# county and the type of test into two columns. I first tried to map an
# str_split function but that did not give me the result I wanted. Then, I
# learned about the separate function and tried to map it as well, which created
# an error. Then, I learned that the separate function does not need a map
# function!

three <- read_csv("Data/311_Service_Requests_from_2010_to_Present.csv", col_types = cols(
  .default = col_character(),
  `Unique Key` = col_double(),
  `Incident Zip` = col_double(),
  `Address Type` = col_logical(),
  `Facility Type` = col_logical(),
  `Due Date` = col_logical(),
  BBL = col_double(),
  `X Coordinate (State Plane)` = col_number(),
  `Y Coordinate (State Plane)` = col_number(),
  `Vehicle Type` = col_logical(),
  `Taxi Company Borough` = col_logical(),
  `Taxi Pick Up Location` = col_logical(),
  `Bridge Highway Name` = col_logical(),
  `Bridge Highway Direction` = col_logical(),
  `Road Ramp` = col_logical(),
  `Bridge Highway Segment` = col_logical(),
  Latitude = col_double(),
  Longitude = col_double()
))

group_three <- three %>%
  clean_names() %>%
  select(created_date, location_type, incident_zip, borough, location) %>%
  filter(!is.na(location_type)) %>%
  filter(!is.na(borough)) %>%
  
  # This decision to remove the NA's in the location_type column was just made to
  # make the graph look better. I will need to reevaluate this decision in my
  # final project.
  
  group_by(location_type, borough) %>%
  summarize(count = n(), .groups = "drop") %>%
  ggplot(aes(x = location_type, y = count, fill = borough)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Amount of Social Distancing Violations per
       Location and Borough")

