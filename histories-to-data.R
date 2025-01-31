## Script to generate hourly case data from "histories"

# (1) "Histories1": We used 2 columns (time & type) "histories" data.
# we generate hourly data of of infection cases named "hist4".


library(dplyr)
library(tidyr)

# Load the data
data <- read.csv("./data/histories.csv")

# Filter only 'infection' entries and assign 1 to them
data <- data %>%
  filter(type == "infection") %>%
  mutate(type = 1)

# Convert 'time' to POSIXct format and create new columns for date and time
data <- data %>%
  mutate(time = as.POSIXct(time, origin = "1970-01-01"),
         date = as.Date(time),
         hour = format(time, "%H:00:00"))

# Group by 'hour' and 'date', and sum the 'type' column to count total infections per hour per date
result <- data %>%
  group_by(date, hour) %>%
  summarise(total_infection = sum(type))

print(result)

# Save the result as a CSV file
write.csv(result, "/data/hourly_data_full.csv", row.names = FALSE)





# (2) "Histories1": We also generated daily infections named "hist5".

library(dplyr)
library(tidyr)

# Load the data again
data <- read.csv("./data/histories.csv")

# Filter only 'infection' entries and assign 1 to them
data <- data %>%
  filter(type == "infection") %>%
  mutate(type = 1)

# Convert 'time' to POSIXct format and create new columns for date
data <- data %>%
  mutate(time = as.POSIXct(time, origin = "1970-01-01"),
         date = as.Date(time))

# Group by 'date' and sum the 'type' column to count total infections per day
result <- data %>%
  group_by(date) %>%
  summarise(total_infection = sum(type))

print(result)

# Save the result as a CSV file
write.csv(result, "./data/hist5.csv", row.names = FALSE)



