#library
library(tidyverse)

#load csv files
races <- read.csv("races.csv")
results <- read.csv("results.csv")
drivers <- read.csv("drivers.csv")
constructors <- read.csv("constructors.csv")
lap_times <- read.csv("lap_times.csv")
pit_stops <- read.csv("pit_stops.csv")
status <- read.csv("status.csv")

# Check data loaded
cat("Data loaded successfully\n")
cat("Number of races:", nrow(races), "\n")
cat("Number of lap time records:", nrow(lap_times), "\n")

# Find 1998 Belgian Grand Prix
belgium_1998 <- races %>%
  filter(year == 1998, name == "Belgian Grand Prix")

print(belgium_1998)

# Get race ID
race_id <- belgium_1998$raceId
cat("Race ID for 1998 Belgian Grand Prix is:", race_id, "\n")

# Get lap times for 1998 Belgian Grand Prix
race_laps <- lap_times %>%
  filter(raceId == race_id)

cat("Number of lap records for this race:", nrow(race_laps), "\n")

# View first few rows
head(race_laps)

# Join driver names
race_laps <- race_laps %>%
  left_join(drivers, by = "driverId") %>%
  mutate(driver_name = paste(forename, surname))

# Check driver names in this race
unique_drivers <- race_laps %>%
  select(driverId, driver_name) %>%
  distinct()

print(unique_drivers)

# Calculate cumulative race time for each driver
race_trace <- race_laps %>%
  arrange(driverId, lap) %>%
  group_by(driverId, driver_name) %>%
  mutate(cumulative_time = cumsum(milliseconds) / 1000) %>%
  ungroup()

# Check result
print(head(race_trace))

# Find leader cumulative time at each lap
leader_time_by_lap <- race_trace %>%
  group_by(lap) %>%
  summarise(leader_time = min(cumulative_time), .groups = "drop")

# Calculate each driver's gap to the leader
race_trace_gap <- race_trace %>%
  left_join(leader_time_by_lap, by = "lap") %>%
  mutate(gap_to_leader = cumulative_time - leader_time)

# Check result
print(head(race_trace_gap))
