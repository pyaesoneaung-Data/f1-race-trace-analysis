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

#print(belgium_1998)

# Get race ID
race_id <- belgium_1998$raceId
cat("Race ID for 1998 Belgian Grand Prix is:", race_id, "\n")

# Get lap times for 1998 Belgian Grand Prix
race_laps <- lap_times %>%
  filter(raceId == race_id)

cat("Number of lap records for this race:", nrow(race_laps), "\n")

# View first few rows
#head(race_laps)

# Join driver names
race_laps <- race_laps %>%
  left_join(drivers, by = "driverId") %>%
  mutate(driver_name = paste(forename, surname))

# Check driver names in this race
#unique_drivers <- race_laps %>%
# select(driverId, driver_name) %>%
#  distinct()

# print(unique_drivers)

# Calculate cumulative race time for each driver
race_trace <- race_laps %>%
  arrange(driverId, lap) %>%
  group_by(driverId, driver_name) %>%
  mutate(cumulative_time = cumsum(milliseconds) / 1000) %>%
  ungroup()

# Check result
#print(head(race_trace))

# Find leader cumulative time at each lap
leader_time_by_lap <- race_trace %>%
  group_by(lap) %>%
  summarise(leader_time = min(cumulative_time), .groups = "drop")

# Calculate each driver's gap to the leader
race_trace_gap <- race_trace %>%
  left_join(leader_time_by_lap, by = "lap") %>%
  mutate(gap_to_leader = cumulative_time - leader_time)

# Check result
#print(head(race_trace_gap))

# Get pit stops for 1998 Belgian Grand Prix
race_pit_stops <- pit_stops %>%
  filter(raceId == race_id) %>%
  select(raceId, driverId, lap, stop, duration, milliseconds)

# Match pit stops with race trace data
pit_stop_points <- race_trace_gap %>%
  inner_join(race_pit_stops, by = c("raceId", "driverId", "lap"))

# Plot race trace with pit stop dots
race_plot <- ggplot(race_trace_gap, aes(x = lap, y = gap_to_leader, color = driver_name)) +
  geom_line(linewidth = 1, alpha = 0.8) +
  geom_point(
    data = pit_stop_points,
    aes(x = lap, y = gap_to_leader),
    size = 3
  ) +
  labs(
    title = "Race Trace: 1998 Belgian Grand Prix",
    subtitle = "Gap to race leader by lap; dots show pit stops",
    x = "Lap",
    y = "Gap to Leader (seconds)",
    color = "Driver"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11)
  )

print(race_plot)

ggsave("belgian_1998_race_trace.png", race_plot, width = 12, height = 7)

cat("Number of pit stops in this race:", nrow(race_pit_stops), "\n")
cat("Number of pit stop points matched to chart:", nrow(pit_stop_points), "\n")
