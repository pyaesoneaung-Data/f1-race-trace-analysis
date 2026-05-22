# Choose race to analyze
target_year <- 1998
target_race_name <- "Belgian Grand Prix"

# Check data loaded
cat("Data loaded successfully\n")
cat("Number of races:", nrow(races), "\n")
cat("Number of lap time records:", nrow(lap_times), "\n")

# Find selected Grand Prix
selected_race <- races %>%
  filter(year == target_year, name == target_race_name)

# Get race ID
race_id <- selected_race$raceId

cat("Selected race:", target_year, target_race_name, "\n")
cat("Race ID:", race_id, "\n")

# Get lap times for selected Grand Prix
race_laps <- lap_times %>%
  filter(raceId == race_id)

cat("Number of lap records for this race:", nrow(race_laps), "\n")

# Join driver names
race_laps <- race_laps %>%
  left_join(drivers, by = "driverId") %>%
  mutate(driver_name = paste(forename, surname))

# Calculate cumulative race time for each driver
race_trace <- race_laps %>%
  arrange(driverId, lap) %>%
  group_by(driverId, driver_name) %>%
  mutate(cumulative_time = cumsum(milliseconds) / 1000) %>%
  ungroup()

# Find leader cumulative time at each lap
leader_time_by_lap <- race_trace %>%
  group_by(lap) %>%
  summarise(leader_time = min(cumulative_time), .groups = "drop")

# Calculate each driver's gap to the leader
race_trace_gap <- race_trace %>%
  left_join(leader_time_by_lap, by = "lap") %>%
  mutate(gap_to_leader = cumulative_time - leader_time)

# Get pit stops for selected Grand Prix
race_pit_stops <- pit_stops %>%
  filter(raceId == race_id) %>%
  select(raceId, driverId, lap, stop, duration, milliseconds)

# Match pit stops with race trace data
pit_stop_points <- race_trace_gap %>%
  inner_join(race_pit_stops, by = c("raceId", "driverId", "lap"))

# Create race result summary
race_results_summary <- results %>%
  filter(raceId == race_id) %>%
  left_join(drivers, by = "driverId") %>%
  left_join(constructors, by = "constructorId") %>%
  left_join(status, by = "statusId") %>%
  mutate(driver_name = paste(forename, surname)) %>%
  select(
    driver_name,
    team = name,
    grid,
    positionOrder,
    laps,
    status
  ) %>%
  arrange(positionOrder)

# Add insight/reason column
max_laps <- max(race_results_summary$laps)

race_results_summary <- race_results_summary %>%
  mutate(
    race_gap_reason = case_when(
      status %in% c("+1 Lap", "+2 Laps", "+3 Laps", "+4 Laps", "+5 Laps") ~
        paste("Finished", status, "behind the leader, so the race trace line appears much higher."),
      laps < max_laps & status != "Finished" ~
        paste("Did not complete the full race because of", status, "so the line stops early or separates."),
      TRUE ~
        "Completed the race normally compared with the leader."
    )
  )

print(race_results_summary)

# Drivers with the largest gap to leader
largest_gap_drivers <- race_trace_gap %>%
  group_by(driver_name) %>%
  summarise(max_gap = max(gap_to_leader), .groups = "drop") %>%
  arrange(desc(max_gap)) %>%
  slice_head(n = 2)

print(largest_gap_drivers)

# Plot race trace with pit stop dots and insight annotation
race_plot <- ggplot(race_trace_gap, aes(x = lap, y = gap_to_leader, color = driver_name)) +
  geom_line(linewidth = 1, alpha = 0.8) +
  geom_point(
    data = pit_stop_points,
    aes(x = lap, y = gap_to_leader),
    size = 3
  ) +
  annotate(
    "text",
    x = 31,
    y = 950,
    label = "Large gap: Coulthard and Nakano finished +5 laps behind",
    size = 4,
    hjust = 0
  ) +
  labs(
    title = paste("Race Trace:", target_year, target_race_name),
    subtitle = "Gap to race leader by lap; dots show pit stops",
    x = "Lap",
    y = "Gap to Leader (seconds)",
    color = "Driver",
    caption = "Insight: Drivers far above the field were several laps behind the leader, not a chart error."
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 10)
  )

print(race_plot)

# Save chart image
output_file <- paste0(
  target_year, "_",
  str_replace_all(str_to_lower(target_race_name), " ", "_"),
  "_race_trace.png"
)

ggsave(output_file, race_plot, width = 12, height = 7)

# Save result summary
summary_file <- paste0(
  target_year, "_",
  str_replace_all(str_to_lower(target_race_name), " ", "_"),
  "_results_summary.csv"
)

write.csv(race_results_summary, summary_file, row.names = FALSE)

cat("Number of pit stops in this race:", nrow(race_pit_stops), "\n")
cat("Number of pit stop points matched to chart:", nrow(pit_stop_points), "\n")
cat("Chart saved as:", output_file, "\n")
cat("Summary saved as:", summary_file, "\n")
