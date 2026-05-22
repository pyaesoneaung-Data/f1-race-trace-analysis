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
