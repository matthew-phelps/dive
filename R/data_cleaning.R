#' Data Cleaning and Standardization
#'
#' Functions to clean and standardize activity data from multiple sources

#' Combine activities from multiple sources
#'
#' Combines Strava and RideWithGPS activities into a single dataset
#' with standardized column names and formats.
#'
#' @param strava_data data.table from Strava API (optional)
#' @param rwgps_data data.table from RideWithGPS API (optional)
#' @return data.table with combined activities
#' @export
combine_activities <- function(strava_data = NULL, rwgps_data = NULL) {
  message("Combining activities from sources...")

  activities_list <- list()

  # Add Strava data if provided
  if (!is.null(strava_data) && nrow(strava_data) > 0) {
    message("  - Strava: ", nrow(strava_data), " activities")
    activities_list[["strava"]] <- data.table::as.data.table(strava_data)
  }

  # Add RideWithGPS data if provided
  if (!is.null(rwgps_data) && nrow(rwgps_data) > 0) {
    message("  - RideWithGPS: ", nrow(rwgps_data), " activities")
    activities_list[["rwgps"]] <- data.table::as.data.table(rwgps_data)
  }

  if (length(activities_list) == 0) {
    message("No activities to combine")
    return(data.table::data.table())
  }

  # Combine all sources
  combined <- data.table::rbindlist(activities_list, fill = TRUE)

  message("Combined total: ", nrow(combined), " activities")

  return(combined)
}

#' Clean and validate activity data
#'
#' Performs cleaning and validation on combined activity data
#'
#' @param activities data.table with activity data
#' @return data.table with cleaned activities
#' @export
clean_activities <- function(activities) {
  if (nrow(activities) == 0) {
    return(activities)
  }

  message("Cleaning activity data...")
  dt <- data.table::copy(activities)

  # Remove activities with missing critical fields
  initial_count <- nrow(dt)

  dt <- dt[!is.na(date) & !is.na(distance)]
  removed <- initial_count - nrow(dt)

  if (removed > 0) {
    message("Removed ", removed, " activities with missing date or distance")
  }

  # Remove activities with zero or negative distance
  dt <- dt[distance > 0]

  # Convert distance to kilometers for easier reading
  dt[, distance_km := distance / 1000]

  # Convert duration to minutes
  dt[, duration_min := duration / 60]

  # Calculate pace (min/km) for running
  dt[activity_type == "running" & duration > 0 & distance > 0,
     pace_min_km := (duration / 60) / distance_km]

  # Calculate speed (km/h) for cycling
  dt[activity_type == "cycling" & duration > 0 & distance > 0,
     speed_kmh := distance_km / (duration / 3600)]

  # Add year, month, week for grouping
  dt[, year := lubridate::year(date)]
  dt[, month := lubridate::month(date)]
  dt[, week := lubridate::week(date)]
  dt[, year_month := format(date, "%Y-%m")]
  dt[, year_week := paste0(year, "-W", sprintf("%02d", week))]

  # Initialize deduplication flags
  dt[, is_duplicate := FALSE]
  dt[, duplicate_of := NA_character_]

  # Sort by date (most recent first)
  data.table::setorder(dt, -date, -start_datetime)

  message("Cleaned ", nrow(dt), " activities")

  return(dt)
}

#' Process raw data into cleaned format
#'
#' High-level function that combines and cleans all activity data
#'
#' @return data.table with cleaned activities
#' @export
process_activities <- function() {
  message("Processing activities...")

  # Load raw data from storage
  strava_raw <- load_raw_data("strava")
  rwgps_raw <- load_raw_data("rwgps")

  # Combine sources
  combined <- combine_activities(strava_raw, rwgps_raw)

  if (nrow(combined) == 0) {
    message("No activities to process")
    return(data.table::data.table())
  }

  # Clean and validate
  cleaned <- clean_activities(combined)

  return(cleaned)
}

#' Get activity summary statistics
#'
#' Generates summary statistics for a set of activities
#'
#' @param activities data.table with activity data
#' @return List with summary statistics
#' @export
summarize_activities <- function(activities) {
  if (nrow(activities) == 0) {
    return(list(
      total_activities = 0,
      total_distance_km = 0,
      total_duration_hours = 0,
      by_type = data.table::data.table()
    ))
  }

  dt <- data.table::as.data.table(activities)

  # Overall summary
  summary <- list(
    total_activities = nrow(dt),
    total_distance_km = sum(dt$distance_km, na.rm = TRUE),
    total_duration_hours = sum(dt$duration / 3600, na.rm = TRUE),
    total_elevation_m = sum(dt$elevation_gain, na.rm = TRUE),
    date_range = range(dt$date, na.rm = TRUE)
  )

  # Summary by activity type
  by_type <- dt[, .(
    count = .N,
    total_distance_km = sum(distance_km, na.rm = TRUE),
    avg_distance_km = mean(distance_km, na.rm = TRUE),
    total_duration_hours = sum(duration / 3600, na.rm = TRUE)
  ), by = activity_type]

  summary$by_type <- by_type

  # Summary by source
  by_source <- dt[, .(
    count = .N,
    total_distance_km = sum(distance_km, na.rm = TRUE)
  ), by = source]

  summary$by_source <- by_source

  return(summary)
}
