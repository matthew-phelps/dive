#' Data Cleaning and Standardization
#'
#' Functions to clean and standardize activity data from multiple sources

#' Harmonize raw Strava data to standard schema
#'
#' Converts raw Strava API data (list format from data-raw/) to standardized data.table.
#' Handles both list-of-lists format (from JSON) and data.frame format.
#'
#' @param strava_raw List of raw activities or data.frame from stored Strava data
#' @return data.table with standardized schema
#' @export
harmonize_strava_raw <- function(strava_raw) {
  if (is.null(strava_raw)) {
    return(data.table::data.table())
  }

  # Handle list format (raw JSON from API)
  if (is.list(strava_raw) && !is.data.frame(strava_raw)) {
    if (length(strava_raw) == 0) {
      return(data.table::data.table())
    }

    # Helper to safely extract scalar values
    get_val <- function(x, default = NA) {
      if (is.null(x)) return(default)
      if (length(x) == 0) return(default)
      return(x)
    }

    # Convert each list element to a standardized row
    rows <- lapply(strava_raw, function(act) {
      # Standardize activity type
      activity_type <- data.table::fcase(
        act$type %in% c("Run", "VirtualRun"), "running",
        act$type %in% c("Ride", "VirtualRide"), "cycling",
        default = "other"
      )

      # Parse start date
      start_datetime <- lubridate::ymd_hms(act$start_date, tz = "UTC")

      list(
        activity_id = as.character(get_val(act$id)),
        source = "strava",
        activity_type = activity_type,
        date = as.Date(start_datetime),
        start_time = format(start_datetime, "%H:%M:%S"),
        start_datetime = start_datetime,
        distance = get_val(act$distance, 0),  # meters
        duration = get_val(act$moving_time, 0),  # seconds
        elevation_gain = get_val(act$total_elevation_gain, 0),  # meters
        has_heartrate = get_val(act$has_heartrate, FALSE),
        avg_heartrate = get_val(act$average_heartrate, NA_real_),
        max_heartrate = get_val(act$max_heartrate, NA_real_),
        name = get_val(act$name, ""),
        raw_type = get_val(act$type, "")
      )
    })

    # Convert to data.table
    dt <- data.table::rbindlist(rows)
    return(dt)
  }

  # Handle data.frame format (from earlier processing)
  if (is.data.frame(strava_raw)) {
    if (nrow(strava_raw) == 0) {
      return(data.table::data.table())
    }

    dt <- data.table::as.data.table(strava_raw)

    # Standardize activity type
    dt[, activity_type := data.table::fcase(
      type %in% c("Run", "VirtualRun"), "running",
      type %in% c("Ride", "VirtualRide"), "cycling",
      default = "other"
    )]

    # Parse start date
    dt[, start_datetime := lubridate::ymd_hms(start_date, tz = "UTC")]
    dt[, date := as.Date(start_datetime)]
    dt[, start_time := format(start_datetime, "%H:%M:%S")]

    # Extract relevant fields with standardized names
    result <- dt[, .(
      activity_id = as.character(id),
      source = "strava",
      activity_type,
      date,
      start_time,
      start_datetime,
      distance = distance,  # meters
      duration = moving_time,  # seconds
      elevation_gain = total_elevation_gain,  # meters
      has_heartrate = has_heartrate,
      avg_heartrate = if ("average_heartrate" %in% names(dt)) average_heartrate else NA_real_,
      max_heartrate = if ("max_heartrate" %in% names(dt)) max_heartrate else NA_real_,
      name = name,
      raw_type = type
    )]

    return(result)
  }

  # Unknown format
  stop("strava_raw must be a list or data.frame")
}

#' Harmonize raw RideWithGPS data to standard schema
#'
#' Converts raw RideWithGPS API data (list format from data-raw/) to standardized data.table.
#' Handles both list-of-lists format (from JSON) and data.frame format.
#'
#' @param rwgps_raw List of raw trips or data.frame from stored RWGPS data
#' @return data.table with standardized schema
#' @export
harmonize_rwgps_raw <- function(rwgps_raw) {
  if (is.null(rwgps_raw)) {
    return(data.table::data.table())
  }

  # Handle list format (raw JSON from API)
  if (is.list(rwgps_raw) && !is.data.frame(rwgps_raw)) {
    if (length(rwgps_raw) == 0) {
      return(data.table::data.table())
    }

    # Helper to safely extract scalar values
    get_val <- function(x, default = NA) {
      if (is.null(x)) return(default)
      if (length(x) == 0) return(default)
      return(x)
    }

    # Convert each list element to a standardized row
    rows <- lapply(rwgps_raw, function(trip) {
      # Standardize activity type (handle "cycling:road", "running", etc.)
      raw_type <- get_val(trip$activity_type, "")
      activity_type <- data.table::fcase(
        grepl("running", raw_type, ignore.case = TRUE), "running",
        grepl("cycling", raw_type, ignore.case = TRUE), "cycling",
        default = "other"
      )

      # Parse start date - RWGPS uses 'departed_at'
      start_datetime <- lubridate::ymd_hms(trip$departed_at, tz = "UTC")

      # Handle heart rate
      avg_hr <- get_val(trip$avg_hr, NA_real_)
      has_hr <- !is.na(avg_hr)

      list(
        activity_id = as.character(get_val(trip$id)),
        source = "rwgps",
        activity_type = activity_type,
        date = as.Date(start_datetime),
        start_time = format(start_datetime, "%H:%M:%S"),
        start_datetime = start_datetime,
        distance = get_val(trip$distance, 0),  # meters
        duration = get_val(trip$moving_time, get_val(trip$duration, 0)),  # seconds, prefer moving_time
        elevation_gain = get_val(trip$elevation_gain, 0),  # meters
        has_heartrate = has_hr,
        avg_heartrate = avg_hr,
        max_heartrate = get_val(trip$max_hr, NA_real_),
        name = get_val(trip$name, ""),
        raw_type = raw_type
      )
    })

    # Convert to data.table
    dt <- data.table::rbindlist(rows)
    return(dt)
  }

  # Handle data.frame format (from earlier processing)
  if (is.data.frame(rwgps_raw)) {
    if (nrow(rwgps_raw) == 0) {
      return(data.table::data.table())
    }

    dt <- data.table::as.data.table(rwgps_raw)

    # Standardize activity type
    dt[, activity_type_std := data.table::fcase(
      grepl("running", activity_type, ignore.case = TRUE), "running",
      grepl("cycling", activity_type, ignore.case = TRUE), "cycling",
      default = "other"
    )]

    # Parse start date
    dt[, start_datetime := lubridate::ymd_hms(departed_at, tz = "UTC")]
    dt[, date := as.Date(start_datetime)]
    dt[, start_time := format(start_datetime, "%H:%M:%S")]

    # Extract relevant fields with standardized names
    result <- dt[, .(
      activity_id = as.character(id),
      source = "rwgps",
      activity_type = activity_type_std,
      date,
      start_time,
      start_datetime,
      distance = if ("distance" %in% names(dt)) distance else NA_real_,  # meters
      duration = if ("moving_time" %in% names(dt)) moving_time else if ("duration" %in% names(dt)) duration else NA_real_,  # seconds
      elevation_gain = if ("elevation_gain" %in% names(dt)) elevation_gain else NA_real_,  # meters
      has_heartrate = if ("avg_hr" %in% names(dt)) !is.na(avg_hr) else FALSE,
      avg_heartrate = if ("avg_hr" %in% names(dt)) avg_hr else NA_real_,
      max_heartrate = if ("max_hr" %in% names(dt)) max_hr else NA_real_,
      name = if ("name" %in% names(dt)) name else NA_character_,
      raw_type = if ("activity_type" %in% names(dt)) activity_type else NA_character_
    )]

    return(result)
  }

  # Unknown format
  stop("rwgps_raw must be a list or data.frame")
}

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
#' High-level function that harmonizes, combines, and cleans all activity data.
#' This is the main entry point for the data processing pipeline.
#'
#' @return data.table with cleaned activities
#' @export
process_activities <- function() {
  message("Processing activities...")

  # Load raw data from storage (in API-specific formats)
  strava_raw <- load_raw_data("strava")
  rwgps_raw <- load_raw_data("rwgps")

  # Harmonize each source to standardized schema
  message("Harmonizing data to standard schema...")
  strava_harmonized <- harmonize_strava_raw(strava_raw)
  rwgps_harmonized <- harmonize_rwgps_raw(rwgps_raw)

  # Combine sources (now that they share the same schema)
  combined <- combine_activities(strava_harmonized, rwgps_harmonized)

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
