#' Fetch Activities from RideWithGPS API
#'
#' This module provides functions to fetch activity metadata from RideWithGPS.
#' Only fetches metadata (no GPS streams/points).

#' Fetch user ID from RideWithGPS
#'
#' Gets the authenticated user's ID
#'
#' @return Numeric user ID
#' @noRd
get_rwgps_user_id <- function() {
  message("Fetching RideWithGPS user info...")

  req <- rwgps_request("v1/users/current.json")

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      message("Error fetching user info: ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp)) {
    stop("Failed to fetch user information from RideWithGPS", call. = FALSE)
  }

  user_info <- httr2::resp_body_json(resp)

  return(user_info$user$id)
}

#' Fetch all trips from RideWithGPS
#'
#' Fetches trip (activity) metadata from RideWithGPS API with pagination.
#' Filters to running and cycling activities only.
#'
#' @param after Optional. Date to fetch activities after. If NULL, fetches all.
#' @param page_size Number of trips per page (default 50, max 200)
#' @return data.table with trip metadata
#' @export
fetch_rwgps_trips <- function(after = NULL, page_size = 50) {
  message("Fetching RideWithGPS trips...")

  # Ensure page_size is within API limits (20-200)
  page_size <- max(20, min(200, page_size))

  all_trips <- list()
  page <- 1
  total_fetched <- 0

  repeat {
    message("Fetching trips page ", page, "...")

    # Build request using v1 API endpoint (note: trips endpoint includes .json)
    req <- rwgps_request("v1/trips.json") |>
      httr2::req_url_query(page = page, page_size = page_size)

    # Execute request
    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) {
        message("Error fetching RideWithGPS trips: ", e$message)
        return(NULL)
      }
    )

    if (is.null(resp)) {
      break
    }

    # Parse response
    result <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    # Check if we got any trips (v1 API uses 'trips' not 'results')
    if (is.null(result$trips) || length(result$trips) == 0) {
      message("No more trips to fetch")
      break
    }

    trips <- result$trips

    # Filter to running and cycling only (if activity_type field exists)
    if ("activity_type" %in% names(trips)) {
      trips <- trips[trips$activity_type %in% c("cycling", "running"), ]
    }

    if (nrow(trips) > 0) {
      # Filter by date if specified
      if (!is.null(after)) {
        trip_dates <- as.Date(trips$departed_at)
        trips <- trips[trip_dates >= as.Date(after), ]
      }

      if (nrow(trips) > 0) {
        all_trips[[length(all_trips) + 1]] <- trips
        total_fetched <- total_fetched + nrow(trips)
        message("Fetched ", nrow(trips), " trips (", total_fetched, " total)")
      }
    }

    # Check if we should continue using pagination metadata
    if (!is.null(result$meta) && !is.null(result$meta$pagination)) {
      page_count <- result$meta$pagination$page_count
      if (page >= page_count) {
        message("Reached end of trips (page ", page, " of ", page_count, ")")
        break
      }
    } else if (length(result$trips) < page_size) {
      # Fallback if no pagination metadata
      message("Reached end of trips (received fewer than page_size)")
      break
    }

    page <- page + 1

    # Rate limiting
    Sys.sleep(0.5)
  }

  if (length(all_trips) == 0) {
    message("No RideWithGPS trips found")
    return(data.table::data.table())
  }

  # Combine all pages
  combined <- data.table::rbindlist(all_trips, fill = TRUE)

  message("Successfully fetched ", nrow(combined), " RideWithGPS trips")

  return(combined)
}

#' Parse RideWithGPS trip data to standard format
#'
#' Converts raw RideWithGPS API response to standardized format
#'
#' @param trips data.frame or data.table from RideWithGPS API
#' @return data.table with standardized column names
#' @noRd
parse_rwgps_trips <- function(trips) {
  if (nrow(trips) == 0) {
    return(data.table::data.table())
  }

  # Convert to data.table
  dt <- data.table::as.data.table(trips)

  # Standardize activity type (v1 API already uses standardized values)
  dt[, activity_type_std := data.table::fcase(
    activity_type == "running", "running",
    activity_type == "cycling", "cycling",
    default = "other"
  )]

  # Parse start date - RideWithGPS uses 'departed_at'
  dt[, start_datetime := lubridate::ymd_hms(departed_at, tz = "UTC")]
  dt[, date := as.Date(start_datetime)]
  dt[, start_time := format(start_datetime, "%H:%M:%S")]

  # Extract relevant fields
  # Note: v1 API already provides distance in meters, duration in seconds, elevation_gain in meters
  result <- dt[, .(
    activity_id = as.character(id),
    source = "rwgps",
    activity_type = activity_type_std,
    date,
    start_time,
    start_datetime,
    distance = if ("distance" %in% names(dt)) distance else NA_real_,  # Already in meters
    duration = if ("duration" %in% names(dt)) duration else if ("moving_time" %in% names(dt)) moving_time else NA_real_,  # Already in seconds
    elevation_gain = if ("elevation_gain" %in% names(dt)) elevation_gain else NA_real_,  # Already in meters
    has_heartrate = if ("avg_hr" %in% names(dt)) !is.na(avg_hr) & !is.null(avg_hr) else FALSE,
    avg_heartrate = if ("avg_hr" %in% names(dt)) avg_hr else NA_real_,
    max_heartrate = if ("max_hr" %in% names(dt)) max_hr else NA_real_,
    name = if ("name" %in% names(dt)) name else NA_character_,
    raw_type = if ("activity_type" %in% names(dt)) activity_type else NA_character_
  )]

  return(result)
}

#' Fetch and parse RideWithGPS trips
#'
#' High-level function that fetches and parses RideWithGPS trips
#'
#' @param after Optional. Date to fetch trips after
#' @return data.table with parsed trip metadata
#' @export
get_rwgps_trips <- function(after = NULL) {
  # Check if credentials are configured
  if (!has_rwgps_credentials()) {
    message("RideWithGPS credentials not configured. Skipping.")
    return(data.table::data.table())
  }

  # Fetch raw trips
  raw_trips <- fetch_rwgps_trips(after = after)

  if (nrow(raw_trips) == 0) {
    return(data.table::data.table())
  }

  # Parse to standard format
  parsed <- parse_rwgps_trips(raw_trips)

  return(parsed)
}

#' Get RideWithGPS user info
#'
#' Fetches basic user information (for testing auth)
#'
#' @return List with user information
#' @export
get_rwgps_user <- function() {
  message("Fetching RideWithGPS user info...")

  req <- rwgps_request("v1/users/current.json")

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      message("Error fetching user info: ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp)) {
    return(NULL)
  }

  user_info <- httr2::resp_body_json(resp)

  message("Authenticated as: ", user_info$user$name)

  return(user_info$user)
}
