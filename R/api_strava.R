#' Fetch Activities from Strava API
#'
#' This module provides functions to fetch activity metadata from Strava.
#' Only fetches metadata (no GPS streams/points).

#' Fetch all activities from Strava
#'
#' Fetches activity metadata from Strava API with pagination.
#' Filters to running and cycling activities only.
#'
#' @param after Optional. Unix timestamp to fetch activities after this date.
#'   If NULL, fetches all activities.
#' @param per_page Number of activities per page (max 200)
#' @return data.table with activity metadata
#' @export
fetch_strava_activities <- function(after = NULL, per_page = 200) {
  message("Fetching Strava activities...")

  all_activities <- list()
  page <- 1
  total_fetched <- 0

  repeat {
    message("Fetching page ", page, "...")

    # Build request
    req <- strava_request("athlete/activities") |>
      httr2::req_url_query(per_page = per_page, page = page)

    if (!is.null(after)) {
      req <- req |> httr2::req_url_query(after = after)
    }

    # Execute request
    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) {
        message("Error fetching Strava activities: ", e$message)
        return(NULL)
      }
    )

    if (is.null(resp)) {
      break
    }

    # Parse response
    activities_list <- httr2::resp_body_json(resp, simplifyVector = FALSE)

    # Check if we got any activities
    if (length(activities_list) == 0) {
      message("No more activities to fetch")
      break
    }

    # Convert list to data frame, flattening nested structures
    activities_df <- lapply(activities_list, function(act) {
      # Helper to safely extract scalar values
      get_val <- function(x, default = NA) {
        if (is.null(x)) return(default)
        if (length(x) == 0) return(default)
        return(x)
      }

      # Extract only scalar fields we care about
      data.frame(
        id = get_val(act$id),
        name = get_val(act$name, ""),
        type = get_val(act$type, ""),
        distance = get_val(act$distance, 0),
        moving_time = get_val(act$moving_time, 0),
        elapsed_time = get_val(act$elapsed_time, 0),
        total_elevation_gain = get_val(act$total_elevation_gain, 0),
        start_date = get_val(act$start_date, ""),
        start_date_local = get_val(act$start_date_local, ""),
        has_heartrate = get_val(act$has_heartrate, FALSE),
        average_heartrate = get_val(act$average_heartrate, NA_real_),
        max_heartrate = get_val(act$max_heartrate, NA_real_),
        stringsAsFactors = FALSE
      )
    })

    activities <- do.call(rbind, activities_df)

    # Filter to running and cycling only
    activities <- activities[
      activities$type %in% c("Run", "Ride", "VirtualRide", "VirtualRun"),
    ]

    if (nrow(activities) > 0) {
      all_activities[[page]] <- activities
      total_fetched <- total_fetched + nrow(activities)
      message("Fetched ", nrow(activities), " activities (", total_fetched, " total)")
    }

    # Check if we should continue (if we got less than requested, we're done)
    if (length(activities) < per_page) {
      message("Reached end of activities")
      break
    }

    page <- page + 1

    # Rate limiting: be nice to Strava API
    Sys.sleep(0.5)
  }

  if (length(all_activities) == 0) {
    message("No Strava activities found")
    return(data.table::data.table())
  }

  # Combine all pages
  combined <- data.table::rbindlist(all_activities, fill = TRUE)

  message("Successfully fetched ", nrow(combined), " Strava activities")

  return(combined)
}

#' Parse Strava activity data to standard format
#'
#' Converts raw Strava API response to standardized format
#'
#' @param activities data.frame or data.table from Strava API
#' @return data.table with standardized column names
#' @noRd
parse_strava_activities <- function(activities) {
  if (nrow(activities) == 0) {
    return(data.table::data.table())
  }

  # Convert to data.table
  dt <- data.table::as.data.table(activities)

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

  # Extract relevant fields
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

#' Fetch and parse Strava activities
#'
#' High-level function that fetches and parses Strava activities
#'
#' @param after Optional. Unix timestamp or Date to fetch activities after
#' @return data.table with parsed activity metadata
#' @export
get_strava_activities <- function(after = NULL) {
  # Convert Date to unix timestamp if needed
  if (!is.null(after) && inherits(after, "Date")) {
    after <- as.numeric(as.POSIXct(after, tz = "UTC"))
  }

  # Fetch raw activities
  raw_activities <- fetch_strava_activities(after = after)

  if (nrow(raw_activities) == 0) {
    return(data.table::data.table())
  }

  # Parse to standard format
  parsed <- parse_strava_activities(raw_activities)

  return(parsed)
}

#' Get Strava athlete info
#'
#' Fetches basic athlete information (for testing auth)
#'
#' @return List with athlete information
#' @export
get_strava_athlete <- function() {
  message("Fetching Strava athlete info...")

  req <- strava_request("athlete")

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      message("Error fetching athlete info: ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp)) {
    return(NULL)
  }

  athlete <- httr2::resp_body_json(resp)

  message("Authenticated as: ", athlete$firstname, " ", athlete$lastname)

  return(athlete)
}
