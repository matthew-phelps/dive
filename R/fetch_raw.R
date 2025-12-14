#' Fetch raw Strava data and save locally
#'
#' Fetches all activities from Strava API and saves the raw JSON response
#' for later processing. This avoids repeatedly hitting the API during development.
#'
#' @param output_file Path to save the raw data (default: "data-raw/strava_raw_full.rds")
#' @param per_page Number of activities per page (max 200)
#' @return Invisible list of raw activity data
#' @export
fetch_raw_strava <- function(output_file = "data-raw/strava_raw_full.rds", per_page = 200) {
  message("Fetching raw Strava data...")

  # Make sure we're authenticated
  if (!is_strava_authenticated()) {
    message("Not authenticated. Running strava_auth()...")
    strava_auth()
  }

  all_pages <- list()
  page <- 1

  repeat {
    message("Fetching page ", page, "...")

    req <- strava_request("athlete/activities") |>
      httr2::req_url_query(per_page = per_page, page = page)

    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) {
        message("Error: ", e$message)
        return(NULL)
      }
    )

    if (is.null(resp)) {
      break
    }

    # Get raw JSON as list (don't simplify)
    activities <- httr2::resp_body_json(resp, simplifyVector = FALSE)

    if (length(activities) == 0) {
      message("No more activities")
      break
    }

    message("  Got ", length(activities), " activities")
    all_pages[[page]] <- activities

    if (length(activities) < per_page) {
      message("Reached end")
      break
    }

    page <- page + 1
    Sys.sleep(0.5)  # Be nice to the API
  }

  # Flatten into single list
  all_activities <- unlist(all_pages, recursive = FALSE)

  message("\nFetched ", length(all_activities), " total activities")

  # Save as RDS
  saveRDS(all_activities, output_file)
  message("Saved to: ", output_file)

  # Print structure info
  if (length(all_activities) > 0) {
    message("\nFields available in activities:")
    message(paste(names(all_activities[[1]]), collapse = ", "))
  }

  invisible(all_activities)
}

#' Export sample activities to JSON
#'
#' Exports a sample of activities to JSON for inspection
#'
#' @param input_file Path to the saved raw data file (default: "data-raw/strava_raw_full.rds")
#' @param output_file Path to save the sample JSON (default: auto-generated from input_file)
#' @param n_samples Number of activities to include in sample (default: 3)
#' @return Invisible sample data
#' @export
export_sample_strava <- function(input_file = "data-raw/strava_raw_full.rds",
                                  output_file = NULL,
                                  n_samples = 3) {
  raw_data <- load_raw_strava(input_file)

  if (is.null(output_file)) {
    output_file <- sub("\\.rds$", "_sample.json", input_file)
  }

  sample_data <- raw_data[1:min(n_samples, length(raw_data))]

  jsonlite::write_json(
    sample_data,
    output_file,
    pretty = TRUE,
    auto_unbox = TRUE
  )

  message("Saved sample (", length(sample_data), " activities) to: ", output_file)

  invisible(sample_data)
}

#' Load saved raw Strava data
#'
#' @param input_file Path to the saved raw data file
#' @return List of raw activity data
#' @export
load_raw_strava <- function(input_file = "data-raw/strava_raw_full.rds") {
  if (!file.exists(input_file)) {
    stop("Raw data file not found: ", input_file, "\n",
         "Run fetch_raw_strava() first!")
  }

  raw_data <- readRDS(input_file)
  message("Loaded ", length(raw_data), " activities from ", input_file)

  return(raw_data)
}

#' Inspect raw Strava data structure
#'
#' @param raw_data List of raw activities (from fetch_raw_strava or load_raw_strava)
#' @export
inspect_raw_strava <- function(raw_data = NULL) {
  if (is.null(raw_data)) {
    raw_data <- load_raw_strava()
  }

  message("\n=== Raw Strava Data Inspection ===")
  message("Total activities: ", length(raw_data))

  if (length(raw_data) == 0) {
    message("No activities to inspect")
    return(invisible(NULL))
  }

  # Fields available
  message("\n=== Available Fields ===")
  print(names(raw_data[[1]]))

  # Sample values from first activity
  message("\n=== Sample Values (First Activity) ===")
  first <- raw_data[[1]]
  cat("ID:", first$id, "\n")
  cat("Name:", first$name, "\n")
  cat("Type:", first$type, "\n")
  cat("Distance:", first$distance, "meters\n")
  cat("Moving time:", first$moving_time, "seconds\n")
  cat("Start date:", first$start_date, "\n")
  cat("Has HR:", first$has_heartrate, "\n")
  if (!is.null(first$average_heartrate)) {
    cat("Avg HR:", first$average_heartrate, "\n")
  }

  # Activity type summary
  types <- sapply(raw_data, function(x) x$type)
  message("\n=== Activity Type Summary ===")
  print(table(types))

  invisible(raw_data)
}

#' Fetch raw RideWithGPS data and save locally
#'
#' Fetches all trips from RideWithGPS API and saves the raw JSON response
#' for later processing.
#'
#' @param output_file Path to save the raw data (default: "data-raw/rwgps_raw_full.rds")
#' @param limit Number of trips per page (default 50)
#' @return Invisible list of raw trip data
#' @export
fetch_raw_rwgps <- function(output_file = "data-raw/rwgps_raw_full.rds", limit = 50) {
  message("Fetching raw RideWithGPS data...")

  # Check if credentials are configured
  if (!has_rwgps_credentials()) {
    message("RideWithGPS credentials not configured.")
    message("Run: keyring::key_set('rwgps_client_id') and keyring::key_set('rwgps_client_secret')")
    return(invisible(list()))
  }

  # Make sure we're authenticated
  if (!is_rwgps_authenticated()) {
    message("Not authenticated. Running rwgps_auth()...")
    rwgps_auth()
  }

  # Get user ID first
  message("Getting user ID...")
  req <- rwgps_request("users/current.json")
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      message("Error fetching user info: ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp)) {
    message("Failed to authenticate with RideWithGPS")
    return(invisible(list()))
  }

  user_info <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  user_id <- user_info$user$id
  message("User ID: ", user_id)

  all_pages <- list()
  offset <- 0

  repeat {
    message("Fetching trips with offset ", offset, "...")

    req <- rwgps_request(paste0("users/", user_id, "/trips.json")) |>
      httr2::req_url_query(limit = limit, offset = offset)

    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) {
        message("Error: ", e$message)
        return(NULL)
      }
    )

    if (is.null(resp)) {
      break
    }

    # Get raw JSON as list
    result <- httr2::resp_body_json(resp, simplifyVector = FALSE)

    if (is.null(result$results) || length(result$results) == 0) {
      message("No more trips")
      break
    }

    trips <- result$results
    message("  Got ", length(trips), " trips")
    all_pages[[length(all_pages) + 1]] <- trips

    if (length(trips) < limit) {
      message("Reached end")
      break
    }

    offset <- offset + limit
    Sys.sleep(0.5)  # Be nice to the API
  }

  # Flatten into single list
  all_trips <- unlist(all_pages, recursive = FALSE)

  message("\nFetched ", length(all_trips), " total trips")

  # Save as RDS
  saveRDS(all_trips, output_file)
  message("Saved to: ", output_file)

  # Print structure info
  if (length(all_trips) > 0) {
    message("\nFields available in trips:")
    message(paste(names(all_trips[[1]]), collapse = ", "))
  }

  invisible(all_trips)
}

#' Export sample trips to JSON
#'
#' Exports a sample of trips to JSON for inspection
#'
#' @param input_file Path to the saved raw data file (default: "data-raw/rwgps_raw_full.rds")
#' @param output_file Path to save the sample JSON (default: auto-generated from input_file)
#' @param n_samples Number of trips to include in sample (default: 3)
#' @return Invisible sample data
#' @export
export_sample_rwgps <- function(input_file = "data-raw/rwgps_raw_full.rds",
                                 output_file = NULL,
                                 n_samples = 3) {
  raw_data <- load_raw_rwgps(input_file)

  if (is.null(output_file)) {
    output_file <- sub("\\.rds$", "_sample.json", input_file)
  }

  sample_data <- raw_data[1:min(n_samples, length(raw_data))]

  jsonlite::write_json(
    sample_data,
    output_file,
    pretty = TRUE,
    auto_unbox = TRUE
  )

  message("Saved sample (", length(sample_data), " trips) to: ", output_file)

  invisible(sample_data)
}

#' Load saved raw RideWithGPS data
#'
#' @param input_file Path to the saved raw data file
#' @return List of raw trip data
#' @export
load_raw_rwgps <- function(input_file = "data-raw/rwgps_raw_full.rds") {
  if (!file.exists(input_file)) {
    stop("Raw data file not found: ", input_file, "\n",
         "Run fetch_raw_rwgps() first!")
  }

  raw_data <- readRDS(input_file)
  message("Loaded ", length(raw_data), " trips from ", input_file)

  return(raw_data)
}

#' Inspect raw RideWithGPS data structure
#'
#' @param raw_data List of raw trips (from fetch_raw_rwgps or load_raw_rwgps)
#' @export
inspect_raw_rwgps <- function(raw_data = NULL) {
  if (is.null(raw_data)) {
    raw_data <- load_raw_rwgps()
  }

  message("\n=== Raw RideWithGPS Data Inspection ===")
  message("Total trips: ", length(raw_data))

  if (length(raw_data) == 0) {
    message("No trips to inspect")
    return(invisible(NULL))
  }

  # Fields available
  message("\n=== Available Fields ===")
  print(names(raw_data[[1]]))

  # Sample values from first trip
  message("\n=== Sample Values (First Trip) ===")
  first <- raw_data[[1]]
  cat("ID:", first$id, "\n")
  cat("Name:", first$name, "\n")
  cat("Type:", first$type, "\n")
  cat("Distance:", first$distance, "km\n")
  if (!is.null(first$duration)) {
    cat("Duration:", first$duration, "seconds\n")
  }
  cat("Departed at:", first$departed_at, "\n")

  # Trip type summary
  types <- sapply(raw_data, function(x) x$type %||% "unknown")
  message("\n=== Trip Type Summary ===")
  print(table(types))

  invisible(raw_data)
}
