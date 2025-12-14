#' Activity Deduplication
#'
#' Functions to detect and handle duplicate activities across data sources

#' Find potential duplicate activities
#'
#' Identifies activities that are likely duplicates based on:
#' - Same date
#' - Start time within ±15 minutes
#' - Distance within ±5%
#'
#' @param activities data.table with activity data
#' @return data.table with potential duplicate pairs
#' @noRd
find_duplicate_pairs <- function(activities) {
  if (nrow(activities) < 2) {
    return(data.table::data.table())
  }

  dt <- data.table::as.data.table(activities)

  # Select only needed columns for the join
  dt_join <- dt[, .(activity_id, source, date, activity_type, start_time,
                    start_datetime, distance, distance_km, has_heartrate)]

  # Create copies for self-join
  dt1 <- data.table::copy(dt_join)
  dt2 <- data.table::copy(dt_join)

  # Self-join on same date and activity type
  pairs <- merge(
    dt1, dt2,
    by = c("date", "activity_type"),
    allow.cartesian = TRUE,
    suffixes = c("_1", "_2")
  )

  # Filter to only cross-source pairs (not same activity)
  pairs <- pairs[activity_id_1 != activity_id_2 & source_1 != source_2]

  if (nrow(pairs) == 0) {
    return(data.table::data.table())
  }

  # Calculate time difference in minutes
  pairs[, time_diff_min := abs(
    as.numeric(difftime(start_datetime_1, start_datetime_2, units = "mins"))
  )]

  # Calculate distance difference percentage
  pairs[, distance_diff_pct := abs(
    (distance_1 - distance_2) / ((distance_1 + distance_2) / 2) * 100
  )]

  # Filter to matches within thresholds
  matches <- pairs[
    time_diff_min <= 15 &
      distance_diff_pct <= 5
  ]

  if (nrow(matches) == 0) {
    return(data.table::data.table())
  }

  # Select relevant columns and rename for clarity
  result <- matches[, .(
    id1 = activity_id_1,
    source1 = source_1,
    date1 = date,
    time1 = start_time_1,
    distance1_km = distance_km_1,
    has_hr1 = has_heartrate_1,
    id2 = activity_id_2,
    source2 = source_2,
    date2 = date,
    time2 = start_time_2,
    distance2_km = distance_km_2,
    has_hr2 = has_heartrate_2,
    time_diff_min,
    distance_diff_pct
  )]

  # Remove duplicate pairs (A-B is same as B-A)
  result[, pair_key := paste(pmin(id1, id2), pmax(id1, id2), sep = "_")]
  result <- result[!duplicated(pair_key)]
  result[, pair_key := NULL]

  return(result)
}

#' Decide which activity to keep in duplicate pairs
#'
#' Priority rules:
#' 1. Keep activity with longest distance
#' 2. If distances similar (within 10-15%), keep one with heart rate data
#' 3. If both have or both lack HR data, keep the longer one
#'
#' @param pairs data.table with duplicate pairs
#' @return data.table with keep/discard decisions
#' @noRd
decide_duplicates <- function(pairs) {
  if (nrow(pairs) == 0) {
    return(data.table::data.table())
  }

  dt <- data.table::copy(pairs)

  # Determine which to keep
  dt[, keep_id := data.table::fcase(
    # If distance1 is more than 10% longer, keep it
    (distance1_km - distance2_km) / distance2_km > 0.10, id1,
    # If distance2 is more than 10% longer, keep it
    (distance2_km - distance1_km) / distance1_km > 0.10, id2,
    # Distances are similar - prefer one with heart rate
    has_hr1 & !has_hr2, id1,
    has_hr2 & !has_hr1, id2,
    # Both have or both lack HR - keep longer
    distance1_km >= distance2_km, id1,
    default = id2
  )]

  dt[, discard_id := ifelse(keep_id == id1, id2, id1)]
  dt[, discard_source := ifelse(keep_id == id1, source2, source1)]

  # Create summary
  decisions <- dt[, .(
    keep_id,
    discard_id,
    discard_source,
    reason = data.table::fcase(
      abs(distance1_km - distance2_km) / pmin(distance1_km, distance2_km) > 0.10,
      "distance_difference",
      (has_hr1 & !has_hr2) | (has_hr2 & !has_hr1),
      "heartrate_preference",
      default = "longer_distance"
    ),
    time_diff_min,
    distance_diff_pct,
    distance1_km,
    distance2_km
  )]

  return(decisions)
}

#' Apply deduplication to activities
#'
#' Marks duplicate activities in the dataset
#'
#' @param activities data.table with activity data
#' @return data.table with is_duplicate and duplicate_of columns updated
#' @export
deduplicate_activities <- function(activities) {
  message("Deduplicating activities...")

  if (nrow(activities) < 2) {
    message("Not enough activities to deduplicate")
    return(activities)
  }

  dt <- data.table::copy(activities)

  # Find potential duplicates
  pairs <- find_duplicate_pairs(dt)

  if (nrow(pairs) == 0) {
    message("No duplicates found")
    return(dt)
  }

  message("Found ", nrow(pairs), " potential duplicate pairs")

  # Decide which to keep
  decisions <- decide_duplicates(pairs)

  message("Decided to discard ", length(unique(decisions$discard_id)), " duplicate activities")

  # Mark duplicates in the dataset
  dt[activity_id %in% decisions$discard_id, is_duplicate := TRUE]

  # Add reference to the kept activity
  for (i in seq_len(nrow(decisions))) {
    dt[
      activity_id == decisions$discard_id[i],
      duplicate_of := decisions$keep_id[i]
    ]
  }

  # Summary statistics
  n_duplicates <- sum(dt$is_duplicate)
  message("Marked ", n_duplicates, " activities as duplicates")

  # Save deduplication decisions
  dedup_info <- list(
    decisions = decisions,
    timestamp = Sys.time(),
    total_duplicates = n_duplicates
  )

  save_dedup_cache(dedup_info)

  return(dt)
}

#' Get non-duplicate activities
#'
#' Returns only activities that are not marked as duplicates
#'
#' @param activities data.table with activity data
#' @return data.table with non-duplicate activities only
#' @export
get_unique_activities <- function(activities) {
  if (nrow(activities) == 0) {
    return(activities)
  }

  dt <- activities[is_duplicate == FALSE]

  message("Returning ", nrow(dt), " unique activities")

  return(dt)
}

#' Get deduplication report
#'
#' Returns a summary of deduplication decisions
#'
#' @return List with deduplication statistics
#' @export
get_deduplication_report <- function() {
  dedup_cache <- load_dedup_cache()

  if (is.null(dedup_cache)) {
    return(list(
      status = "No deduplication has been performed",
      total_duplicates = 0,
      decisions = data.table::data.table()
    ))
  }

  report <- list(
    timestamp = dedup_cache$timestamp,
    total_duplicates = dedup_cache$total_duplicates,
    decisions = dedup_cache$decisions
  )

  # Summary by source
  if (nrow(dedup_cache$decisions) > 0) {
    by_source <- dedup_cache$decisions[, .N, by = discard_source]
    report$by_source <- by_source

    by_reason <- dedup_cache$decisions[, .N, by = reason]
    report$by_reason <- by_reason
  }

  return(report)
}
