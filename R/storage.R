#' Get the dive data directory path
#'
#' @return Character string with path to data directory
#' @noRd
get_data_dir <- function() {
  dir_path <- path.expand("./data-raw")
  return(dir_path)
}

#' Get the cache directory path
#'
#' @return Character string with path to cache directory
#' @noRd
get_cache_dir <- function() {
  cache_path <- file.path(get_data_dir(), "cache")
  return(cache_path)
}

#' Initialize the dive data directory structure
#'
#' Creates ~/.dive/ and ~/.dive/cache/ directories if they don't exist.
#' This function is called automatically when the app starts.
#'
#' @return Invisible TRUE if successful
#' @export
init_storage <- function() {
  data_dir <- get_data_dir()
  cache_dir <- get_cache_dir()

  # Create main data directory
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
    message("Created dive data directory: ", data_dir)
  }

  # Create cache subdirectory
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    message("Created cache directory: ", cache_dir)
  }

  # Create initial metadata file if it doesn't exist
  metadata_path <- file.path(data_dir, "metadata.qs")
  if (!file.exists(metadata_path)) {
    initial_metadata <- list(
      last_sync_strava = NULL,
      last_sync_rwgps = NULL,
      created_at = Sys.time()
    )
    qs::qsave(initial_metadata, metadata_path, preset = "fast")
    message("Initialized metadata file")
  }

  invisible(TRUE)
}

#' Save raw data from a source
#'
#' @param data Data frame or data.table to save
#' @param source Character string: "strava" or "rwgps"
#' @return Invisible path to saved file
#' @noRd
save_raw_data <- function(data, source = c("strava", "rwgps")) {
  source <- match.arg(source)
  init_storage()

  filename <- switch(source,
    strava = "raw_strava.qs",
    rwgps = "raw_ridewithgps.qs"
  )

  filepath <- file.path(get_data_dir(), filename)
  qs::qsave(data, filepath, preset = "balanced", nthreads = 2)

  message("Saved raw ", source, " data: ", nrow(data), " activities")
  invisible(filepath)
}

#' Load raw data from a source
#'
#' @param source Character string: "strava" or "rwgps"
#' @return Data frame/data.table if file exists, NULL otherwise
#' @noRd
load_raw_data <- function(source = c("strava", "rwgps")) {
  source <- match.arg(source)

  filename <- switch(source,
    strava = "raw_strava.rds",
    rwgps = "raw_rwgps.rds"
  )

  filepath <- file.path(get_data_dir(), filename)

  if (!file.exists(filepath)) {
    return(NULL)
  }

  data <- readRDS(filepath)
  return(data)
}

#' Save cleaned/deduplicated activities
#'
#' @param data Data.table of cleaned activities
#' @return Invisible path to saved file
#' @noRd
save_cleaned_data <- function(data) {
  init_storage()
  filepath <- file.path(get_data_dir(), "activities_cleaned.qs")
  qs::qsave(data, filepath, preset = "balanced", nthreads = 2)

  message("Saved cleaned data: ", nrow(data), " activities")
  invisible(filepath)
}

#' Load cleaned/deduplicated activities
#'
#' @return Data.table if file exists, NULL otherwise
#' @noRd
load_cleaned_data <- function() {
  filepath <- file.path(get_data_dir(), "activities_cleaned.qs")

  if (!file.exists(filepath)) {
    return(NULL)
  }

  data <- qs::qread(filepath, nthreads = 2)
  return(data)
}

#' Save deduplication cache
#'
#' @param dedup_info List with deduplication decisions
#' @return Invisible path to saved file
#' @noRd
save_dedup_cache <- function(dedup_info) {
  init_storage()
  filepath <- file.path(get_data_dir(), "dedup_cache.qs")
  qs::qsave(dedup_info, filepath, preset = "fast")
  invisible(filepath)
}

#' Load deduplication cache
#'
#' @return List if file exists, NULL otherwise
#' @noRd
load_dedup_cache <- function() {
  filepath <- file.path(get_data_dir(), "dedup_cache.qs")

  if (!file.exists(filepath)) {
    return(NULL)
  }

  dedup_info <- qs::qread(filepath)
  return(dedup_info)
}

#' Save metadata (sync timestamps, etc.)
#'
#' @param metadata List with metadata information
#' @return Invisible path to saved file
#' @noRd
save_metadata <- function(metadata) {
  init_storage()
  filepath <- file.path(get_data_dir(), "metadata.qs")
  qs::qsave(metadata, filepath, preset = "fast")
  invisible(filepath)
}

#' Load metadata
#'
#' @return List with metadata
#' @export
load_metadata <- function() {
  init_storage()  # Ensures metadata exists
  filepath <- file.path(get_data_dir(), "metadata.qs")
  metadata <- qs::qread(filepath)
  return(metadata)
}

#' Save cached summary data
#'
#' @param data Summary data to cache
#' @param cache_name Name of cache file (without .qs extension)
#' @return Invisible path to saved file
#' @noRd
save_cache <- function(data, cache_name) {
  init_storage()
  filepath <- file.path(get_cache_dir(), paste0(cache_name, ".qs"))
  qs::qsave(data, filepath, preset = "fast", nthreads = 2)
  invisible(filepath)
}

#' Load cached summary data
#'
#' @param cache_name Name of cache file (without .qs extension)
#' @return Cached data if file exists, NULL otherwise
#' @noRd
load_cache <- function(cache_name) {
  filepath <- file.path(get_cache_dir(), paste0(cache_name, ".qs"))

  if (!file.exists(filepath)) {
    return(NULL)
  }

  data <- qs::qread(filepath, nthreads = 2)
  return(data)
}

#' Check if raw data exists for a source
#'
#' @param source Character string: "strava" or "rwgps"
#' @return Logical
#' @noRd
has_raw_data <- function(source = c("strava", "rwgps")) {
  source <- match.arg(source)

  filename <- switch(source,
    strava = "raw_strava.qs",
    rwgps = "raw_ridewithgps.qs"
  )

  filepath <- file.path(get_data_dir(), filename)
  return(file.exists(filepath))
}

#' Check if cleaned data exists
#'
#' @return Logical
#' @noRd
has_cleaned_data <- function() {
  filepath <- file.path(get_data_dir(), "activities_cleaned.qs")
  return(file.exists(filepath))
}

#' Get data directory info for debugging
#'
#' @return List with directory information
#' @export
get_storage_info <- function() {
  data_dir <- get_data_dir()
  cache_dir <- get_cache_dir()

  info <- list(
    data_dir = data_dir,
    cache_dir = cache_dir,
    data_dir_exists = dir.exists(data_dir),
    cache_dir_exists = dir.exists(cache_dir),
    files = if (dir.exists(data_dir)) list.files(data_dir, recursive = TRUE) else character(0)
  )

  return(info)
}
