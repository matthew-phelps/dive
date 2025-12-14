# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`dive` is an R package built with the `{golem}` framework for creating a Shiny application that tracks and visualizes running/cycling activities from Strava and RideWithGPS. It handles OAuth authentication, incremental data sync, deduplication, and high-performance data processing with `data.table` and `qs`.

## Development Commands

### Package Development
```r
# Load package for development (use this instead of library(dive))
devtools::load_all()

# Generate documentation from roxygen2 comments
devtools::document()

# Check package (runs R CMD check)
devtools::check()

# Install package locally
devtools::install()
```

### Running the App
```r
# Launch Shiny app in development mode
devtools::load_all()
dive::run_app()
```

### Data Fetching (Development/Testing)
```r
# Fetch raw data to data-raw/ for development
devtools::load_all()
dive::fetch_raw_strava()   # Saves to data-raw/strava_raw_full.rds
dive::fetch_raw_rwgps()    # Saves to data-raw/rwgps_raw_full.rds

# Export sample JSON for inspection
dive::export_sample_strava()  # Creates data-raw/strava_raw_full_sample.json
dive::export_sample_rwgps()   # Creates data-raw/rwgps_raw_full_sample.json
```

## Architecture

### Authentication System

Both Strava and RideWithGPS use OAuth2 but have **critical differences**:

**Strava:**
- Standard OAuth2 flow using `httr2::oauth_flow_auth_code()`
- Form-encoded token exchange (standard)
- Credentials: `strava_client_id`, `strava_client_secret` in keyring
- Tokens cached in `~/.dive/.strava_token.rds`

**RideWithGPS:**
- **Non-standard OAuth2**: requires JSON body for token exchange (not form-encoded)
- Must use manual token exchange with `httr2::req_body_json()`
- Credentials stored as `rwgps_api_key`, `rwgps_api_secret` in keyring (RideWithGPS calls OAuth credentials "API key/secret" in their UI)
- Tokens cached in `~/.dive/.rwgps_token.rds`
- **API quirk**: Some endpoints require `.json` suffix (e.g., `v1/users/current.json`, `v1/trips.json`) despite OpenAPI spec inconsistencies

**SSL on macOS:**
- RideWithGPS API calls may fail if `CURL_CA_BUNDLE` environment variable points to outdated/invalid cert bundle
- Use `dive::fix_ssl_macos()` to unset cert bundle env vars and use macOS SecureTransport
- This is required before authenticating with RideWithGPS on macOS

### Data Storage Architecture

All data lives in `~/.dive/` (created by `init_storage()`):

```
~/.dive/
├── .strava_token.rds           # OAuth token (managed by httr2)
├── .rwgps_token.rds            # OAuth token (manual management)
├── raw_strava.qs               # Raw API responses (append-only)
├── raw_ridewithgps.qs          # Raw API responses (append-only)
├── activities_cleaned.qs       # Cleaned + deduplicated activities
├── metadata.qs                 # Last sync timestamps
├── dedup_cache.qs              # Deduplication decisions log
└── cache/
    ├── summary_weekly.qs
    ├── summary_monthly.qs
    ├── summary_yearly.qs
    └── rolling_12month.qs
```

**Storage philosophy:**
- Raw data is **never modified** after initial save (append-only)
- All transformations happen at read-time or in separate cleaned files
- Use `qs` format for fast serialization (faster than RDS, smaller than CSV)
- Cache expensive computations in `cache/` directory

### Data Processing Pipeline

The pipeline follows these stages:

1. **Fetch** (`auth_*.R`, `api_*.R`): OAuth + API calls to get activity metadata
2. **Harmonize** (`data_cleaning.R`): Convert API-specific formats to canonical schema
3. **Combine** (`data_cleaning.R`): Merge harmonized data from both sources
4. **Clean** (`data_cleaning.R`): Validate, remove invalid data, add calculated fields
5. **Deduplicate** (`data_deduplication.R`): Match activities across sources using date/time/distance heuristics
6. **Cache** (`storage.R`): Save computed summaries with `qs` for fast reload

**Key functions:**
- `harmonize_strava_raw()`: Converts Strava list (from API) → canonical schema
- `harmonize_rwgps_raw()`: Converts RWGPS list (from API) → canonical schema
- `combine_activities()`: Merges harmonized data.tables with `rbindlist()`
- `clean_activities()`: Post-merge validation and calculated fields (adds pace for running)
- `process_activities()`: High-level orchestrator of the entire pipeline

### Canonical Schema

All activities are harmonized to this standardized schema (16 fields):

| Field | Type | Unit | Source Mapping |
|-------|------|------|----------------|
| `activity_id` | character | - | Strava: `id`, RWGPS: `id` |
| `source` | character | - | "strava" or "rwgps" |
| `activity_type` | character | - | Standardized: "running", "cycling", "other" |
| `date` | Date | - | Calendar date of activity |
| `start_time` | character | HH:MM:SS | Time of day (from UTC datetime) |
| `start_datetime` | POSIXct | UTC | Strava: `start_date`, RWGPS: `departed_at` |
| `distance` | numeric | meters | Strava: `distance`, RWGPS: `distance` |
| `duration` | numeric | seconds | Strava: `moving_time`, RWGPS: `moving_time` (fallback: `duration`) |
| `elevation_gain` | numeric | meters | Strava: `total_elevation_gain`, RWGPS: `elevation_gain` |
| `has_heartrate` | logical | - | Strava: `has_heartrate`, RWGPS: inferred from `avg_hr` |
| `avg_heartrate` | numeric | bpm | Strava: `average_heartrate`, RWGPS: `avg_hr` |
| `max_heartrate` | numeric | bpm | Strava: `max_heartrate`, RWGPS: `max_hr` |
| `avg_speed` | numeric | km/h | Strava: `average_speed` (m/s → km/h), RWGPS: `avg_speed` |
| `max_speed` | numeric | km/h | Strava: `max_speed` (m/s → km/h), RWGPS: `max_speed` |
| `name` | character | - | Activity name/title |
| `raw_type` | character | - | Original type from API for debugging |

**Activity Type Mapping:**
- Strava: "Run", "VirtualRun" → "running"; "Ride", "VirtualRide" → "cycling"
- RWGPS: "running", "cycling:*" → standardized values

**Unit Consistency:**
- Both APIs use meters for distance ✓
- Both APIs use seconds for duration ✓
- Strava uses `moving_time`, RWGPS provides both `moving_time` (preferred) and `duration`
- Strava provides speed in m/s (converted to km/h), RWGPS provides speed in km/h ✓

### Deduplication Logic

Activities are considered duplicates if ALL match:
- Same date (calendar day)
- Start time within ±15 minutes
- Distance within ±5%

When duplicates found:
1. Keep activity with longest distance
2. If distances similar (within 10-15%), prefer one with heart rate data
3. Log all decisions in `dedup_cache.qs` for transparency

### API Client Architecture

**Function naming convention:**
- `auth_*.R`: OAuth flow functions (e.g., `strava_auth()`, `rwgps_auth()`)
- `api_*.R`: API request builders (e.g., `strava_request()`, `rwgps_request()`)
- `fetch_*.R`: High-level data fetching orchestration

**Request builders:**
- `strava_request(endpoint)` - returns httr2 request with auth + retries
- `rwgps_request(endpoint)` - same pattern, handles multi-segment paths with `req_url_path()`

**Important:** RideWithGPS `rwgps_request()` uses `httr2::req_url_path()` to set full path at once (e.g., `/api/v1/trips.json`) rather than appending segments, avoiding path parsing issues with slashes.

### Credential Management

Credentials stored in system keyring (never in code/config):

```r
# Strava setup
keyring::key_set("strava_client_id")
keyring::key_set("strava_client_secret")

# RideWithGPS setup
keyring::key_set("rwgps_api_key")      # What RWGPS calls "API key"
keyring::key_set("rwgps_api_secret")   # What RWGPS calls "API secret"
```

OAuth tokens cached separately in `~/.dive/` (see Data Storage above).

## Common Issues

### RideWithGPS 404 errors
- Ensure endpoints include version and correct `.json` suffixes: `v1/users/current.json`, `v1/trips.json`
- Check `rwgps_request()` is using `req_url_path()` not `req_url_path_append()`

### SSL certificate errors on macOS
- Run `dive::fix_ssl_macos()` before authenticating with RideWithGPS
- This unsets `CURL_CA_BUNDLE` to use macOS system keychain

### Data not syncing
- Check `dive::load_metadata()` for last sync timestamps
- Verify credentials with `keyring::key_list()`
- Test auth with `dive::strava_auth()` or `dive::rwgps_auth()`

## Key R Package Patterns

This is a `{golem}` Shiny app package:
- `R/app_*.R` files contain app-level code (UI, server, config)
- `R/mod_*.R` files would contain Shiny modules (none exist yet)
- Use `golem::add_*()` functions to add dependencies, modules, etc.
- Configuration in `inst/golem-config.yml`

Use `data.table` for all data manipulation (faster than dplyr for this use case).

## git

- Do not add claude citation to git messages
