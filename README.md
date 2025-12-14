# dive - Activity Tracking Dashboard

A Shiny application for tracking, analyzing, and visualizing running and cycling activities from Strava and RideWithGPS.

## Features

- **Multi-source sync**: Fetch activities from both Strava and RideWithGPS
- **Smart deduplication**: Automatically detect and handle duplicate activities across sources
- **Summary statistics**: View weekly, monthly, and yearly activity summaries
- **Distance trends**: Visualize 12-month rolling and calendar year cumulative distances
- **Fast data processing**: Uses `data.table` and `qs` for high-performance operations
- **Secure credentials**: OAuth tokens stored securely with `keyring`

## Installation

### Prerequisites

- R >= 4.1.0
- devtools package

### Install from source

```r
# Install devtools if needed
install.packages("devtools")

# Install dive package
devtools::install()
```

## Setup

### 1. API Credentials Setup

#### Strava API

You already have Strava credentials. Store them securely:

```r
# Set Strava credentials in keyring
keyring::key_set("strava_client_id")
keyring::key_set("strava_client_secret")
```

#### RideWithGPS API

To use RideWithGPS, you need to register an application:

1. **Create a RideWithGPS account** at https://ridewithgps.com (if you don't have one)

2. **Register your application**:
   - Contact RideWithGPS support or check their API documentation at https://ridewithgps.com/api
   - Note: RideWithGPS may require email approval for API access
   - You'll need to provide:
     - Application name (e.g., "dive - Personal Activity Tracker")
     - Redirect URI (e.g., `http://localhost:1410/`)
     - Description of your use case

3. **Store credentials** once you receive them:
   ```r
   keyring::key_set("rwgps_client_id")
   keyring::key_set("rwgps_client_secret")
   ```

### 2. Data Directory

The app stores data in `~/.dive/` by default. This directory is created automatically on first run.

## Usage

### Launch the app

```r
library(dive)
run_app()
```

### First-time setup

1. Launch the app with `dive::run_app()`
2. Authenticate with Strava (follow OAuth flow in your browser)
3. Authenticate with RideWithGPS (if configured)
4. Click "Sync Activities" to fetch your activity data
5. Explore your stats and visualizations!

### Data sync

- Click the **Sync Activities** button in the app to fetch new activities
- Only new activities since the last sync are fetched (incremental updates)
- Deduplication runs automatically after each sync

## Architecture

### Data Storage

All data is stored locally in `~/.dive/`:

```
~/.dive/
├── raw_strava.qs              # Raw Strava metadata (never modified)
├── raw_ridewithgps.qs         # Raw RideWithGPS metadata (never modified)
├── activities_cleaned.qs      # Cleaned and deduplicated activities
├── metadata.qs                # Last sync timestamps
├── dedup_cache.qs            # Deduplication decisions
└── cache/
    ├── summary_weekly.qs      # Pre-computed weekly summaries
    ├── summary_monthly.qs     # Pre-computed monthly summaries
    ├── summary_yearly.qs      # Pre-computed yearly summaries
    └── rolling_12month.qs     # Rolling 12-month calculations
```

### Data Processing Pipeline

1. **Fetch**: Download activity metadata from APIs
2. **Clean**: Standardize both sources to common schema
3. **Deduplicate**: Match activities across sources using date/time/distance
4. **Summarize**: Compute statistics with `data.table`, cache with `qs`
5. **Visualize**: Display in Shiny UI

### Deduplication Logic

Activities are considered duplicates if they match on:
- Same date (within same calendar day)
- Start time within ±15 minutes
- Distance within ±5%

When duplicates are found:
1. Keep the activity with the longest distance
2. If distances are similar (within 10-15%), prefer the one with heart rate data
3. Store deduplication decisions for transparency

## Development

### Package structure

This package uses the `{golem}` framework for Shiny app development.

```
dive/
├── R/                    # R source code
│   ├── app_*.R          # Main app files (UI, server, config)
│   ├── auth_*.R         # OAuth modules
│   ├── api_*.R          # API client functions
│   ├── data_*.R         # Data processing functions
│   ├── mod_*.R          # Shiny modules
│   ├── storage.R        # Data storage functions
│   └── sync.R           # Sync orchestration
├── inst/                # Installed files
│   ├── app/www/         # Static web assets
│   └── golem-config.yml # App configuration
├── man/                 # Documentation (generated)
└── dev/                 # Development scripts
```

### Testing

```r
# Load package for development
devtools::load_all()

# Generate documentation
devtools::document()

# Run tests
devtools::test()

# Check package
devtools::check()
```

## License

Apache License 2.0 - see [LICENSE](LICENSE) file for details.

## Roadmap

### Phase 1 (Current)
- [x] Package setup with golem
- [ ] API authentication (Strava & RideWithGPS)
- [ ] Activity metadata fetching
- [ ] Data cleaning and deduplication
- [ ] Summary statistics
- [ ] Distance visualizations

### Phase 2 (Future)
- [ ] GPS data fetching and storage
- [ ] Heat map visualization with Leaflet
- [ ] Activity detail views
- [ ] Advanced filtering and date pickers
- [ ] Export functionality

## Troubleshooting

### OAuth issues

If you encounter OAuth errors:
1. Check that your credentials are stored: `keyring::key_list()`
2. Delete cached tokens and re-authenticate
3. Verify redirect URIs match your app registration

### Data sync issues

If sync fails:
1. Check API rate limits (Strava: 100 requests/15min, 1000 requests/day)
2. Check internet connection
3. Review error messages in the R console
4. Check `~/.dive/metadata.qs` for last sync timestamps

### RideWithGPS API access

RideWithGPS API access may require approval. If you haven't received credentials:
- The app will work with Strava only
- You can add RideWithGPS support later once you receive API access
