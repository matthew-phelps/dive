#' Strava OAuth2 Authentication
#'
#' This module handles OAuth2 authentication with the Strava API.
#' Tokens are stored securely in the system keyring.

#' Get Strava OAuth client
#'
#' Creates an OAuth client for Strava API authentication
#'
#' @return httr2 oauth_client object
#' @noRd
get_strava_client <- function() {
  # Try to get credentials from keyring
  client_id <- tryCatch(
    keyring::key_get("strava_client_id"),
    error = function(e) NULL
  )

  client_secret <- tryCatch(
    keyring::key_get("strava_client_secret"),
    error = function(e) NULL
  )

  if (is.null(client_id) || is.null(client_secret)) {
    stop(
      "Strava API credentials not found in keyring.\n",
      "Please set them with:\n",
      "  keyring::key_set('strava_client_id')\n",
      "  keyring::key_set('strava_client_secret')",
      call. = FALSE
    )
  }

  client <- httr2::oauth_client(
    id = client_id,
    secret = client_secret,
    token_url = "https://www.strava.com/oauth/token",
    name = "dive_strava"
  )

  return(client)
}

#' Get path to Strava token cache
#'
#' @return Character string with path to token cache file
#' @noRd
get_strava_token_path <- function() {
  file.path(get_data_dir(), ".strava_token.rds")
}

#' Authenticate with Strava
#'
#' Performs OAuth2 authentication flow with Strava.
#' Opens a browser for user authorization, then caches the token.
#'
#' @param force Logical. If TRUE, force re-authentication even if token exists
#' @return httr2 oauth_token object
#' @export
strava_auth <- function(force = FALSE) {
  init_storage()

  client <- get_strava_client()
  token_path <- get_strava_token_path()

  # Check if we have a cached token
  if (!force && file.exists(token_path)) {
    message("Loading cached Strava token...")
    token <- tryCatch(
      readRDS(token_path),
      error = function(e) {
        message("Failed to load cached token: ", e$message)
        NULL
      }
    )

    if (!is.null(token) && !is.null(token$access_token)) {
      message("Using existing Strava authentication")
      return(token)
    }
  }

  # Perform OAuth flow
  message("Starting Strava OAuth flow...")
  message("A browser window will open for authentication.")

  # Check if SSL workaround is needed
  if (Sys.getenv("CURL_CA_BUNDLE") != "") {
    message("\nNote: If you encounter SSL errors, run fix_ssl_macos() first\n")
  }

  token <- httr2::oauth_flow_auth_code(
    client = client,
    auth_url = "https://www.strava.com/oauth/authorize",
    scope = "activity:read_all",
    redirect_uri = "http://localhost:1410/"
  )

  # Cache the token
  saveRDS(token, token_path)
  message("Strava authentication successful!")

  return(token)
}

#' Get cached Strava token
#'
#' Returns the cached Strava token if it exists and is valid,
#' otherwise prompts for authentication.
#'
#' @return httr2 oauth_token object
#' @noRd
get_strava_token <- function() {
  token_path <- get_strava_token_path()

  if (file.exists(token_path)) {
    token <- tryCatch(
      readRDS(token_path),
      error = function(e) NULL
    )

    # If we have a token, return it (httr2 will refresh if needed)
    if (!is.null(token) && !is.null(token$access_token)) {
      return(token)
    }
  }

  # Need to authenticate
  return(strava_auth())
}

#' Check if Strava is authenticated
#'
#' @return Logical indicating if valid Strava token exists
#' @export
is_strava_authenticated <- function() {
  token_path <- get_strava_token_path()

  if (!file.exists(token_path)) {
    return(FALSE)
  }

  token <- tryCatch(
    readRDS(token_path),
    error = function(e) NULL
  )

  if (is.null(token)) {
    return(FALSE)
  }

  # Check if token has access_token
  return(!is.null(token$access_token))
}

#' Revoke Strava authentication
#'
#' Deletes the cached Strava token. User will need to re-authenticate.
#'
#' @return Invisible TRUE
#' @export
strava_deauth <- function() {
  token_path <- get_strava_token_path()

  if (file.exists(token_path)) {
    file.remove(token_path)
    message("Strava authentication revoked")
  } else {
    message("No Strava authentication found")
  }

  invisible(TRUE)
}

#' Create authenticated Strava API request
#'
#' Creates an httr2 request object with Strava authentication
#'
#' @param endpoint API endpoint path (without base URL)
#' @return httr2 request object
#' @noRd
strava_request <- function(endpoint) {
  token <- get_strava_token_with_refresh()

  req <- httr2::request("https://www.strava.com/api/v3") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_auth_bearer_token(token$access_token) |>
    httr2::req_user_agent("dive R package (https://github.com/yourusername/dive)") |>
    httr2::req_retry(max_tries = 3)

  return(req)
}

#' Get Strava token with automatic refresh
#'
#' @return httr2 oauth_token object
#' @noRd
get_strava_token_with_refresh <- function() {
  token_path <- get_strava_token_path()

  if (!file.exists(token_path)) {
    return(strava_auth())
  }

  token <- readRDS(token_path)

  # Check if token is expired (with 5 minute buffer)
  if (!is.null(token$expires_at)) {
    now <- as.numeric(Sys.time())
    if (now >= (token$expires_at - 300)) {
      message("Access token expired, refreshing...")
      token <- refresh_strava_token(token)
    }
  }

  return(token)
}

#' Refresh Strava access token
#'
#' @param token Current token with refresh_token
#' @return Updated token
#' @noRd
refresh_strava_token <- function(token) {
  client <- get_strava_client()

  if (is.null(token$refresh_token)) {
    stop("No refresh token available. Please re-authenticate with strava_auth()")
  }

  req <- httr2::request("https://www.strava.com/oauth/token") |>
    httr2::req_method("POST") |>
    httr2::req_body_form(
      client_id = client$id,
      client_secret = client$secret,
      grant_type = "refresh_token",
      refresh_token = token$refresh_token
    )

  resp <- httr2::req_perform(req)
  new_token_data <- httr2::resp_body_json(resp)

  # Update token with new values
  token$access_token <- new_token_data$access_token
  token$expires_at <- as.numeric(Sys.time()) + new_token_data$expires_in
  token$refresh_token <- new_token_data$refresh_token

  # Save updated token
  token_path <- get_strava_token_path()
  saveRDS(token, token_path)

  message("Token refreshed successfully")
  return(token)
}
