#' RideWithGPS OAuth2 Authentication
#'
#' This module handles OAuth2 authentication with the RideWithGPS API.
#' Note: RideWithGPS calls OAuth credentials "API key" and "API secret" in their UI.
#' Tokens are stored securely in the system keyring.

#' Get RideWithGPS OAuth client
#'
#' Creates an OAuth client for RideWithGPS API authentication.
#' Note: RideWithGPS calls these "API key" and "API secret" even for OAuth.
#'
#' @return httr2 oauth_client object
#' @noRd
get_rwgps_client <- function() {
  # Try to get credentials from keyring
  # Note: RideWithGPS calls these "API key" and "API secret" in their UI
  api_key <- tryCatch(
    keyring::key_get("rwgps_api_key"),
    error = function(e) NULL
  )

  api_secret <- tryCatch(
    keyring::key_get("rwgps_api_secret"),
    error = function(e) NULL
  )

  if (is.null(api_key) || is.null(api_secret)) {
    stop(
      "RideWithGPS API credentials not found in keyring.\n",
      "Please set them with:\n",
      "  keyring::key_set('rwgps_api_key')     # Your API key from RideWithGPS\n",
      "  keyring::key_set('rwgps_api_secret')  # Your API secret from RideWithGPS\n\n",
      "Get these from: https://ridewithgps.com/account/developers",
      call. = FALSE
    )
  }

  client <- httr2::oauth_client(
    id = api_key,
    secret = api_secret,
    token_url = "https://ridewithgps.com/oauth/token.json",
    name = "dive_rwgps"
  )

  return(client)
}

#' Get path to RideWithGPS token cache
#'
#' @return Character string with path to token cache file
#' @noRd
get_rwgps_token_path <- function() {
  file.path(get_data_dir(), ".rwgps_token.rds")
}

#' Authenticate with RideWithGPS
#'
#' Performs OAuth2 authentication flow with RideWithGPS.
#' Opens a browser for user authorization, then caches the token.
#'
#' @param force Logical. If TRUE, force re-authentication even if token exists
#' @return httr2 oauth_token object
#' @export
rwgps_auth <- function(force = FALSE) {
  init_storage()

  client <- get_rwgps_client()
  token_path <- get_rwgps_token_path()

  # Check if we have a cached token
  if (!force && file.exists(token_path)) {
    message("Loading cached RideWithGPS token...")
    token <- tryCatch(
      readRDS(token_path),
      error = function(e) {
        message("Failed to load cached token: ", e$message)
        NULL
      }
    )

    if (!is.null(token) && !is.null(token$access_token)) {
      message("Using existing RideWithGPS authentication")
      return(token)
    }
  }

  # Perform OAuth flow
  message("Starting RideWithGPS OAuth flow...")
  message("A browser window will open for authentication.")

  # Workaround for macOS SSL issues
  old_bundle <- Sys.getenv("CURL_CA_BUNDLE")
  Sys.setenv(CURL_CA_BUNDLE = "")
  on.exit(Sys.setenv(CURL_CA_BUNDLE = old_bundle))

  # Custom OAuth flow to use JSON token exchange (RWGPS requirement)
  redirect_uri <- "http://localhost:1410/"

  # Generate random state for security
  state <- paste0(sample(c(letters, LETTERS, 0:9), 32, replace = TRUE), collapse = "")

  # Generate authorization URL
  # Note: RWGPS doesn't use scope parameter - scope is automatically set to "user"
  auth_url <- httr2::oauth_flow_auth_code_url(
    client = client,
    auth_url = "https://ridewithgps.com/oauth/authorize",
    redirect_uri = redirect_uri,
    state = state
  )

  # Show URL for debugging (without exposing full client_id)
  message("\nAuthorization URL generated:")
  message("  Base: https://ridewithgps.com/oauth/authorize")
  message("  Client ID (first 8 chars): ", substr(client$id, 1, 8), "...")
  message("  Redirect URI: ", redirect_uri)
  message("  Scope: (auto-assigned as 'user' by RWGPS)")

  # Open browser for authorization
  message("\nOpening browser for authorization...")
  utils::browseURL(auth_url)

  # Listen for callback
  code_result <- httr2::oauth_flow_auth_code_listen(redirect_uri = redirect_uri)

  # Parse the code
  code <- httr2::oauth_flow_auth_code_parse(
    query = code_result,
    state = state
  )

  # Exchange code for token using JSON body (RWGPS requirement)
  message("Exchanging authorization code for access token...")
  message("Using redirect_uri: ", redirect_uri)

  token_req <- httr2::request(client$token_url) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(
      grant_type = "authorization_code",
      code = code,
      client_id = client$id,
      client_secret = client$secret,
      redirect_uri = redirect_uri
    ))

  # Perform request with better error handling
  token_resp <- tryCatch(
    httr2::req_perform(token_req),
    error = function(e) {
      # Try to extract the response for better error reporting
      if (inherits(e, "httr2_failure")) {
        resp <- e$resp
        status <- httr2::resp_status(resp)
        body <- tryCatch(
          httr2::resp_body_json(resp),
          error = function(e2) httr2::resp_body_string(resp)
        )

        stop(
          "RideWithGPS token exchange failed:\n",
          "  Status: ", status, "\n",
          "  Response: ", paste(utils::capture.output(print(body)), collapse = "\n"),
          "\n\nDebug info:",
          "\n  Token URL: ", client$token_url,
          "\n  Client ID: ", substr(client$id, 1, 8), "...",
          "\n  Redirect URI: ", redirect_uri,
          call. = FALSE
        )
      }
      stop(e)
    }
  )

  token_data <- httr2::resp_body_json(token_resp)

  # Build token object
  token <- list(
    access_token = token_data$access_token,
    token_type = token_data$token_type %||% "Bearer"
  )

  # Add optional fields if present
  if (!is.null(token_data$expires_in)) {
    token$expires_at <- as.numeric(Sys.time()) + token_data$expires_in
  }
  if (!is.null(token_data$refresh_token)) {
    token$refresh_token <- token_data$refresh_token
  }

  # Cache the token
  saveRDS(token, token_path)
  message("RideWithGPS authentication successful!")

  return(token)
}

#' Get cached RideWithGPS token
#'
#' Returns the cached RideWithGPS token if it exists and is valid,
#' otherwise prompts for authentication.
#'
#' @return httr2 oauth_token object
#' @noRd
get_rwgps_token <- function() {
  token_path <- get_rwgps_token_path()

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
  return(rwgps_auth())
}

#' Check if RideWithGPS is authenticated
#'
#' @return Logical indicating if valid RideWithGPS token exists
#' @export
is_rwgps_authenticated <- function() {
  token_path <- get_rwgps_token_path()

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

#' Revoke RideWithGPS authentication
#'
#' Deletes the cached RideWithGPS token. User will need to re-authenticate.
#'
#' @return Invisible TRUE
#' @export
rwgps_deauth <- function() {
  token_path <- get_rwgps_token_path()

  if (file.exists(token_path)) {
    file.remove(token_path)
    message("RideWithGPS authentication revoked")
  } else {
    message("No RideWithGPS authentication found")
  }

  invisible(TRUE)
}

#' Create authenticated RideWithGPS API request
#'
#' Creates an httr2 request object with RideWithGPS authentication
#'
#' @param endpoint API endpoint path (without base URL)
#' @return httr2 request object
#' @noRd
rwgps_request <- function(endpoint) {
  token <- get_rwgps_token_with_refresh()

  req <- httr2::request("https://ridewithgps.com/api") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_auth_bearer_token(token$access_token) |>
    httr2::req_user_agent("dive R package (https://github.com/yourusername/dive)") |>
    httr2::req_retry(max_tries = 3)

  return(req)
}

#' Get RideWithGPS token with automatic refresh
#'
#' @return httr2 oauth_token object
#' @noRd
get_rwgps_token_with_refresh <- function() {
  token_path <- get_rwgps_token_path()

  if (!file.exists(token_path)) {
    return(rwgps_auth())
  }

  token <- readRDS(token_path)

  # Check if token is expired (with 5 minute buffer)
  if (!is.null(token$expires_at)) {
    now <- as.numeric(Sys.time())
    if (now >= (token$expires_at - 300)) {
      message("Access token expired, refreshing...")
      token <- refresh_rwgps_token(token)
    }
  }

  return(token)
}

#' Refresh RideWithGPS access token
#'
#' @param token Current token with refresh_token
#' @return Updated token
#' @noRd
refresh_rwgps_token <- function(token) {
  client <- get_rwgps_client()

  if (is.null(token$refresh_token)) {
    stop("No refresh token available. Please re-authenticate with rwgps_auth()")
  }

  req <- httr2::request("https://ridewithgps.com/oauth/token.json") |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(
      grant_type = "refresh_token",
      refresh_token = token$refresh_token,
      client_id = client$id,
      client_secret = client$secret
    ))

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      if (inherits(e, "httr2_failure")) {
        resp <- e$resp
        status <- httr2::resp_status(resp)
        body <- tryCatch(
          httr2::resp_body_json(resp),
          error = function(e2) httr2::resp_body_string(resp)
        )

        stop(
          "RideWithGPS token refresh failed:\n",
          "  Status: ", status, "\n",
          "  Response: ", paste(utils::capture.output(print(body)), collapse = "\n"),
          call. = FALSE
        )
      }
      stop(e)
    }
  )

  new_token_data <- httr2::resp_body_json(resp)

  # Update token with new values
  token$access_token <- new_token_data$access_token
  token$expires_at <- as.numeric(Sys.time()) + new_token_data$expires_in
  token$refresh_token <- new_token_data$refresh_token

  # Save updated token
  token_path <- get_rwgps_token_path()
  saveRDS(token, token_path)

  message("Token refreshed successfully")
  return(token)
}

#' Check if RideWithGPS credentials are configured
#'
#' @return Logical indicating if credentials exist in keyring
#' @export
has_rwgps_credentials <- function() {
  api_key <- tryCatch(
    keyring::key_get("rwgps_api_key"),
    error = function(e) NULL
  )

  api_secret <- tryCatch(
    keyring::key_get("rwgps_api_secret"),
    error = function(e) NULL
  )

  return(!is.null(api_key) && !is.null(api_secret))
}
