#' Fix macOS SSL certificate issues
#'
#' This function unsets CURL_CA_BUNDLE to allow R's curl to use
#' macOS's native SecureTransport SSL backend with system keychain.
#'
#' @export
fix_ssl_macos <- function() {
  # Unset certificate bundle variables
  # macOS curl uses SecureTransport which gets certs from system keychain
  Sys.unsetenv("CURL_CA_BUNDLE")
  Sys.unsetenv("CURL_CA_PATH")

  message("Applied SSL fix for macOS")
  message("Using macOS SecureTransport with system keychain")
}
