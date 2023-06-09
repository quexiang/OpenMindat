############ Initializing API Call ############
#library(httr)
#library(jsonlite)
#' Initializing Mindat API
#' @description Initializing API Call. Setup the base_url, token and format.
#' @usage mindat_connection(token, base_url = default, fmt = "json")
#' @param token  string. You can apply a token from Mindat.org.
#' @param base_url  string.The base url of mindat API, default is "https://api.mindat.org".
#' @param fmt  string. The format of the request and response, default is json.
#' @examples
#' mindat_connection("ad9c15fa95d8063908cb5bf186c9e79f", "https://api.mindat.org","json")
#' mindat_connection("ad9c15fa95d8063908cb5bf186c9e79f")
mindat_connection <- function(token, base_url = "https://api.mindat.org",fmt ="json"){
  set_api_token(token)
  mindat_setup(base_url)
}
############ Initializing API Call ############

