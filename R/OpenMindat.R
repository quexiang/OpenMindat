############ Initializing API Call ############
#library(httr)
#library(jsonlite)
#' Initializing Mindat API
#' @description Initializing API Call. Setup the base_url, token and format.
#' @usage mindat_connection(token, base_url = default, fmt = "json")
#' @param token  string. You can apply a token from Mindat.org.
#' @param base_url  string.The base url of mindat API, default is "https://api.mindat.org".
#' @param fmt  string. The format of the request and response, default is json.
#' @export
mindat_connection <- function(token, base_url = "https://api.mindat.org",fmt ="json",page_size = 800){
  set_api_token(token)
  mindat_setup(base_url,page_size = page_size)
}
############ Initializing API Call ############

