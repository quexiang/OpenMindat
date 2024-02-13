############ Initializing API Call ############
#library(httr)
#library(jsonlite)
#' Initializing Mindat API
#' @description Initializing API Call. Setup the base_url, token and format.
#' @usage mindat_connection(token, base_url = "https://api.mindat.org",fmt ="json",page_size = 800)
#' @param token  string. You can apply a token from Mindat.org.
#' @param base_url  string.The base url of mindat API, default is "https://api.mindat.org".
#' @param page_size interger, setting the page size of responsed data from the API server.
#' @param fmt  string. The format of the request and response, default is json.
#' @return No return value. A connection to the Mindat server will be established with your input token cached.
#' @examples
#' mindat_connection("9ce67655d74bcd981e937be80dcea9cb",page_size = 1500)
#' @export
mindat_connection <- function(token, base_url = "https://api.mindat.org",fmt ="json",page_size = 800){
  set_api_token(token)
  mindat_setup(base_url,page_size = page_size)
}
############ Initializing API Call ############

