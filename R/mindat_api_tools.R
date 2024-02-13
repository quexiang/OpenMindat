########### mindat_api_tools.R #############
#' Create a default uri builder.
#' @description Create a default uri builder which can create the request uri according to the query conditions.
#' @usage default_uri_builder(api_base_uri,config, querystring = '')
#' @param api_base_uri string. The mindat api base uri.
#' @param config list of configuration.
#' @param querystring query string.
#' @returns uri string
#' @noRd
default_uri_builder<-function(api_base_uri,config, querystring = ''){
  endpoint_base <- config[['endpoint_base']]
  uri <- paste(api_base_uri, '/', endpoint_base, sep = "")
  if(querystring != ''){
    uri <- paste(uri, querystring, sep="?")
  }
  uri
}

#' set up a api endpoint named name
#' @description Setup up a api endpoint.
#' @usage mindat_api_endpoint(name,epb,ubuilder = default_uri_builder,qparams=list(),...)
#' @param name string .
#' @param epb list.
#' @param ubuilder function default is the default_uri_builder.
#' @param qparams list.
#' @param ... Further named parameters, other conditions.
#' @returns uri string
#' @noRd
mindat_api_endpoint<-function(name,epb,ubuilder = default_uri_builder,qparams=list(),...){
  if(!is.function(ubuilder)){
    stop("ubuilder must be a function")
  }

  config <- c(list(...), endpoint_base = epb, uri_builder = ubuilder,
              query_params = qparams)

  api_end_points<-mindat_cache_return_or_setup('api_end_points', function(){
    return (list())
  })

  api_end_points[[name]] <- config

  mindat_cache_set('api_end_points', api_end_points)
}


#' stop_not_param
#' @description if the query param is not in the list of mindat api, stop and report the errors.Throws error if a parameter is not found in query.
#' This function needs to be improved. For different endpoints of mindat.org API, the fields that can be queried are different.
#' @usage stop_not_param (comp_params, query)
#' @param comp_params list. list of compulsory, the internal params in the mindat api.
#' @param query list of query name/value pairs.
#' @return No return value.It will stop and report the errors if the query param is not in the list of mindat api.
#' @noRd
stop_not_param<-function(comp_params, query){
 q_params <- names(query)
 for(c_param in comp_params) {
   if(!is.element(c_param, q_params)){
     stop(sprintf("Query string param '%s' is missing", c_param))
   }
 }
}

#' build uri
#' @description build a request uri based on .
#' @usage build_uri(endpoint,query = list(),api_base = NULL,...)
#' @param endpoint list.
#' @param query list, query conditions.
#' @param api_base string, base url for mindat api
#' @param ... Further named parameters, other conditions.
#' @returns uri string
#' @noRd
build_uri<-function(endpoint, query = list(), api_base = NULL,...){
  # passed or global
  if(is.null(api_base)){
    if(mindat_cache_has('api_base')){
      api_base <- mindat_cache_get('api_base')
    } else {
      stop("No API base path was configured")
    }
  }

  api_end_points<-mindat_cache_get('api_end_points')
  config <- api_end_points[[endpoint]]
  if(is.null(config)){
    stop(sprintf("No config for endpoint '%s' is registered", endpoint))
  }
  query <- c(query, list(...))

  # check params for end point
  if(!is.null(config['query_params']) && length(config['query_params'][[1]] > 0)){
    stop_not_param(config['query_params'][[1]], query)
  }
  qs<-mindat_build_querystring(query)
  builder <- config[['uri_builder']]
  uri <- builder(api_base, config, querystring = qs)
  uri
}

#' set_api_base
#' @description set base uri of current environment
#' @usage set_api_base (api_base)
#' @param api_base string. The base uri of mindat api.
#' @return No return value. The api based url (api_base) will be cached. Users can retrieve the value by calling mindat_cache_get('api_base').
#' @examples
#' set_api_base("9ce67655d74bcd981e937be80dcea9cb")
#' @export
set_api_base<-function(api_base){
  mindat_cache_set('api_base', api_base)
}

#' set_api_token
#' @description set the token of current environment
#' @usage set_api_token (api_token)
#' @param api_token string. The token of mindat api.
#' @return No return value. The api_token will be cached. Users can retrieve the value by calling mindat_cache_get('api_token').
#' @examples
#' set_api_token("9ce67655d74bcd981e937be80dcea9cb")
#' @export
set_api_token<-function(api_token){
  mindat_cache_set('api_token', api_token)
}

#' set_page_size
#' @description set the page_size of response records.
#' @usage set_page_size (page_size)
#' @param page_size string. The token of mindat api.
#' @return No return value. The 'page_size' will be cached.
#'        The page_size information is added to the query string of every request sent to the Mindat server via the "OpenMindat" package.
#' @examples
#' set_page_size(800)
#' @export
set_page_size<-function(page_size = 800){
  mindat_cache_set('page_size', page_size)
}

########### mindat_api_tools.R #############
