########### mindat_api_tools.R #############
#' Create a default uri builder.
#' @description Create a default uri builder which can create the request uri according to the query conditions.
#' @usage default_uri_builder(api_base_uri,config,...,querystring=c(''))
#' @param api_base_uri string. The mindat api base uri.
#' @param config list of configuration.
#' @param querystring query string.
#' @returns uri string
#' @examples
#' default_uri_builder()
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
#' @usage mindat_api_endpoint(name,endpoint_base,...,default_uri_builder,c(fields))
#' @param name string .
#' @param endpoint_base list.
#' @param ... Further named parameters, other conditions.
#' @param uri_builder function default is the default_uri_builder.
#' @param query_params list.
#' @returns uri string
#' @examples
#' mindat_api_endpoint("geomaterials","geomaterials/%s",default_uri_builder,c(''))
mindat_api_endpoint<-function(name, endpoint_base, uri_builder = default_uri_builder,query_params = list(),...){
  if(!is.function(uri_builder)){
    stop("uri_builder must be a function")
  }

  config <- c(list(...), endpoint_base = endpoint_base, uri_builder = uri_builder,
              query_params = query_params)

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
#' @examples
#' stop_not_param(comp_params,query)
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
#' @usage build_uri(name,endpoint_base,...,default_uri_builder,c(fields))
#' @param name string .
#' @param endpoint list.
#' @param uri_builder function default is the default_uri_builder.
#' @param query_params list.
#' @param ... Further named parameters, other conditions.
#' @returns uri string
#' @examples
#' build_uri("geomaterials","geomaterials/%s",default_uri_builder,c(''))
#' build_uri('minerals_ima_list', 'minerals_ima/%s', uri_builder = mindat_uri_builder)
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
#' @examples
#' set_api_base("https://api.mindat.org")
set_api_base<-function(api_base){
  mindat_cache_set('api_base', api_base)
}

#' set_api_token
#' @description set the token of current environment
#' @usage set_api_token (api_token)
#' @param api_token string. The token of mindat api.
#' @examples
#' set_api_token("ad9c15fa95d8063908cb5bf186c9e79f")
set_api_token<-function(api_token){
  mindat_cache_set('api_token', api_token)
}

#' set_page_size
#' @description set the page_size of response records.
#' @usage set_page_size (page_size)
#' @param page_size string. The token of mindat api.
#' @examples
#' set_api_token("ad9c15fa95d8063908cb5bf186c9e79f")
set_page_size<-function(page_size = 1500){
  mindat_cache_set('page_size', page_size)
}

########### mindat_api_tools.R #############
