############ Initializing API Call ############
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

############  mindat_cache.R  #################
mindat_cache <- new.env()

#' Set cache name and value
#' @description Assigns the value to the cache named varname in current environment.
#' @usage mindat_cache_set(varname, value)
#' @param varname  string. The cached varname.
#' @param value  string.
#' @examples
#' mindat_cache_set("api_test", "https://api.mindat.org")
#' mindat_cache_set("api_token","ad9c15fa95d8063908cb5bf186c9e79f")
mindat_cache_set<-function(varname, value)
{
  assign(varname, value, envir = mindat_cache)
}

#' Get cache value
#' @description Retrieve the value of the cache named varname in current environment.
#' @usage mindat_cache_get(varname)
#' @param varname  string
#' @returns cached value. A string, list or other objects.
#' @examples
#' mindat_cache_get("api_test")
#' mindat_cache_get("api_token")
mindat_cache_get<-function(varname)
{
  return (get(varname, envir=mindat_cache))
}

#' Delete a cached value by the users input varname
#' @description Remove (clear) the cache named varname in current environment.
#' @usage mindat_cache_delete(varname)
#' @param varname string input a cached name.Set a cached value empty by the given varname. A string, list or other objects.
#' @examples
#' mindat_cache_delete("api_test")
#' mindat_cache_delete("api_token")
mindat_cache_delete<-function(varname)
{
  assign(varname, NULL, envir = mindat_cache)
}

#' Remove all cached values
#' @description Clear all current cached values. Set current environment cache empty.
#' @usage mindt_cache_empty()
#' @examples
#' mindt_cache_empty()
mindt_cache_empty<-function(){
  rm(list = ls(envir = mindat_cache))
}

#' Check if the current environment has the cached value of varname.
#' @description Check whether or not the current environment has the cache named varname.
#' @usage mindat_cache_has(varname)
#' @param varname string.
#' @returns Boolean value. if the varname is found in current environment cache, return True otherwise return False.
#' @examples
#' mindat_cache_has("api_test")
#' mindat_cache_has("api_token")
mindat_cache_has <-function(varname)
{
  if(exists(varname, envir= mindat_cache)){

    return (!is.null(mindat_cache_get(varname)))
  }
  return (FALSE)
}

#' Check if the current environment has the cached function named varname.
#' @description Check whether the current environment has the cached function named varname,if has, return it.
#'              if not, setup up a new cache function named varname.
#' @usage mindat_cache_return_or_setup(varname,setupfun)
#' @param varname string.
#' @returns If the varname is found in current environment cache, return cached function.
#'          If not, eval the function and return cached function.
#' @examples
#' mindat_cache_return_or_setup("end_point",function(){return (list())})
mindat_cache_return_or_setup<-function(varname, setupfun)
{
  if(!mindat_cache_has(varname))
  {
    if(is.function(setupfun)){
      setupcall <- as.call(list(setupfun))
    } else {
      setupcall <- call(setupfun)
    }
    setupvalue <- eval(setupcall)
    mindat_cache_set(varname, setupvalue)
    mindat_cache_get(varname)
  }
  return (mindat_cache_get(varname))
}
########### mindat_cache.R ###########


########### mindat_api_tools.R #############
#' Create a default uri builder.
#' @description Create a default uri builder which can create the request uri according to the query conditions.
#' @usage default_uri_builder(api_base_uri,config,...,querystring=c(''))
#' @param api_base_uri string. The mindat api base uri.
#' @param config list of configuration.
#' @param ... Further named parameters,other conditions,such as fields = "id,name,ima_status".
#' @param querystring query string.
#' @returns uri string
#' @examples
#' default_uri_builder()
default_uri_builder<-function(api_base_uri,config, ..., querystring = ''){
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
mindat_api_endpoint<-function(name, endpoint_base, ..., uri_builder = default_uri_builder,query_params = list()){
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

#' build uri
#' @description build a request uri based on .
#' @usage mindat_api_endpoint(name,endpoint_base,...,default_uri_builder,c(fields))
#' @param name string .
#' @param endpoint_base list.
#' @param ... Further named parameters, other conditions.
#' @param uri_builder function default is the default_uri_builder.
#' @param query_params list.
#' @returns uri string
#' @examples
#' mindat_api_endpoint("geomaterials","geomaterials/%s",default_uri_builder,c(''))
#' mindat_api_endpoint('minerals_ima_list', 'minerals_ima/%s', uri_builder = mindat_uri_builder)
build_uri<-function(endpoint, ..., query = list(), api_base = NULL){
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

  # check compulsory params for end point
  if(!is.null(config['query_params']) && length(config['query_params'][[1]] > 0)){
    .stop_on_missing_param(config['query_params'][[1]], query)
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

##' stop_missing_param
##' @description if the query param is not in the list of mindat api, stop and report the errors.
##' @usage stop_missing_param (compulsory_params, query)
##' @param compulsory_params list. The internal params in the mindat api.
##' @param query list. list of query fields and conditions.
##' @examples
##' stop_missing_param(compulsory_params,query)
#stop_missing_param<-function(compulsory_params, query){
#  q_params <- names(query)
#  for(c_param in compulsory_params) {
#    if(!is.element(c_param, q_params)){
#      stop(sprintf("Query string param '%s' is missing", c_param))
#    }
#  }
#}
########### mindat_api_tools.R #############


########### mindat_rest_api.R ###########
#' mindat_uri_builder
#' @description generate the mindat_uri_builder
#' @usage mindat_uri_builder (api_base_url, config,querystring)
#' @param api_base_url list. The base url of mindat api.
#' @param config list. config of current environment.
#' @param querystring list. list of query fields and conditions.
#' @examples
#' mindat_uri_builder(api_base_url,c(''),querystring)
mindat_uri_builder<- function(api_base_url, config, querystring = ''){
  if(querystring != ''){
    uri <- paste(api_base_url, '/', sprintf(config[['endpoint_base']], querystring) ,sep = "")
  }
  else{
    config_str <- sub('%s','',config[['endpoint_base']])
    uri <- paste(api_base_url,config_str,sep = "/")
  }
  uri
}

#' mindat_set_up_endpoints
#' @description generate the basic mindat endpoints for current mindat API.
#' @usage mindat_set_up_endpoints ()
#' @examples
#' mindat_set_up_endpoints()
mindat_set_up_endpoints<-function(){
  # single ima mineral
  mindat_api_endpoint('minerals_ima', 'minerals_ima/%s', uri_builder = mindat_uri_builder,
                     compulsory_params = list('id'))

  # ima minerals list
  mindat_api_endpoint('minerals_ima_list', 'minerals_ima/%s', uri_builder = mindat_uri_builder)

  # single locality
  mindat_api_endpoint('localities', 'localities/%s',uri_builder = mindat_uri_builder,
                     compulsory_params = list('id'))
  # localities
  mindat_api_endpoint('localities_list', 'localities/%s',uri_builder = mindat_uri_builder)

  # locality_age
  mindat_api_endpoint('locality_age_list', 'locality_age/%s',uri_builder = mindat_uri_builder)
  mindat_api_endpoint('locality_age', 'locality_age/%s',uri_builder = mindat_uri_builder,
                      compulsory_params = list('id'))

  # locality_status
  mindat_api_endpoint('locality_status_list', 'locality_status/%s',uri_builder = mindat_uri_builder)
  mindat_api_endpoint('locality_status', 'locality_status/%s',uri_builder = mindat_uri_builder,
                      compulsory_params = list('id'))

  mindat_api_endpoint('locality_type_list', 'locality_type/%s',uri_builder = mindat_uri_builder)
  mindat_api_endpoint('locality_type', 'locality_type/%s',uri_builder = mindat_uri_builder,
                      compulsory_params = list('id'))

  #geomaterials
  mindat_api_endpoint('geomaterials', 'geomaterials/%s',uri_builder = mindat_uri_builder,
                      compulsory_params = list('id'))
  #geomaterials_list
  mindat_api_endpoint('geomaterials_list', 'geomaterials/%s',uri_builder = mindat_uri_builder)

  #geomaterials_varieties/{id}/varieties/
  mindat_api_endpoint('geomaterials_varieties', 'geomaterials/%s/varieties',uri_builder = mindat_uri_builder)

  #geomaterials/dict/
  mindat_api_endpoint('geomaterials/dict', 'geomaterials/dict/%s',uri_builder = mindat_uri_builder)

  #geomaterials_search/
  mindat_api_endpoint('geomaterials_search', 'geomaterials_search/%s',uri_builder = mindat_uri_builder)

  #dana-8
  mindat_api_endpoint('dana-8', 'dana-8/%s',uri_builder = mindat_uri_builder,
                      compulsory_params = list('id'))

  mindat_api_endpoint('dana-8', 'dana-8/%s',uri_builder = mindat_uri_builder)

  #dana-8/groups
  mindat_api_endpoint('dana-8/groups', 'dana-8/groups/%s',uri_builder = mindat_uri_builder,
                      compulsory_params = list('id'))
  #dana-8/subgroups
  mindat_api_endpoint('dana-8/subgroups', 'dana-8/subgroup/%s',uri_builder = mindat_uri_builder)

  #countries
  mindat_api_endpoint('countries', 'countries/%s',uri_builder = mindat_uri_builder)

  #countries
  mindat_api_endpoint('countries', 'countries/%s',uri_builder = mindat_uri_builder,
                      compulsory_params = list('id'))
}

#' mindat_setup
#' @description set up of the mindat basic uri, endpoints, and cache
#' @usage mindat_set_up_endpoints (base_uri)
#' @examples
#' mindat_setup('https://api.mindat.org')
mindat_setup<-function(base_uri = 'https://api.mindat.org'){
  set_api_base(bas_uri)
  mindat_set_up_endpoints()
  mindat_cache_set('api_format', 'json')
}


########### mindat_rest_api.R ###########

########## mindat_network.R ############
##' collapse_array_columns_map
##' @description set up of the mindat basic uri, endpoints, and cache
##' @usage collapse_array_columns_map (base_uri)
##' @param name description
##' @return description
##' @examples
#' collapse_array_columns_map(c('id'))
# collapse_array_columns_map<- function (element){
#   if (length (element) > 1){
#     mapped<- paste (element, collapse=";")
#   }else {
#     mapped<- element
#   }
#   mapped
# }

#' mindat_make_data_frame
#' @description convert the response json to dataframe of R
#' @usage mindat_make_data_frame (reg_list)
#' @param reg_list response json data to list format obj.
#' @return df_out, R data frame
#' @examples
#' mindat_make_data_frame(data_list[["records"]])
mindat_make_data_frame<-function(reg_list){
  if (is.list(reg_list)){
    #df_out <- data.frame(reg_list)
    df_out <-as.data.frame(do.call(cbind,reg_list))
  }
  else{
     mineralcnt <-reg_list$count
     data_results<-reg_list$results
     df_out <- data.frame(data_results)
     next_url<- reg_list$`next`
     while(!is.null(next_url)){
       query<-list(format= "json")
       all_data2<-GET(next_url,add_headers('Authorization'= paste('Token ',YOUR_API_KEY,sep = "")),query = query)
       str(content(all_data2))
       all_data2_text <- content(all_data2,"text", encoding = "UTF-8")
       all_data2_json <- fromJSON(all_data2_text,flatten = TRUE)
       data_results2<-all_data2_json$results
       df_tmp = data.frame(data_results2)
       df_out <- rbind(df_out,df_tmp)
       next_url<- all_data2_json$`next`
     }
  }
  df_out
}

#' mindat_parse_raw_data
#' @description parse the raw response of json to dataframe of R. If the raw_data obtained from the response is paged,
#' request all the pages and then add them into the df_out data frame.
#' @usage mindat_parse_raw_data (raw_data)
#' @param raw_data content of the response body
#' @return df_out, R data frame
#' @examples
#' mindat_parse_raw_data(raw_data)
mindat_parse_raw_data<-function(raw_data){
  data_list <- fromJSON(raw_data)
  if ("results" %in% names(data_list) ){
        if(mindat_cache_has('api_token')){
          api_token <- mindat_cache_get('api_token')
        } else {
          stop("No API token configured")
        }
        data_results<-data_list$results
        df_out <- data.frame(data_results)
        next_url<- data_list$`next`
        while(!is.null(next_url)){
          query<-list(format= "json")
          all_data2<-GET(next_url,add_headers('Authorization'= paste('Token ',api_token,sep = "")),query = query)
          str(content(all_data2))
          all_data2_text <- content(all_data2,"text", encoding = "UTF-8")
          all_data2_json <- fromJSON(all_data2_text,flatten = TRUE)
          data_results2<-all_data2_json$results
          df_tmp = data.frame(data_results2)
          df_out <- rbind(df_out,df_tmp)
          next_url<- all_data2_json$`next`
        }
        df_out
  }
  else{
    df_out <- mindat_make_data_frame(data_list)#data_list[["records"]]
    df_out
  }
}

#' mindat_extract_response_body
#' @description .
#' @usage mindat_extract_response_body (response)
#' @param response response json
#' @return if status of the response is sucess (200),return the all_data_text(the content of response).
#' Otherwise,report the errors.
#' @examples
#' mindat_extract_response_body(raw_data)
mindat_extract_response_body<-function(response){
  if (200 == status_code(response)){
    # Content in the API
    str(content(response))
    # Converting content to text
    all_data_text <- content(response,"text", encoding = "UTF-8")
  }
  else{
    stop(sprintf('Error in API: The server return a status %s,which indicates that something went wrong with your request.',status_code(response)))
  }
}

#' mindat_get_data_from_uri
#' @description retrieve data from the uri.
#' @usage mindat_get_data_from_uri (uri)
#' @param uri request uri
#' @return df. R data frame of the request uri.
#' @examples
#' mindat_get_data_from_uri("https://api.mindat.org/geomaterials/1/")
mindat_get_data_from_uri<-function(uri){
  if(mindat_cache_has('api_token')){
    api_token <- mindat_cache_get('api_token')
  } else {
    stop("No API token configured")
  }
  response <- GET(uri,add_headers('Authorization'= paste('Token ',api_token,sep = "")))
  raw_data <- mindat_extract_response_body(response)
  df<-mindat_parse_raw_data(raw_data)
  df
}

#' mindat_build_querystring
#' @description Build query string based on the query conditions.
#' @usage mindat_build_querystring (args)
#' @param args query args.
#' @return qs. generated query string.
#' @examples
#' mindat_build_querystring(args)
mindat_build_querystring<-function(args){
  qs <- ''
  for (argName in names(args)) {
    strArgValue <- as.character(args[argName][[1]])
    encodedArgValue <-  URLencode(strArgValue)
    if (argName == "id"){
      qs <-paste(qs, encodedArgValue, '&', sep = "")
      #break
    }
    else if(argName == "ids"){
      qs <-paste('?id__in=',qs, encodedArgValue, '&', sep = "")
    }
    else{
      qs <- paste(qs, argName, "=", encodedArgValue, '&', sep = "")
    }
  }
  qs <- substr(qs,0,nchar(qs)-1)
  qs
}
########## mindat_network.R ###########

########### mindat_querys.R #############
#' mindat_query
#' @description Basic function for query dataset at a specified endpoit.
#' @usage mindat_query (endpoint,query)
#' @param endpoint query endpoint, e.g.'minerals_ima'.
#' @return df query resutls in data frame format.
#' @examples
#'  mindat_query('minerals_ima', query = c(list(id = id)))
mindat_query<-function(endpoint, query = list()){
  query <- lapply(query, params_to_string)
  uri <- build_uri(endpoint, query = query)
  df <- mindat_get_data_from_uri(uri)
  df
}

#' params_to_string
#' @description Prase params to string,so that the query function can deal with the other exteranl condition set by the users.
#' @usage params_to_string (params)
#' @param params convert params to string,which is used by the mindat query function.
#' @return str .
#' @examples
#'  params_to_string(params)
params_to_string<-function(params){
  if(!(is.vector(params))){
    stop("Vector expected")
  }
  if(length(params) > 1){
    str <- params[[1]]
    for (p in params[2:length(params)]) {
      str <- paste(str, ",", p, sep = "")
    }
  } else {
    str <- params
  }
  return (str)
}

#' mindat_mineral_ima
#' @description retrieve ima mineral by its id.
#' @usage mindat_mineral_ima (id )
#' @param id mindat id
#' @return df. query resutls in data frame format.
#' @examples
#'  mindat_mineral_ima(id = 6)
mindat_mineral_ima<-function(id, ...){
  l<-list(...)
  mindat_query('minerals_ima', query = c(list(id = id), l))
}

#' mindat_mineral_ima_list
#' @description retrieve ima mineral list
#' @usage mindat_mineral_ima (...)
#' @param ... , Further named parameters.
#' @return df, data frame of mineral list.
#' @examples
#'  mindat_mineral_ima_list()
#'  mindat_mineral_ima_list(ids = c('3','5','7','9','222'))
mindat_mineral_ima_list<-function(...){
  l<-list(...)
  mindat_query('minerals_ima_list', query = l)
}

#' mindat_localities_list
#' @description retrieve localities list
#' @usage mindat_localities_list (...)
#' @param ... Further named parameters.
#' @return df. data frame of localities list.
#' @examples
#'  mindat_localities_list()
#'  mindat_localities_list(ids = c('3','5','7','9','222'))
mindat_localities_list<-function(...){
  l<-list(...)
  mindat_query('localities_list', query = l)
}

#' mindat_localitiy
#' @description retrieve locality by its id
#' @usage mindat_localitiy (id)
#' @param id the mindat localitiy id
#' @param ..., Further named parameters.
#' @return df, data frame of locality
#' @examples
#'  mindat_localitiy(id = 222)
#'  mindat_localitiy(id = 222, fields = "id,name")
mindat_localitiy<-function(id,...){
  l<-list(...)
  mindat_query('localities', query = c(list(id = id), l))
}

#' mindat_locality_status
#' @description retrieve all locality status by its id
#' @usage mindat_locality_status (...)
#' @param id the mindat localitiy status id.
#' @param ..., Further named parameters.
#' @return df, data frame of locality status
#' @examples
#'  mindat_locality_status(id = 222)
#'  mindat_locality_status(fields = "id,name")
mindat_locality_status<-function(id,...){
  l<-list(...)
  mindat_query('locality_status', query = c(list(id = id), l))
}

#' mindat_locality_status_list
#' @description retrieve all locality status list
#' @usage mindat_locality_status_list (...)
#' @param ..., Further named parameters.
#' @return df, data frame of locality status list
#' @examples
#'  mindat_locality_status_list()
#'  mindat_locality_status_list(fields = "id,name")
mindat_locality_status_list<-function(...){
  l<-list(...)
  mindat_query('locality_status_list', query = l)
}

#' mindat_locality_type_list
#' @description retrieve all locality type list
#' @usage mindat_locality_type_list (...)
#' @param ..., Further named parameters.
#' @return df, data frame of locality type list
#' @examples
#'  mindat_locality_type_list()
#'  mindat_locality_type_list(fields = "id,name")
mindat_locality_type_list<-function(...){
  l<-list(...)
  mindat_query('locality_type_list',l)
}

#' mindat_locality_type
#' @description retrieve  locality type by its id
#' @usage mindat_locality_type (...)
#' @param id locality type id
#' @param ..., Further named parameters.
#' @return df, data frame of locality type list
#' @examples
#'  mindat_locality_type(id = 222)
mindat_locality_type<-function(id,...){
  l<-list(...)
  mindat_query('locality_type', query = c(list(id = id), l))
}


#' mindat_geomaterial
#' @description retrieve  geomaterial  by its id
#' @usage mindat_geomaterial (id)
#' @param id geomaterial id
#' @param ..., Further named parameters.
#' @return df, data frame of locality type list
#' @examples
#' mindat_geomaterial(id = 1)
mindat_geomaterial<-function(id,...){
  l<-list(...)
  mindat_query('geomaterials', query  = c(list(id = id), l))
  # mindat_query('geomaterials', query  = c(list(id = id), l),compulsory_params = list('id'))
}

#' mindat_geomaterial_varieties
#' @description retrieve the geomaterial varieties by the id of geomaterial.
#' @usage mindat_geomaterial_varieties (id)
#' @param id geomaterial id
#' @param ..., Further named parameters.
#' @return df, data frame of locality type list
#' @examples
#' mindat_geomaterial_varieties(id = 1)
mindat_geomaterial_varieties<-function(id,...){
  l<-list(...)
  mindat_query('geomaterials_varieties', query  = c(list(id = id), l))
  # mindat_query('geomaterials', query  = c(list(id = id), l),compulsory_params = list('id'))
}

#' mindat_geomaterial_list
#' @description retrieve all the geomaterial list or the geomaterial by given conditions.
#' @usage mindat_geomaterial_list ()
#' @param ..., Further named parameters.
#' @return df, data frame of locality type list
#' @examples
#' mindat_geomaterial_list()
mindat_geomaterial_list<-function(...){
  l<-list(...)
  # if(length(l)){
  #   print(l)
  # }
  mindat_query('geomaterials_list', query = l)
}

#' mindat_country
#' @description retrieve the country by given its id.
#' @usage mindat_country (id)
#' @param id, country id in mindat.
#' @param ..., Further named parameters.
#' @return df, a data frame of country
#' @examples
#' mindat_country(id = 1)
mindat_country<-function(id,...){
  l<-list(...)
  mindat_query('countries', query  = c(list(id = id), l))#,compulsory_params = list('id')
}

#' mindat_countries
#' @description retrieve all countries list or the contries by given conditions.
#' @usage mindat_countries ()
#' @param ..., Further named parameters.
#' @return df, data frame of countries list
#' @examples
#' mindat_countries()
mindat_countries<-function(...){
  l<-list(...)
  mindat_query('countries', query = l)
}

# mindat_mineral_geo<-function(){
#
# }
# mindat_geo_minerals<-function(){
#
# }

########### mindat_querys.R #############

########### mindat_geomaterials_tools.R #############
#' geomaterials_contain_all_elems
#' @description retrieve the geomaterials that contain all of the elements.This function queries
#' the list of geological materials that contain the specified elements.
#' It performs the query operation by calling the mindat_geomaterial_list function
#' @usage geomaterials_contain_all_elems (icl_elms_vector)
#' @param icl_elms_vector, vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments that can be
#' passed to the mindat_geomaterial_list function.
#' @return df, a data frame of geomaterials
#' @examples
#' geomaterials_contain_all_elems(c('H','Be'))
geomaterials_contain_all_elems<- function(icl_elms_vector,...){
  df <- mindat_geomaterial_list(ids = c(''),elements_inc = icl_elms_vector,...)
  df
}

#' geomaterials_that do not contain the elements
#' @description retrieve the geomaterials that do nont contain the input elements.Queries the list of geological materials that
#' do not contain the specified elements.
#' @usage geomaterials_without_elems(ecl_elms_vector, ...)
#' @param ecl_elms_vector, vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' geomaterials_without_elems(c('H','Be'))
geomaterials_without_elems <- function(ecl_elms_vector,...){
  df <- mindat_geomaterial_list(ids = c(''),elements_exc = ecl_elms_vector,...)
  df
}

#' geomaterials_that do not contain the elements
#' @description Queries the list of geological materials that simultaneously contain the specified elements and do not contain the specified elements.
#' @usage geomaterials_contain_all_and_without_elems(icl_elm_vector, ecl_elms_vector, ...)
#' @param icl_elm_vector vector of elements.
#' @param ecl_elms_vector vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' This function queries the list of geological materials that do not contain the specified elements.
#' It performs the query operation by calling the mindat_geomaterial_list function.
#' @examples
#' geomaterials_contain_all_and_without_elems(c('H','Be'),c('O'))
geomaterials_contain_all_and_without_elems <- function(icl_elm_vector,ecl_elms_vector,...){
  df <- mindat_geomaterial_list(ids = c(''),elements_inc = icl_elm_vector,elements_exc = ecl_elms_vector,...)
  df
}

#' geomaterials that contain any of the given elements
#' @description : Queries the list of geological materials that contain any of the specified elements.
#' @usage geomaterials_contain_any_elems(any_elems, ...)
#' @param any_elems vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' This function queries the list of geological materials that contain any of the specified elements.
#' It performs the query operation by looping through each specified element and calling the mindat_geomaterial_list function.
#' @examples
#' geomaterials_contain_any_elems(c('H','Be'))
geomaterials_contain_any_elems <- function(any_elems,...){
  df_out <- data.frame()
  for (elem in any_elems){
    df <- mindat_geomaterial_list(ids = c(''),elements_inc = c(elem),...)
    df_out <- rbind(df_out,df)
  }
  df_out
}

#' geomaterials that have the given cleavagetype
#' @description : Queries the list of geomaterials that have the specified cleavagetype
#' @usage geomaterials_cleavagetype(types, ...)
#' @param types vector of given cleavagetype
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' Items Enum: "Distinct/Good" "Imperfect/Fair" "None Observed" "Perfect" "Poor/Indistinct" "Very Good"
#' @examples
#' geomaterials_cleavagetype(c('Hexagonal'))
geomaterials_cleavagetype <- function(types,...){
  if(length(types)>1){
    # merge_types<- paste(types,"&",sep = "",collapse ='')
    # merge_types <- substr(merge_types,0,nchar(merge_types)-1)
    # df <- mindat_geomaterial_list(ids = c(''),cleavagetype = merge_types,...)
    df_out <- data.frame()
    for (type in types){
      df <- mindat_geomaterial_list(ids = c(''),cleavagetype = c(type),...)
      df_out <- rbind(df_out,df)
    }
    df_out
  }
  else{
    df_out <- mindat_geomaterial_list(ids = c(''),cleavagetype = types,...)
    df_out
  }
}

#' geomaterials that have the given colors
#' @description : Queries the list of geomaterials that have the specified colors
#' @usage geomaterials_colour(colors, ...)
#' @param colors vector of given colors
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' such: "Brown" "Yellow" "green"
#' @examples
#' geomaterials_colour(c('Yellow'))
geomaterials_colour<- function(colors,...){
  if(length(colors)>1){
    df_out <- data.frame()
    for (color in colors){
      df <- mindat_geomaterial_list(ids = c(''),colour = c(color),...)
      df_out <- rbind(df_out,df)
    }
    df_out
  }
  else{
    df_out <- mindat_geomaterial_list(ids = c(''),colour = colors,...)
    df_out
  }
}

#' geomaterials that have the given crystal
#' @description : Queries the list of geomaterials that have the specified crystal system
#' @usage geomaterials_crystal_system(crystals, ...)
#' @param crystals vector of given crystals
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' Items Enum: "Amorphous" "Hexagonal" "Icosahedral" "Isometric" "Monoclinic" "Orthorhombic" "Tetragonal" "Triclinic" "Trigonal"
#' @examples
#' geomaterials_crystal_system(c('Hexagonal'))
#' geomaterials_crystal_system(c('Hexagonal','Amorphous'))
geomaterials_crystal_system <- function(crystals,...){
  if(length(crystals)>1){
    df_out <- data.frame()
    for (crystal in crystals){
      df <- mindat_geomaterial_list(ids = c(''),crystal_system = c(crystal),...)
      df_out <- rbind(df_out,df)
    }
    df_out
  }
  else{
    df_out <- mindat_geomaterial_list(ids = c(''),crystal_system = crystals,...)
    df_out
  }
}

#' retrieve the geomaterials whose density are higher density than the given value.
#' @description : Queries the list of geomaterials that have higher density than gt.
#' @usage geomaterials_dens_greater_than(gt, ...)
#' @param gt float value
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that has higher density than the given density.
#' @examples
#' geomaterials_dens_greater_than(6)
#' geomaterials_dens_greater_than(3)
geomaterials_dens_greater_than<- function(gt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),density__from = gt,...)
  df_out
}

#' retrieve the geomaterials whose density are lower density than the given value.
#' @description : Queries the list of geomaterials that have lower density than lt.
#' @usage geomaterials_dens_less_than(lt, ...)
#' @param gt float value
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that has higher density than the given density.
#' @examples
#' geomaterials_dens_less_than(6)
#' geomaterials_dens_less_than(3)
geomaterials_dens_less_than<- function(lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),density__to = lt,...)
  df_out
}

#' retrieve the geomaterials whose density are higher and lower than the given value.
#' @description : Queries the list of geomaterials that have lower density than lt.
#' @usage geomaterials_dens_range(gt,lt, ...)
#' @param gt float value
#' @param lt float value
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that has the density within the range of (gt,lt).
#' @examples
#' geomaterials_dens_range(3,3.5)
geomaterials_dens_range <- function(gt,lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),density__from = gt,density__to = lt,...)
  df_out
}

#' retrieve the geomaterials that have the given diapheny.
#' @description : Queries the list of geomaterials that have the given diapheny.
#' @usage geomaterials_diapheny(diapheny, ...)
#' @param diapheny string
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' Items Enum: "Opaque" "Translucent" "Transparent"
#' @examples
#' geomaterials_diapheny("Opaque")
geomaterials_diapheny <- function(diapheny,...){
  if(length(diapheny)>1){
    df_out <- data.frame()
    for (dia in diapheny){
      df <- mindat_geomaterial_list(ids = c(''),diapheny = c(dia),...)
      df_out <- rbind(df_out,df)
    }
    df_out
  }
  else{
    df_out <- mindat_geomaterial_list(ids = c(''),diapheny = diapheny,...)
    df_out
  }
}

#' retrieve the geomaterials that have the given entrytype
#' @description : Queries the list of geomaterials that have the given entrytype
#' @usage geomaterials_entrytype(type, ...)
#' @param types list of entry types.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' Items Enum: 0 1 2 3 4 5 6 7 8
#' @examples
#' geomaterials_entrytype(c('1'))
#' geomaterials_entrytype(c('1','2'))
geomaterials_entrytype <- function(types,...){
  if(length(types)>1){
    df_out <- data.frame()
    for (type in types){
      df <- mindat_geomaterial_list(ids = c(''),entrytype = c(type),...)
      df_out <- rbind(df_out,df)
    }
    df_out
  }
  else{
    df_out <- mindat_geomaterial_list(ids = c(''),entrytype = types,...)
    df_out
  }
}

#' retrieve the geomaterials that have the given expand.
#' @description : Queries the list of geomaterials that have the given expand.
#' @usage geomaterials_expand(expand_fields, ...)
#' @param expanded_fields list of expand.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' Items Enum: "description" "type_localities" "localities" "relations" "~all" "*"
#' @examples
#' geomaterials_expand(c('description'))
#' geomaterials_expand(c('type_localities','localities'))
geomaterials_expand <- function(expanded_fields,...){
  if(length(expanded_fields)>1){
    df_out <- data.frame()
    for (efild in expanded_fields){
      df <- mindat_geomaterial_list(ids = c(''),expand = c(efild),...)
      df_out <- rbind(df_out,df)
    }
    df_out
  }
  else{
    df_out <- mindat_geomaterial_list(ids = c(''),expand = expanded_fields,...)
    df_out
  }
}

#' retrieve the geomaterials that have the given fracturetype.
#' @description : Queries the list of geomaterials that have the given types.
#' @usage geomaterials_fracturetype(types, ...)
#' @param types list of types.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' Items Enum: "Conchoidal" "Fibrous" "Hackly" "Irregular/Uneven" "Micaceous" "None observed" "Splintery" "Step-Like" "Sub-Conchoidal"
#' @examples
#' geomaterials_fracturetype(c('Conchoidal'))
#' geomaterials_fracturetype(c('Conchoidal','Fibrous'))
geomaterials_fracturetype <- function(types,...){
  if(length(types)>1){
    df_out <- data.frame()
    for (type in types){
      df <- mindat_geomaterial_list(ids = c(''),fracturetype = c(type),...)
      df_out <- rbind(df_out,df)
    }
    df_out
  }
  else{
    df_out <- mindat_geomaterial_list(ids = c(''),fracturetype = types,...)
    df_out
  }
}

#' retrieve the geomaterials whose hardness are higher than the given value.
#' @description : Queries the list of geomaterials that have higher hardness than gt.
#' @usage geomaterials_hardness_gt(hmin, ...)
#' @param hmin float value
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that has higher hardness than the given value.
#' @examples
#' geomaterials_hardness_gt(6)
#' geomaterials_hardness_gt(3)
geomaterials_hardness_gt<- function(hmin,...){

  df_out <- mindat_geomaterial_list(ids = c(''),hardness__from = hmin,...)
  df_out

}

#' retrieve the geomaterials whose hardness are lower than the given value.
#' @description : Queries the list of geomaterials that have lower hardness than hmax.
#' @usage geomaterials_hardness_lt(hmax, ...)
#' @param hmax float value
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that has lower hardness than the given value.
#' @examples
#' geomaterials_hardness_lt(6)
#' geomaterials_hardness_lt(3)
geomaterials_hardness_lt<- function(hmax,...){
  df_out <- mindat_geomaterial_list(ids = c(''),hardness__to = hmax,...)
  df_out
}


#' retrieve the geomaterials whose hardness is within the given range.
#' @description : Queries the list of geomaterials that have hardness within the given range.
#' @usage geomaterials_hardness_range(hmin,hmax, ...)
#' @param hmin float value
#' @param hmax float value
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that has the hardness within the range of (hmin,hmax).
#' @examples
#' geomaterials_hardness_range(3,3.5)
geomaterials_hardness_range<-function(hmin,hmax,...){
  df_out <- mindat_geomaterial_list(ids = c(''),hardness__from =hmin ,hardness__to = hmax,...)
  df_out
}

#' retrieve the geomaterials approved by IMA or not.
#' @description : Queries the geomaterials within or without the ima.
#' @usage geomaterials_ima(btrue,...)
#' @param btrue boolean value
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that are approved by the IMA or not.
#' @examples
#' geomaterials_ima(TRUE)
#' geomaterials_ima(FALSE)
geomaterials_ima<- function(btrue =TRUE,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ima = btrue,...)
  df_out
}

#' retrieve the geomaterials that have the given lustretype.
#' @description : Queries the geomaterials that have the given lustretype.
#' @usage geomaterials_lustretype(types, ...)
#' @param types string of the type name.Please refer to the details.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' Items Enum: "Adamantine" "Dull" "Earthy" "Greasy" "Metallic" "Pearly" "Resinous" "Silky" "Sub-Adamantine" "Sub-Metallic" "Sub-Vitreous" "Vitreous" "Waxy"
#' @examples
#' geomaterials_lustretype(c("Adamantine"))
#' geomaterials_lustretype(c("Adamantine","Dull"))
geomaterials_lustretype <- function(types,...){
  if(length(types)>1){
    df_out <- data.frame()
    for (type in types){
      df <- mindat_geomaterial_list(ids = c(''),lustretype = c(type),...)
      df_out <- rbind(df_out,df)
    }
    df_out
  }
  else{
    df_out <- mindat_geomaterial_list(ids = c(''),lustretype = types,...)
    df_out
  }
}

#' retrieve the geomaterials that include non-utf mineral names or not.
#' @description : Queries the geomaterials include non-utf mineral names or not.
#' @usage geomeaterials_non_utf(types, ...)
#' @param types string of the type name.Please refer to the details.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve the geomaterials that contain or not contain the non-utf name.
#' @examples
#' geomeaterials_non_utf(TRUE)
#' geomeaterials_non_utf(FALSE)
geomeaterials_non_utf<- function(btrue =TRUE,...){
  df_out <- mindat_geomaterial_list(ids = c(''),non_utf = btrue,...)
  df_out
}

#' retrieve the geomaterials that have the given optical signs.
#' @description : Queries the geomaterials have the given optical signs.
#' @usage geomaterials_opticalsign(signs, ...)
#' @param signs list of the signs.Please refer to the details.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' Enum: "+", "+/-", "-"
#' @examples
#' geomaterials_opticalsign("+")
#' geomaterials_opticalsign("+/-")
geomaterials_opticalsign<- function(signs,...){
  if(length(signs)>1){
    df_out <- data.frame()
    for (sign in signs){
      df <- mindat_geomaterial_list(ids = c(''),opticalsign = c(sign),...)
      df_out <- rbind(df_out,df)
    }
    df_out
  }
  else{
    df_out <- mindat_geomaterial_list(ids = c(''),opticalsign = signs,...)
    df_out
  }
}


#' retrieve the geomaterials that have the given optical types.
#' @description : Queries the geomaterials have the given optical types
#' @usage geomaterials_opticaltype(types, ...)
#' @param types list of the types.Please refer to the details.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' Enum: "Biaxial" "Isotropic" "Uniaxial"
#' @examples
#' geomaterials_opticaltype(c("Biaxial"))
#' geomaterials_opticaltype(c("Biaxial", "Isotropic"))
geomaterials_opticaltype <- function(types,...){
  if(length(types)>1){
    df_out <- data.frame()
    for (type in types){
      df <- mindat_geomaterial_list(ids = c(''),opticaltype = c(type),...)
      df_out <- rbind(df_out,df)
    }
    df_out
  }
  else{
    df_out <- mindat_geomaterial_list(ids = c(''),opticaltype = types,...)
    df_out
  }
}

#' retrieve the geomaterials that refractive index higher than the given value.
#' @description : Queries the geomaterials have the higher refractive index than the given value.
#' @usage geomaterials_ri_gt(gt, ...)
#' @param gt float value.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve the geomaterials with the refractive index higher than the given value.
#' @examples
#' geomaterials_ri_gt(1.6)
geomaterials_ri_gt <- function(gt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ri__from = gt,...)
  df_out
}

#' retrieve the geomaterials that refractive index lower than the given value.
#' @description : Queries the geomaterials have the lower refractive index than the given value.
#' @usage geomaterials_ri_gt(lt, ...)
#' @param lt float value.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve the geomaterials with the refractive index lower than the given value.
#' @examples
#' geomaterials_ri_lt(1.8)
geomaterials_ri_lt <- function(lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ri__to = lt,...)
  df_out
}

#' retrieve the geomaterials whose refractive index is within the given range.
#' @description : Queries the list of geomaterials that have refractive index within the given range.
#' @usage geomaterials_ri_range(gt,lt, ...)
#' @param gt float value
#' @param lt float value
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that has the refractive index within the range of (gt,lt).
#' @examples
#' geomaterials_ri_range(1.6,1.8)
geomaterials_ri_range <- function(gt,lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ri__from = gt,ri__to = lt,...)
  df_out
}


#' retrieve the geomaterials that have streak.
#' @description : Queries the list of geomaterials that have the given steak.
#' @usage geomaterials_streak(str,...)
#' @param str string. steak
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve the geomaterials that has the given steak.
#' @examples
#' geomaterials_streak("black")
geomaterials_streak <- function(str,...){
  df_out <- mindat_geomaterial_list(ids = c(''),steak = str,...)
  df_out
}

#' retrieve the geomaterials by given the synid.
#' @description : Queries the list of geomaterials that have the given synid.
#' @usage geomaterials_synid(idnum,...)
#' @param idnum integer, syn id number
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve the geomaterials that has the given steak.
#' @examples
#' geomaterials_synid(0)
#' geomaterials_synid(2897)
geomaterials_synid <- function(idnum,...){
  df_out <- mindat_geomaterial_list(ids = c(''),synid = idnum,...)
  df_out
}

#' retrieve the geomaterials updated at the given time.
#' @description : Queries the list of geomaterials that have the given synid.
#' @usage geomaterials_updated_at(strDate,...)
#' @param strDate string<date-time>, Last updated datetime in format %Y-%m-%d %H:%M:%S
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve the geomaterials that have the latest updated at the given time.
#' @examples
#' geomaterials_updated_at("2022-03-09 01:13:59")
geomaterials_updated_at <- function(strDate,...){
  df_out <- mindat_geomaterial_list(ids = c(''),updated_at = strDate,...)
  df_out
}

#' retrieve the geomaterials that have the given varietyof.
#' @description : Queries the list of geomaterials that have the given varietyof.
#' @usage geomaterials_varietyof(intvalue,...)
#' @param intvalue integer,
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve the localities that have the given varietyof.
#' @examples
#' geomaterials_varietyof(1720)
geomaterials_varietyof<- function(intvalue,...){
  df_out <- mindat_geomaterial_list(ids = c(''),varietyof = intvalue,...)
  df_out
}

#' retrieve the localities list that are belong to a given country.
#' @description : Queries the list of localities that are within a given country.
#' @usage localities_list_country(intvalue,...)
#' @param country name of country,
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of localities
#' @details
#'Enum: "Afghanistan" "Albania" "Algeria" "American Samoa" "Andorra" "Angola" "Anguilla"
#' "Antigua and Barbuda" "Argentina" "Armenia" "Aruba" "Ashmore and Cartier Islands" "Australia"
#' "Austria" "Azerbaijan" "Bahamas" "Bahrain" "Bangladesh" "Barbados" "Belarus" "Belgium" "Belize"
#' "Benin" "Bermuda" "Bhutan" "Bolivia" "Bosnia And Herzegovina" "Botswana" "Bouvet Island" "Brazil"
#' "British Indian Ocean Territories" "British Solomon Islands" "British Virgin Islands" "Brunei"
#' "Bulgaria" "Burkina Faso" "Burundi" "Cambodia" "Cameroon" "Canada" "Cape Verde" "Cayman Islands"
#' "Central African Republic" "Chad" "Chile" "China" "Christmas Island" "Cocos Islands" "Colombia"
#' "Comoro Islands" "Cook Islands" "Costa Rica" "Croatia" "Cuba" "Cyprus" "Czech Republic"
#' "Democratic Republic of the Congo" "Denmark" "Djibouti" "Dominica" "Dominican Republic" "East Timor"
#' "Ecuador" "Egypt" "El Salvador" "Equatorial Guinea" "Estonia" "Ethiopia" "Faeroe Islands" "Falkland Islands"
#' "Federated States of Micronesia" "Fiji" "Finland" "France" "French Guiana" "French Polynesia" "Gabon" "Gambia"
#' "Georgia" "Germany" "Ghana" "Gibraltar" "Greece" "Greenland" "Grenada" "Guadeloupe" "Guam" "Guatemala"
#' "Guernsey" "Guinea" "Guinea-Bissau" "Guyana" "Haiti" "Honduras" "Hong Kong" "Hungary" "Iceland" "India"
#' "Indonesia" "Iran" "Iraq" "Ireland" "Isle of Man" "Israel" "Italy" "Ivory Coast (Côte d'Ivoire)" "Jamaica"
#' "Japan" "Jersey" "Jordan" "Kazakhstan" "Kenya" "Kiribati " "Kosovo" "Kuwait" "Kyrgyzstan" "Laos" "Latvia"
#' "Lebanon" "Lesotho" "Liberia" "Libya" "Liechtenstein" "Lithuania" "Luxembourg" "Macao" "Madagascar" "Malawi"
#' "Malaysia" "Maldives" "Mali" "Malta" "Martinique" "Mauritania" "Mauritius" "Mexico" "Moldova" "Monaco" "Mongolia"
#' "Montenegro" "Montserrat" "Morocco" "Mozambique" "Myanmar" "Namibia" "Nauru" "Nepal" "Netherlands"
#' "Netherlands Antilles" "New Caledonia" "New Zealand" "Nicaragua" "Niger" "Nigeria" "North Korea" "Norway" "Oman"
#' "Pakistan" "Panama" "Papua New Guinea" "Paraguay" "Peru" "Philippines" "Poland" "Portugal" "Puerto Rico" "Qatar"
#' "Republic of Congo (Brazzaville)" "Republic of Macedonia" "Reunion Island" "Romania" "Russia" "Rwanda" "Saint Helena"
#' "Saint Lucia " "Saint Vincent and the Grenadines" "San Marino" "Sao Tome And Principe" "Saudi Arabia" "Senegal"
#' "Serbia" "Seychelles" "Sierra Leone" "Singapore" "Slovakia" "Slovenia" "Solomon Islands" "Somalia" "South Africa"
#' "South Korea" "Spain" "Sri Lanka" "St Christopher-Nevis Islands" "Sudan" "Suriname" "Swaziland" "Sweden" "Switzerland"
#' "Syria" "Taiwan" "Tajikistan" "Tanzania" "Thailand" "Togo" "Tonga" "Trinidad And Tobago" "Tunisia" "Turkey" "Turkmenistan"
#' "Turks And Caicos Islands" "Tuvalu" "U.S. Virgin Islands" "Uganda" "Ukraine" "United Arab Emirates" "United Kingdom"
#' "United States" "Uruguay" "Uzbekistan" "Vanuatu (Republic of Vanuatu; New Hebrides) " "Venezuela" "Vietnam" "Western Sahara"
#' "Western Samoa" "Yemen" "Zambia" "Zimbabwe"
#' @examples
#' localities_list_country("China")
localities_list_country<- function(country,...){
  df_out <- mindat_localities_list(ids = c(''),country = country,...)
  df_out
}

#' retrieve the localities that contain the given description
#' @description : Queries the list of localities that contain the given description.
#' @usage localities_list_description(desc,...)
#' @param desc string,
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of localities
#' @details
#' retrieve the localities that contain the given description
#' @examples
#' localities_list_description("Chinese")
localities_list_description<- function(desc,...){
  df_out <- mindat_localities_list(ids = c(''),description = desc,...)
  df_out
}

#' localities that do not contain the given elements
#' @description Queries the list of localities that do not contain the given elements.
#' @usage localities_list_elems_exc(exc_elems_list, ...)
#' @param exc_elems_list vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of localities
#' @details
#' This function queries the list of localities that do contain the specified elements.
#' @examples
#' localities_list_elems_exc(c('H','Be'))
localities_list_elems_exc<- function(exc_elems_list,...){
  df_out <- mindat_localities_list(ids = c(''),elements_exc = exc_elems_list,...)
  df_out
}

#' localities that contain the given elements
#' @description Queries the list of localities that contain the given elements.
#' @usage localities_list_elems_inc(inc_elems_list, ...)
#' @param inc_elems_list vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of localities
#' @details
#' This function queries the list of localities that contain the given elements.
#' @examples
#' localities_list_elems_inc(c('H','Be'))
localities_list_elems_inc<- function(inc_elems_list,...){
  df_out <- mindat_localities_list(ids = c(''),elements_exc = inc_elems_list,...)
  df_out
}

#' localities that contain the given elements but not contain some other given elements.
#' @description Queries the list of localities that contain the given elements,but not contain some other given elements.
#' @usage localities_list_elems_inc(inc_elems_list,exc_elems_list ...)
#' @param inc_elems_list vector of elements.
#' @param exc_elems_list vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of localities
#' @details
#' This function queries the list of localities that contain the given elements,but not contain some other given elements.
#' @examples
#' localities_list_elems_inc_exc(c('H','Be'),c('O'))
localities_list_elems_inc_exc <-function(inc_elems_list,exc_elems_list,...){
  df_out <- mindat_localities_list(ids = c(''),elements_inc = inc_elems_list, elements_exc = inc_elems_list,...)
  df_out
}


#' localities that contain the given expands.
#' @description Queries the list of localities that contain the given expands.
#' @usage localities_list_expand(expands,...)
#' @param expands vector of expands.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of localities
#' @details
#' Items Enum: "geomaterials" "~all" "*"
#' This function queries the list of localities that contain the given expands.
#' @examples
#' localities_list_expand(c("geomaterials"))
#' localities_list_expand(c("~all"))
localities_list_expand <-function(expands,...){
  df_out <- mindat_localities_list(ids = c(''),elements_inc = inc_elems_list, expand = expands,...)
  df_out
}

#' localities that contain the given txt name.
#' @description Queries the list of localities that contain the given txt name.
#' @usage localities_list_txt(txt,...)
#' @param txt string.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of localities
#' @details
#' This function queries the list of localities that contain the given txt name.
#' @examples
#' localities_list_txt(c("Chinese"))
localities_list_txt <-function(txt,...){
  df_out <- mindat_localities_list(ids = c(''),txt = txt,...)
  df_out
}

#' retrieve the localities list updated at the given time.
#' @description : Queries the list of localities that have the given time
#' @usage localities_list_updated_at(updateDate,...)
#' @param updateDate string<date-time>, Last updated datetime in format %Y-%m-%d %H:%M:%S
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of localities
#' @details
#' retrieve the localities that have the latest updated at the given time.
#' @examples
#' localities_list_updated_at("2022-03-09 01:13:59")
localities_list_updated_at<-function(updateDate,...){
  df_out <- mindat_localities_list(ids = c(''),updated_at = updateDate,...)
  df_out
}

#' retrieve the localities list.
#' @description : Queries the list of localities.
#' @usage localities_list_all(...)
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of localities
#' @details
#' retrieve all the localities.
#' @examples
#' localities_list_all()
localities_list_all<-function(...){
  df_out <- mindat_localities_list(ids = c(''),...)
  df_out
}

#' retrieve the localities by a given mindat id.
#' @description : Queries the localitiy by given id.
#' @usage localities_retrieve_id(id,...)
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of localities
#' @details
#' retrieve the localities by a given id.
#' @examples
#' localities_list_all(id =1)
localities_retrieve_id<-function(id,...){
  df_out <- mindat_localitiy(id,...)
  df_out
}
##############

#' locality_age
#' @description retrieve locality age by its id
#' @usage locality_age (id)
#' @param id the mindat localitiy id
#' @param ..., Further named parameters.
#' @return df, data frame of locality age.
#' @examples
#'  locality_age(id = 222)
#'  locality_age(id = 222, fields = "id,name")
locality_age<-function(id,...){
  l<-list(...)
  mindat_query('locality_age', query = c(list(id = id), l))
}

#' locality_age_list
#' @description retrieve all locality age list or by its conditions
#' @usage locality_age_list (...)
#' @param id the mindat localitiy age id
#' @param ..., Further named parameters.
#' @return df, data frame of locality age.
#' @examples
#'  locality_age_list()
#'  locality_age_list(fields = "id,name")
locality_age_list<-function(...){
  l<-list(...)
  mindat_query('locality_age_list', query = l)
}


#' localities_status_list
#' @description retrieve all locality status list.
#' @usage localities_status_list (...)
#' @param ..., Further named parameters.
#' @return df, data frame of locality age.
#' @examples
#'  localities_status_list()
#'  localities_status_list(fields = "id,name")
localities_status_list <-function(...){
  df_out <- mindat_locality_status_list(...)
  df_out
}

#' localities_status_retrieve
#' @description retrieve locality status by its id.
#' @usage localities_status_retrieve (...)
#' @param id the mindat localitiy status id
#' @param ..., Further named parameters.
#' @return df, data frame of locality status.
#' @examples
#'  localities_status_retrieve()
#'  localities_status_retrieve(fields = "id,name")
localities_status_retrieve<- function(id,...){
  df_out <- mindatnat_locality_status(id,...)
  df_out
}

#' localitiy_type_retrieve
#' @description retrieve locality type by its id.
#' @usage localitiy_type_retrieve (id,...)
#' @param id the mindat localitiy status id
#' @param ..., Further named parameters.
#' @return df, data frame of locality status.
#' @examples
#'  localitiy_type_retrieve(1)
#'  localitiy_type_retrieve(fields = "id,name")
localitiy_type_retrieve<- function(id,...){
  df_out <- mindat_locality_type(id,...)
  df_out
}

#' locality_type_list
#' @description retrieve all locality type list.
#' @usage locality_type_list (...)
#' @param ..., Further named parameters.
#' @return df, data frame of locality type.
#' @examples
#'  locality_type_list()
#'  locality_type_list(fields = "id,name")
locality_type_list <- function(...){
  df_out <- mindat_locality_type_list(...)
  df_out
}

#' minerals_ima_list
#' @description retrieve all mineral ima list.
#' @usage minerals_ima_list (...)
#' @param ..., Further named parameters.
#' @return df, data frame of locality type.
#' @examples
#'  minerals_ima_list()
#'  minerals_ima_list(fields = "id,name")
minerals_ima_list<- function(...){
  df_out <- mindat_mineral_ima_list(...)
  df_out
}

#' minerals_ima_list_expand
#' @description retrieve mineral ima list with the given expand.
#' @usage minerals_ima_list (expand,...)
#' @param expand description
#' @param ..., Further named parameters.
#' @return df, data frame of locality type.
#' @details
#' Items Enum: "~all" "*"
#'
#' @examples
#'  minerals_ima_list_expand("~all")
#'  minerals_ima_list_expand("~all",fields = "id,name")
minerals_ima_list_expand <- function(expand){
  df_out <-mindat_mineral_ima_list(ids = c(''),expand = expand,...)
  df_out
}

#' minerals_ima_list_ima
#' @description retrieve mineral ima list with the given intValue.
#' @usage minerals_ima_list_ima (intValue,...)
#' @param intValue Integer
#' @param ..., Further named parameters.
#' @return df, data frame of locality type.
#' @details
#' Integer
#' @examples
#'  minerals_ima_list_ima(1)
minerals_ima_list_ima<- function(intValue){
  df_out <-mindat_mineral_ima_list(ids = c(''),ima = intValue,...)
  df_out
}

#' retrieve the mineral_ima list updated at the given time.
#' @description : Queries the list of mineral_ima that have the given time
#' @usage minerals_ima_updated_at(updateDate,...)
#' @param updateDate string<date-time>, Last updated datetime in format %Y-%m-%d %H:%M:%S
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of localities
#' @details
#' retrieve the localities that have the latest updated at the given time.
#' @examples
#' minerals_ima_updated_at("2022-03-09 01:13:59")
minerals_ima_updated_at <- function(updateDate,...){
  df_out <-mindat_mineral_ima_list(ids = c(''),updated_at = updateDate,...)
  df_out
}

#' minerals_ima_retrieve
#' @description retrieve mineral ima by its id.
#' @usage minerals_ima_retrieve (id,...)
#' @param id the mindat ima id
#' @param ..., Further named parameters.
#' @return df, data frame of ima mineral by a given id.
#' @examples
#'  minerals_ima_retrieve(id = 1)
minerals_ima_retrieve <- function(id,...){
  df_out <- mindat_mineral_ima(id,...)
  df_out
}

########### mindat_geomaterials_tools.R #############
