############ Initializing API Call ############
mindat_connection <- function(token, base_url = "https://api.mindat.org",fmt ="json"){
  set_api_token(token)
  mindat_setup(base_url)
}
############ Initializing API Call ############

############  mindat_cache.R  #################
mindat_cache <- new.env()

mindat_cache_set<-function(varname, value)
{
  assign(varname, value, envir = mindat_cache)
}

mindat_cache_get<-function(varname)
{
  return (get(varname, envir=mindat_cache))
}

mindat_cache_delete<-function(varname)
{
  assign(varname, NULL, envir = mindat_cache)
}

mindt_cache_empty<-function(){
  rm(list = ls(envir = mindat_cache))
}

mindat_cache_has <-function(varname)
{
  if(exists(varname, envir= mindat_cache)){

    return (!is.null(mindat_cache_get(varname)))
  }
  return (FALSE)
}

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
default_uri_builder<-function(api_base_uri,config, ..., querystring = ''){
  endpoint_base <- config[['endpoint_base']]
  uri <- paste(api_base_uri, '/', endpoint_base, sep = "")
  if(querystring != ''){
    uri <- paste(uri, querystring, sep="?")
  }
  uri
}

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
set_api_base<-function(api_base){
  mindat_cache_set('api_base', api_base)
}

set_api_token<-function(api_token){
  mindat_cache_set('api_token', api_token)
}

stop_missing_param<-function(compulsory_params, query){
  q_params <- names(query)
  for(c_param in compulsory_params) {
    if(!is.element(c_param, q_params)){
      stop(sprintf("Query string param '%s' is missing", c_param))
    }
  }
}
########### mindat_api_tools.R #############


########### mindat_rest_api.R ###########
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
mindat_setup<-function(bas_uri = 'https://api.mindat.org'){
  set_api_base(bas_uri)
  mindat_set_up_endpoints()
  mindat_cache_set('api_format', 'json')
}


########### mindat_rest_api.R ###########

########## mindat_network.R ###########
collapse_array_columns_map<- function (element){
  if (length (element) > 1){
    mapped<- paste (element, collapse=";")
  }else {
    mapped<- element
  }
  mapped
}

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
mindat_query<-function(endpoint, query = list()){
  query <- lapply(query, params_to_string)
  uri <- build_uri(endpoint, query = query)
  df <- mindat_get_data_from_uri(uri)
  df
}

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

mindat_mineral_ima<-function(id, ...){
  l<-list(...)
  mindat_query('minerals_ima', query = c(list(id = id), l))
}

mindat_mineral_ima_list<-function(...){
  l<-list(...)
  mindat_query('minerals_ima_list', query = l)
}

mindat_localities_list<-function(...){
  l<-list(...)
  mindat_query('localities_list', query = l)
}

mindat_localitiy<-function(id,...){
  l<-list(...)
  mindat_query('localities', query = c(list(id = id), l))
}

mindat_locality_age<-function(id,...){
  l<-list(...)
  mindat_query('locality_age', query = c(list(id = id), l))
}

mindat_locality_age_list<-function(...){
  l<-list(...)
  mindat_query('locality_age_list', query = l)
}

mindatnat_locality_status<-function(id,...){
  l<-list(...)
  mindat_query('locality_status', query = c(list(id = id), l))
}

mindat_locality_status_list<-function(...){
  l<-list(...)
  mindat_query('locality_status_list', query = l)
}

mindat_locality_type_list<-function(...){
  l<-list(...)
  mindat_query('locality_type_list',l)
}

mindat_locality_type<-function(id,...){
  l<-list(...)
  mindat_query('locality_type', query = c(list(id = id), l))
}

mindat_geomaterial<-function(id,...){
  l<-list(...)
  mindat_query('geomaterials', query  = c(list(id = id), l))
  # mindat_query('geomaterials', query  = c(list(id = id), l),compulsory_params = list('id'))
}

mindat_geomaterial_varieties<-function(id,...){
  l<-list(...)
  mindat_query('geomaterials_varieties', query  = c(list(id = id), l))
  # mindat_query('geomaterials', query  = c(list(id = id), l),compulsory_params = list('id'))
}

mindat_geomaterial_list<-function(...){
  l<-list(...)
  # if(length(l)){
  #   print(l)
  # }
  mindat_query('geomaterials_list', query = l)
}

mindat_country<-function(id,...){
  l<-list(...)
  mindat_query('countries', query  = c(list(id = id), l))#,compulsory_params = list('id')
}

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
geomaterials_contain_all_elems<- function(icl_elms_vector,...){
  df <- mindat_geomaterial_list(ids = c(''),elements_inc = icl_elms_vector,...)
  df
}
geomaterials_without_elems <- function(ecl_elms_vector,...){
  df <- mindat_geomaterial_list(ids = c(''),elements_exc = ecl_elms_vector,...)
  df
}
geomaterials_contain_all_and_without_elems <- function(icl_elm_vector,ecl_elms_vector,...){
  df <- mindat_geomaterial_list(ids = c(''),elements_inc = icl_elm_vector,elements_exc = ecl_elms_vector,...)
  df
}

geomaterials_contain_any_elems <- function(any_elems,...){
  df_out <- data.frame()
  for (elem in any_elems){
    df <- mindat_geomaterial_list(ids = c(''),elements_inc = c(elem),...)
    df_out <- rbind(df_out,df)
  }
  df_out
}

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

geomaterials_dens_greater_than<- function(gt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),density__from = gt,...)
  df_out
}

geomaterials_dens_less_than<- function(lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),density__to = lt,...)
  df_out
}

geomaterials_dens_range <- function(gt,lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),density__from = gt,density__to = lt,...)
  df_out
}

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

geomaterials_hardness_gt<- function(hmin,...){

  df_out <- mindat_geomaterial_list(ids = c(''),hardness__from = hmin,...)
  df_out

}

geomaterials_hardness_lt<- function(hmax,...){
  df_out <- mindat_geomaterial_list(ids = c(''),hardness__to = hmax,...)
  df_out
}

geomaterials_ima<- function(btrue =TRUE,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ima = btrue,...)
  df_out
}

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


geomeaterials_non_utf<- function(btrue =TRUE,...){
  df_out <- mindat_geomaterial_list(ids = c(''),non_utf = btrue,...)
  df_out
}

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

geomaterials_ri_gt <- function(value,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ri__from = value,...)
  df_out
}

geomaterials_ri_lt <- function(value,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ri__to = value,...)
  df_out
}

geomaterials_ri_range <- function(gt,lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ri__from = gt,ri__to = lt,...)
  df_out
}

geomaterials_streak <- function(str,...){
  df_out <- mindat_geomaterial_list(ids = c(''),steak = str,...)
  df_out
}

geomaterials_synid <- function(idnum,...){
  df_out <- mindat_geomaterial_list(ids = c(''),synid = idnum,...)
  df_out
}

geomaterials_updated_at <- function(strDate,...){
  df_out <- mindat_geomaterial_list(ids = c(''),updated_at = strDate,...)
  df_out
}

geomaterials_varietyof<- function(intvalue,...){

  df_out <- mindat_geomaterial_list(ids = c(''),varietyof = intvalue,...)
  df_out
}

localities_list_country<- function(country,...){
  df_out <- mindat_localities_list(ids = c(''),country = intvalue,...)
  df_out
}

localities_list_description<- function(desc,...){
  df_out <- mindat_localities_list(ids = c(''),description = desc,...)
  df_out
}

localities_list_elems_exc<- function(exc_elems_list,...){
  df_out <- mindat_localities_list(ids = c(''),elements_exc = exc_elems_list,...)
  df_out
}

localities_list_elems_inc<- function(inc_elems_list,...){
  df_out <- mindat_localities_list(ids = c(''),elements_exc = inc_elems_list,...)
  df_out
}

localities_list_elems_inc_ecl <-function(inc_elems_list,exc_elems_list,...){
  df_out <- mindat_localities_list(ids = c(''),elements_inc = inc_elems_list, elements_exc = inc_elems_list,...)
  df_out
}

localities_list_expand <-function(expands,...){
  df_out <- mindat_localities_list(ids = c(''),elements_inc = inc_elems_list, expand = expands,...)
  df_out
}

localities_list_txt <-function(txt,...){
  df_out <- mindat_localities_list(ids = c(''),txt = txt,...)
  df_out
}

localities_list_updated_at<-function(updateDate,...){
  df_out <- mindat_localities_list(ids = c(''),updated_at = updateDate,...)
  df_out
}

localities_list_all<-function(...){
  df_out <- mindat_localities_list(ids = c(''),...)
  df_out
}

localities_retrieve_id<-function(id,...){
  df_out <- mindat_localitiy(id,...)
  df_out
}
##############
locality_age_list <-function(...){
  df_out <- mindat_locality_age_list(...)
  df_out
}
locality_age_retrieve_id<-function(id,...){
  df_out <- mindat_locality_age(id,...)
  df_out
}

localities_status_list <-function(...){
  df_out <- mindat_locality_status_list(...)
  df_out
}
localities_status_retrieve<- function(id,...){
  df_out <- mindatnat_locality_status(id,...)
  df_out
}

localitiy_type_retrieve<- function(id,...){
  df_out <- mindat_locality_type(id,...)
  df_out
}

locality_type_list <- function(...){
  df_out <- mindat_locality_type_list(...)
  df_out
}

minerals_ima_list<- function(...){
  df_out <- mindat_mineral_ima_list(...)
  df_out
}

minerals_ima_list_expand <- function(expand){
  df_out <-mindat_mineral_ima_list(ids = c(''),expand = expand,...)
  df_out
}
minerals_ima_list_ima<- function(intValue){
  df_out <-mindat_mineral_ima_list(ids = c(''),ima = intValue,...)
  df_out
}
minerals_ima_updated_at <- function(updateDate,...){
  df_out <-mindat_mineral_ima_list(ids = c(''),updated_at = updateDate,...)
  df_out
}
minerals_ima_retrieve <- function(id,...){
  df_out <- mindat_mineral_ima(id,...)
  df_out
}



########### mindat_geomaterials_tools.R #############
