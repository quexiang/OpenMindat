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
  mindat_api_endpoint('geomaterials_search', 'geomaterials_search/?%s',uri_builder = mindat_uri_builder)

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
#' @param base_uri base uri of mindat API.
#' @usage mindat_set_up_endpoints (base_uri)
#' @examples
#' mindat_setup('https://api.mindat.org')
mindat_setup<-function(base_uri = 'https://api.mindat.org'){
  set_api_base(base_uri)
  mindat_set_up_endpoints()
  mindat_cache_set('api_format', 'json')
}


########### mindat_rest_api.R ###########
