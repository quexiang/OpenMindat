########### mindat_querys.R #############
#' mindat_query
#' @description Basic function for query dataset at a specified endpoit.
#' @usage mindat_query (endpoint, query = list())
#' @param endpoint query endpoint, e.g.'minerals_ima'.
#' @param query list for query conditions.
#' @return df query resutls in data frame format.
#' @examples
#' \dontrun{
#' df <-mindat_query("geomaterials_list",list(ids="",hardness_min=9))
#' }
#' @export
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
#' \dontrun{
#'  params_to_string("")
#' }
#' @export
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
#' @usage mindat_mineral_ima (id,...)
#' @param id mindat id
#' @param ..., Further parameters.
#' @return df. query resutls in data frame format.
#' @examples
#' \dontrun{
#'  df<- mindat_mineral_ima(3337)
#' }
#' @export
mindat_mineral_ima<-function(id,...){
  l<-list(...)
  mindat_query('minerals_ima', query = c(list(id = id), l))
}

#' mindat_mineral_ima_list
#' @description retrieve ima mineral list
#' @usage mindat_mineral_ima_list (...)
#' @param ... , Further named parameters.
#' @return df, data frame of mineral list.
#' @examples
#' \dontrun{
#'  df<- mindat_mineral_ima_list()
#' }
#' @export
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
#' \dontrun{
#'  df<- mindat_localities_list()
#' }
#' @export
mindat_localities_list<-function(...){
  l<-list(...)
  mindat_query('localities_list', query = l)
}

#' mindat_localitiy
#' @description retrieve locality by its id
#' @usage mindat_localitiy (id,...)
#' @param id the mindat localitiy id
#' @param ..., Further named parameters.
#' @return df, data frame of locality
#' @examples
#' \dontrun{
#'  df<- mindat_localitiy(3337)
#' }
#' @export
mindat_localitiy<-function(id,...){
  l<-list(...)
  mindat_query('localities', query = c(list(id = id), l))
}

#' mindat_locality_status
#' @description retrieve all locality status by its id
#' @usage mindat_locality_status (id,...)
#' @param id the mindat localitiy status id.
#' @param ..., Further named parameters.
#' @return df, data frame of locality status
#' @examples
#' \dontrun{
#'  df<- mindat_locality_status(10)
#' }
#' @export
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
#' \dontrun{
#'  df<- mindat_locality_status_list()
#' }
#' @export
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
#' \dontrun{
#'  df<- mindat_locality_type_list()
#' }
#' @export
mindat_locality_type_list<-function(...){
  l<-list(...)
  mindat_query('locality_type_list',l)
}

#' mindat_locality_type
#' @description retrieve  locality type by its id
#' @usage mindat_locality_type (id,...)
#' @param id locality type id
#' @param ..., Further named parameters.
#' @return df, data frame of locality type list
#' @examples
#' \dontrun{
#'  df<- mindat_locality_type(50)
#' }
#' @export
mindat_locality_type<-function(id,...){
  l<-list(...)
  mindat_query('locality_type', query = c(list(id = id), l))
}


#' mindat_geomaterial
#' @description retrieve  geomaterial  by its id
#' @usage mindat_geomaterial (id,...)
#' @param id geomaterial id
#' @param ..., Further named parameters.
#' @return df, data frame of locality type list
#' @examples
#' \dontrun{
#'  df<- mindat_geomaterial(3337)
#' }
#' @export
mindat_geomaterial<-function(id,...){
  l<-list(...)
  mindat_query('geomaterials', query  = c(list(id = id), l))
  # mindat_query('geomaterials', query  = c(list(id = id), l),compulsory_params = list('id'))
}

#' mindat_geomaterial_varieties
#' @description retrieve the geomaterial varieties by the id of geomaterial.
#' @usage mindat_geomaterial_varieties (id,...)
#' @param id geomaterial id
#' @param ..., Further named parameters.
#' @return df, data frame of locality type list
#' @examples
#' \dontrun{
#'  df<- mindat_geomaterial_varieties(3337)
#' }
#' @export
mindat_geomaterial_varieties<-function(id,...){
  l<-list(...)
  mindat_query('geomaterials_varieties', query  = c(list(id = id), l))
  # mindat_query('geomaterials', query  = c(list(id = id), l),compulsory_params = list('id'))
}

#' mindat_geomaterial_list
#' @description retrieve all the geomaterial list or the geomaterial by given conditions.
#' @usage mindat_geomaterial_list(...)
#' @param ..., Further named parameters.
#' @return df, data frame of locality type list
#' @examples
#' \dontrun{
#'  df<- mindat_geomaterial_list()
#' }
#' @export
mindat_geomaterial_list<-function(...){
  l<-list(...)
  # if(length(l)){
  #   print(l)
  # }
  mindat_query('geomaterials_list', query = l)
}

#' mindat_geomaterial_search
#' @description retrieve all the geomaterial list or the geomaterial by given conditions.
#' @usage mindat_geomaterial_search (...)
#' @param ..., Further named parameters.
#' @return df, data frame of geomaterials mathch the search
#' @examples
#' \dontrun{
#'  df<- mindat_geomaterial_search(q="Quartz")
#' }
#' @export
mindat_geomaterial_search<-function(...){
  l<-list(...)
  # if(length(l)){
  #   print(l)
  # }
  mindat_query('geomaterials_search', query = l)
}

#' mindat_country
#' @description retrieve the country by given its id.
#' @usage mindat_country (id,...)
#' @param id, country id in mindat.
#' @param ..., Further named parameters.
#' @return df, a data frame of country
#' @examples
#' \dontrun{
#'  df<- mindat_country(1)
#' }
#' @export
mindat_country<-function(id,...){
  l<-list(...)
  mindat_query('countries', query  = c(list(id = id), l))#,compulsory_params = list('id')
}

#' mindat_countries
#' @description retrieve all countries list or the contries by given conditions.
#' @usage mindat_countries (...)
#' @param ..., Further named parameters.
#' @return df, data frame of countries list
#' @examples
#' \dontrun{
#'  df<- mindat_countries()
#' }
#' @export
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
