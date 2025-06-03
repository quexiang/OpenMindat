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

#' mindat_locentries_list
#' @description retrieve Mindat locentries. A 'locentry' is a record of specific geomaterial (mineral, etc) at a specific locality.
#' @usage mindat_locentries_list (...)
#' @param ..., Further named parameters.
#' @return df, data frame of locentries list.
#' @examples
#' \dontrun{
#'  df<- mindat_locentries_list()
#' }
#' @export
mindat_locentries_list<-function(...){
  l<-list(...)
  mindat_query('locentries_list', query = l)
}

#' mindat_locentries_retrieve
#' @description retrieve Mindat locentries by a given ID. A 'locentry' is a record of specific geomaterial (mineral, etc) at a specific locality.
#' @usage mindat_locentries_retrieve (id,...)
#' @param id Locentry id (Integer)
#' @param ..., Further named parameters.
#' @return df, data frame of locentries list.
#' @examples
#' \dontrun{
#'  df<- mindat_locentries_retrieve(2)
#' }
#' @export
mindat_locentries_retrieve<-function(id,...){
  l<-list(...)
  mindat_query('locentries', query = c(list(id = id), l))
}

#' mindat_locentries_stat
#' @description retrieve Mindat locality-geomaterial pairs and some statistics list.
#' @usage mindat_locentries_stat (...)
#' @param ..., Further named parameters.
#' @return df, data frame of locentries statstics list.
#' @examples
#' \dontrun{
#'  df<- mindat_locentries_stat()
#' }
#' @export
mindat_locentries_stat<-function(...){
  l<-list(...)
  mindat_query('locentries_statistics_list', query = l)
}

#' mindat_locentries_lstm_id
#' @description retrieve Mindat locality-geomaterial pairs and some statistics list.
#' @usage mindat_locentries_lstm_id (id,...)
#' @param id A unique integer value identifying this locstatsmin.
#' @param ..., Further named parameters.
#' @return df, data frame of locentries statstics list.
#' @examples
#' \dontrun{
#'  df<- mindat_locentries_lstm_id(2)
#' }
#' @export
mindat_locentries_lstm_id<-function(id,...){
  l<-list(...)
  mindat_query('locentries_statistics', query = c(list(id = id), l))
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
#' @param ..., Further named parameters (e:Exact.If 1 returns only exact matech;ima:if 1 returns only ima-approved minerals;size:limit of returned records).
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

#' mindat_crystalclasses
#' @description retrieve crystalclasses by its id
#' @usage mindat_crystalclasses (id,...)
#' @param id crystalclasses id
#' @param ..., Further named parameters.
#' @return df, data frame of crystalclasses
#' @examples
#' \dontrun{
#'  df<- mindat_crystalclasses(4)
#' }
#' @export
mindat_crystalclasses<-function(id,...){
  l<-list(...)
  mindat_query('crystalclasses', query  = c(list(id = id), l))
}

#' mindat_crystalclass_list
#' @description retrieve all the crystalclasses list or the crystalclasses list by given conditions.
#' @usage mindat_crystalclass_list(...)
#' @param ..., Further named parameters.
#' @return df, data frame of crystalclasses list
#' @examples
#' \dontrun{
#'  df<- mindat_crystalclass_list()
#' }
#' @export
mindat_crystalclass_list<-function(...){
  l<-list(...)
  mindat_query('crystalclasses_list', query = l)
}


#' mindat_spacegroups
#' @description retrieve spacegroups by its id
#' @usage mindat_spacegroups (id,...)
#' @param id spacegroups id
#' @param ..., Further named parameters.
#' @return df, data frame of spacegroups
#' @examples
#' \dontrun{
#'  df<- mindat_spacegroups(4)
#' }
#' @export
mindat_spacegroups<-function(id,...){
  l<-list(...)
  mindat_query('spacegroups', query  = c(list(id = id), l))
}

#' mindat_spacegroups_list
#' @description retrieve all the spacegroups list or the spacegroups list by given conditions.
#' @usage mindat_spacegroups_list(...)
#' @param ..., Further named parameters.
#' @return df, data frame of spacegroups list
#' @examples
#' \dontrun{
#'  df<- mindat_spacegroups_list()
#' }
#' @export
mindat_spacegroups_list<-function(...){
  l<-list(...)
  mindat_query('spacegroups_list', query = l)
}


#' mindat_spacegroupsets
#' @description retrieve spacegroups by its id
#' @usage mindat_spacegroupsets (id,...)
#' @param id spacegroupsets id
#' @param ..., Further named parameters.
#' @return df, data frame of spacegroupsets
#' @examples
#' \dontrun{
#'  df<- mindat_spacegroupsets(4)
#' }
#' @export
mindat_spacegroupsets<-function(id,...){
  l<-list(...)
  mindat_query('spacegroupsets', query  = c(list(id = id), l))
}

#' mindat_spacegroupsets_list
#' @description retrieve all the spacegroups list or the spacegroups list by given conditions.
#' @usage mindat_spacegroupsets_list(...)
#' @param ..., Further named parameters.
#' @return df, data frame of spacegroupsets list
#' @examples
#' \dontrun{
#'  df<- mindat_spacegroups_list()
#' }
#' @export
mindat_spacegroupsets_list<-function(...){
  l<-list(...)
  mindat_query('spacegroupsets_list', query = l)
}

#' mindat_dana8_groups
#' @description retrieve all the classifications of dana8.
#' @usage mindat_dana8_groups(...)
#' @param ..., Further named parameters.
#' @return df, data frame of dana8 classification list
#' @examples
#' \dontrun{
#'  df<- mindat_dana8_groups()
#' }
#' @export
mindat_dana8_groups<-function(...){
  l<-list(...)
  mindat_query('dana-8/groups', query = l)
}


#' mindat_dana8_subgroups
#' @description retrieve all the subgroups of dana8.
#' @usage mindat_dana8_subgroups(...)
#' @param ..., Further named parameters.
#' @return df, data frame of subgroups of dana8 classification.
#' @examples
#' \dontrun{
#'  df<- mindat_dana8_subgroups()
#' }
#' @export
mindat_dana8_subgroups<-function(...){
  l<-list(...)
  mindat_query('dana-8/subgroups', query = l)
}

#' mindat_nickel_strunz10_classes
#' @description retrieve the class list of Nickel-Strunz 10th edition classifications.
#' @usage mindat_nickel_strunz10_classes(...)
#' @param ..., Further named parameters.
#' @return df, data frame of classes of Nickel-Strunz 10th edition classifications.
#' @examples
#' \dontrun{
#'  df<- mindat_nickel_strunz10_classes()
#' }
#' @export
mindat_nickel_strunz10_classes<-function(...){
  l<-list(...)
  mindat_query('nickel-strunz-10/classes', query = l)
}

#' mindat_nickel_strunz10_subclasses
#' @description retrieve the subclass list of Nickel-Strunz 10th edition classifications.
#' @usage mindat_nickel_strunz10_subclasses(...)
#' @param ..., Further named parameters.
#' @return df, data frame of subclasses of Nickel-Strunz 10th edition classifications.
#' @examples
#' \dontrun{
#'  df<- mindat_nickel_strunz10_subclasses()
#' }
#' @export
mindat_nickel_strunz10_subclasses<-function(...){
  l<-list(...)
  mindat_query('nickel-strunz-10/subclasses', query = l)
}


#' mindat_nickel_strunz10_families
#' @description retrieve the families list of Nickel-Strunz 10th edition classifications.
#' @usage mindat_nickel_strunz10_families(...)
#' @param ..., Further named parameters.
#' @return df, data frame of families of Nickel-Strunz 10th edition classifications.
#' @examples
#' \dontrun{
#'  df<- mindat_nickel_strunz10_families()
#' }
#' @export
mindat_nickel_strunz10_families<-function(...){
  l<-list(...)
  mindat_query('nickel-strunz-10/families', query = l)
}

########### mindat_querys.R #############
