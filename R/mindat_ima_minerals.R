################ mindat_ima_minerals.R  #################

#' minerals_ima_list
#' @description retrieve all mineral ima list.
#' @usage minerals_ima_list (...)
#' @param ..., Further named parameters.
#' @return df, data frame of minerals.
#' @examples
#' \dontrun{
#' df <-minerals_ima_list()
#' }
#' @details
#' This function is to retrieve the IMA minerals list.
#' @export
minerals_ima_list<- function(...){
  df_out <- mindat_mineral_ima_list(...)
  df_out
}

#' minerals_ima_list_expand
#' @description retrieve mineral ima list with the given expand.
#' @usage minerals_ima_list_expand (expand,...)
#' @param expand description
#' @param ..., Further named parameters.
#' @return df, data frame of ima minerals with expanded fields.
#' @examples
#' \dontrun{
#' df <-minerals_ima_list_expand("~all")
#' }
#' @details
#' This function is related to the filed "expand" of ima mineral.
#' Items Enum: "~all" "*"
#' @export
minerals_ima_list_expand <- function(expand,...){
  query_expand <- c(expand, list(...))
  df_out <-mindat_mineral_ima_list(ids = c(''),expand = query_expand)
  df_out
}

#' minerals_ima_list_ima
#' @description retrieve mineral ima list with the given intValue.
#' @usage minerals_ima_list_ima (intValue,...)
#' @param intValue Integer
#' @param ..., Further named parameters.
#' @return df, data frame of locality type.
#' @examples
#' \dontrun{
#' df <-minerals_ima_list_ima(1)
#' }
#' @details
#' This function is related to the filed "ima" of ima minerals.
#' Integer. 0: "PENDING_PUBLICATION"
#'          1: "APPROVED"
#' @export
minerals_ima_list_ima<- function(intValue,...){
  df_out <-mindat_mineral_ima_list(ids = c(''),ima = intValue,...)
  df_out
}

#' retrieve the mineral_ima list updated at the given time.
#' @description : Queries the list of mineral_ima that have the given time
#' @usage minerals_ima_updated_at(updateDate,...)
#' @param updateDate str, Last updated datetime in format %Y-%m-%d %H:%M:%S
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of localities
#' @examples
#' \dontrun{
#' strdate<- "2023-09-13 17:36:19"
#' df <-minerals_ima_updated_at(strdate)
#' }
#' @details
#' This function is related to the filed "updated_at" of ima minerals.
#' retrieve the localities that have the latest updated at the given time.
#' @export
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
#' \dontrun{
#' df <-minerals_ima_retrieve(3337)
#' }
#' @details
#' This function is related to the filed "id" of ima minerals.
#' @export
minerals_ima_retrieve <- function(id,...){
  df_out <- mindat_mineral_ima(id,...)
  df_out
}

################ mindat_ima_minerals.R  #################
