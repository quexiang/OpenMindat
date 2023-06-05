################ mindat_ima_minerals.R  #################

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

################ mindat_ima_minerals.R  #################
