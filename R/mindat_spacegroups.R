########### mindat_spacegroups.R #############

#' spacegroups that match a given spacegroup ID (integer)
#' @description : Queries a list of spacegroup that match a given spacegroup ID
#' @usage spacegroups_by_id(spacegroup_id, ...)
#' @param spacegroup_id integer spacegroup ID .
#' The field "spacegroup_id" is a integer of spacegroup ID.
#' @param ..., Further parameters like "sgtext"(space group text) .Other optional arguments-Additional arguments.
#' @return df, a data frame of spacegroup
#' @examples
#' \dontrun{
#' df <-spacegroups_by_id(2)
#' }
#' @details
#' This function filter data by a given given spacegroup ID.
#' @export
spacegroups_by_id <- function(spacegroup_id,...){
  df_out <- mindat_spacegroups(id = spacegroup_id,...)
  df_out
}

#' return a full list of spacegroups
#' @description : Queries a full list of spacegroup
#' @usage spacegroups_list(...)
#' @param ..., Further parameters like "sgtext"(space group text) .Other optional arguments-Additional arguments.
#' @return df, a data frame of the full list of spacegroups
#' @examples
#' \dontrun{
#' df <-spacegroups_list()
#' }
#' @details
#' This function return a full list of spacegroups.
#' @export
spacegroups_list <- function(...){
  df_out <- mindat_spacegroups_list(...)
  df_out
}


#' spacegroups that match a given crystalclass ID (integer)
#' @description : Queries a list of spacegroup that match a given crystalclass ID
#' @usage spacegroups_cclass(crystalclass_id, ...)
#' @param crystalclass_id integer crystalclass ID .
#' The field "crystalclass_id" is a integer of crystalclass ID.
#' @param ..., Further parameters like "sgtext"(space group text) .Other optional arguments-Additional arguments.
#' @return df, a data frame of spacegroup
#' @examples
#' \dontrun{
#' df <-spacegroups_cclass(2)
#' }
#' @details
#' This function filter data by a given given crystalclass ID.
#' @export
spacegroups_cclass <- function(crystalclass_id,...){
    df_out <- mindat_spacegroups_list(ids = c(''),cclass = crystalclass_id,...)
    df_out
}


#' spacegroups that match a given sgtext (string)
#' @description : Queries a list of spacegroups that match a given sgtext (string)
#' @usage spacegroups_sgtext(sgtext, ...)
#' @param sgtext string space group text (case-insensitive).
#' The field "sgtext" is a string of space group text.
#' @param ..., Further parameters like "cclass"(Crystalclass) .Other optional arguments-Additional arguments.
#' @return df, a data frame of spacegroups
#' @examples
#' \dontrun{
#' df <-spacegroups_sgtext("P1")
#' }
#' @details
#' This function filter data by a given given crystalclass ID.
#' @export
spacegroups_sgtext <- function(sgtext,...){
  df_out <- mindat_spacegroups_list(ids = c(''),sgtext = sgtext,...)
  df_out
}


#' spacegroupsets that match a given spacegroupets ID (integer)
#' @description : Queries a list of spacegroupsets that match a given spacegroupets ID
#' @usage spacegroupsets_by_id(spacegroupsets_id, ...)
#' @param spacegroupsets_id integer spacegroup ID .
#' The field "spacegroupets_id" is a integer of spacegroupets ID.
#' @param ..., Further parameters like "sgtext"(space group text) .Other optional arguments-Additional arguments.
#' @return df, a data frame of spacegroupsets
#' @examples
#' \dontrun{
#' df <-spacegroupsets_by_id(2)
#' }
#' @details
#' This function filter data by a given given spacegroup ID.
#' @export
spacegroupsets_by_id <- function(spacegroupsets_id,...){
  df_out <- mindat_spacegroupsets(id = spacegroupsets_id,...)
  df_out
}

#' return a full list of spacegroupsetsets
#' @description : Queries a full list of spacegroupsets
#' @usage spacegroupsets_list(...)
#' @param ..., Further parameters like "sgtext"(space group text) .Other optional arguments-Additional arguments.
#' @return df, a data frame of the full list of spacegroupsets
#' @examples
#' \dontrun{
#' df <-spacegroupsets_list()
#' }
#' @details
#' This function return a full list of spacegroupsets.
#' @export
spacegroupsets_list <- function(...){
  df_out <- mindat_spacegroupsets_list(...)
  df_out
}


#' spacegroupsets that match a given crystalclass ID (integer)
#' @description : Queries a list of spacegroupsets that match a given crystalclass ID
#' @usage spacegroupsets_cclass(crystalclass_id, ...)
#' @param crystalclass_id integer crystalclass ID .
#' The field "crystalclass_id" is a integer of crystalclass ID.
#' @param ..., Further parameters like "sgtext"(space group text) .Other optional arguments-Additional arguments.
#' @return df, a data frame of spacegroup
#' @examples
#' \dontrun{
#' df <-spacegroupsets_cclass(2)
#' }
#' @details
#' This function filter data by a given given crystalclass ID.
#' @export
spacegroupsets_cclass <- function(crystalclass_id,...){
  df_out <- mindat_spacegroupsets_list(ids = c(''),cclass = crystalclass_id,...)
  df_out
}


#' spacegroupsets that match a given sgtext (string)
#' @description : Queries a list of spacegroupsets that match a given sgtext (string)
#' @usage spacegroupsets_sgtext(sgtext, ...)
#' @param sgtext string space group text (case-insensitive).
#' The field "sgtext" is a string of space group text.
#' @param ..., Further parameters like "cclass"(Crystalclass) .Other optional arguments-Additional arguments.
#' @return df, a data frame of spacegroupsets
#' @examples
#' \dontrun{
#' df <-spacegroupsets_sgtext("P1")
#' }
#' @details
#' This function filter data by a given given crystalclass ID.
#' @export
spacegroupsets_sgtext <- function(sgtext,...){
  df_out <- mindat_spacegroupsets_list(ids = c(''),sgtext = sgtext,...)
  df_out
}

########### mindat_spacegroups.R #############
