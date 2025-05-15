########### mindat_crystalclasses.R #############
#' crystalclasses that match a given vector of symbols (case-insenstive)
#' @description : Queries a list of crystalclasses that match a given list of symbols
#' @usage crystalclasses_symbols(symbols, ...)
#' @param symbols vector of given crystals (array of strings or null).
#' The field "symbol" is to describe the symbol of Crystal class dictionary.
#' @param ..., Further parameters like "system"(crystal system) .Other optional arguments-Additional arguments.
#' @return df, a data frame of crystalclasses
#' @examples
#' \dontrun{
#' df <-crystalclasses_symbols(c("2/m","mm2"))
#' }
#' @details
#' This function filter data by a given list of symbols of crystal class dictionary
#' case-insensitive
#' @export
crystalclasses_symbols <- function(symbols,...){
  if(length(symbols)>1){
    df_out <- data.frame()
    for (symbol in symbols){
      df <- mindat_crystalclass_list(ids = c(''),symbol = c(symbol),...)
      df_out <- rbind(df_out,df)
    }
    df_out
  }
  else{
    df_out <- mindat_crystalclass_list(ids = c(''),symbol = symbols,...)
    df_out
  }
}

#' crystalclasses that match a given vector of crystal system (case-insenstive)
#' @description : Queries a list of crystalclasses that match a given list of crystal system.
#' @usage crystalclasses_systems(systems, ...)
#' @param systems vector of given system.
#'"crystal system of the mineral;
#'"Amorphous","Hexagonal","Icosahedral","Isometric","Monoclinic","Orthorhombic","Tetragonal","Triclinic","Trigonal"
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of crystalclasses
#' @examples
#' \dontrun{
#' df <-geomaterials_crystal_system(c("Icosahedral"))
#' }
#' @details
#' This function filter data by a given list of crystal_system of crystal class dictionary
#' case-insensitive
#' @export
crystalclasses_systems <- function(systems,...){
  if(length(systems)>1){
    df_out <- data.frame()
    for (system in systems){
      df <- mindat_crystalclass_list(ids = c(''),system = c(system),...)
      df_out <- rbind(df_out,df)
    }
    df_out
  }
  else{
    df_out <- mindat_crystalclass_list(ids = c(''),system = systems,...)
    df_out
  }
}
########### mindat_crystalclasses.R #############
