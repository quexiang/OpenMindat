########### mindat_geomaterials.R #############
#' geomaterials_contain_all_elems
#' @description retrieve the geomaterials that contain all of the elements.This function queries
#' the list of geomaterials that contain all the given elements.
#' It performs the query operation by calling the mindat_geomaterial_list function
#' @usage geomaterials_contain_all_elems(icl_elms_vector,...)
#' @param icl_elms_vector, vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments that can be
#' passed to the mindat_geomaterial_list function.
#' @return df, a data frame of geomaterials list.
#' @examples
#' \dontrun{
#' df <-geomaterials_contain_all_elems (c('Fe','S'),fields ="id,name,mindat_formula,elements")
#' }
#' @details
#' This function releated to the field "elements_inc" of geomaterials.
#' @export
geomaterials_contain_all_elems<- function(icl_elms_vector,...){
  df <- mindat_geomaterial_list(ids = c(''),elements_inc = icl_elms_vector,...)
  df
}

#' geomaterials that do not contain the elements
#' @description retrieve the geomaterials that do not contain any of the given elements.
#' @usage geomaterials_not_contain_elems (ecl_elms_vector, ...)
#' @param ecl_elms_vector, vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of geomaterials list.
#' @examples
#' \dontrun{
#' df <-geomaterials_not_contain_elems (c('Fe','S', 'O'), fields ="id,name,mindat_formula,elements")
#' }
#' @details
#' This function releated to the field "elements_exc" of geomaterials.
#' @export
geomaterials_not_contain_elems <- function(ecl_elms_vector,...){
  df <- mindat_geomaterial_list(ids = c(''),elements_exc = ecl_elms_vector,...)
  df
}

#' geomaterials that contain all of some given elements but without any of some other given elements.
#' @description Queries the list of geomaterials that contain all the given elements listed in icl_elm_vector,
#' but do not contain the given elements listed in ecl_elms_vector.
#' @usage geomaterials_contain_all_but_not_elems(icl_elm_vector, ecl_elms_vector, ...)
#' @param icl_elm_vector vector of elements.
#' @param ecl_elms_vector vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of geomaterials.
#' @examples
#' \dontrun{
#' df<-geomaterials_contain_all_but_not_elems (c('Fe','S'), c('O')) +
#'    geomaterials_contain_all_but_not_elems(fields ="id,name,mindat_formula,elements")
#' }
#' @details
#' This function releated to the field "elements_inc" and "elements_exc" of geomaterials.
#' This function queries the list of geological materials that contain an given list of elements (icl_elm_vector),
#' but not contain the other list of elements (ecl_elms_vector).
#' It performs the query operation by calling the mindat_geomaterial_list function.
#' @export
geomaterials_contain_all_but_not_elems <- function(icl_elm_vector,ecl_elms_vector,...){
  df <- mindat_geomaterial_list(ids = c(''),elements_inc = icl_elm_vector,elements_exc = ecl_elms_vector,...)
  df
}

#' geomaterials that contain any one of the given elements
#' @description : Queries the list of geological materials that contain any one of the given elements.
#' @usage geomaterials_contain_any_elems(any_elems, ...)
#' @param any_elems vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of geomaterials.
#' @examples
#' \dontrun{
#' df <-geomaterials_contain_any_elems (c('Fe','S'),fields ="id,name,mindat_formula,elements")
#' }
#' @details
#' This function releated to the field "elements_inc" of geomaterials.
#' This function queries the list of geological materials that contain any element of an given list (any_elems).
#' It performs the query operation by looping through each given element and calling the mindat_geomaterial_list function.
#' @export
geomaterials_contain_any_elems <- function(any_elems,...){
  df_out <- data.frame()
  for (elem in any_elems){
    df <- mindat_geomaterial_list(ids = c(''),elements_inc = c(elem),...)
    df_out <- unique(rbind(df_out,df))
  }
  df_out
}

#' geomaterials that contain any of some given elements but with out any of some other given elements
#' @description : Queries the list of geological materials that contain any one of the given elements.
#' @usage geomaterials_contain_any_but_not_elems(any_elems_vector,ecl_elms_vector,...)
#' @param any_elems_vector vector of elements. vector of any elements contained.
#' @param ecl_elms_vector  vector of elements. vector of any elements excluded.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of geomaterials.
#' @examples
#' \dontrun{
#' df <-geomaterials_contain_any_but_not_elems(c('Fe','S'), c('O'))
#' }
#' @details
#' This function releated to the field "elements_inc" of geomaterials.
#' This function queries the list of geological materials that contain any element of an given list (any_elems).
#' It performs the query operation by looping through each given element and calling the mindat_geomaterial_list function.
#' @export
geomaterials_contain_any_but_not_elems <- function(any_elems_vector,ecl_elms_vector,...){
  df_out <- data.frame()
  for (elem in any_elems_vector){
    df <- mindat_geomaterial_list(ids = c(''),elements_inc = c(elem),elements_exc = ecl_elms_vector,...)
    df_out <- unique(rbind(df_out,df))
  }
  df_out
}

#' geomaterials_contain_only_elems
#' @description retrieve the geomaterials that only contain elements in an given list (icl_only_elms_vector).
#' @usage geomaterials_contain_only_elems (icl_only_elms_vector,...)
#' @param icl_only_elms_vector, vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments that can be
#' passed to the mindat_geomaterial_list function.
#' @return df, a data frame of geomaterials.
#' @examples
#' \dontrun{
#' df <-geomaterials_contain_only_elems(c('Fe','S'),fields ="id,name,mindat_formula,elements")
#' }
#' @details
#' This function releated to the fields "elements_inc" and "elements_exc" of geomaterials.
#' Here is a list of all elements that can make up geomaterials:'H','Li', 'Be', 'B', 'C', 'N', 'O', 'F',
#' 'Na', 'Mg', 'Al','Si', 'P', 'S','Cl',K', 'Ca', 'Sc', 'Ti', 'V', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn',
#' 'Ga', 'Ge','As','Se', 'Br', 'Rb', 'Sr', 'Y', 'Zr', 'Nb', 'Mo', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd','In', 'Sn',
#' 'Sb', 'Te', 'I', 'Cs',Ba', 'La', 'Ce', 'Nd', 'Sm', 'Gd', 'Dy', 'Er','Yb', 'Hf', 'Ta', 'W', 'Re', 'Os',
#' 'Ir', 'Pt', 'Au','Hg','Tl', 'Pb', 'Bi', 'Th', 'U'
#'  It performs the query operation by calling the mindat_geomaterial_list function
#' @export
geomaterials_contain_only_elems<- function(icl_only_elms_vector,...){
  all_mineral_vector <- c('H','Li', 'Be', 'B', 'C', 'N', 'O', 'F', 'Na', 'Mg', 'Al','Si', 'P', 'S','Cl',
     'K', 'Ca', 'Sc', 'Ti', 'V', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', 'Ga', 'Ge','As','Se', 'Br',
     'Rb', 'Sr', 'Y', 'Zr', 'Nb', 'Mo', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd','In', 'Sn', 'Sb', 'Te', 'I', 'Cs',
     'Ba', 'La', 'Ce', 'Nd', 'Sm', 'Gd', 'Dy', 'Er','Yb', 'Hf', 'Ta', 'W', 'Re', 'Os', 'Ir', 'Pt', 'Au',
     'Hg','Tl', 'Pb', 'Bi', 'Th', 'U')
  all_mineral_vector <- all_mineral_vector[! all_mineral_vector %in% icl_only_elms_vector]
  df <- mindat_geomaterial_list(ids = c(''),elements_inc = icl_only_elms_vector,elements_exc = all_mineral_vector,...)
  df
}


#' geomaterials that match an given cleavagetype
#' @description : Queries the list of geomaterials that match an given cleavagetype
#' @usage geomaterials_cleavagetype(types, ...)
#' @param types vector of given cleavagetype (array of strings or null).
#' The field "cleavage" is used to describe the crystallographic orientation of cleavage directions or planes and quality.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_cleavagetype(c("Poor/Indistinct"))
#' }
#' @details
#' This function releated to the field "cleavagetype" of geomaterials.
#' Items Enum: "Distinct/Good" "Imperfect/Fair" "None Observed" "Perfect" "Poor/Indistinct" "Very Good"
#' @export
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
#' @description : Queries the list of geomaterials that match a given colors.
#' @usage geomaterials_colour(colors, ...)
#' @param colors vector of given colors.
#' colors of the mineral or rock - individual minerals at localities can also have color information.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_colour(c("bright blue"))
#' }
#' @details
#' This function releated to the field "colour" of geomaterials.
#' For example: "Brown", "Yellow", "green", "Pink","White","Orange","Blue","Gold","Dark brown","Purple".
#' @export
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
#' @description : Queries the list of geomaterials that have the given crystal system.
#' @usage geomaterials_crystal_system(crystals, ...)
#' @param crystals vector of given crystals.
#'"crystal system of the mineral;
#'"Amorphous","Hexagonal","Icosahedral","Isometric","Monoclinic","Orthorhombic","Tetragonal","Triclinic","Trigonal"
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_crystal_system(c("Icosahedral"))
#' }
#' @details
#' This function releated to the field "crystal_system" of geomaterials.
#' Items Enum: "Amorphous" "Hexagonal" "Icosahedral" "Isometric" "Monoclinic" "Orthorhombic" "Tetragonal" "Triclinic" "Trigonal"
#' @export
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

#' retrieve the geomaterials whose birifrigence are higher than the given value.
#' @description : Queries the list of geomaterials that minmum value of the given birifrigence value.
#' @usage geomaterials_bi_greater_than(gt, ...)
#' @param gt float value.Birifrigence is calculated from refractive index as (rimax-rimin).Range: bi_min - bi_max.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_bi_greater_than(0.2)
#' }
#' @details
#' This function releated to the field "bi_min" of geomaterials.
#' retrieve all the geomaterials that has higher birifrigence than the given value(gt).
#' @export
geomaterials_bi_greater_than<- function(gt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),bi_min = gt,...)
  df_out
}

#' retrieve the geomaterials whose birifrigence are lower density than the given value.
#' @description : Queries the list of geomaterials that have lower birifrigence than lt.
#' @usage geomaterials_bi_less_than(lt, ...)
#' @param lt float value.Birifrigence is calculated from refractive index as (rimax-rimin).Range: bi_min - bi_max.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_bi_less_than(0.3)
#' }
#' @details
#' This function releated to the field "bi_max" of geomaterials.
#' retrieve all the geomaterials that has higher birifrigence than the given value(lt).
#' @export
geomaterials_bi_less_than<- function(lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),bi_max = lt,...)
  df_out
}

#' retrieve the geomaterials whose birifrigence are higher and lower than the given value.
#' @description : Queries the list of geomaterials that have lower birifrigence than lt.
#' @usage geomaterials_bi_range(gt,lt, ...)
#' @param gt float value.Birifrigence is calculated from refractive index as (rimax-rimin).Range: bi_min - bi_max.
#' @param lt float value.Birifrigence is calculated from refractive index as (rimax-rimin).Range: bi_min - bi_max.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_bi_range(0.2,0.3)
#' }
#' @details
#' This function releated to the fields "bi_min"and "bi_max" of geomaterials.
#' retrieve all the geomaterials that has the birifrigence within the given range of (gt,lt).
#' @export
geomaterials_bi_range <- function(gt,lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),bi_min = gt,bi_max = lt,...)
  df_out
}


#' retrieve the geomaterials whose density are higher than a given value.
#' @description : Queries the list of geomaterials that have higher density than gt.
#' @usage geomaterials_dens_greater_than(gt, ...)
#' @param gt float value.
#' dmeas: measured density of the mineral.
#' This is either the lower limit (if there is a dmeas2) or average (if there is no dmeas2).
#' dmeas2:measured maximum density of mineral
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_dens_greater_than(2)
#' }
#' @details
#' This function releated to the field "density_min" of geomaterials.
#' retrieve all the geomaterials that has higher density than the given density(gt).
#' @export
geomaterials_dens_greater_than<- function(gt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),density_min = gt,...)
  df_out
}

#' retrieve the geomaterials whose density are lower density than a given value.
#' @description : Queries the list of geomaterials that have lower density than lt.
#' @usage geomaterials_dens_less_than(lt, ...)
#' @param lt float value.
#' dmeas: measured density of the mineral.
#' This is either the lower limit (if there is a dmeas2) or average (if there is no dmeas2).
#' dmeas2:measured maximum density of mineral
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_dens_less_than(3)
#' }
#' @details
#' This function releated to the field "density_max" of geomaterials.
#' retrieve all the geomaterials that has higher density than the given density(lt).
#' @export
geomaterials_dens_less_than<- function(lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),density_max = lt,...)
  df_out
}

#' retrieve the geomaterials whose density are within an given value.
#' @description : Queries the list of geomaterials that match an given range.
#' @usage geomaterials_dens_range(gt,lt, ...)
#' @param gt float value
#' @param lt float value
#' dmeas: measured density of the mineral.
#' This is either the lower limit (if there is a dmeas2) or average (if there is no dmeas2).
#' dmeas2:measured maximum density of mineral
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_dens_range(2,3)
#' }
#' @details
#' This function releated to the fields "density_min" and "density_max" of geomaterials.
#' retrieve all the geomaterials records that has the density within an given range of (gt,lt).
#' @export
geomaterials_dens_range <- function(gt,lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),density_min = gt,density_max = lt,...)
  df_out
}

#' retrieve the geomaterials that have an given diapheny.
#' @description : Queries the list of geomaterials that have an given diapheny.
#' @usage geomaterials_diapheny(diapheny, ...)
#' @param diapheny string. The diaphany of the mineral - transparent; translucent; opaque
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_diapheny("Transparent")
#' }
#' @details
#' This function releated to the field "diapheny" of geomaterials.
#' The diaphany of the mineral(Items Enum): "Opaque" "Translucent" "Transparent"
#' @export
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
#' @usage geomaterials_entrytype(types,...)
#' @param types list of entry types.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_entrytype(c('1'))
#' }
#' @details
#' This function releated to the field "entrytype" of geomaterials.
#' Items Enum: 0 1 2 3 4 5 6 7 8
#' Multiple choice:
#' 0- mineral; 1-synonym; 2-variety; 3-mixture; 4-series; 5-grouplist; 6-polytype; 7-rock; 8-commodity
#' Releated field: entrytype_text (description of the entrytype).
#' @export
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
#' @usage geomaterials_expand(expanded_fields,...)
#' @param expanded_fields list of expand (Array of strings (Expanded fields)).Select fields to expand.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_expand("~all")
#' }
#' @details
#' This function releated to the field "expand" of geomaterials.
#' The field expand(Items Enum): "description" "type_localities" "localities" "relations" "~all" "*"
#' @export
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
#' @description : Queries the list of geomaterials that have the given fracturetype.
#' @usage geomaterials_fracturetype(types, ...)
#' @param types list of types.fracturetype(Array of strings or null):
#' How the mineral breaks-"Conchoidal" "Fibrous" "Hackly" "Irregular/Uneven" "Micaceous" "None observed" "Splintery" "Step-Like" "Sub-Conchoidal".
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_fracturetype(c("Step-Like"))
#' }
#' @details
#' This function releated to the field "fracturetype" of geomaterials.
#' fracturetype(Items Enum): "Conchoidal" "Fibrous" "Hackly" "Irregular/Uneven" "Micaceous" "None observed" "Splintery" "Step-Like" "Sub-Conchoidal"
#' @export
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

#' retrieve the geomaterials by an given value of groupid.
#' @description : Queries the list of geomaterials that match an given groupid.
#' @usage geomaterials_by_groupid(gid,...)
#' @param gid integer value. The id of the group to which this mineral belongs
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_by_groupid(0)
#' }
#' @details
#' This function releated to the field "groupid" of geomaterials.
#' retrieve all the geomaterials that match an given groupid.
#' @export
geomaterials_by_groupid<- function(gid,...){

  df_out <- mindat_geomaterial_list(ids = c(''),groupid = gid,...)
  df_out

}

#' retrieve the geomaterials whose hardness are higher than an given value.
#' @description : Queries the list of geomaterials that have higher hardness than an given value(hmin).
#' @usage geomaterials_hardness_gt(hmin, ...)
#' @param hmin float value of the Mohs scale of mineral hardness, which ranging from 0 to 10.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_hardness_gt(8)
#' }
#' @details
#' This function releated to the field "hardness_min" of geomaterials.
#' retrieve all the geomaterials that has higher hardness than the given value(hmin).
#' hmin:the given value of minimum Moh's hardness
#' @export
geomaterials_hardness_gt<- function(hmin,...){
  df_out <- mindat_geomaterial_list(ids = c(''),hardness_min = hmin,...)
  df_out

}

#' retrieve the geomaterials whose hardness are lower than an given value.
#' @description : Queries the list of geomaterials that have lower hardness than an given vlaue(hmax).
#' @usage geomaterials_hardness_lt(hmax, ...)
#' @param hmax float value of the Mohs scale of mineral hardness, which ranging from 0 to 10.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_hardness_lt(2)
#' }
#' @details
#' This function releated to the field "hardness_max" of geomaterials.
#' retrieve all the geomaterials that has lower hardness than an given value(hmax).
#' hamx: maximum Moh's hardness
#' @export
geomaterials_hardness_lt<- function(hmax,...){
  df_out <- mindat_geomaterial_list(ids = c(''),hardness_max = hmax,...)
  df_out
}


#' retrieve the geomaterials whose hardness is within the given range.
#' @description : Queries the list of geomaterials that have hardness within the given range.
#' @usage geomaterials_hardness_range(hmin,hmax, ...)
#' @param hmin float value of the Mohs scale of mineral hardness, which ranging from 0 to 10.
#' @param hmax float value of the Mohs scale of mineral hardness, which ranging from 0 to 10.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_hardness_range(2,3)
#' }
#' @details
#' This function releated to the fields "hardness_min" and "hardness_max" of geomaterials.
#' retrieve all the geomaterials that has the hardness within an given range(hmin,hmax).
#' hmin:the given value of minimum Moh's hardness
#' hamx: maximum Moh's hardness
#' @export
geomaterials_hardness_range<-function(hmin,hmax,...){
  df_out <- mindat_geomaterial_list(ids = c(''),hardness_min =hmin ,hardness_max = hmax,...)
  df_out
}

#' retrieve the geomaterials approved by IMA or not.
#' @description : Queries the geomaterials within or without the ima.
#' @usage geomaterials_ima(btrue,...)
#' @param btrue boolean value.TRUE IMA approved, otherwise not approved.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_ima(TRUE)
#' }
#' @details
#' This function releated to the field "ima" of geomaterials.
#' retrieve all the geomaterials that are approved by the IMA or not.
#' @export
geomaterials_ima<- function(btrue =TRUE,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ima = btrue,...)
  df_out
}

#' retrieve the geomaterials match given notes.
#' @description : Queries the geomaterials with an given .
#' @usage geomaterials_ima_notes(enum_item,...)
#' @param enum_item  	Array of integers or null.
#' Ima notes: multiple choice (OR) : "GROUP" "INTERMEDIATE" "NAMED_AMPHIBOLE" "PENDING_APPROVAL"
#'  "PUBLISHED_WITHOUT_APPROVAL" "REDEFINED" "REJECTED" "RENAMED" "UNNAMED_INVALID" "UNNAMED_VALID"
#' @param ..., Further named parameters.Other optional arguments.
#' @examples
#' \dontrun{
#' df <-geomaterials_ima_notes(c("PENDING_APPROVAL"))
#' }
#' @return df, a data frame of geomaterials
#' @details
#' This function releated to the field "ima_notes" of geomaterials.
#' Rejected by the IMA; Pending approval; IMA Approved Group Name; Redefined by the IMA;
#' Renamed by the IMA; Intermediate member of a solid-solution series; Published without approval;
#' Unnamed (probably valid); Unnamed (probably invalid); Named Amphibole
#'
#' retrieve all the geomaterials that match the input IMA notes.
#' @export
geomaterials_ima_notes<- function(enum_item,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ima_notes = enum_item,...)
  df_out
}

#' retrieve the geomaterials matched given IMA status.
#' @description : Queries the geomaterials with an given ima status.
#' @usage geomaterials_ima_status(enum_status,...)
#' @param enum_status  Ima status: multiple choice (OR):"APPROVED" "DISCREDITED" "GRANDFATHERED" "PENDING_PUBLICATION" "QUESTIONABLE"
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_ima_status(c("APPROVED"))
#' }
#' @details
#' This function releated to the field "ima_status" of geomaterials.
#' retrieve all the geomaterials that match the input IMA notes.
#' @export
geomaterials_ima_status<- function(enum_status,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ima_status = enum_status,...)
  df_out
}

#' retrieve the geomaterials matched a given string in its name.
#' @description : Queries the geomaterials with a given name.
#' @usage geomaterials_name(str_name,...)
#' @param str_name Text search supporting: _ as wildcards, e.g. "qu_rtz", "bario*"
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_name("qu_rtz")
#' }
#' @details
#' This function releated to the field "name" of geomaterials.
#' retrieve all the geomaterials that match the input IMA notes.
#' @export
geomaterials_name<- function(str_name,...){
  df_out <- mindat_geomaterial_list(ids = c(''),name = str_name,...)
  df_out
}


#' retrieve the geomaterials matched a given string in its meteoritical code.
#' @description : Queries the geomaterials with a given string matched its given meteoritical_code.
#' @usage geomaterials_meteoritical_code(str_meteoritical_code,...)
#' @param str_meteoritical_code boolean, meteoritical code exists. Include non-empty (TRUE) / include empty only (FALSE).
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_meteoritical_code(TRUE)
#' }
#' @details
#' This function releated to the field "meteoritical_code_exists" of geomaterials.
#' Meteoritical code exists. Include non-empty (true) / include empty only (false).
#' retrieve all the geomaterials that match the input str_meteoritical_code.
#' @export
geomaterials_meteoritical_code<- function(str_meteoritical_code,...){
  df_out <- mindat_geomaterial_list(ids = c(''), meteoritical_code_exists = str_meteoritical_code,...)
  df_out
}

#' retrieve the geomaterials that match an given lustretype.
#' @description : Queries the geomaterials that match an given lustretype.
#' @usage geomaterials_lustretype(types, ...)
#' @param types string of the type name (Array of strings or null).
#' adamantine, subadamtine, vitreous, subvitreous, resinous, waxy, greasy, silky, pearly, metallic, submetallic, dull, earthy
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_lustretype(c("Adamantine"))
#' }
#' @details
#' This function releated to the field "lustretype" of geomaterials.
#' lustretype(Items Enum): "Adamantine" "Dull" "Earthy" "Greasy" "Metallic" "Pearly" "Resinous" "Silky"
#' "Sub-Adamantine" "Sub-Metallic" "Sub-Vitreous" "Vitreous" "Waxy"
#' multiple choice (AND)
#' @export
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

#' retrieve the geomaterials by an given ordering.
#' @description : Queries the geomaterials by an given ordering.
#' @usage geomeaterials_ordering(ord, ...)
#' @param ord string of field.
#'  Prepend "-" to the field name for descending order.
#'  Enum: "approval_year" "id" "minstats__ms_locentries" "minstats__ms_photos" "name" "updttime" "weighting".
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomeaterials_ordering(-id)
#' }
#' @details
#' This function releated to the field "ordering" of geomaterials.
#' ordering=-id - sort by id descending. Prepend "-" to the field name for descending order.
#' fields:"approval_year" "id" "minstats__ms_locentries" "minstats__ms_photos" "name" "updttime" "weighting".
#' retrieve the geomaterials by an given ordering.
#' @export
geomeaterials_ordering<- function(ord,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ordering = ord,...)
  df_out
}


#' retrieve the geomaterials by an given id of polytype of (the id of the mineral that this record is the polytype of. )
#' @description : Queries the geomaterials by an given id for its  polytype.
#' A mineral that differs from another only in the stacking of similar structural units in its atomic structure
#' @usage geomaterials_polytypeof(ptype, ...)
#' @param ptype integer. an mindat id of the mineral that this record is the polytype of
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_polytypeof(0,fields = "id,name,polytypeof")
#' }
#' @details
#' This function releated to the field "polytypeof" of geomaterials.
#' retrieve the geomaterials with an given id of polytypeof.
#' @export
geomaterials_polytypeof<- function(ptype,...){
  df_out <- mindat_geomaterial_list(ids = c(''),polytypeof = ptype,...)
  df_out
}


#' retrieve the geomaterials that less than the given optical 2v.
#' @description : Queries the geomaterials have the lower optical 2v value than the given lt.
#' @usage geomaterials_optical2v_max(lt, ...)
#' @param lt list of the signs.Please refer to the details.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_optical2v_max(0.2)
#' }
#' @details
#' This function releated to the field "optical2v_max" of geomaterials.
#' optical2vcalc:The calculated 2V angle minimum or average of biaxial minerals
#' optical2vcalc2:The calculated 2V angle maximum of biaxial minerals
#' optical2vmeasured:The measured 2V angle minimum or average of biaxial minerals
#' optical2vmeasured2:The measured 2V angle maximum of biaxial minerals
#' @export
geomaterials_optical2v_max<- function(lt,...){
    df_out <- mindat_geomaterial_list(ids = c(''),optical2v_max = lt,...)
    df_out

}

#' retrieve the geomaterials that has higher value than the given optical 2v.
#' @description : Queries the geomaterials have the higher optical 2v value than the given gt.
#' @usage geomaterials_optical2v_min(gt, ...)
#' @param gt given value of optical 2v of mineral.Please refer to the details.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_optical2v_min(0.1)
#' }
#' @details
#'  This function releated to the field "optical2v_mix" of geomaterials.
#' optical2vcalc:The calculated 2V angle minimum or average of biaxial minerals
#' optical2vcalc2:The calculated 2V angle maximum of biaxial minerals
#' optical2vmeasured:The measured 2V angle minimum or average of biaxial minerals
#' optical2vmeasured2:The measured 2V angle maximum of biaxial minerals
#' @export
geomaterials_optical2v_min<- function(gt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),optical2v_min = gt,...)
  df_out

}

#' retrieve the geomaterials that has the given range of optical 2v.
#' @description : Queries the geomaterials have the higher optical 2v value than the given lt.
#' @usage geomaterials_optical2v_range(gt,lt,...)
#' @param gt given value of minimum of optical 2v of mineral.Please refer to the details.
#' @param lt an given value of maximum of optical 2v of mineral.Please refer to the details.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_optical2v_range(0.1,0.2)
#' }
#' @details
#' This function releated to the field "optical2v_min" and "optical2v_max" of geomaterials.
#' optical2vcalc:The calculated 2V angle minimum or average of biaxial minerals
#' optical2vcalc2:The calculated 2V angle maximum of biaxial minerals
#' optical2vmeasured:The measured 2V angle minimum or average of biaxial minerals
#' optical2vmeasured2:The measured 2V angle maximum of biaxial minerals
#' @export
geomaterials_optical2v_range<- function(gt,lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),optical2v_min = gt,optical2v_max = lt,...)
  df_out

}


#' retrieve the geomaterials that match an given optical signs.
#' @description : Queries the geomaterials match an given optical signs.
#' @usage geomaterials_opticalsign(signs, ...)
#' @param signs list of the signs(string or null).
#' sign for uniaxial and biaxial minerals: +;-;+/- .Please refer to the details.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_opticalsign("-")
#' }
#' @details
#' This function releated to the field "opticalsign" of geomaterials.
#' Optical sign: single choice (Enum): "+", "+/-", "-"
#' @export
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


#' retrieve the geomaterials that match an given optical type.
#' @description : Queries the geomaterials match an given optical type.
#' @usage geomaterials_opticaltype(types, ...)
#' @param types list of the types for the field of opticaltype. Please refer to the details.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_opticaltype("Isotropic")
#' }
#' @details
#' This function releated to the field "opticaltype" of geomaterials.
#' transparent mineral.
#' opticaltype(Enum) :"Biaxial" "Isotropic" "Uniaxial"
#' @export
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

#' retrieve the geomaterials that include non-utf mineral names or not.
#' @description : Queries the geomaterials include non-utf mineral names or not.
#' @usage geomeaterials_non_utf(btrue =TRUE,...)
#' @param btrue boolean. Include non-UTF mineral names?.Default is TRUE.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomeaterials_non_utf(TRUE,fields = "id,name,non_utf")
#' }
#' @details
#' This function releated to the field "non_utf" of geomaterials.
#' retrieve the geomaterials that contain (or not contain) the non-utf name.
#' @export
geomeaterials_non_utf<- function(btrue =TRUE,...){
  df_out <- mindat_geomaterial_list(ids = c(''),non_utf = btrue,...)
  df_out
}

#' retrieve the geomaterials that refractive index higher than an given value(gt).
#' @description : Queries the geomaterials have the higher refractive index than an given value(gt).
#' @usage geomaterials_ri_gt(gt, ...)
#' @param gt float value. Refractive index, from (rimax>=).
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_ri_gt(0.3)
#' }
#' @details
#' This function releated to the field "ri_min" of geomaterials.
#' retrieve the geomaterials with the refractive index higher than an given value(gt).
#' @export
geomaterials_ri_gt <- function(gt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ri_min = gt,...)
  df_out
}

#' retrieve the geomaterials that refractive index lower than an given value(lt).
#' @description : Queries the geomaterials have the lower refractive index than an given value(lt).
#' @usage geomaterials_ri_lt(lt,...)
#' @param lt float value. Refractive index, to (rimin<=)
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_ri_lt(0.5)
#' }
#' @details
#' This function releated to the field "ri_max" of geomaterials.
#' retrieve the geomaterials with the refractive index lower than an given value(lt).
#' @export
geomaterials_ri_lt <- function(lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ri_max = lt,...)
  df_out
}

#' retrieve the geomaterials whose refractive index is within an given range(gt,lt).
#' @description : Queries the list of geomaterials that have refractive index within an given range(gt,lt).
#' @usage geomaterials_ri_range(gt,lt, ...)
#' @param gt float value. Refractive index, from (rimax>=).
#' @param lt float value. Refractive index, to (rimin<=)
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_ri_range(0.2,0.5)
#' }
#' @details
#' This function releated to the fields "ri_min" and "ri_max" of geomaterials.
#' retrieve all the geomaterials that has the refractive index within the range of (gt,lt).
#' @export
geomaterials_ri_range <- function(gt,lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ri_min = gt,ri_max = lt,...)
  df_out
}

#' retrieve the geomaterials that match an given streak.
#' @description : Queries the list of geomaterials that match an given steak.
#' @usage geomaterials_streak(str,...)
#' @param str string.  The color of the streak (color of powdered mineral)
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_streak("orange")
#' }
#' @details
#' This function releated to the fields "steak" of geomaterials.
#' The color of the streak (color of powdered mineral).
#' retrieve the geomaterials that has the given steak.
#' @export
geomaterials_streak <- function(str,...){
  df_out <- mindat_geomaterial_list(ids = c(''),streak = str,...)
  df_out
}

#' retrieve the geomaterials by an given synid.
#' @description : Queries the list of geomaterials that match an given synid.
#' @usage geomaterials_synid(idnum,...)
#' @param idnum integer,an given synonym id.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_synid(3777)
#' }
#' @details
#' This function releated to the fields "synid" of geomaterials.
#' The id of the geomaterial that is the synonym of this record (this geomaterial cannot be added to a locality).
#' retrieve the geomaterials that has an given synid.
#' @export
geomaterials_synid <- function(idnum,...){
  df_out <- mindat_geomaterial_list(ids = c(''),synid = idnum,...)
  df_out
}

#' retrieve the geomaterials updated at an given time.
#' @description : Queries the list of geomaterials that were updated at an given time
#' @usage geomaterials_updated_at(strDate,...)
#' @param strDate str, Last updated datetime in format %Y-%m-%d %H:%M:%S
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' strdate<- "2023-09-13 17:36:19"
#' df <-geomaterials_updated_at(strdate)
#' }
#' @details
#' This function releated to the fields "updated_at" of geomaterials.
#' Last updated datetime in format %Y-%m-%d %H:%M:%S
#' retrieve the geomaterials that have the latest updated at the given time.
#' @export
geomaterials_updated_at <- function(strDate,...){
  df_out <- mindat_geomaterial_list(ids = c(''),updated_at = strDate,...)
  df_out
}

#' retrieve the geomaterials that are varieties of an given id of geomaterials.
#' @description : Queries the list of geomaterials that match the given varietyof.
#' @usage geomaterials_varietyof(intvalue,...)
#' @param intvalue integer, id of mineral that has this variety.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_varietyof(3337)
#' }
#' @details
#' This function releated to the fields "varietyof" of geomaterials.
#' Varieties are geomaterials that have a special distinction from the main geomaterial ie. amethyst var. quartz
#' retrieve the geomaterials that are varieties of an given id of geomaterials.
#' @export
geomaterials_varietyof<- function(intvalue,...){
  df_out <- mindat_geomaterial_list(ids = c(''),varietyof = intvalue,...)
  df_out
}

#' retrieve the geomaterials by a given name.
#' @description : Queries the list of geomaterials by a given name.
#' @usage geomaterials_search_name(name,...)
#' @param name string. Text search supporting wildcards, e.g. qu_rtz, bario*"
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a list of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_search_name("Quartz")
#' }
#' @details
#' This function releated to the fields "name" of geomaterials.
#' retrieve the geomaterial list that match the given name.
#' @export
geomaterials_search_name<- function(name,...){
  df_out <- mindat_geomaterial_search(q = name,...)
  df_out
}


#' retrieve the geomaterials records of empty or not empty of a given field.
#' @description : Queries the list of geomaterials with an empty or not empty of a given field.
#' @usage geomaterials_field_exists(fieldname,bexists,...)
#' @param fieldname string
#' @param bexists bool
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a list of geomaterials
#' @examples
#' \dontrun{
#' df <-geomaterials_field_exists("meteoritical_code")
#' }
#' @details
#' This function releated to all the fields of geomaterials.
#' e.g. meteoritical_code_exists.Meteoritical code exists. Include non-empty (true) / include empty only (false)
#' retrieve the geomaterial list with an empty or not empty of a given field.
#' @export
geomaterials_field_exists<- function(fieldname,bexists,...){
  if (bexists == TRUE){
    filed_str<-paste(fieldname,'_exists=true',sep="")
    df_out <- mindat_geomaterial_list(ids = c(''),filed_exists=filed_str,...)
  }
  else {
    filed_str <- paste(fieldname,'_exists=false',sep="")
    df_out <- mindat_geomaterial_list(ids = c(''),filed_str=filed_str,...)
  }
  df_out
}
########### mindat_geomaterials.R #############
