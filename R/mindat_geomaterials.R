########### mindat_geomaterials.R #############
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
#' @description retrieve the geomaterials that do not contain the input elements.Queries the list of geological materials that
#' do not contain the specified elements.
#' @usage geomaterials_not_contain_elems (ecl_elms_vector, ...)
#' @param ecl_elms_vector, vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of geomaterials
#' @examples
#' geomaterials_not_contain_elems (c('H','Be'))
geomaterials_not_contain_elems <- function(ecl_elms_vector,...){
  df <- mindat_geomaterial_list(ids = c(''),elements_exc = ecl_elms_vector,...)
  df
}

#' geomaterials_that do not contain the elements
#' @description Queries the list of geological materials that simultaneously contain the specified elements and do not contain the specified elements.
#' @usage geomaterials_contain_all_but_without_elems(icl_elm_vector, ecl_elms_vector, ...)
#' @param icl_elm_vector vector of elements.
#' @param ecl_elms_vector vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' This function queries the list of geological materials that do not contain the specified elements.
#' It performs the query operation by calling the mindat_geomaterial_list function.
#' @examples
#' geomaterials_contain_all_but_without_elems(c('H','Be'),c('O'))
geomaterials_contain_all_but_without_elems <- function(icl_elm_vector,ecl_elms_vector,...){
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

#' geomaterials_contain_only_elems
#' @description retrieve the geomaterials that contain only the elements users input.This function queries
#' the list of geological materials that only contain the specified elements.
#' It performs the query operation by calling the mindat_geomaterial_list function
#' @usage geomaterials_contain_only_elems (icl_only_elms_vector)
#' @param icl_only_elms_vector, vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments that can be
#' passed to the mindat_geomaterial_list function.
#' @return df, a data frame of geomaterials
#' @examples
#' geomaterials_contain_only_elems(c('Si','O'))
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

#' retrieve the geomaterials whose birifrigence are higher than the given value.
#' @description : Queries the list of geomaterials that minmum value of the given birifrigence value.
#' @usage geomaterials_bi_greater_than(gt, ...)
#' @param gt float value
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that has higher birifrigence than the given birifrigence
#' @examples
#' geomaterials_bi_greater_than(0.3)
geomaterials_bi_greater_than<- function(gt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),bi_min = gt,...)
  df_out
}

#' retrieve the geomaterials whose birifrigence are lower density than the given value.
#' @description : Queries the list of geomaterials that have lower birifrigence than lt.
#' @usage geomaterials_bi_less_than(lt, ...)
#' @param lt float value
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that has higher birifrigence than the given birifrigence.
#' @examples
#' geomaterials_bi_less_than(0.8)
geomaterials_bi_less_than<- function(lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),bi_max = lt,...)
  df_out
}

#' retrieve the geomaterials whose birifrigence are higher and lower than the given value.
#' @description : Queries the list of geomaterials that have lower birifrigence than lt.
#' @usage geomaterials_bi_range(gt,lt, ...)
#' @param gt float value
#' @param lt float value
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that has the birifrigence within the range of (gt,lt).
#' @examples
#' geomaterials_bi_range(0.6,0.7)
geomaterials_bi_range <- function(gt,lt,...){
  df_out <- mindat_geomaterial_list(ids = c(''),density_min = gt,density_max = lt,...)
  df_out
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
  df_out <- mindat_geomaterial_list(ids = c(''),density_min = gt,...)
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
  df_out <- mindat_geomaterial_list(ids = c(''),density_max = lt,...)
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
  df_out <- mindat_geomaterial_list(ids = c(''),density_min = gt,density_max = lt,...)
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
#' Multiple choice:
#' 0- mineral; 1-synonym; 2-variety; 3-mixture; 4-series; 5-grouplist; 6-polytype; 7-rock; 8-commodity
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


#' retrieve the geomaterials by an given value of groupid.
#' @description : Queries the list of geomaterials that have the given groupid.
#' @usage geomaterials_by_groupid(hmin, ...)
#' @param gid integer value
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that has the given groupid.
#' @examples
#' geomaterials_by_groupid(6)
#' geomaterials_by_groupid(3)
geomaterials_by_groupid<- function(id,...){

  df_out <- mindat_geomaterial_list(ids = c(''),groupid = gid,...)
  df_out

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

  df_out <- mindat_geomaterial_list(ids = c(''),hardness_min = hmin,...)
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
  df_out <- mindat_geomaterial_list(ids = c(''),hardness_max = hmax,...)
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
  df_out <- mindat_geomaterial_list(ids = c(''),hardness_min =hmin ,hardness_max = hmax,...)
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

#' retrieve the geomaterials match given notes.
#' @description : Queries the geomaterials with an given .
#' @usage geomaterials_ima(btrue,...)
#' @param enum_item  Ima notes: multiple choice (OR) : "GROUP" "INTERMEDIATE" "NAMED_AMPHIBOLE" "PENDING_APPROVAL" "PUBLISHED_WITHOUT_APPROVAL" "REDEFINED" "REJECTED" "RENAMED" "UNNAMED_INVALID" "UNNAMED_VALID"
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that match the input IMA notes.
#' @examples
#' geomaterials_ima_notes("PUBLISHED_WITHOUT_APPROVAL")

geomaterials_ima_notes<- function(enum_item,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ima_notes = enum_item,...)
  df_out
}

#' retrieve the geomaterials matched given IMA status.
#' @description : Queries the geomaterials with your given ima status.
#' @usage geomaterials_ima_status(enum_status,...)
#' @param enum_status  Ima status: multiple choice (OR):"APPROVED" "DISCREDITED" "GRANDFATHERED" "PENDING_PUBLICATION" "QUESTIONABLE"
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that match the input IMA notes.
#' @examples
#' geomaterials_ima_status("PENDING_PUBLICATION")

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
#' @details
#' retrieve all the geomaterials that match the input IMA notes.
#' @examples
#' geomaterials_name("qu_rtz")
#' geomaterials_name("qu*")

geomaterials_name<- function(str_name,...){
  df_out <- mindat_geomaterial_list(ids = c(''),name = str_name,...)
  df_out
}


#' retrieve the geomaterials matched a given string in its meteoritical code.
#' @description : Queries the geomaterials with a given string matched its given meteoritical_code.
#' @usage geomaterials_meteoritical_code(str_name,...)
#' @param str_meteoritical_code Text search supporting: _ as wildcards.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that match the input meteoritical_code.
#' @examples

geomaterials_meteoritical_code<- function(str_meteoritical_code,...){
  df_out <- mindat_geomaterial_list(ids = c(''),meteoritical_code_exists = str_meteoritical_code,...)
  df_out
}

#' Meteoritical code exists. Include non-empty (true) / include empty only (false)
#' @description : Queries the geomaterials with the meteoritical_code_exists or not.
#' @usage geomaterials_meteoritical_code_exists(true,...)
#' @param bl_exists boolean
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve all the geomaterials that match the input meteoritical_code_exists or not.
#' @examples
#' geomaterials_meteoritical_code_exists(TRUE)

geomaterials_meteoritical_code_exists<- function(bl_exists,...){
  df_out <- mindat_geomaterial_list(ids = c(''),meteoritical_code_exists = bl_exists,...)
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

#' retrieve the geomaterials by an given ordering.
#' @description : Queries the geomaterials by an given ordering.
#' @usage geomeaterials_ordering(ord, ...)
#' @param ord string Enum: "approval_year" "id" "minstats__ms_locentries" "minstats__ms_photos" "name" "updttime" "weighting".
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve the geomaterials by an given ordering.
#' @examples
#' geomeaterials_ordering("approval_year")
#' geomeaterials_ordering("minstats__ms_photos")
geomeaterials_ordering<- function(ord,...){
  df_out <- mindat_geomaterial_list(ids = c(''),ordering = ord,...)
  df_out
}


#' retrieve the geomaterials by an given polytypeof
#' @description : Queries the geomaterials by an given polytypeof.
#' @usage geomeaterials_polytypeof(ptype, ...)
#' @param ptype integer .
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of geomaterials
#' @details
#' retrieve the geomaterials with an given polytypeof.
#' @examples
#' geomeaterials_polytypeof("approval_year")

geomeaterials_polytypeof<- function(ptype,...){
  df_out <- mindat_geomaterial_list(ids = c(''),polytypeof = ptype,...)
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
geomeaterials_non_utf<- function(btrue =TRUE,...){
  df_out <- mindat_geomaterial_list(ids = c(''),non_utf = btrue,...)
  df_out
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
  df_out <- mindat_geomaterial_list(ids = c(''),ri_min = gt,...)
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
  df_out <- mindat_geomaterial_list(ids = c(''),ri_max = lt,...)
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
  df_out <- mindat_geomaterial_list(ids = c(''),ri_min = gt,ri_max = lt,...)
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
#' retrieve the geomaterials that have the given varietyof.
#' @examples
#' geomaterials_varietyof(1720)
geomaterials_varietyof<- function(intvalue,...){
  df_out <- mindat_geomaterial_list(ids = c(''),varietyof = intvalue,...)
  df_out
}

#' retrieve the geomaterials by a given name.
#' @description : Queries the list of geomaterials by a given name.
#' @usage geomaterials_search_name(name,...)
#' @param name string,
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a list of geomaterials
#' @details
#' retrieve the geomaterial list that match the given name.
#' @examples
#' geomaterials_search_name("Quartz")
geomaterials_search_name<- function(name,...){
  df_out <- mindat_geomaterial_search(q = name,...)
  df_out
}
########### mindat_geomaterials.R #############
