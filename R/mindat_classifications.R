########### mindat_classifications.R #############
#' dana_8 classification
#' @description : Queries a list of Dana 8th edition classifications.
#' @usage Dana8_groups(...)
#' @param ..., Further parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of dana8 groups
#' @examples
#' \dontrun{
#' df <-Dana8_groups()
#' }
#' @details
#' This function return a list of dana8 groups
#' case-insensitive
#' @export
Dana8_groups <- function(...){
    df_out <- mindat_dana8_groups(...)
    df_out
}

#' dana_8 subgroups
#' @description : Queries a list of subgroups of the Dana 8th edition classifications.
#' @usage Dana8_subgroups(...)
#' @param ..., Further parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of dana8 subgroups
#' @examples
#' \dontrun{
#' df <-Dana8_subgroups()
#' }
#' @details
#' This function return a list of dana8 subgroups
#' case-insensitive
#' @export
Dana8_subgroups <- function(...){
  df_out <- mindat_dana8_subgroups(...)
  df_out
}


#' Nickel-strunz10-classes
#' @description : Queries a  list of Nickel-Strunz 10th edition classifications.
#' @usage Nickel_strunz10_classes(...)
#' @param ..., Further parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of the classes of Nickel-Strunz 10th edition classifications.
#' @examples
#' \dontrun{
#' df <-Nickel_strunz10_classes()
#' }
#' @details
#' This function return a list of Nickel-Strunz-10 classes.
#' case-insensitive
#' @export
Nickel_strunz10_classes <- function(...){
  df_out <- mindat_nickel_strunz10_classes(...)
  df_out
}

#' Nickel-strunz10-subclasses
#' @description : Queries a  list of the subclasses of Nickel-Strunz 10th edition classifications.
#' @usage Nickel_strunz10_subclasses(...)
#' @param ..., Further parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of the subclasses of Nickel-Strunz 10th edition classifications.
#' @examples
#' \dontrun{
#' df <-Nickel_strunz10_subclasses()
#' }
#' @details
#' This function return a list of Nickel-Strunz-10 subclasses.
#' case-insensitive
#' @export
Nickel_strunz10_subclasses <- function(...){
  df_out <- mindat_nickel_strunz10_subclasses(...)
  df_out
}

#' Nickel-strunz10-families
#' @description : Queries a  list of the families of Nickel-Strunz 10th edition classifications.
#' @usage Nickel_strunz10_families(...)
#' @param ..., Further parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of the families of Nickel-Strunz 10th edition classifications.
#' @examples
#' \dontrun{
#' df <-Nickel_strunz10_families()
#' }
#' @details
#' This function return a list of Nickel-Strunz-10 families
#' @export
Nickel_strunz10_families <- function(...){
  df_out <- mindat_nickel_strunz10_families(...)
  df_out
}

########### mindat_classifications.R #############
