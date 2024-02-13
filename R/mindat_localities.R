################ mindat_localities.R  #################
#' retrieve the localities list that are belong to a given country.
#' @description : Queries the list of localities that are within a given country.
#' @usage localities_list_country(country,...)
#' @param country name of country,
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of localities
#' @examples
#' \dontrun{
#' df <-localities_list_country ("Norway")
#' }
#' @details
#' This function releated to the field "country" of localities.
#'Enum: "Afghanistan" "Albania" "Algeria" "American Samoa" "Andorra" "Angola" "Anguilla"
#' "Antigua and Barbuda" "Argentina" "Armenia" "Aruba" "Ashmore and Cartier Islands" "Australia"
#' "Austria" "Azerbaijan" "Bahamas" "Bahrain" "Bangladesh" "Barbados" "Belarus" "Belgium" "Belize"
#' "Benin" "Bermuda" "Bhutan" "Bolivia" "Bosnia And Herzegovina" "Botswana" "Bouvet Island" "Brazil"
#' "British Indian Ocean Territories" "British Solomon Islands" "British Virgin Islands" "Brunei"
#' "Bulgaria" "Burkina Faso" "Burundi" "Cambodia" "Cameroon" "Canada" "Cape Verde" "Cayman Islands"
#' "Central African Republic" "Chad" "Chile" "China" "Christmas Island" "Cocos Islands" "Colombia"
#' "Comoro Islands" "Cook Islands" "Costa Rica" "Croatia" "Cuba" "Cyprus" "Czech Republic"
#' "Democratic Republic of the Congo" "Denmark" "Djibouti" "Dominica" "Dominican Republic" "East Timor"
#' "Ecuador" "Egypt" "El Salvador" "Equatorial Guinea" "Estonia" "Ethiopia" "Faeroe Islands" "Falkland Islands"
#' "Federated States of Micronesia" "Fiji" "Finland" "France" "French Guiana" "French Polynesia" "Gabon" "Gambia"
#' "Georgia" "Germany" "Ghana" "Gibraltar" "Greece" "Greenland" "Grenada" "Guadeloupe" "Guam" "Guatemala"
#' "Guernsey" "Guinea" "Guinea-Bissau" "Guyana" "Haiti" "Honduras" "Hong Kong" "Hungary" "Iceland" "India"
#' "Indonesia" "Iran" "Iraq" "Ireland" "Isle of Man" "Israel" "Italy" "Ivory Coast (Côte d'Ivoire)" "Jamaica"
#' "Japan" "Jersey" "Jordan" "Kazakhstan" "Kenya" "Kiribati " "Kosovo" "Kuwait" "Kyrgyzstan" "Laos" "Latvia"
#' "Lebanon" "Lesotho" "Liberia" "Libya" "Liechtenstein" "Lithuania" "Luxembourg" "Macao" "Madagascar" "Malawi"
#' "Malaysia" "Maldives" "Mali" "Malta" "Martinique" "Mauritania" "Mauritius" "Mexico" "Moldova" "Monaco" "Mongolia"
#' "Montenegro" "Montserrat" "Morocco" "Mozambique" "Myanmar" "Namibia" "Nauru" "Nepal" "Netherlands"
#' "Netherlands Antilles" "New Caledonia" "New Zealand" "Nicaragua" "Niger" "Nigeria" "North Korea" "Norway" "Oman"
#' "Pakistan" "Panama" "Papua New Guinea" "Paraguay" "Peru" "Philippines" "Poland" "Portugal" "Puerto Rico" "Qatar"
#' "Republic of Congo (Brazzaville)" "Republic of Macedonia" "Reunion Island" "Romania" "Russia" "Rwanda" "Saint Helena"
#' "Saint Lucia " "Saint Vincent and the Grenadines" "San Marino" "Sao Tome And Principe" "Saudi Arabia" "Senegal"
#' "Serbia" "Seychelles" "Sierra Leone" "Singapore" "Slovakia" "Slovenia" "Solomon Islands" "Somalia" "South Africa"
#' "South Korea" "Spain" "Sri Lanka" "St Christopher-Nevis Islands" "Sudan" "Suriname" "Swaziland" "Sweden" "Switzerland"
#' "Syria" "Taiwan" "Tajikistan" "Tanzania" "Thailand" "Togo" "Tonga" "Trinidad And Tobago" "Tunisia" "Turkey" "Turkmenistan"
#' "Turks And Caicos Islands" "Tuvalu" "U.S. Virgin Islands" "Uganda" "Ukraine" "United Arab Emirates" "United Kingdom"
#' "United States" "Uruguay" "Uzbekistan" "Vanuatu (Republic of Vanuatu; New Hebrides) " "Venezuela" "Vietnam" "Western Sahara"
#' "Western Samoa" "Yemen" "Zambia" "Zimbabwe"
#' @export
localities_list_country<- function(country,...){
  df_out <- mindat_localities_list(ids = c(''),country = country,...)
  df_out
}

#' retrieve the localities that contain the given description
#' @description : Queries the list of localities that contain the given description.
#' @usage localities_list_description(desc,...)
#' @param desc string,
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of localities
#' @examples
#' \dontrun{
#' df <-localities_list_description("volcano")
#' }
#' @details
#' This function releated to all the field "description" of localities.
#' retrieve the localities that contain the given description
#' @export
localities_list_description<- function(desc,...){
  df_out <- mindat_localities_list(ids = c(''),description = desc,...)
  df_out
}

#' localities that do not contain the given elements
#' @description Queries the list of localities that do not contain the given elements.
#' @usage localities_list_elems_exc(exc_elems_list, ...)
#' @param exc_elems_list vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of localities
#' @examples
#' \dontrun{
#' df <-localities_list_elems_exc(c("H", "O", "Si","Fe"),fields="id,name")
#' }
#' @details
#' This function releated to the field "elements_exc" of localities.
#' This function queries the list of localities that do contain the specified elements.
#' @export
localities_list_elems_exc<- function(exc_elems_list,...){
  df_out <- mindat_localities_list(ids = c(''),elements_exc = exc_elems_list,...)
  df_out
}

#' localities that contain the given elements
#' @description Queries the list of localities that contain the given elements.
#' @usage localities_list_elems_inc(inc_elems_list, ...)
#' @param inc_elems_list vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of localities
#' @examples
#' \dontrun{
#' df <-localities_list_elems_inc(c("Dy"))
#' }
#' @details
#' This function releated to the field "elements_inc" of localities.
#' This function queries the list of localities that contain the given elements.
#' @export
localities_list_elems_inc<- function(inc_elems_list,...){
  df_out <- mindat_localities_list(ids = c(''),elements_inc = inc_elems_list,...)
  df_out
}

#' localities that contain the given elements but not contain some other given elements.
#' @description Queries the list of localities that contain the given elements,but not contain some other given elements.
#' @usage localities_list_elems_inc_exc(inc_elems_list,exc_elems_list, ...)
#' @param inc_elems_list vector of elements.
#' @param exc_elems_list vector of elements.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of localities
#' @examples
#' \dontrun{
#' df <-localities_list_elems_inc_exc(c("Dy"), c("Li"))
#' }
#' @details
#' This function releated to the fields "elements_inc" and "elements_exc" of localities.
#' This function queries the list of localities that contain the given elements,but not contain some other given elements.
#' @export
localities_list_elems_inc_exc <-function(inc_elems_list,exc_elems_list,...){
  df_out <- mindat_localities_list(ids = c(''),elements_inc = inc_elems_list, elements_exc = exc_elems_list,...)
  df_out
}


#' localities that contain the given expands.
#' @description Queries the list of localities that contain the given expands.
#' @usage localities_list_expand(expands,...)
#' @param expands vector of expands.
#' @param ..., Further named parameters.Other optional arguments-Additional arguments.
#' @return df, a data frame of localities
#' @examples
#' \dontrun{
#' df <-localities_list_expand("~all")
#' }
#' @details
#' This function releated to the fields "expand" of localities.
#' Items Enum: "geomaterials" "~all" "*"
#' This function queries the list of localities that contain the given expands.
#' @export
localities_list_expand <-function(expands,...){
  df_out <- mindat_localities_list(ids = c(''), expand = expands,...)
  df_out
}

#' localities that contain the given txt name.
#' @description Queries the list of localities that contain the given txt name.
#' @usage localities_list_txt(txt,...)
#' @param txt string.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of localities
#' @examples
#' \dontrun{
#' df <-localities_list_txt("lava")
#' }
#' @details
#' This function releated to the fields "txt" of localities.
#' This function queries the list of localities that contain the given txt name.
#' @export
localities_list_txt <-function(txt,...){
  df_out <- mindat_localities_list(ids = c(''),txt = txt,...)
  df_out
}

#' retrieve the localities list updated at the given time.
#' @description : Queries the list of localities that have the given time
#' @usage localities_list_updated_at(updateDate,...)
#' @param updateDate str, Last updated datetime in format %Y-%m-%d %H:%M:%S
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of localities
#' @examples
#' \dontrun{
#' strdate<- "2023-09-13 17:36:19"
#' df <-localities_list_updated_at(strdate)
#' }
#' @details
#' This function releated to all the fields "updated_at" of localities.
#' retrieve the localities that have the latest updated at the given time.
#' @export
localities_list_updated_at<-function(updateDate,...){
  df_out <- mindat_localities_list(ids = c(''),updated_at = updateDate,...)
  df_out
}

#' retrieve the localities list.
#' @description : Queries the list of localities.
#' @usage localities_list_all(...)
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of localities
#' @examples
#' \dontrun{
#' df <-localities_list_all(fields = "id,name,latitude,longitude")
#' }
#' @details
#' This function releated to  the fields "ids" of localities.
#' retrieve all the localities.
#' @export
localities_list_all<-function(...){
  df_out <- mindat_localities_list(ids = c(''),...)
  df_out
}

#' retrieve the localities by a given mindat id.
#' @description : Queries the localitiy by given id.
#' @usage localities_retrieve_id(id,...)
#' @param id, integer. the mindat localitiy id.
#' @param ..., Further named parameters.Other optional arguments.
#' @return df, a data frame of localities
#' @examples
#' \dontrun{
#' df <-localities_retrieve_id(3337)
#' }
#' @details
#' This function releated to all the fields "id" of localities.
#' retrieve the localities by a given id.
#' @export
localities_retrieve_id<-function(id,...){
  df_out <- mindat_localitiy(id,...)
  df_out
}
##############

#' locality_age
#' @description retrieve locality age by its id
#' @usage locality_age (id,...)
#' @param id, the mindat localitiy age id.
#' @param ..., Further named parameters.
#' @return df, data frame of locality age.
#' @examples
#' \dontrun{
#' df <-locality_age(3337)
#' }
#' @details
#' This function releated to the fields "id" of locality_age and locality.
#' @export
locality_age<-function(id,...){
  l<-list(...)
  mindat_query('locality_age', query = c(list(id = id), l))
}

#' locality_age_list
#' @description retrieve all locality age list or by its conditions
#' @usage locality_age_list (...)
#' @param ..., Further named parameters.
#' @return df, data frame of locality age.
#' @examples
#' \dontrun{
#' df <-locality_age_list()
#' }
#' @details
#' This function is to retrieve all the locality_age list.
#' @export
locality_age_list<-function(...){
  l<-list(...)
  mindat_query('locality_age_list', query = l)
}


#' localities_status_list
#' @description retrieve all locality status list.
#' @usage localities_status_list (...)
#' @param ..., Further named parameters.
#' @return df, data frame of locality status
#' @examples
#' \dontrun{
#' df <-localities_status_list()
#' }
#' @details
#' This function is to retrieve all the locality_status list.
#' @export
localities_status_list <-function(...){
  df_out <- mindat_locality_status_list(...)
  df_out
}

#' localities_status_retrieve
#' @description retrieve locality status by its id.
#' @usage localities_status_retrieve (id,...)
#' @param id the mindat localitiy status id
#' @param ..., Further named parameters.
#' @return df, data frame of locality status.
#' @examples
#' \dontrun{
#' df <-localities_status_retrieve(10)
#' }
#' @details
#' This function is to retrieve the locality_status by an given id of locality.
#' @export
localities_status_retrieve<- function(id,...){
  df_out <- mindat_locality_status(id,...)
  df_out
}

#' localitiy_type_retrieve
#' @description retrieve locality type by an given id of locality.
#' @usage localitiy_type_retrieve (id,...)
#' @param id the mindat localitiy id
#' @param ..., Further named parameters.
#' @return df, data frame of locality status.
#' @examples
#' \dontrun{
#' df <-localitiy_type_retrieve(50)
#' }
#' @details
#'  This function is to retrieve the locality types by an given id of locality.
#' @export
localitiy_type_retrieve <- function(id,...){
  df_out <- mindat_locality_type( id = id,...)
  df_out
}

#' locality_type_list
#' @description retrieve all locality type list.
#' @usage locality_type_list (...)
#' @param ..., Further named parameters.
#' @return df, data frame of locality type.
#' @examples
#' \dontrun{
#' df <-locality_type_list()
#' }
#' @details
#' This function is to retrieve the locality types list.
#' @export
locality_type_list <- function(...){
  df_out <- mindat_locality_type_list(...)
  df_out
}

################ mindat_localities.R  #################
