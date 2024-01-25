
########## mindat_data_parse.R ############
#' @import httr
#' @importFrom httr GET
#' @name GET
#' @title the function is form the httr GET function.
usethis::use_import_from("httr", "GET")

#' @import utils
#' @importFrom utils URLencode
#' @name URLencode
#' @title the function is form the utils URLencode function.
usethis::use_import_from("utils", "URLencode")


#' @import utils
#' @importFrom utils str
#' @name str
#' @title the function is form the utils str function.
usethis::use_import_from("utils", "str")

#' @import utils
#' @importFrom utils write.table
#' @name write.table
#' @title the function is form the utils write.table function.
usethis::use_import_from("utils", "write.table")
#importFrom("utils", "URLencode", "str", "write.table")

#' mindat_make_data_frame
#' @import httr
#' @description convert the response json to dataframe of R
#' @usage mindat_make_data_frame (reg_list)
#' @param reg_list response json data to list format obj.
#' @return df_out, R data frame
#' @export
mindat_make_data_frame<-function(reg_list){
  if (is.list(reg_list)){
    #df_out <- data.frame(reg_list)
    df_out <-as.data.frame(do.call(cbind,reg_list))
  }
  else{
    mineralcnt <-reg_list$count
    data_results<-reg_list$results
    df_out <- data.frame(data_results)
    next_url<- reg_list$`next`
    while(!is.null(next_url)){
      query<-list(format= "json")
      all_data2<-GET(next_url,add_headers('Authorization'= paste('Token ','YOUR_API_KEY',sep = "")),query = query)
      str(content(all_data2))
      all_data2_text <- content(all_data2,"text", encoding = "UTF-8")
      #all_data2_json <- fromJSON(readLines(all_data2_text),flatten = TRUE, warn=F)
      all_data2_json <- fromJSON(all_data2_text,flatten = TRUE)
      data_results2<-all_data2_json$results
      df_tmp = data.frame(data_results2)
      df_out <- rbind(df_out,df_tmp)
      next_url<- all_data2_json$`next`
    }
  }
  df_out
}

#' mindat_parse_raw_data
#' @import jsonlite
#' @description parse the raw response of json to dataframe of R. If the raw_data obtained from the response is paged,
#' request all the pages and then add them into the df_out data frame.
#' @usage mindat_parse_raw_data (raw_data)
#' @param raw_data content of the response body
#' @return df_out, R data frame
#' @export
mindat_parse_raw_data<-function(raw_data){
  data_list <- fromJSON(raw_data)
  if ("results" %in% names(data_list) ){
    if(mindat_cache_has('api_token')){
      api_token <- mindat_cache_get('api_token')
    } else {
      stop("No API token configured")
    }
    data_results<-data_list$results
    df_out <- data.frame(data_results)
    next_url<- data_list$`next`
    while(!is.null(next_url)){
      query<-list(format= "json")
      all_data2<-GET(next_url,add_headers('Authorization'= paste('Token ',api_token,sep = "")),query = query)
      str(content(all_data2))
      all_data2_text <- content(all_data2,"text", encoding = "UTF-8")
      #all_data2_json <- fromJSON(readLines(all_data2_text),flatten = TRUE, warn=F)
      all_data2_json <- fromJSON(all_data2_text,flatten = TRUE)
      data_results2<-all_data2_json$results
      df_tmp = data.frame(data_results2)
      df_out <- rbind(df_out,df_tmp)
      next_url<- all_data2_json$`next`
    }
    df_out
  }
  else{
    df_out <- mindat_make_data_frame(data_list)#data_list[["records"]]
    df_out
  }
}

#' mindat_extract_response_body
#' @import jsonlite
#' @description .
#' @usage mindat_extract_response_body (response)
#' @param response response json
#' @return if status of the response is sucess (200),return the all_data_text(the content of response).
#' Otherwise,report the errors.
#' @export
mindat_extract_response_body<-function(response){
  if (200 == status_code(response)){
    # Content in the API
    str(content(response))
    # Converting content to text
    all_data_text <- content(response,"text", encoding = "UTF-8")
  }
  else{
    stop(sprintf('Error in API: The server return a status %s,which indicates that something went wrong with your request.',status_code(response)))
  }
}

#' mindat_get_data_from_uri
#' @import httr
#' @description retrieve data from the uri.
#' @usage mindat_get_data_from_uri (uri)
#' @param uri request uri
#' @return df. R data frame of the request uri.
#' @export
mindat_get_data_from_uri<-function(uri){
  if(mindat_cache_has('api_token')){
    api_token <- mindat_cache_get('api_token')
  } else {
    stop("No API token configured")
  }
  response <- GET(uri,add_headers('Authorization'= paste('Token ',api_token,sep = "")))
  raw_data <- mindat_extract_response_body(response)
  df<-mindat_parse_raw_data(raw_data)
  df
}

#' mindat_build_querystring
#' @import httr
#' @import stringi
#' @description Build query string based on the query conditions.
#' @usage mindat_build_querystring (args)
#' @param args query args.
#' @return qs. generated query string.
#' @export
mindat_build_querystring<-function(args){
  qs <- ''
  for (argName in names(args)) {
    strArgValue <- as.character(args[argName][[1]])
    encodedArgValue <-  URLencode(strArgValue)
    if (argName == "id"){
      qs <-paste(qs, encodedArgValue, '&', sep = "")
      #break
    }
    else if(argName == "ids"){
      qs <-paste('?id__in=',qs, encodedArgValue, '&', sep = "")
    }
    else if(argName == "filed_exists"){
      qs <-paste(qs, encodedArgValue, '&', sep = "")
    }
    else{
      qs <- paste(qs, argName, "=", encodedArgValue, '&', sep = "")
    }
  }
  #qs <- substr(qs,0,nchar(qs)-1)
  page_size <- mindat_cache_get('page_size')
  if(grepl('\\?',qs)){
    qs <- paste(qs, 'page_size', "=", page_size, sep = "")
  }
  else{
    if('&' == substr(qs,nchar(qs),nchar(qs))){
      qs<- substr(qs,0,nchar(qs)-1)
      qs <- paste(qs, '/?page_size', "=", page_size, sep = "")
    }
    else{
      qs <- paste(qs, '?page_size', "=", page_size, sep = "")
    }
  }
  qs
}
########## mindat_data_parse.R  ###########
