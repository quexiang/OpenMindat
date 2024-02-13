#library(stringr)


#' Output the file extension of a filename
#' @description Convert the mindat R dataframe to JSON-LD string
#' @usage getExtension (filename)
#' @param filename R dataframe of retrieived data from Mindat database.
#' @examples
#' filename<- "fname.txt"
#' fname_extension<- getExtension(filename)
#' @return 'getExtension()' returns a string which is the suffix string of a file name.
#' @export
getExtension <- function(filename){
  ext<- strsplit(filename, ".", fixed=T)[[1]][-1]
  ext
}

#' Output file as a given format
#' @import stringr
#' @import readxl
#' @description Convert the mindat R dataframe to JSON-LD string
#' @usage ConvertDF2JsonLD(inputdata,template = NULL)
#' @param inputdata R dataframe of retrieived data from Mindat database.
#' @param template filepath to the template
#' @return 'ConvertDF2JsonLD()' returns a string written in Json-LD format converted from an input R data frame (df).
#' @examples
#' \dontrun{
#' df <-geomaterials_search_name("Quartz")
#' df_out <-ConvertDF2JsonLD(df)
#' }
#' @export
ConvertDF2JsonLD<- function(inputdata,template = NULL ){ #"/inst/extdata/OpenMindat_Schema_JSON-LD.xlsx"
  #read a document files  template for output the 'JSON-LD' as references
  if (is.null(template)) {
      template = system.file("extdata", path = "OpenMindat_Schema_JSON-LD.xlsx", package = "OpenMindat", mustWork = TRUE)
  }

  fields_settings  <- read_excel(template,sheet ='fields_settings')
  context_settings <- read_excel(template,sheet ='context')

  colnames <- colnames(inputdata)
  d_clo_list <- strsplit(colnames," ")

  contex_list <- ""
  df_out_fields <-data.frame()
  for (c_li in d_clo_list){
    df_cur <- fields_settings[fields_settings$fields == c_li,]
    df_out_fields <- rbind(df_out_fields,df_cur)
  }
  contex_names_list <- unique(df_out_fields$context_name)

  contex_list <- list()
  for (s_contex in contex_names_list){
    context_tmp <- strsplit(toString(s_contex),",")
    for (ctx_tmp in context_tmp){
      contex_list <-c(contex_list,ctx_tmp)
    }
  }
  contex_list <-unique(contex_list)

  type_list<- list()
  type_name_list <- unique(df_out_fields$type)
  for (s_type in type_name_list){
    type_tmp <- strsplit(toString(s_type),",")
    for (tp_tmp in type_tmp){
      type_list <-c(type_list,tp_tmp)
    }
  }
  type_list <-unique(type_list)

  str_context <- "\"@context\":{\n"
  for (i_ctx in contex_list){
    str_fld <- paste("\"",i_ctx,sep = "")
    str_fld <- paste(str_fld,"\"",sep = "")
    str_url <- paste("\"",context_settings[context_settings$context_name == i_ctx ,]$context_url,sep = "")
    str_url<-paste(str_url,"\"",sep="")
    cur_context<- paste(str_fld,str_url,sep = ":")
    cur_context<- paste(cur_context,",\n")
    str_context<- paste(str_context,cur_context,sep = " ")
  }
  str_context<- str_sub(str_context,end = -3)
  str_context<- paste(str_context,"}",sep = "")

  print_fields_list <- list()
  for (clo_name in d_clo_list){
    #print(clo_name)
    fst<- fields_settings[fields_settings$fields == clo_name,]
    idx_list <- unlist(gregexpr(',', fst$ref_fields))
    if (fst$ref_field_num == 1){
      print_fields_list <- append(print_fields_list,substr(fst$ref_fields, 1, idx_list[fst$ref_field_num]-1))
    }
    else{
      print_fields_list<- append(print_fields_list,substr(fst$ref_fields, idx_list[fst$ref_field_num-1], idx_list[fst$ref_field_num]-1))
    }
  }

  str_graph <- "\"@graph\":["
  for (idf in 1:nrow(inputdata)){
    cur_str <- "{ \n"
    cur_type <-"\"@type\":["

    for(itp in type_list){
      str_itp <- paste("\"",itp,sep = "")
      str_itp <- paste(str_itp,"\",")
      cur_type <- paste(cur_type,str_itp, sep = "" )
      #cur_str <- paste(cur_str,cur_type)
      #cur_str<- paste(cur_str,",")
    }
    cur_type<- str_sub(cur_type,end = -2)
    cur_type <- paste(cur_type,"],", sep = "" )
    cur_str <- paste(cur_str,cur_type)

    for (c_idx in 1:length(print_fields_list)){
      cur_field <- paste("\"",print_fields_list[c_idx])
      cur_field <- paste(cur_field,"\"")
      cur_fvalue <- gsub("[\r\n]", "", inputdata[idf,toString(d_clo_list[c_idx])])
      cur_fvalue <- gsub("\"","\'", cur_fvalue)
      cur_fvalue<- paste("\"",cur_fvalue)
      cur_fvalue <- paste(cur_fvalue,"\"")

      cur_fkv <- paste(cur_field,cur_fvalue,sep=":")
      cur_fkv<- paste(cur_fkv,",")
      cur_str<- paste(cur_str,cur_fkv,sep = "\n")
    }
    cur_str<- str_sub(cur_str,end = -3)
    cur_str <- paste(cur_str,"}\n")
    str_graph<- paste(str_graph,cur_str,sep = "")
    str_graph<- paste(str_graph,",")
  }
  str_graph<- str_sub(str_graph,end = -3)
  str_graph<- paste(str_graph,"]")

  str_out <-"{ \n"
  str_out<- paste(str_out,str_context)
  str_out<- paste(str_out,str_graph,sep = ",\n")
  str_out <-paste(str_out,"}")
  str_out
}

#' Convert a dataframe to a string of TTL format
#' @import stringr
#' @import readxl
#' @description Convert the mindat R dataframe to TTL string
#' @usage ConvertDF2TTL (inputdata,template = NULL)
#' @param inputdata R dataframe of retrieived data from Mindat database.
#' @param template filepath to the template
#' @return 'ConvertDF2TTL()' returns a string written in TTL (pronounced 'turtle') format converted from an input R data frame (df).
#' @examples
#' \dontrun{
#' df <-geomaterials_search_name("Quartz")
#' df_out <-ConvertDF2TTL(df)
#' }
#' @export
ConvertDF2TTL<- function(inputdata,template = NULL){

  if (is.null(template)) {
    template = system.file("extdata", path = "OpenMindat_Schema_TTL.xlsx", package = "OpenMindat", mustWork = TRUE)
  }

  fields_settings  <- read_excel(template,sheet ='fields_settings')
  prefix_settings <- read_excel(template,sheet ='prefix')

  colnames <- colnames(inputdata)
  d_clo_list <- strsplit(colnames," ")

  contex_list <- ""
  df_out_fields <-data.frame()
  for (c_li in d_clo_list){
    df_cur <- fields_settings[fields_settings$fields == c_li,]
    df_out_fields <- rbind(df_out_fields,df_cur)
  }
  contex_names_list <- unique(df_out_fields$context_name)

  contex_list <- list()
  for (s_contex in contex_names_list){
    context_tmp <- strsplit(toString(s_contex),",")
    for (ctx_tmp in context_tmp){
      contex_list <-c(contex_list,ctx_tmp)
    }
  }
  contex_list <-unique(contex_list)

  type_list<- list()
  type_name_list <- unique(df_out_fields$type)
  for (s_type in type_name_list){
    type_tmp <- strsplit(toString(s_type),",")
    for (tp_tmp in type_tmp){
      type_list <-c(type_list,tp_tmp)
    }
  }
  type_list <-unique(type_list)

  str_prefix <-""
  for (i_ctx in contex_list){
    cur_prefix <- "@prefix"
    cur_prefix <- paste(cur_prefix,i_ctx,sep = " ")
    str_url <- "<"
    str_url <- paste(str_url,prefix_settings[prefix_settings$prefix == i_ctx ,]$prefix_url,sep = "")
    str_url<-paste(str_url,"> .",sep="")
    cur_prefix<- paste(cur_prefix,str_url,sep = ":")
    str_prefix<- paste(str_prefix,cur_prefix,sep = "")
    str_prefix<- paste(str_prefix," \n",sep = "")

  }
  str_prefix<- paste(str_prefix," \n ",sep = "")

  print_fields_list <- list()
  for (clo_name in d_clo_list){
    fst<- fields_settings[fields_settings$fields == clo_name,]
    idx_list <- unlist(gregexpr(',', fst$ref_fields))
    if (fst$ref_field_num == 1){
      print_fields_list <- append(print_fields_list,substr(fst$ref_fields, 1, idx_list[fst$ref_field_num]-1))
    }
    else{
      print_fields_list<- append(print_fields_list,substr(fst$ref_fields, idx_list[fst$ref_field_num-1], idx_list[fst$ref_field_num]-1))
    }
  }

  str_graph <- ""
  for (idf in 1:nrow(inputdata)){
    cur_str<- "<"
    cur_str<- paste(cur_str,toString(idf),"> a ")
    cur_type <- ""
    for(itp in type_list){
      cur_type <- paste(cur_type,itp,",")
    }
    cur_type<- str_sub(cur_type,end = -2)
    cur_str<- paste(cur_str,cur_type,"; " )

    for (c_idx in 1:length(print_fields_list)){
        cur_field <- print_fields_list[c_idx]
        cur_fvalue <- gsub("[\r\n]", "", inputdata[idf,toString(d_clo_list[c_idx])])
        cur_fvalue <- gsub("\"","\'", cur_fvalue)
        cur_fkv <- paste(cur_field,cur_fvalue,sep=" ")
        cur_fkv<- paste(cur_fkv,";")
        cur_str<- paste(cur_str,cur_fkv,sep = "\n")
    }
    str_graph <- paste(str_graph,cur_str)
    str_graph<- str_sub(str_graph,end = -3)
    str_graph <- paste(str_graph," .  \n")
  }
  str_out<- paste(str_prefix,str_graph)
  str_out
}

#' Output file as a given format
#' @description Save the mindat R dataframe to a specify format
#' @usage saveMindatDataAs (inputdata,outputfname)
#' @param inputdata R dataframe of retrieived data from Mindat database.
#' @param outputfname string. the output file name.
#' @return No return value.If successful, the input data frame(df) will be saved to the specified file. Otherwise, it will report an error.
#' @examples
#' \dontrun{
#' df <-geomaterials_search_name("Quartz")
#' saveMindatDataAs(df,"test.jsonld")
#' }
#' @export
saveMindatDataAs <-function(inputdata,outputfname){
  fmt <- getExtension(outputfname)
  if (fmt == 'csv'){
    inputdata<-data.frame(lapply(inputdata, as.character), stringsAsFactors= FALSE)
    write.table(inputdata, file= outputfname, row.names = FALSE, col.names = TRUE, sep=",")
  }
  else if(fmt == 'jsonld'){
    out<- ConvertDF2JsonLD(inputdata)
    write(out, file = outputfname)
  }
  else if (fmt == 'txt'){
    write.table(inputdata, file = outputfname , sep = "\t",
                row.names = TRUE, col.names = NA)
  }
  else if (fmt =='JSON'){
    write.table(toJSON(inputdata), file = outputfname)
  }
  else if(fmt == 'ttl'){
    write(ConvertDF2TTL(inputdata),file = outputfname)
  }
  else if (fmt ==''){
    write.table(inputdata, file = outputfname , sep = "\t",
                row.names = TRUE, col.names = NA)
  }
  else{
    stop(sprintf("Sorry, the current OpenMindat Pacakge can not output your format: ", fmt))
  }
}



