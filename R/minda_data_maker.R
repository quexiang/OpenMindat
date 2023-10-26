#library(stringr)

#' Output file as a given format
#' @description Convert the mindat R dataframe to JSON-LD string
#' @usage ConvertDF2JsonLD (DataFrame)
#' @param inputdata R dataframe of retrieived data from Mindat database.
#' @param template filepat to the template
#' @examples
#' ConvertDF2JsonLD(mindat_geomaterial_list(ids = c('3','3337'),fields = "id,name"))
#'
ConvertDF2JsonLD<- function(inputdata,template = "OpenMindat_Schema_JSON-LD.xlsx"){
  #read a document files  template for output the 'JSON-LD' as references
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

  str_context <- "\"@context\":{"
  for (i_ctx in contex_list){
    str_fld <- paste("\"",i_ctx,sep = "")
    str_fld <- paste(str_fld,"\"",sep = "")
    str_url <- paste("\"",context_settings[context_settings$context_name == i_ctx ,]$context_url,sep = "")
    str_url<-paste(str_url,"\"")
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
    for(itp in type_list){
      str_itp <- paste("\"",itp,sep = "")
      str_itp <- paste(str_itp,"\"")
      cur_type <- paste("\"@type\"",str_itp, sep = ":" )
      cur_str <- paste(cur_str,cur_type)
      cur_str<- paste(cur_str,",")
    }

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
  str_out<- paste(str_out,str_graph,sep = ",")
  str_out <-paste(str_out,"}")
  str_out
}


#' Output file as a given format
#' @description Save the mindat R dataframe to a specify format
#' @usage saveMindatDataAs (filename,format = 'csv')
#' @param inputdata R dataframe of retrieived data from Mindat database.
#' @param outputfname string. the output file name.
#' @param fmt  output format
#' @examples
#' saveMindatDataAs("out.csv")
saveMindatDataAs <-function(inputdata,outputfname,fmt ='csv'){
  #mindat_cache_set('api_token', api_token)
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
  else if(fmt == 'TTL'){

  }
  else{

  }
}



