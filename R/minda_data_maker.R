
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
  else{

  }
}
