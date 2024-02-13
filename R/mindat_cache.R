############  mindat_cache.R  #################
#' Create a cache environment named midnat_cache
#' @description Create a new mindat cache environment.
#' @noRd
mindat_cache <- new.env()

#' Set cache name and value
#' @description Assigns the value to the cache named varname in current environment.
#' @usage mindat_cache_set(varname, value)
#' @param varname  string. The cached varname.
#' @param value  string.
#' @return No return value. The value will be cached in memory, and the cached value can be fetched by calling the function mindat_cache_get with the assigned varname.
#' @examples
#' mindat_cache_set('api_token',"9ce67655d74bcd981e937be80dcea9cb")
#' @export
mindat_cache_set<-function(varname, value)
{
  assign(varname, value, envir = mindat_cache)
}

#' Get cache value
#' @description Retrieve the value of the cache named varname in current environment.
#' @usage mindat_cache_get(varname)
#' @param varname  string
#' @returns cached value. A string, list or other objects.
#' @examples
#' token<- mindat_cache_get('api_token')
#' @export
mindat_cache_get<-function(varname)
{
  return (get(varname, envir=mindat_cache))
}

#' Delete a cached value by the users input varname
#' @description Remove (clear) the cache named varname in current environment.
#' @usage mindat_cache_delete(varname)
#' @param varname string input a cached name.Set a cached value empty by the given varname. A string, list or other objects.
#' @return No return value. The cached variable named varname will be clear.
#' @examples
#' mindat_cache_delete('api_token')
#' @export
mindat_cache_delete<-function(varname)
{
  assign(varname, NULL, envir = mindat_cache)
}

#' Remove all cached values
#' @description Clear all current cached values. Set current environment cache empty.
#' @usage mindat_cache_empty()
#' @return No return value. All cached content will be cleared.
#' @examples
#' mindat_cache_empty()
#' @export
mindat_cache_empty<-function(){
  rm(list = ls(envir = mindat_cache))
}

#' Check if the current environment has the cached value of varname.
#' @description Check whether or not the current environment has the cache named varname.
#' @usage mindat_cache_has(varname)
#' @param varname string.
#' @returns Boolean value. if the varname is found in current environment cache, return True otherwise return False.
#' @examples
#' b_has <- mindat_cache_has('api_token')
#' @export
mindat_cache_has <-function(varname)
{
  if(!exists(varname, envir= mindat_cache)){
    return (FALSE)
  }
  else{

    return (!is.null(mindat_cache_get(varname)))
  }
}

#' Check if the current environment has the cached function named varname.
#' @description Check whether the current environment has the cached function named varname,if has, return it.
#'              if not, setup up a new cache function named varname.
#' @usage mindat_cache_return_or_setup(varname,setupfun)
#' @param varname string.
#' @param setupfun boolean, if the cached is a setup function.
#' @returns If the varname is found in current environment cache, return cached function.
#'          If not, eval the function and return cached function.
#' @examples
#'  aep<-  api_end_points<-mindat_cache_return_or_setup('api_end_points', function(){return (list()) })
#' @export
mindat_cache_return_or_setup<-function(varname, setupfun)
{
  if(!mindat_cache_has(varname))
  {
    if(is.function(setupfun)){
      setupcall <- as.call(list(setupfun))
    } else {
      setupcall <- call(setupfun)
    }
    setupvalue <- eval(setupcall)
    mindat_cache_set(varname, setupvalue)
    mindat_cache_get(varname)
  }
  return (mindat_cache_get(varname))
}
########### mindat_cache.R ###########
