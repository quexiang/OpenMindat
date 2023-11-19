
test_that("Test set_api_base function", {
  expect_no_error(set_api_base("https://api.mindat.org"))
  expect_equal(mindat_cache_has("api_base"),TRUE)
  expect_equal(mindat_cache_get("api_base"),"https://api.mindat.org")
})


test_that("Test set_api_token function", {
  expect_no_error(set_api_token("9ce67655d74bcd981e937be80dcea9cb"))
  expect_equal(mindat_cache_has("api_token"),TRUE)
  expect_equal(mindat_cache_get("api_token"),"9ce67655d74bcd981e937be80dcea9cb")
})

test_that("Test default_uri_builder function", {
  config <- c(endpoint_base = "geomaterials/")
  expect_equal(default_uri_builder("https://api.mindat.org",config),"https://api.mindat.org/geomaterials/")
  #expect_equal(mindat_cache_get("api_token"),"https://api.mindat.org/geomaterials/")
})

test_that("Test mindat_api_endpoint function", {
  config <- c(endpoint_base = "geomaterials/")
  default_uri_builder("https://api.mindat.org",config)
  expect_no_error(mindat_api_endpoint("geomaterials", "geomaterials/",default_uri_builder,c('')))
  test_error_uribuilder = "error uri builder"
  expect_error(mindat_api_endpoint("geomaterials", "geomaterials",test_error_uribuilder,c('')))
})


# test_that("Test build_uri function", {
#   config <- c(endpoint_base = "geomaterials/")
#   default_uri_builder("https://api.mindat.org",config)
#   mindat_api_endpoint("geomaterials","geomaterials/",default_uri_builder)
#   qs<- "https://api.mindat.org/geomaterials/?fields=id,name/"
#   qs <- paste(qs,'?', 'page_size = 10', sep = "")
#   expect_no_error(build_uri("geomaterials",fields = "id,name",api_base = "https://api.mindat.org"))
# })


test_that("Test stop_not_param function", {
  query = list(fields = "id,name",id__in = "1,2,3,222",elements_inc = c('H','Be'))
  comp_params = c('fields','id__in',"elements_inc")
  expect_no_error(stop_not_param(comp_params,query))
  query_err = list(fields = "id,name",id = "1,2,3,222",elements = c('H','Be'))
  expect_error(stop_not_param(comp_params,query_err))
})

