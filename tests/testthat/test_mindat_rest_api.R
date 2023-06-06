

test_that("Test mindat_uri_builder function", {
  config <- c(endpoint_base = "geomaterials/%s")
  base_uri = "https://api.mindat.org"
  uri <- mindat_uri_builder(base_uri,config)
  expect_equal(mindat_uri_builder(base_uri,config),"https://api.mindat.org/geomaterials/")
  expect_equal(mindat_uri_builder(base_uri,config,"fields=id,name"),"https://api.mindat.org/geomaterials/fields=id,name")
})


test_that("Test mindat_set_up_endpoints function", {
  expect_no_error(mindat_set_up_endpoints())
})


test_that("Test mindat_setup function", {
  expect_no_error(mindat_setup())
})
