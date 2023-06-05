
test_that("Test set_api_base function", {
  expect_no_error(set_api_base("https://api.mindat.org"))
  expect_equal(mindat_cache_has("api_base"),TRUE)
  expect_equal(mindat_cache_get("api_base"),"https://api.mindat.org")
})


test_that("Test set_api_token function", {
  expect_no_error(set_api_token("ad9c15fa95d8063908cb5bf186c9e79f"))
  expect_equal(mindat_cache_has("api_token"),TRUE)
  expect_equal(mindat_cache_get("api_token"),"ad9c15fa95d8063908cb5bf186c9e79f")
})

test_that("Test default_uri_builder function", {
  config <- c(endpoint_base = "geomaterials/")
  expect_equal(default_uri_builder("https://api.mindat.org",config),"https://api.mindat.org/geomaterials/")
  #expect_equal(mindat_cache_get("api_token"),"https://api.mindat.org/geomaterials/")
})

