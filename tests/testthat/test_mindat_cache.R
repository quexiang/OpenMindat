
test_that("Test mindat_cache_has, mindat_cache_set, mindat_cache_get, and mindat_cache_delete", {
  expect_no_error(mindat_cache_set("api_test", "https://api.mindat.org"))
  expect_equal(mindat_cache_has("api_test"),TRUE)
  expect_equal(mindat_cache_get("api_test"),"https://api.mindat.org")
  expect_no_error(mindat_cache_delete("api_test"))
  expect_equal(mindat_cache_has("api_test"),FALSE)
})

test_that("Test mindat_cache_return_or_setup", {
  expect_no_error(mindat_cache_return_or_setup("minda_api_function",setupfun <-function(){}))
  expect_equal(is.function(setupfun),TRUE)
  expect_error(mindat_cache_return_or_setup("minda_api_function","https://api.mindat.org"))
})

