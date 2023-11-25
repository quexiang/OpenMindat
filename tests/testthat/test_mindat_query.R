#You should apply for and get your own token from Mindat.org.
This_is_a_temporary_token = "9ce67655d74bcd981e937be80dcea9cb"
expect_no_error(mindat_connection(This_is_a_temporary_token,page_size = 1500))

test_that("Test mindat_query function", {
  request_uri = "https://api.mindat.org/geomaterials/6"
  endpoint<-"geomaterials"
  expect_no_error(df <- mindat_query(endpoint, query = c(list(id = 6))))
  expect_equal(typeof(df), "list")
})


test_that("Test params_to_string function", {
  test_id =  6
  expect_equal(params_to_string(c(test_id)),6)
  test_str = c('6','7','8')
  expect_equal(params_to_string(test_str),"6,7,8")
})


test_that("Test mindat_mineral_ima function", {
  #expect_no_error(mindat_connection(This_is_a_temporary_token))
  expect_no_error(df <- mindat_mineral_ima(1))
  expect_equal(typeof(df), "list")
})

test_that("Test mindat_mineral_ima_list function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- mindat_mineral_ima_list(ids = c('1','3','9')))
  expect_equal(typeof(df), "list")
})


test_that("Test mindat_localities_list function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  df <- mindat_localities_list(ids = c('2','3','9','222'))
  expect_no_error(df <- mindat_localities_list(ids = c('2','3','9','222')))
  expect_equal(typeof(df), "list")
})

test_that("Test mindat_localitiy function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- mindat_localitiy(2))
  expect_equal(typeof(df), "list")
})

test_that("Test mindat_locality_status function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- mindat_locality_status(2))
  expect_equal(typeof(df), "list")
})


test_that("Test mindat_locality_status_list function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- mindat_locality_status_list(ids = c('2','3','9','222')))
  expect_equal(typeof(df), "list")
})


test_that("Test mindat_locality_type function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- mindat_locality_type(222))
  expect_equal(typeof(df), "list")
})


test_that("Test mindat_geomaterial function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- mindat_geomaterial(1))
  expect_equal(typeof(df), "list")
})


test_that("Test mindat_geomaterial_varieties function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- mindat_geomaterial_varieties(2))
  expect_equal(typeof(df), "list")
})


test_that("Test mindat_geomaterial_list function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- mindat_geomaterial_list(ids = c('1','3','9')))
  expect_equal(typeof(df), "list")
})


test_that("Test mindat_country function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- mindat_country(id=1))
  expect_equal(typeof(df), "list")
})

test_that("Test mindat_countries function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- mindat_countries())
  expect_equal(typeof(df), "list")
})


