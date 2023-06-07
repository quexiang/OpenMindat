test_that("Test minerals_ima_list function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- minerals_ima_list())
  expect_equal(typeof(df), "list")
})

test_that("Test minerals_ima_list_expand function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- minerals_ima_list_expand("~all"))
  expect_equal(typeof(df), "list")
})

test_that("Test minerals_ima_list_ima function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- minerals_ima_list_ima(1))
  expect_equal(typeof(df), "list")
})


test_that("Test minerals_ima_updated_at function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- minerals_ima_updated_at("2022-03-09 01:13:59"))
  expect_equal(typeof(df), "list")
})

test_that("Test minerals_ima_retrieve function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- minerals_ima_retrieve(id =1))
  expect_equal(typeof(df), "list")
})


