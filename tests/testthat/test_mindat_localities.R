test_that("Test localities_list_country function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- localities_list_country("China"))
  expect_equal(typeof(df), "list")
})

test_that("Test localities_list_description function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- localities_list_description("Chinese"))
  expect_equal(typeof(df), "list")
})

test_that("Test localities_list_elems_exc function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- localities_list_elems_exc(c('H','O')))
  expect_equal(typeof(df), "list")
})

test_that("Test localities_list_elems_inc function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- localities_list_elems_inc(c('Be')))
  expect_equal(typeof(df), "list")
})

test_that("Test localities_list_elems_inc_exc function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- localities_list_elems_inc_exc(c('Be'),c('O')))
  expect_equal(typeof(df), "list")
})

test_that("Test localities_list_expand function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- localities_list_expand(c("geomaterials")))
  expect_equal(typeof(df), "list")
})

test_that("Test localities_list_txt function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- localities_list_txt(c("Chinese")))
  expect_equal(typeof(df), "list")
})


test_that("Test localities_list_updated_at function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- localities_list_updated_at("2022-03-09 01:13:59"))
  expect_equal(typeof(df), "list")
})

#may take long time
test_that("Test localities_list_all function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- localities_list_all())
  expect_equal(typeof(df), "list")
})

test_that("Test localities_retrieve_id function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- localities_retrieve_id(id=6))
  expect_equal(typeof(df), "list")
})

test_that("Test locality_age function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- locality_age(id=222))
  expect_equal(typeof(df), "list")
})

test_that("Test locality_age_list function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- locality_age_list())
  expect_equal(typeof(df), "list")
})

test_that("Test localities_status_list function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- localities_status_list())
  expect_equal(typeof(df), "list")
})

test_that("Test localities_status_retrieve function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- localities_status_retrieve(2))
  expect_equal(typeof(df), "list")
})

test_that("Test localitiy_type_retrieve function", {
  test_base_token =  #Your_token
  expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- localitiy_type_retrieve(1))
  expect_equal(typeof(df), "list")
})

test_that("Test locality_type_list function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- locality_type_list())
  expect_equal(typeof(df), "list")
})

