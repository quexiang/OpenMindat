library(httr)
library(jsonlite)
#You should apply for and get your own token from Mindat.org.
This_is_a_temporary_token = "9ce67655d74bcd981e937be80dcea9cb"
expect_no_error(mindat_connection(This_is_a_temporary_token,page_size = 1500))

test_that("Test localities_list_country function", {
  expect_no_error(df <- localities_list_country("Sweden"))
  expect_equal(typeof(df), "list")
})

# test_that("Test localities_list_description function", {
#   expect_no_error(df <- localities_list_description("Chinese"))
#   expect_equal(typeof(df), "list")
# })

test_that("Test localities_list_elems_exc function", {
  expect_no_error(df <- localities_list_elems_exc(c("H", "O", "Si", "Al", "Fe", "Ca", "Na", "K", "P", "C", "Mn", "F", "Mg", "S"),fields = "id,name"))
  expect_equal(typeof(df), "list")
})

test_that("Test localities_list_elems_inc function", {
  expect_no_error(df <- localities_list_elems_inc(c("Dy")))
  expect_equal(typeof(df), "list")
})

test_that("Test localities_list_elems_inc_exc function", {
  expect_no_error(df <- localities_list_elems_inc_exc(c('Be'),c("Li")))
  expect_equal(typeof(df), "list")
})

# test_that("Test localities_list_expand function", {
#   #test_base_token =  #Your_token
#   #expect_no_error(mindat_connection(test_base_token))
#   expect_no_error(df <- localities_list_expand(c("geomaterials")))
#   expect_equal(typeof(df), "list")
# })

test_that("Test localities_list_txt function", {
  expect_no_error(df <- localities_list_txt(c("Chinese")))
  expect_equal(typeof(df), "list")
})


# test_that("Test localities_list_updated_at function", {
#   expect_no_error(df <- localities_list_updated_at("2022-03-09 01:13:59"))
#   expect_equal(typeof(df), "list")
# })

#may take long time
# test_that("Test localities_list_all function", {
#   #test_base_token =  #Your_token
#   #expect_no_error(mindat_connection(test_base_token))
#   expect_no_error(df <- localities_list_all())
#   expect_equal(typeof(df), "list")
# })

test_that("Test localities_retrieve_id function", {
  expect_no_error(df <- localities_retrieve_id(id=6))
  expect_equal(typeof(df), "list")
})

test_that("Test locality_age function", {
  expect_no_error(df <- locality_age(id=222))
  expect_equal(typeof(df), "list")
})

test_that("Test locality_age_list function", {
  expect_no_error(df <- locality_age_list())
  expect_equal(typeof(df), "list")
})

test_that("Test localities_status_list function", {
  expect_no_error(df <- localities_status_list())
  expect_equal(typeof(df), "list")
})

test_that("Test localities_status_retrieve function", {
  expect_no_error(df <- localities_status_retrieve(2))
  expect_equal(typeof(df), "list")
})

test_that("Test localitiy_type_retrieve function", {
  expect_no_error(df <- localitiy_type_retrieve(id = 1))
  expect_equal(typeof(df), "list")
})

test_that("Test locality_type_list function", {
  expect_no_error(df <- locality_type_list())
  expect_equal(typeof(df), "list")
})

