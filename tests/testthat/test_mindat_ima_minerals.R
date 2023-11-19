library(httr)
library(jsonlite)
#You should apply for and get your own token from Mindat.org.
This_is_a_temporary_token = "9ce67655d74bcd981e937be80dcea9cb"
expect_no_error(mindat_connection(This_is_a_temporary_token,page_size = 1500))

# test_that("Test minerals_ima_list function", {
#   expect_no_error(df <- minerals_ima_list())
#   expect_equal(typeof(df), "list")
# })

# test_that("Test minerals_ima_list_expand function", {
#   expect_no_error(df <- minerals_ima_list_expand("~all"))
#   expect_equal(typeof(df), "list")
# })

test_that("Test minerals_ima_list_ima function", {
  expect_no_error(df <- minerals_ima_list_ima(1))
  expect_equal(typeof(df), "list")
})


# test_that("Test minerals_ima_updated_at function", {
#   expect_no_error(df <- minerals_ima_updated_at("2022-03-09 01:13:59"))
#   expect_equal(typeof(df), "list")
# })

test_that("Test minerals_ima_retrieve function", {
  expect_no_error(df <- minerals_ima_retrieve(id =1))
  expect_equal(typeof(df), "list")
})


