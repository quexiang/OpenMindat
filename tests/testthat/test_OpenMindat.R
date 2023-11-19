#You should apply for and get your own token from Mindat.org.
This_is_a_temporary_token = "9ce67655d74bcd981e937be80dcea9cb"

test_that("Initializing Mindat API with only token", {
  expect_no_error(mindat_connection(This_is_a_temporary_token))
  expect_no_error(mindat_connection(This_is_a_temporary_token,page_size = 1500))
})

test_that("Initializing Mindat API with token and base_uri", {
  expect_no_error(mindat_connection(This_is_a_temporary_token,"https://api.mindat.org"))
})

test_that("Initializing Mindat API with token, base_uri ,and fmt", {
  expect_no_error(mindat_connection(This_is_a_temporary_token,"https://api.mindat.org","json"))
})
