Your_token = #Your_token

test_that("Initializing Mindat API with only token", {
  expect_no_error(mindat_connection(Your_token))
})

test_that("Initializing Mindat API with token and base_uri", {
  expect_no_error(mindat_connection(Your_token,"https://api.mindat.org"))
})

test_that("Initializing Mindat API with token, base_uri ,and fmt", {
  expect_no_error(mindat_connection(Your_token,"https://api.mindat.org","json"))
})
