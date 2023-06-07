test_that("Test geomaterials_contain_all_elems function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_contain_all_elems(c('Be','O')))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_without_elems function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_without_elems(c('Be','O')))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_contain_all_and_without_elems function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_contain_all_and_without_elems(c('Be','O'),c('H')))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_contain_any_elems function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_contain_any_elems(c('Be','Cr')))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_cleavagetype function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_cleavagetype("Imperfect/Fair"))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_colour function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_colour(c("green")))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_crystal_system function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_crystal_system(c("Hexagonal")))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_dens_greater_than function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_dens_greater_than(3))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_dens_less_than function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_dens_less_than(0.2))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_dens_range function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_dens_range(3,3.2))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_diapheny function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_diapheny(c("Opaque")))
  expect_equal(typeof(df), "list")
})



test_that("Test geomaterials_entrytype function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_entrytype(c('1')))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_expand function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_expand(c('type_localities')))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_fracturetype function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_fracturetype(c('Fibrous')))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_hardness_gt function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_hardness_gt(9))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_hardness_lt function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_hardness_lt(1))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_hardness_range function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_hardness_range(1,1.2))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_ima function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_ima(TRUE))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_lustretype function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_lustretype(c("Adamantine")))
  expect_equal(typeof(df), "list")
})

test_that("Test geomeaterials_non_utf function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomeaterials_non_utf(TRUE))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_opticalsign function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_opticalsign("-"))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_ri_gt function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_ri_gt(1.6))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_ri_lt function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_ri_lt(1.7))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_ri_range function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_ri_range(1.6,1.7))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_streak function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_streak("black"))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_synid function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_synid(2897))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_updated_at function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_updated_at("2022-03-09 01:13:59"))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_varietyof function", {
  #test_base_token =  #Your_token
  #expect_no_error(mindat_connection(test_base_token))
  expect_no_error(df <- geomaterials_varietyof(1720))
  expect_equal(typeof(df), "list")
})


