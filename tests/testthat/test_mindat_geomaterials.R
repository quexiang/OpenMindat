library(httr)
library(jsonlite)
#You should apply for and get your own token from Mindat.org.
This_is_a_temporary_token = "9ce67655d74bcd981e937be80dcea9cb"
expect_no_error(mindat_connection(This_is_a_temporary_token,page_size = 1500))

test_that("Test geomaterials_contain_all_elems function", {
  expect_no_error(df <- geomaterials_contain_all_elems(c('Fe','S'),fields ="id,name,mindat_formula,elements,sigelements"))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_not_contain_elems function", {
  expect_no_error(df <- geomaterials_not_contain_elems (c('H','O',"Si","Al","Fe"),fields ="id,name,mindat_formula"))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_contain_all_but_not_elems function", {
  expect_no_error(df <- geomaterials_contain_all_but_not_elems(c('H','Be'),c('O')))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_contain_any_but_not_elems function", {
  expect_no_error(df <- geomaterials_contain_any_but_not_elems(c('H','Be'),c('O')))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_contain_any_elems function", {
  expect_no_error(df <- geomaterials_contain_any_elems(c('Dy','Be')))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_contain_only_elems function", {
  expect_no_error(df <- geomaterials_contain_only_elems(c('Si','O')))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_cleavagetype function", {
  expect_no_error(df <- geomaterials_cleavagetype("Imperfect/Fair"))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_colour function", {
  expect_no_error(df <- geomaterials_colour(c("green")))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_crystal_system function", {
  expect_no_error(df <- geomaterials_crystal_system(c("Hexagonal")))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_dens_greater_than function", {
  expect_no_error(df <- geomaterials_dens_greater_than(3))
  expect_equal(typeof(df), "list")
})


# test_that("Test geomaterials_dens_less_than function", {
#   expect_no_error(df <- geomaterials_dens_less_than(1))
#   expect_equal(typeof(df), "list")
# })

test_that("Test geomaterials_dens_range function", {
  expect_no_error(df <- geomaterials_dens_range(3,3.2))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_diapheny function", {
  expect_no_error(df <- geomaterials_diapheny(c("Opaque"),fields = "id,name,diapheny"))
  expect_equal(typeof(df), "list")
})



test_that("Test geomaterials_entrytype function", {
  expect_no_error(df <- geomaterials_entrytype(c('2'),fields = "name,entrytype"))
  expect_equal(typeof(df), "list")
})

# test_that("Test geomaterials_expand function", {
#   #You should apply for and get your own token from Mindat.org.
#   # This_is_a_temporary_token = "2082edf7b8dab2b9887f3c2393e822c6"
#   # expect_no_error(mindat_connection(This_is_a_temporary_token))
#   expect_no_error(df <- geomaterials_expand("locality",fields = "id,name,locality"))
#   expect_equal(typeof(df), "list")
# })

test_that("Test geomaterials_fracturetype function", {
  expect_no_error(df <- geomaterials_fracturetype(c('Fibrous')))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_hardness_gt function", {
  expect_no_error(df <- geomaterials_hardness_gt(9))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_hardness_lt function", {
  expect_no_error(df <- geomaterials_hardness_lt(1))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_hardness_range function", {
  expect_no_error(df <- geomaterials_hardness_range(3,3.2))
  expect_equal(typeof(df), "list")
})


# test_that("Test geomaterials_ima function", {
#   expect_no_error(df <- geomaterials_ima(TRUE))
#   expect_equal(typeof(df), "list")
# })


test_that("Test geomaterials_lustretype function", {
  expect_no_error(df <- geomaterials_lustretype(c("Adamantine")))
  expect_equal(typeof(df), "list")
})

# test_that("Test geomeaterials_non_utf function", {
#   expect_no_error(df <- geomeaterials_non_utf(TRUE,fields = "id,name,non_utf"))
#   expect_equal(typeof(df), "list")
# })

test_that("Test geomaterials_opticalsign function", {
  expect_no_error(df <- geomaterials_opticalsign("-",fields = "id,name,opticalsign"))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_ri_gt function", {
  expect_no_error(df <- geomaterials_ri_gt(1.6))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_ri_lt function", {
  expect_no_error(df <- geomaterials_ri_lt(1.7))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_ri_range function", {
  expect_no_error(df <- geomaterials_ri_range(1.6,1.7))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_streak function", {
  expect_no_error(df <- geomaterials_streak("black"))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_synid function", {
  expect_no_error(df <- geomaterials_synid(2897))
  expect_equal(typeof(df), "list")
})

# test_that("Test geomaterials_updated_at function", {
#   expect_no_error(df <- geomaterials_updated_at("2022-03-09 01:13:59"))
#   expect_equal(typeof(df), "list")
# })


test_that("Test geomaterials_varietyof function", {
  expect_no_error(df <- geomaterials_varietyof(1720))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_bi_greater_than function", {
  expect_no_error(df <- geomaterials_bi_greater_than(3,fields = "id,name,opticalsign,bi_min,bi_max"))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_bi_less_than function", {
  expect_no_error(df <- geomaterials_bi_less_than(0.8,fields = "id,name,bi_min,bi_max"))
  expect_equal(typeof(df), "list")
})

test_that("Test geomaterials_bi_range function", {
  expect_no_error(df <- geomaterials_bi_range(0.2,0.3,fields = "id,name,bi_min,bi_max"))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_by_groupid function", {
  expect_no_error(df <-  geomaterials_by_groupid(6,fields = "id,name,groupid"))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_ima_notes function", {
  expect_no_error(df <- geomaterials_ima_notes("PUBLISHED_WITHOUT_APPROVAL"))
  expect_equal(typeof(df), "list")
})


test_that("Test geomaterials_ima_status function", {
  expect_no_error(df <- geomaterials_ima_status("PENDING_PUBLICATION"))
  expect_equal(typeof(df), "list")
})

# test_that("Test geomaterials_meteoritical_code function", {
#   expect_no_error(df <- geomaterials_meteoritical_code("TRUE"))
#   expect_equal(typeof(df), "list")
# })

# test_that("Test geomaterials_name function", {
#   expect_no_error(df <- geomaterials_name("qu*"))
#   expect_equal(typeof(df), "list")
# })
