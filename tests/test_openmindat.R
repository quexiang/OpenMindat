library(httr)
library(jsonlite)
library(OpenMindat)
library(testthat)
#library(pkg)

#test_check("OpenMindat")
###test all basic mindat query#####
# Initializing API Call
#test_base_uri = "https://api.mindat.org"
test_base_token = "" #You should get a token from mindat.org
###
###aa9c25fa95d8063908cb2bf186c9e79f

mindat_connection(test_base_token)
response2 <- mindat_geomaterial(id=6)


query_test <- geomaterials_search_name("Quartz")

resss <-localities_list_country("Myanmar")
resss <-localities_list_country("United States")

res <- geomaterials_search_name("Quartz")
#write out to csv file
response2<-data.frame(lapply(response2, as.character), stringsAsFactors= FALSE)

res<- geomaterials_contain_only_elems(c('Si','O'))

response_fields<- mindat_geomaterial(id = 6)
response_fields_ids<- mindat_geomaterial_list(ids = c('6','7','8'),fields ="id,name,updttime,ima_formula,ima_status,ima_notes,key_elements",elements_inc = "H,O",elements_exc ="Be")#ids = c('6','7','8'),

df_contains_all <- geomaterials_contain_all_elems(c('Be','Cr','O'))
df_contains_all<-data.frame(lapply(df_contains_all, as.character), stringsAsFactors= FALSE)
write.table(df_contains_all, file="test.csv", row.names = FALSE, col.names = TRUE, sep=",")


#df_contains_all<-data.frame(lapply(df_contains_all, as.character), stringsAsFactors= FALSE)
#write.table(df_contains_all, file="test.csv", row.names = TRUE, col.names = TRUE, sep=",")

df_contains_any <- geomaterials_contain_any_elems(c('Be','Cr'))
df_notcontains <-  geomaterials_contain_all_and_without_elems(c('Be','O'),c('H'))

#df_contain_butnot <- geomaterials_contain_elems_butnot_elems(c('Be','O'),c('H'))
df_cleavagetype <- geomaterials_cleavagetype(c("None Observed"))
df_cleavagetype2 <- geomaterials_cleavagetype(c("None Observed","Imperfect/Fair"))
df_color <- geomaterials_colour(c("yellow"))
df_color2 <- geomaterials_colour(c("yellow","brown"))
df_crystal <-geomaterials_crystal_system(c("Amorphous"))
df_crystal2 <-geomaterials_crystal_system(c("Hexagonal"))
df_crystal3 <-geomaterials_crystal_system(c("Amorphous","Hexagonal"))
df_density_gt <- geomaterials_dens_greater_than(3)
df_density_lt <- geomaterials_dens_less_than(0.2)
df_density_range <- geomaterials_dens_range(3,3.5)
df_diapheny <- geomaterials_diapheny(c('Opaque'))
df_diapheny <- geomaterials_diapheny(c('Opaque','Translucent'))
df_entrytype <- geomaterials_entrytype(c('1','2'))
df_expand <- geomaterials_expand(c('description','type_localities'))#ids = c('1','2','3')
df_fracturetype <-geomaterials_fracturetype(c("Irregular/Uneven"))
df_hardness_gt <- geomaterials_hardness_gt(9,fields ="id,name")
df_hardness_lt <-geomaterials_hardness_lt(1)
df_hardness_range <-geomaterials_hardness_range(1,1.2)

df_ima <- geomaterials_ima(FALSE)
df_lustretype <- geomaterials_lustretype(c("Adamantine"))
df_non_utf <-  geomeaterials_non_utf(TRUE)
df_opticalsign <- geomaterials_opticalsign("+")
df_optical_type <-geomaterials_opticaltype(types)
df_polytypeof <- geomaterials_polytypeof(0)
df_streak <- geomaterials_streak("black")
df_updatetime <- geomaterials_updated_at("2023-05-07")
df_varietyof <- geomaterials_varietyof(1)

df_ri_gt <- geomaterials_ri_gt(1.6)
df_ri_lt <- geomaterials_ri_lt(1.7)
df_ri_range <- geomaterials_ri_range(1.6,1.7)


df_localitity <- localities_retrieve_id(id = 22)

df_localities <- localities_list_country("China")
df_localities_desc <- localities_list_description("Chinese")
df_loc_exc <-localities_list_elems_exc(c("H","O"))
df_loc_icl <- localities_list_elems_inc(c("Dy"))
df_loc_inc_exc <- localities_list_elems_inc_exc(c("Be"),c("H","O"))
df_ima_integer <- minerals_ima_list_ima(1)

#df_streak <- geomaterials_streak(str)
#df_synid <- geomaterials_synid(idnum)
df_mineral_retrive = minerals_ima_retrieve(2)

response <- mindat_geomaterial_list(ids = c('3','5','7','9','222'))
response <- mindat_geomaterial_list()
response_varieties <- mindat_geomaterial_varieties(id=8)

response2 <- mindat_mineral_ima(id=1)
id_list2<-c('1','2','3','4','6')
response2_list <- mindat_mineral_ima_list(ids =id_list2)
response3_list <- mindat_mineral_ima_list(ids =id_list2,fields = "id,name,ima_formula,ima_symbol")
response3 <- mindat_localitiy(id = 58150)
response4 <- midnat_country(id = 6)
response5 <- locality_age_retrieve_id(id = 6)
response6 <- midnat_locality_status(id = 6)
response7 <- midnat_locality_type(id = 212)
###test all basic mindat query#####

test_that("Well formed URIs with default builder and no params", {
  mindat_api_endpoint('geomaterials', 'geomaterials/')
  expect_equal(build_uri('geomaterials', api_base = test_base_uri), 'https://api.mindat.org/geomaterials/')
})

test_that("Well formed URIs with default builder with params as list", {
  q<-list(1)
  expect_equal(build_uri('geomaterials', api_base = test_base_uri, query = q), 'https://api.mindat.org/geomaterials/')
})


test_that("Well formed URIs with default builder and params as ecliptic parameters", {
  expect_equal(build_uri('testend1', api_base = test_base_uri, p1 = 'one', p2 = 'two'), 'https://api.mindat.org/controller1?p1=one&p2=two')
})

test_that("Error if no config for an endpoint", {
  expect_error(build_uri('testend_nonexistent'))
})

test_that("Globally setted api_base leads to correct URI", {
  mindat_api_endpoint('testend2', 'controller2')
  mindat_cache_delete('api_base')
  set_api_base(test_base_uri)
  expect_equal(build_uri('testend2'), 'https://api.mindat.org/controller2')
})


# test on compulsory query string params

test_that("Error if a an expected query string param is missing", {
  mindat_api_endpoint('testend3', 'controller3', query_params = list('mustbe'))

  expect_error(build_uri('testend3'))
})


test_that("Error if uri_builder not a not a function", {
  # some cases
  expect_error(mindat_api_endpoint('testend4', 'controller4', uri_builder = NULL))
  expect_error(mindat_api_endpoint('testend4', 'controller4', uri_builder = "wrongBuilder"))
  expect_error(mindat_api_endpoint('testend4', 'controller4', uri_builder = list("a")))
})
###test mindat_api ####



###test parse api ####
bad_http_response <- 'HTTP/1.1 400 OK\r\nContent-Type: application/json; charset=utf-8\r\nVary: Accept-Encoding,User-Agent\r\n\r\nTHE BODY'
good_http_response <- 'HTTP/1.1 200 OK\r\nContent-Type: application/json; charset=utf-8\r\nVary: Accept-Encoding,User-Agent\r\n\r\nTHE BODY'

# network tests
test_that("Error on non 200 status response", {
  expect_error(mindat_extract_response_body(bad_http_response))
})

test_that("Body extraction on successful request", {
  expect_equal(mindat_extract_response_body(good_http_response), 'THE BODY')
})

# PARSING

# response to
# "http://paleobiodb.org/data1.1/occs/list.json?base_name=Alopex&interval=Quaternary&show=loc,time"
# 1/2/2014
# 7 records
# record 199941 includes extra column cny
api_json_response_sample <- "{\"records\": [{\"oid\":198254,\"typ\":\"occ\",\"cid\":20434,\"tna\":\"Alopex lagopus\",\"rnk\":3,\"tid\":43911,\"eag\":1.80000,\"lag\":0.30000,\"rid\":[2048],\"cc2\":\"Canada\",\"sta\":\"Yukon\",\"cxi\":33,\"ein\":740,\"lin\":923},{\"oid\":198265,\"typ\":\"occ\",\"cid\":20436,\"tna\":\"Alopex lagopus\",\"rnk\":3,\"tid\":43911,\"eag\":1.80000,\"lag\":0.30000,\"rid\":[2048],\"cc2\":\"Canada\",\"sta\":\"Yukon\",\"cxi\":33,\"ein\":740,\"lin\":923},{\"oid\":199751,\"typ\":\"occ\",\"cid\":20626,\"tna\":\"Alopex lagopus\",\"rnk\":3,\"tid\":43911,\"eag\":0.12600,\"lag\":0.01170,\"rid\":[1784],\"cc2\":\"Canada\",\"sta\":\"Yukon\",\"cxi\":922,\"ein\":922,\"lin\":922},{\"oid\":199941,\"typ\":\"occ\",\"cid\":20643,\"tna\":\"Alopex lagopus\",\"rnk\":3,\"tid\":43911,\"eag\":0.12600,\"lag\":0.01170,\"rid\":[2872],\"cc2\":\"United States\",\"sta\":\"Alaska\",\"cny\":\"North Slope\",\"cxi\":922,\"ein\":922,\"lin\":922},{\"oid\":373590,\"typ\":\"occ\",\"cid\":35359,\"tna\":\"Alopex\",\"rnk\":5,\"tid\":41193,\"eag\":5.33300,\"lag\":0.01170,\"rid\":[9534],\"cc2\":\"Hungary\",\"sta\":\"Barany\",\"cxi\":1,\"ein\":34,\"lin\":33},{\"oid\":485281,\"typ\":\"occ\",\"cid\":48587,\"tna\":\"Alopex lagopus\",\"rnk\":3,\"tid\":43911,\"eag\":0.12600,\"lag\":0.01170,\"rid\":[12964],\"cc2\":\"Russian Federation\",\"sta\":\"Respublika Saha (Jakutija)\",\"cxi\":922,\"ein\":922,\"lin\":922},{\"oid\":766675,\"typ\":\"occ\",\"cid\":81830,\"tna\":\"Alopex lagopus\",\"rnk\":3,\"tid\":43911,\"eag\":0.12600,\"lag\":0.01170,\"rid\":[27585],\"cc2\":\"United States\",\"sta\":\"Alaska\",\"cxi\":922,\"ein\":922,\"lin\":922}]}"

api_json_response_sample_returns <- '{\n"records": [\n{"oid":1001,"typ":"occ","cid":160,"tna":"Wellerella","rnk":5,"tid":29018,"oei":"Missourian","eag":305.90000,"lag":303.40000,"rid":[12]}\n]\n}\n'

test_that(".parse_raw_data for a JSON string response is a data.frame", {

  parsed <- mindat_parse_raw_data(api_json_response_sample)
  expect_is(parsed, 'data.frame')
})

test_that("parse_raw_data for a JSON string that includes \n yields just one row in the dataframe", {

  parsed <- mindat_parse_raw_data(api_json_response_sample_returns)
  expect_equal(dim(parsed)[1], 10)
})

# test_that("A JSON string with different columns for some occurency resolves to a data.frame comprising all the columns", {
#
#   expected_names <- c("oid", "typ", "cid", "tna", "rnk", "tid", "eag", "lag", "rid", "cc2", "sta", "cxi", "ein", "lin")  #oei, "cny"
#   resp_df<-mindat_parse_raw_data(api_json_response_sample)
#   resp_names <- names(resp_df)
#
#   expect_identical(resp_names, expected_names)
# })


###test parse api ####
###test mindat query ####

test_that("Param with several (as list) values joins to csv ok", {
  expect_equal(params_to_string(list("aa","bb")), 'aa,bb')
})

test_that("Param with several (as character vector) values joins to csv ok", {
  expect_equal(params_to_string(c("aa","bb")), 'aa,bb')
})

test_that("Param with one value remains", {
  expect_equal(params_to_string("aa"), "aa")
})

###test mindat query#####


###test all basic mindat query#####
test_that("mindat_mineral output is a dataframe, and the names are characters", {
  set_api_token(test_base_token)
  response<- mindat_mineral (id=1)
  # mindat_cache_has('token')
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]==1)
})

###test all basic mindat query#####




