
test_that("Test mindat_make_data_frame function", {
  test_data <-args <- list(ids = "6,7,8",fields ="id,name,updttime,ima_formula,ima_status,ima_notes,key_elements",elements_inc = "H,O")
  expect_no_error(df <- mindat_make_data_frame(test_data))
  expect_equal(typeof(df), "list")
})


test_that("Test mindat_parse_raw_data function", {
  raw_data <- "{\"id\":6,\"longid\":\"1:1:6:0\",\"guid\":\"1d46f167-d1c9-494e-b46b-bfe43acc7596\",\"name\":\"Abraum Salts\",\"updttime\":\"2017-07-06 15:35:51\",\"mindat_formula\":\"\",\"mindat_formula_note\":\"\",\"ima_formula\":\"\",\"ima_status\":[],\"ima_notes\":[],\"varietyof\":0,\"synid\":0,\"polytypeof\":0,\"groupid\":0,\"entrytype\":0,\"entrytype_text\":\"mineral\",\"description_short\":\"Literally \\\"waste (dump) salts.\\\"\",\"impurities\":\"\",\"elements\":\"-\",\"sigelements\":\"-\",\"tlform\":\"\",\"cim\":\"\",\"occurrence\":\"\",\"otheroccurrence\":\"\",\"industrial\":\"\",\"discovery_year\":\"\",\"diapheny\":\"\",\"cleavage\":\"\",\"parting\":\"\",\"tenacity\":\"\",\"colour\":\"\",\"csmetamict\":0,\"opticalextinction\":\"\",\"hmin\":0.0,\"hardtype\":0,\"hmax\":0.0,\"vhnmin\":\"\",\"vhnmax\":\"\",\"vhnerror\":0,\"vhng\":0,\"vhns\":0,\"luminescence\":\"\",\"lustre\":\"\",\"lustretype\":\"\",\"aboutname\":\"\",\"other\":\"\",\"streak\":\"\",\"csystem\":\"\",\"cclass\":0,\"spacegroup\":0,\"a\":\"\",\"b\":\"\",\"c\":\"\",\"alpha\":\"\",\"beta\":\"\",\"gamma\":\"\",\"aerror\":0,\"berror\":0,\"cerror\":0,\"alphaerror\":0,\"betaerror\":0,\"gammaerror\":0,\"va3\":0.0,\"z\":0,\"dmeas\":\"0\",\"dmeas2\":\"0\",\"dcalc\":\"\",\"dmeaserror\":0,\"dcalcerror\":0,\"cleavagetype\":\"\",\"fracturetype\":\"\",\"morphology\":\"\",\"twinning\":\"\",\"epitaxidescription\":\"\",\"opticaltype\":\"\",\"opticalsign\":\"\",\"opticalalpha\":\"\",\"opticalbeta\":\"\",\"opticalgamma\":\"\",\"opticalomega\":\"\",\"opticalepsilon\":\"\",\"opticalalpha2\":\"0\",\"opticalbeta2\":\"0\",\"opticalgamma2\":\"0\",\"opticalepsilon2\":\"0\",\"opticalomega2\":\"0\",\"opticaln\":\"\",\"opticaln2\":\"\",\"optical2vcalc\":\"\",\"optical2vmeasured\":\"\",\"optical2vcalc2\":\"\",\"optical2vmeasured2\":\"\",\"opticalalphaerror\":0,\"opticalbetaerror\":0,\"opticalgammaerror\":0,\"opticalomegaerror\":0,\"opticalepsilonerror\":0,\"opticalnerror\":0,\"optical2vcalcerror\":0,\"optical2vmeasurederror\":0,\"opticaldispersion\":\"\",\"opticalpleochroism\":\"\",\"opticalpleochorismdesc\":\"\",\"opticalbirefringence\":\"\",\"opticalcomments\":\"\",\"opticalcolour\":\"\",\"opticalinternal\":\"\",\"opticaltropic\":\"\",\"opticalanisotropism\":\"\",\"opticalbireflectance\":\"\",\"opticalr\":\"\",\"uv\":\"\",\"ir\":\"\",\"magnetism\":\"\",\"type_specimen_store\":\"\",\"commenthard\":\"\",\"strunz10ed1\":\"0\",\"strunz10ed2\":\"0\",\"strunz10ed3\":\"0\",\"strunz10ed4\":\"\",\"dana8ed1\":\"0\",\"dana8ed2\":\"0\",\"dana8ed3\":\"0\",\"dana8ed4\":\"0\",\"thermalbehaviour\":\"\",\"commentluster\":\"\",\"commentbreak\":\"\",\"commentdense\":\"\",\"commentcrystal\":\"\",\"commentcolor\":\"\",\"electrical\":\"\",\"tranglide\":\"\",\"nolocadd\":0,\"weighting\":1,\"specdispm\":\"\",\"spacegroupset\":\"0\",\"approval_year\":0,\"publication_year\":0,\"ima_history\":\"\",\"rock_parent\":0,\"rock_parent2\":0,\"rock_root\":0,\"rock_bgs_code\":\"\",\"meteoritical_code\":\"\",\"key_elements\":\"\",\"shortcode_ima\":\"\"}"
  expect_no_error(df<-mindat_parse_raw_data(raw_data))
  expect_equal(typeof(df), "list")
})


test_that("Test mindat_extract_response_body function", {
  test_base_token =   #Your_token
  mindat_connection(test_base_token)
  request_uri = "https://api.mindat.org/geomaterials/6"
  api_token <- mindat_cache_get('api_token')
  response_data <- GET(request_uri,add_headers('Authorization'= paste('Token ',api_token,sep = "")))
  expect_no_error(mindat_extract_response_body(response_data))
})


test_that("Test mindat_get_data_from_uri function", {
  test_base_token = #Your_token
  mindat_connection(test_base_token)
  request_uri = "https://api.mindat.org/geomaterials/6"
  expect_no_error(df <- mindat_get_data_from_uri(request_uri))
  expect_equal(typeof(df), "list")

})


test_that("Test mindat_build_querystring function", {
  args <- list(ids = "6,7,8",fields ="id,name,updttime,ima_formula,ima_status,ima_notes,key_elements",elements_inc = "H,O")
  expect_equal( mindat_build_querystring(args),"?id__in=6,7,8&fields=id,name,updttime,ima_formula,ima_status,ima_notes,key_elements&elements_inc=H,O")
})
