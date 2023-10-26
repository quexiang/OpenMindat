install.packages("httr")
install.packages("jsonlite")
install.packages("OpenMindat")
install.packages("usethis")
library(OpenMindat)
library(httr)
library(jsonlite)
library(usethis)

#input your Mindat API token
mindat_connection("3214e7170011236535c9a6e17d4ebd69", page_size = 1000) #including the quotation marks

help(package = OpenMindat)

#query all the IMA mineral species list
df_ima_minerals <- minerals_ima_list()

#return localities that contain all the given elements
critical_min_loc <- localities_list_elems_inc(c("Al", "As", "Sb", "Ba", "Be"
                            ))

#critical_min_loc <- localities_list_elems_inc(c("Al", "As", "Sb", "Ba", "Be", "Bi", "Ce", "Cs", "Cr", "Co", "Dy",
#                                                "Er", "Eu", "F", "Gd", "Ga", "Ge", "Hf", "Ho",
#                                                "In",
#                                                "Yb", "Zn", "Zr"
#))


resp_materials_elms_df <- geomaterials_contain_any_elems(c('Be','Cr'))

#get locality info by using ID, e.g., Idaho - 3724
idaho <- localities_retrieve_id(3724)
idaho_elements <- idaho$elements

#Latah County, Idaho
Latah_County_Idaho <- localities_retrieve_id(28063)

#get all localities in a country, e.g., Norway
localities_norway <- localities_list_country("Norway")

#get geomaterials list by using element conditions
#Retrieve a list of mineral species matching certain chemical criteria, such as
#‘mineral species containing nickel or cobalt, with sulphur but without oxygen’
#entrytype=0 - minerals
#synid=0 - not synonym
resp_materials_elms_df_1 <- geomaterials_contain_all_but_without_elems(c('Ni','S'),c('O'), entrytype=0, synid=0)
resp_materials_elms_df_2 <- geomaterials_contain_all_but_without_elems(c('Co','S'),c('O'), entrytype=0, synid=0)

#by default the merge function will remove the duplicated records
result <- merge(resp_materials_elms_df_1, resp_materials_elms_df_2, all.x=TRUE, all.y=TRUE)

#here if we query with the type "1" (synonym) the returned results look weird - seems no all synonyms
#in the database are marked type "1"
resp_materials_elms_df_3 <- geomaterials_contain_all_but_without_elems(c('Ni','S'),c('O'), entrytype=1)
resp_materials_elms_df_4 <- geomaterials_contain_all_but_without_elems(c('Co','S'),c('O'), entrytype=1)

# 0 - mineral
# 1 - synonym
# 2 - variety
# 3 - mixture
# 4 - series
# 5 - grouplist
# 6 - polytype
# 7 - rock
# 8 - commodity

# Validate alternative mineral/rock names and retrieve the formal name - e.g. enter "Amethyst", return "Quartz"
tt = geomaterials_name("Amethyst")
# ss = mindat_geomaterial(id = tt$varietyof) - this function takes only one parameter id
ss = mindat_geomaterial_list(ids = c(tt$varietyof), entrytype=0,
                               ima_status = "APPROVED")


# Get the rock hierarchy information
# entrytype=7 - rock
df_geomaterial_rock_parent <- mindat_geomaterial_list(ids = c(''),
                                                      entrytype=7,
                                                      fields = c("name","description_short", "rock_parent","rock_parent2")
                                                      )

