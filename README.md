# OpenMindat
R package for OpenMindat

`OpenMindat` is a package for downloading, processing data from [mindat.org Database](https://www.mindat.org/).

### Quick start

**Install**
Install OpenMindat from source codes

1. Download all the source codes and Open the OpenMindat.Rproj with RStudio.

2. Install packages "httr" and "jsonlite". 

3. Select the "Install Package" in the "Build" menu of RStudio (Ctrl + Shift + B). 

4. Create a New R Script (Ctrl + Shift + N).

5. Load the packages "OpenMindat", "httr", and "jsonlite". 

```coffee
#install.packages("httr")
#install.packages("jsonlite")
#install.packages("OpenMindat")
library(OpenMindat)
library(httr)
library(jsonlite)
```

**Set up your mindat api token**
1. You should first get your own Mindat api token `YourToken` . ([How to get your mindat API token ?](https://www.mindat.org/a/how_to_get_my_mindat_api_key))

2. Set up your token `YourToken`

```coffee
#input your Mindat API token
mindat_connection("YourToken") #including the quotation marks
```

3. You can now retrieve datasets of interest using functions in the "OpenMindat" package. Below are a few examples.

```coffee

#query the geomaterials that contain all of the elements (e.g. Be, Cr):
resp_materials_elms_df <- geomaterials_contain_all_elems(c('Be','Cr'))

#query the geomaterials that contain any of the elemnts(e.g. Be,Cr):
resp_materials_elms_df <- geomaterials_contain_any_elems(c('Be','Cr'))

#query the geomaterials that contain all of the elemnts(e.g. Be,Cr) without the element (e.g. H):
resp_materials_elms_df <- geomaterials_contain_all_and_without_elems(c('Be','Cr'),c('H'))

#query the geomaterials by given crystal system (e.g. Hexagonal):
resp_materials_crystalsys_df <- geomaterials_crystal_system(c("Hexagonal"))

#query the geomaterials by given cleavagetype (e.g. Imperfect/Fair):
resp_materials_cleav_df <- geomaterials_cleavagetype(c("Imperfect/Fair"))

#query the geomaterials whose hardness greater than a given value(e.g. 9):
resp_materials_hard_df <- geomaterials_hardness_gt(9)

#query the geomaterials whose hardness less than a given value(e.g. 1):
resp_materials_hard_df <- geomaterials_hardness_lt(1)

#query the geomaterials whose hardness within a range e.g.(1,1.2):
resp_materials_hard_df <- geomaterials_hardness_range(1,1.2)

#query the mindat geomaterials by a given id, e.g.6: 
resp_materials_id_df <- mindat_geomaterial(id=6)

#query all the IMA list
df_ima_minerals <- minerals_ima_list()

#query all the IMA list (only show the ima mame list):
df_ima_minerals <- minerals_ima_list(fields = "name")

#query the IMA minerals by a given id e.g. 1 :
df_ima_minerals <- minerals_ima_retrieve(id =1)

#query localities in a given country (e.g. Chian):
df_localities <- localities_list_country("China")

#query localities contain the elements(e.g. Be,Si) withou the elements(e.g. H,Al) :
df_loc_inc_exc <- localities_list_elems_inc_exc(c("Be","Si"),c("H","Al"))

```

**Bug Reports**

[Any issues or bugs?](https://github.com/quexiang/OpenMindat/issues )

**Related Articles**

[OpenMindat: Open and FAIR mineralogy data from the Mindat database](https://doi.org/10.1002/gdj3.204)
