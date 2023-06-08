# OpenMindat
R package for OpenMindat

`OpenMindat` is a package for downloading, processing data from [mindat.org Database](https://www.mindat.org/).

### Quick start

**Install**
Install OpenMindat from source codes

1.Download all the source codes and Open the OpenMindat.Rproj with RStudio.

2.Select the "Install Package" in the "Build" menu of RStudio (Ctrl+Shift+B).

3.Create a New R Script (Ctrl + Shift + N).

4.Load the required packages

```coffee
#install.packages("OpenMindat")
library(OpenMindat)
library(http)
library(jsonlite)
```

**Set up your mindat api token**

1. You should first get your own Mindat api token`YourToken` . ([How to get your mindat API token ?](https://www.mindat.org/a/how_to_get_my_mindat_api_key))

2. Set up your token `YourToken`

```coffee
#input `YourToken`
mindat_connection(YourToken)
```

3.Then you can easily retrieve the dataset as you want: 

```coffee

#query the geomaterials that contain all of the elements (Be, Cr):
resp_materials_elms_df <- geomaterials_contain_all_elems(c('Be','Cr'))

#query the geomaterials that contain any of the elemnts(Be,Cr):
resp_materials_elms_df <- geomaterials_contain_any_elems(c('Be','Cr'))

#query the geomaterials that contain all of the elemnts(Be,Cr) without the element (H):
resp_materials_elms_df <- geomaterials_contain_all_and_without_elems(c('Be','Cr'),c('H'))

#query the geomaterials by given crystal system (Hexagonal):
resp_materials_crystalsys_df <- geomaterials_crystal_system(c("Hexagonal"))

#query the geomaterials by given cleavagetype (Imperfect/Fair):
resp_materials_cleav_df <- geomaterials_cleavagetype(c("Imperfect/Fair"))

#query the geomaterials whose hardness greater than a given value(9):
resp_materials_hard_df <- geomaterials_hardness_gt(9)

#query the geomaterials whose hardness less than a given value(1):
resp_materials_hard_df <- geomaterials_hardness_lt(1)

#query the geomaterials whose hardness within a range(1,1.2):
resp_materials_hard_df <- geomaterials_hardness_range(1,1.2)

#query the mindat geomaterials by a given id: 
resp_materials_id_df <- mindat_geomaterial(id=6)

#query all the IMA list
df_ima_minerals <- minerals_ima_list()

#query all the IMA list (only show the ima mame list):
df_ima_minerals <- minerals_ima_list(fields = "name")

#query the IMA minerals by a given id :
df_ima_minerals <- minerals_ima_retrieve(id =1)

#query localities in a given country (Chian):
df_localities <- localities_list_country("China")

#query localities contain the elements(Be,Si) withou the elements(H,Al) :
df_loc_inc_exc <- localities_list_elems_inc_exc(c("Be","Si"),c("H","Al"))

```

**Bug Reports**

[Any issues or bugs?](https://github.com/quexiang/OpenMindat/issues )

**Related Articles**

[OpenMindat: Open and FAIR mineralogy data from the Mindat database](https://doi.org/10.1002/gdj3.204)
