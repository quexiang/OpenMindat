# OpenMindat
R package for OpenMindat

### Overview
`OpenMindat` is a package for retrieving and processing data from ['mindat.org' Database](https://www.mindat.org/) through its open data API.

University of Idaho and 'mindat.org' collaborate on '['OpenMindat'](https://www.mindat.org/mesg-567769.html)' project.

### Quick start


**Install and load necessary packages**

`OpenMindat` can be installed directly through the `remotes` package:

```coffee
install.packages("remotes")
remotes::install_github("quexiang/OpenMindat")
```


Or through devtools:

```coffee
install.packages('devtools')
library('devtools')
install_github('quexiang/OpenMindat')
```

Or download the source code locally and install it as follows:

  1. Download all the source codes and Open the OpenMindat.Rproj with RStudio.
  
  2. Install packages `usethis`,`httr`,`readxl`, and `jsonlite`. 
  
  3. Currently the `OpenMindat` package can be installed from source files. Select the `Install Package` in the `Build` menu of RStudio (Ctrl + Shift + B). 


Then, you can create a new file to load the dependent package, and then call the function of this package to retrieve and get the data you need from the Mindat API:

1. Create a New R Script (Ctrl + Shift + N).

2. Load the packages `OpenMindat`, `usethis`,`httr`,`readxl`, and `jsonlite`. 


```coffee
#install.packages('httr')
#install.packages('jsonlite')
#install.packages('usethis')
#install.packages('readxl')
#install.packages('OpenMindat')
library(OpenMindat)
library(httr)
library(usethis)
library(jsonlite)
library(readxl)
```

**Set up API connection with your 'mindat.org' API token and use the functions**

1. You should first get your own 'mindat.org' API token `YourToken` . ([How to get your 'mindat.org' API token ?](https://www.mindat.org/a/how_to_get_my_mindat_api_key))

2. Set up your token `YourToken`

```coffee
#input your Mindat API token
mindat_connection("YourToken") #including the quotation marks
```

3. You can now retrieve datasets of interest using functions in the `OpenMindat` package. Below are a few examples.

(1) Retrieve geomaterials records by chemical elements：
```coffee
#query the geomaterials that contain all of the elements (e.g. Be, Cr):
resp_materials_elms_df <- geomaterials_contain_all_elems(c('Be','Cr'))

#query the geomaterials that contain any of the elemnts(e.g. Be,Cr):
resp_materials_elms_df <- geomaterials_contain_any_elems(c('Be','Cr'))

#query the geomaterials that contain all of the elemnts(e.g. Be,Cr) but without the element (e.g. H):
resp_materials_elms_df <- geomaterials_contain_all_but_not_elems(c('Be','Cr'),c('H'))

#query the geomaterials that contain any of the elemnts(e.g. Be,Cr) but without the element (e.g. H):
resp_materials_elms_df <- geomaterials_contain_any_but_not_elems(c('Be','Cr'),c('H'))
```

(2) Retrieve geomaterials records by physical properties:
```coffee
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
```

(3) Retrieve geomaterials records by wildcard names and others:
```coffee
df1<-geomaterials_search_name("Quartz")
df2<-geomaterials_name("qu_rtz")
df3<-geomaterials_name("_u_r_z")
df4<-geomaterials_name("qu*")
df5<- geomaterials_field_exists("meteoritical_code",TRUE)
df6<-mindat_geomaterial(id=3337)
df7<-geomaterials_varietyof(3337)
df8<-geomaterials_entrytype(c('2'))
```
(4) Retrieve geomaterials records by combined conditions:
```coffee
df<-geomaterials_contain_all_elems(c('Li','O'), hardness_min = 5.8, hardness_max = 6, crystal_system = "Triclinic",ima_status = "APPROVED",entrytype = 0)
```
(5) Retrieve IMA minerals:
```coffee
#query all the IMA list
df_ima_minerals <- minerals_ima_list()

#query all the IMA list (only show the ima mame list):
df_ima_minerals <- minerals_ima_list(fields = "name")

#query the IMA minerals by a given id e.g. 1 :
df_ima_minerals <- minerals_ima_retrieve(id =1)

```
(6) Retrieve Localities by descriptions and elements:
```coffee
#query localities in a given country (e.g. China):
df_localities <- localities_list_country("China")

#query localities that contain a given description:
df_volcano<-localities_list_description("volcano")

#query localities contain the elements(e.g. Be,Si) withou the elements(e.g. H,Al) :
df_loc_inc_exc <- localities_list_elems_inc_exc(c("Be","Si"),c("H","Al"))
```
(7) Output the retrieved R dataframe to files:
```coffee

  df <- geomaterials_hardness_gt(9.8,fields = "id,longid,name,ima_formula")
  library(readxl)
  out <- ConvertDF2JsonLD(df)
  saveMindatDataAs(df,"df_geomaterials.jsonld")
  saveMindatDataAs(df,"df_geomaterials.ttl")
  saveMindatDataAs(df,"df_geomaterials.txt")
  saveMindatDataAs(df,"df_geomaterials.csv")

```

**Documention of function list**

An initial version of the `OpenMindat` documentation is available, inclduing a function list and the description of each function, which can be called using the code below.
```coffee
help(package = OpenMindat)

```

**Bug Reports**

[Any issues or bugs?](https://github.com/quexiang/OpenMindat/issues )

**Related Articles**

Ma, X., Ralph, J., Zhang, J., Que, X., Prabhu, A., Morrison, S.M., Hazen, R.M., Wyborn, L., Lehnert, K., 2023. OpenMindat: Open and FAIR mineralogy data from the Mindat database. Geoscience Data Journal, In Press, https://doi.org/10.1002/gdj3.204

[![R-CMD-check](https://github.com/quexiang/OpenMindat/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/quexiang/OpenMindat/actions/workflows/R-CMD-check.yaml)
