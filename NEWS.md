
# OpenMindat 1.0.1
  1. `mindat_connection()` update the default settings of base_url to avoid failure in data retrieval.
  2.  The Mindat API server updated its endpoints of 'minerals-ima','locality-age', 'locality-status', 'locality-type', and 'page-size'. We kept consist with the server to aviod failure in data retrieval.
  3.  Add some new endpoints: "locentries","locentries_statistics","nickel-strunz-10","dana8"etc.
  4.  Add files 'mindat_crystalclasses.R', 'mindat_spacegroups.R'. 
  5.  Add functions: "mindat_crystalclasses" , "mindat_spacegroups", "mindat_spacegroups_list", 
  "mindat_spacegroupsets","mindat_spacegroupsets_list","spacegroups_by_id ","spacegroups_list ",
  "spacegroups_cclass","spacegroups_sgtext","spacegroupsets_by_id","spacegroupsets_list","spacegroupsets_cclass",
  "spacegroupsets_sgtext","crystalclasses_symbols","crystalclasses_systems".
  6.  update test files.
  7.  We replaced the field 'id__in' with 'id_in' according to the Mindat API Server's updated.
  
  
# OpenMindat 1.0.0

# OpenMindat 0.0.9

* Initial CRAN submission.
