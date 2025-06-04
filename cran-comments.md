## Resubmission
This is a resubmission. In this version I have:

* Update the default settings of base_url to avoid failure in data retrieval.

* Updated the endpoints of 'minerals-ima','locality-age', 'locality-status', 'locality-type', and 'page-size' to keep consist with the API Server.

* Add some new endpoints: "locentries","locentries_statistics","nickel-strunz-10","dana8"etc.

* Fixed mindat_connection() function and updated its default settings of base_url to avoid failure in data retrieval.

* Add functions: "mindat_crystalclasses" , "mindat_spacegroups", "mindat_spacegroups_list",
  "mindat_spacegroupsets","mindat_spacegroupsets_list","spacegroups_by_id ","spacegroups_list ",
  "spacegroups_cclass","spacegroups_sgtext","spacegroupsets_by_id","spacegroupsets_list","spacegroupsets_cclass",
  "spacegroupsets_sgtext","crystalclasses_symbols","crystalclasses_systems".
  
* Replaced the field 'id__in' with 'id_in' according to the Mindat API Server's updated.


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
