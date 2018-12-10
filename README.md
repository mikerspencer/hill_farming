[![DOI](https://zenodo.org/badge/160194504.svg)](https://zenodo.org/badge/latestdoi/160194504)

# Hill farming study

Repo contains work looking at the economic impact, especially employment, of agriculture in upland areas of Scotland.


## Organisation

Split into folders:

* **processing** files to create the index of hilliness from input data
* **exploratory** checking variables and exploring data created/remapped
* **outputs** images for posters, presentations and reports.


## Software

Software versions used are newer than:

* R 3.4
* GRASS 7.4


## Recreation

To recreate the spatial data processing (i.e. convert input data to parish boundaries), use the `processing/grass_*.R` files. Preparing agricultural and population census data is done with `processing/read_census.R` and `processing/read_ag_census.R`. To create the hilliness score use `exploratory/define_hill-farming.*`, which also explores the input variables used.
