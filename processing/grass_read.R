# -------------------------
# -------------------------
# For reading files into GRASS
# -------------------------
# -------------------------

f = list.files(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial"), pattern="*.shp$")

f1 = substr(f, 1, nchar(f) - 4)

# shp
lapply(1:length(f), function(i){
   system(paste0("v.in.ogr input=", normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial/", f[i], " layer=", f1[i], " output=", f1[i]))
})

# geopackage
system(paste0("v.in.ogr input=", normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial/ag_parishes_2016.gpkg layer=original_parishes output=ag_parishes"))

# raster
system(paste0("r.in.gdal input=", normalizePath("~"), "/GIS/OS/Terrain50_Scotland.tif output=Terrain50_Scotland"))
