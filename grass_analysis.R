# -------------------------
# -------------------------
# For analysing data in GRASS
# -------------------------
# -------------------------

# Terrain preparation
system("r.slope.aspect elevation=Terrain50@hill_farms slope=Terrain50_slope aspect=Terrain50_aspect")

# Extract terrain data
system("g.copy --overwrite vector=ag_parishes@hill_farms,terrain_parish")
system("v.db.addcolumn map=terrain_parish columns=area_part DOUBLE PRECISION")
system("v.to.db map=terrain_parish option=area columns=area_part")
system("v.rast.stats map=terrain_parish@hill_farms raster=Terrain50@hill_farms column_prefix=elev_ method=minimum,maximum,average,median")
system("v.rast.stats map=terrain_parish@hill_farms raster=Terrain50_slope@hill_farms column_prefix=slope_ method=minimum,maximum,average,median")

# Query vector layers
f = list.files(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial"), pattern="*.shp$")
f = substr(f, 1, nchar(f) - 4)

# Get total area of each vector
lapply(f, function(i){
   system(paste0("v.db.addcolumn map=", i, "@hill_farms columns='", i, "_t_area DOUBLE PRECISION'"))
   system(paste0("v.to.db map=", i, "@hill_farms option=area columns=", i, "_t_area"))
})

# Split polygons
lapply(f, function(i){
   system(paste0("v.overlay --overwrite ainput=", i, "@hill_farms binput=ag_parishes@hill_farms operator=and output=", i, "_parishes"))
})

# Get sub area of each cut vector
lapply(f, function(i){
   system(paste0("v.db.addcolumn map=", i, "_parishes@hill_farms columns='", i, "_area DOUBLE PRECISION'"))
   system(paste0("v.to.db map=", i, "_parishes@hill_farms option=area columns=", i, "_area"))
})

# Export
lapply(c(f, "terrain"), function(i){
   system(paste0("v.out.ogr --overwrite input=", i, "_parishes@hill_farms output=", normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/", i, "_parishes.csv format=CSV"))
})
