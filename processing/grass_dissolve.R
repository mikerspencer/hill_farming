# -------------------------
# -------------------------
# Dissolve SNH polygons
# -------------------------
# -------------------------

library(rgrass7)

f = c("NNR_SCOTLAND", "SAC_SCOTLAND", "SPA_SCOTLAND", "SSSI_SCOTLAND", "WILDLAND_SCOTLAND")

# Copy
# Make dissolving column
# Dissolve

lapply(f, function(i){
   system(paste0("g.copy vector=", i, ",", i, "_cp"))
   system(paste0("v.db.addcolumn map=", i, "_cp columns='dissolve INT'"))
   system(paste0("v.db.update map=", i, "_cp column=dissolve value=1"))
   system(paste0("v.dissolve input=", i, "_cp column=dissolve output=", i, "_diss"))
})


# Split polygons
lapply(f, function(i){
   system(paste0("v.overlay --overwrite ainput=", i, "_diss@hill_farms binput=ag_parishes@hill_farms operator=and output=", i, "_parishes"))
})

# Get sub area of each cut vector
lapply(f, function(i){
   system(paste0("v.db.addcolumn map=", i, "_parishes@hill_farms columns='", i, "_area DOUBLE PRECISION'"))
   system(paste0("v.to.db map=", i, "_parishes@hill_farms option=area columns=", i, "_area"))
})

# Export
lapply(f, function(i){
   system(paste0("v.out.ogr --overwrite input=", i, "_parishes@hill_farms output=", normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/", i, "_parishes.csv format=CSV"))
})
