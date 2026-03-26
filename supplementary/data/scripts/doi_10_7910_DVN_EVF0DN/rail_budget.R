#######################
# MAKE RAILROAD BUDGET BY COUNTRY YEAR
#######################


# Load cshapes
cshp <- readRDS(cshp.path)
cshp <- as_Spatial(cshp)
if(DEBUG){
  cshp <- cshp[cshp$name_uni %in% c("United Kingdom", "Switzerland", "Germany", "San Marino", "France"),]
}
cshp <- cshp[cshp$from <= max(YRS) & 
               cshp$to >= min(YRS),]
cshp$year <- NULL

# Load rshapes
rshp <- readOGR(rs.path, 
                "RShapes")

# Export
clusterExport(cl, c("cshp", "rshp", "YRS"))

# Count km
rshp.length <- foreach(i = seq_along(cshp), 
                       .options.multicore = list(preschedule = FALSE),
                       .combine = rbind) %dopar% {
                         # Crop
                         rs <- rshp[gIntersects(rshp, cshp[i,], byid = T)[1,],]

                         # Count km per year
                         if(length(rs) > 0){
                           rs$length <- geosphere::lengthLine(rs)/1000
                           cbind(cshp@data[rep(i, length(YRS)),],
                                 cshp.id = i,
                                 year = YRS,
                                 length = sapply(YRS, function(y){
                                   sum(rs$length[rs$year == y])
                                 })
                           )
                           
                         } else {
                           cbind(cshp@data[rep(i, length(YRS)),],
                                 cshp.id = i,
                                 year = YRS,
                                 length = 0)
                         }
                       }

# Save
saveRDS(rshp.length, 
        file = file.path(out.path, "rshp_length_df.rds"))
