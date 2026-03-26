# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(terra)

pop <- rast(here("Data", "input", "grid", "worldpop", "ppp_2018_1km_Aggregated.tif"))
ext(pop) <- ext(-180, 180, -72, 84)
pop <- ifel(is.na(pop), 0, pop)
writeRaster(pop, here("Data", "inter", "grid", "pop_proj.tif"), overwrite = TRUE, progress = 0)
