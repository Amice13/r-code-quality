reticulate::use_condaenv("C:/Users/nmarantz/AppData/Local/Programs/ArcGIS/Pro/bin/Python/envs/arcgispro-py3")

arcpy <- reticulate::import("arcpy")


library(arcgisbinding)
arc.check_product()
library(arrow)
library(dplyr)
library(reticulate)
library(here)
library(sf)
library(stringr)

arcpy$CheckOutExtension("Spatial")
arcpy$env$overwriteOutput = TRUE

arcpy$env$workspace = paste0(here::here(), "/data-raw-temp/usgs")

clip_feature = paste0(here::here(), "/data/samp_trt.shp")

dir.create("data/clipped")

files <- arcpy$ListRasters() |> str_subset("^USGS")

for (raster in files) {
  out <- file.path(paste0(here::here(), "/data/clipped/c", 
                          sub("\\.tif$", ".shp", raster)))
  
  # Convert digital elevation model raster to slope raster
  slope_raster <- arcpy$sa$SurfaceParameters(
    in_raster = raster,
    parameter_type="SLOPE",
    local_surface_type="QUADRATIC",
    neighborhood_distance="9.25925927753796E-05 DecimalDegrees",
    use_adaptive_neighborhood="FIXED_NEIGHBORHOOD",
    z_unit="METER",
    output_slope_measurement="PERCENT_RISE",
    project_geodesic_azimuths="GEODESIC_AZIMUTHS",
    use_equatorial_aspect="NORTH_POLE_ASPECT",
    in_analysis_mask = clip_feature
  )
  
  # Reclassify: <15 -> 0; >=15 -> 1
  slope_raster <- arcpy$ddd$Reclassify(
    in_raster=slope_raster,
    reclass_field="VALUE",
    remap="0 15 0;15 10000 1;NODATA 0",
    missing_values="NODATA"
  )
  
  # Convert raster to polygon
  arcpy$conversion$RasterToPolygon(
    in_raster=slope_raster,
    out_polygon_features=out,
    simplify="SIMPLIFY",
    raster_field="Value",
    create_multipart_features="SINGLE_OUTER_PART",
    max_vertices_per_feature="#"
  )
  
  cat(sprintf("%s has been converted and saved as %s\n", raster, out))
}

arcpy$env$workspace = paste0(here::here(), "/data/clipped")

# Create a string with the shapefiles for the SCAG region
scag <- dir("data/clipped") |>
  str_subset("^cUSGS_13_n(34|35|36).*\\.shp$") |>
  paste0(collapse = ";")

# Merge SCAG files
arcpy$management$Merge(
  inputs=scag,
  output=paste0(here::here(), "/data/scag_merge.shp"),
  add_source="NO_SOURCE_INFO"
)

# Create a string with the shapefiles for the ABAG region
abag <- dir("data/clipped") |>
  str_subset("^cUSGS_13_n(37|38|39).*\\.shp$") |>
  paste0(collapse = ";")

# Merge ABAG files
arcpy$management$Merge(
  inputs=abag,
  output=paste0(here::here(), "/data/ba_merge.shp"),
  add_source="NO_SOURCE_INFO"
)

# SCAG

scag_ids <- read_feather("data/SCAG_samp.feather") |>
  dplyr::select(PARCEL_SCAGUID16) |>
  pull()
  
dir.create("data/scag_samp")

for (county in c("Orange", "Riverside", "San_Bernardino", "Ventura")) {
  st_read(paste0("data-raw-temp/scag/2016_Land_Use_Information_for_", 
                 county, "_County.shp")) |> 
    dplyr::filter(SCAGUID16 %in% scag_ids) |>
    dplyr::select(SCAGUID16) |>
    st_write(paste0("data/scag_samp/", county, ".gpkg"), 
             delete_layer = TRUE, 
             append=FALSE)
}

st_read("data-raw-temp/scag/41ef1833997543258dd88c6077f00f61.gdb", 
        layer = "LandUse_Combined_LA") |> 
  dplyr::filter(SCAGUID16 %in% scag_ids) |>
  dplyr::select(SCAGUID16) |>
  st_write("data/scag_samp/Los_Angeles.gpkg", 
           delete_layer = TRUE, 
           append=FALSE)

try(
  arcpy$management$CreateFileGDB(
    out_folder_path=here::here(),
    out_name="slope",
    out_version="CURRENT"
  )
)

arcpy$conversion$ExportFeatures(
  in_features=paste0(here::here(), "/data/scag_merge.shp"),
  out_features=paste0(here::here(),"/slope.gdb/scag_slope"),
  where_clause="",
  use_field_alias_as_name="NOT_USE_ALIAS",
  sort_field="#"
)

for (county in c("Orange", "Riverside", "San_Bernardino", 
                 "Ventura", "Los_Angeles")) {
  
  arcpy$conversion$ExportFeatures(
    in_features=paste0(here::here(), "/data/scag_samp/",
                       county, ".gpkg/main.", county),
    out_features=paste0(here::here(), "/slope.gdb/", county),
    where_clause="",
    use_field_alias_as_name="NOT_USE_ALIAS",
    sort_field="#"
  )
  
  arcpy$analysis$PairwiseIntersect(
    in_features=paste0(here::here(),"/slope.gdb/", county,";",
                       here::here(), "/slope.gdb/scag_slope"),
    out_feature_class=paste0(here::here(), "/slope.gdb/", county, "_int"),
    join_attributes="ALL",
    cluster_tolerance="#",
    output_type="INPUT"
  )
  
  arcpy$management$CalculateGeometryAttributes(
    in_features=paste0(here::here(), "/slope.gdb/", county, "_int"),
    geometry_property="area AREA_GEODESIC",
    length_unit="",
    area_unit="SQUARE_FEET_US",
    coordinate_system='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]',
    coordinate_format="SAME_AS_INPUT"
  )
}


# Bay Area

st_read("data/bay_samp.gdb", layer = "Parcels_BAY_STUDY_AREA_UA") |>
  st_write("slope.gdb", layer = "abag_parcels")

arcpy$conversion$ExportFeatures(
  in_features=paste0(here::here(), "/data/ba_merge.shp"),
  out_features=paste0(here::here(),"/slope.gdb/abag_slope"),
  where_clause="",
  use_field_alias_as_name="NOT_USE_ALIAS",
  sort_field="#"
)

arcpy$analysis$PairwiseIntersect(
  in_features=paste0(here::here(), "/slope.gdb/abag_parcels", ";",
                     here::here(), "/slope.gdb/abag_slope"),
  out_feature_class=paste0(here::here(), "/slope.gdb/abag_int"),
  join_attributes="ALL",
  cluster_tolerance="#",
  output_type="INPUT"
)

arcpy$management$CalculateGeometryAttributes(
  in_features=paste0(here::here(), "/slope.gdb/abag_int"),
  geometry_property="area AREA_GEODESIC",
  length_unit="",
  area_unit="SQUARE_FEET_US",
  coordinate_system='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]',
  coordinate_format="SAME_AS_INPUT"
)

# Calculate proportion of steep slope and combine regions

scag_slp <- bind_rows(st_read("slope.gdb", layer = "Orange_int") |>
                        as.data.frame(),
                      st_read("slope.gdb", layer = "Riverside_int") |>
                        as.data.frame(),
                      st_read("slope.gdb", layer = "San_Bernardino_int") |>
                        as.data.frame(),
                      st_read("slope.gdb", layer = "Ventura_int") |>
                        as.data.frame(),
                      st_read("slope.gdb", layer = "Los_Angeles_int") |>
                        as.data.frame()) |> 
  group_by(SCAGUID16, gridcode) |> 
  summarise(disag_area = sum(area)) |> 
  group_by(SCAGUID16, .drop = FALSE) |> 
  mutate(tot_area = sum(disag_area)) |> 
  mutate(prop_steep_slope = (gridcode*disag_area)/tot_area) |>
  dplyr::select(SCAGUID16, prop_steep_slope) |>
  ungroup() |>
  rename(PARCEL_ID = SCAGUID16) |> 
  group_by(PARCEL_ID) |> 
  summarise(prop_steep_slope = sum(prop_steep_slope)) |> 
  ungroup()
  

abag_slp <- st_read("slope.gdb", layer = "abag_int") |>
  as.data.frame() |>
  group_by(PARCEL_ID, gridcode) |> 
  summarise(disag_area = sum(area)) |> 
  group_by(PARCEL_ID, .drop = FALSE) |> 
  mutate(tot_area = sum(disag_area)) |> 
  mutate(prop_steep_slope = (gridcode*disag_area)/tot_area) |>
  dplyr::select(PARCEL_ID, prop_steep_slope) |>
  summarise(prop_steep_slope = sum(prop_steep_slope)) |> 
  ungroup()

bind_rows(scag_slp, abag_slp) |>
  arrow::write_feather("data/slope.feather")
