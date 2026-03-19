###############################
# PostGRES 2 R Spatial Functions
# #############################


dbReadSpatial <- function(con, schemaname="public", 
                          tablename, geomcol="the_geom", idcol=NULL, where="",
                          dropNULLGeometries = TRUE) {
  
  # con:          A PostgreSQL connection (from RPostgreSQL)
  # spatial.df:   A Spatial Data Frame object
  # schemaname:   Target schema name
  # tablename:    Target table name
  # geomcol:      Name of the geometry column in the target table (target table may not have more than one geometry column!)
  # idcol:        Name of the column with unique IDs to be used in the ID slot of the spatial objects (not relevant for point data)
  # Added possibility for subsetting table via where clause (Carl)
  
  require(RPostgreSQL)
  require(rgdal)
  ## Build query and fetch the target table
  # Get column names
  q.res <- dbSendQuery(con, statement=paste("SELECT column_name FROM information_schema.columns WHERE table_name ='", tablename, "' AND table_schema ='", schemaname, "';", sep=""))
  schema.table = paste(schemaname, ".", tablename, sep="")
  q.df <- fetch(q.res, -1)
  # Some safe programming
  if (!(geomcol %in% q.df[,1])) {stop(paste("No", geomcol, "column in specified table."))}
  if (!is.null(idcol)) {
    if (!(idcol %in% q.df[,1])) {stop(paste("Specified idname '", idcol, "' not found.", sep=""))}
  }
  # Get table
  query <- paste('SELECT', paste(paste0('"', q.df[,1][q.df[,1] != geomcol], '"'), collapse=", "), 
                 paste(', ST_ASTEXT(', geomcol, ') AS the_geom FROM', sep=""), schema.table, 
                 " ", where, ";")
  t.res <- dbSendQuery(con, statement=query)
  t.df <- fetch(t.res, -1)
  
  ## Get geometry ID column number
  if (!is.null(idcol)) {
    idcolnum <- which(names(t.df) == idcol)
  } else {
    t.df$id.new <- seq_len(nrow(t.df))
    idcolnum <- which(names(t.df) == "id.new")
  }
  
  ## Get geometry column number
  geomcolnum <- which(names(t.df) == "the_geom")
  
  ## Build spatial data frame using OGR
  write.df <- t.df[,geomcolnum,drop=FALSE]
  names(write.df) <- "WKT"
  filename <- paste("vector_", as.character(format(Sys.time(), "%H_%M_%S")), sep="")
  filename.csv <- paste(filename, ".csv", sep="")
  write.csv(write.df, paste(gsub("[\\]", "/", tempdir()), "/", filename.csv, sep=""), row.names=TRUE)
  down.spdf <- readOGR(dsn=paste(gsub("[\\]", "/", tempdir()), "/", filename.csv, sep=""), 
                       layer=filename, verbose=FALSE, dropNULLGeometries = dropNULLGeometries)
  rv <- file.remove(paste(gsub("[\\]", "/", tempdir()), "/", filename.csv, sep=""))
  if(dropNULLGeometries){
    data.df <- data.frame(t.df[!grepl("EMPTY",write.df[,1]),-geomcolnum])
  } else {
    data.df <- data.frame(t.df[,-geomcolnum])
  }
  names(data.df) <- names(t.df)[-geomcolnum]  
  
  # For Spatial Points Data Frame  
  if (grepl("POINT", t.df[1,geomcolnum])) {
    spatial.df <-  SpatialPointsDataFrame(down.spdf@coords, data.df, match.ID=FALSE)
  }
  # For Spatial Polygons/Lines Data Frame    
  if (grepl("POLYGON", t.df[1,geomcolnum]) | grepl("LINE", t.df[1,geomcolnum])) {
    spatial.df <- down.spdf
    spatial.df@data <- data.df
    if(dropNULLGeometries){
      spatial.df <- spChFIDs(spatial.df, paste(t.df[!grepl("EMPTY",write.df[,1]),idcolnum]))
    } else {
      spatial.df <- spChFIDs(spatial.df, paste(t.df[,idcolnum]))
    }
    
  }
  return(spatial.df)
}

dbWriteSpatial <- function(con, spatial.df, schemaname="public", tablename, replace=FALSE) {
  
  # con:          A PostgreSQL connection (from RPostgreSQL)
  # spatial.df:   A Spatial Data Frame object
  # schemaname:   Target schema name
  # tablename:    Target table name
  # replace:      Replace the target table if it already exists
  require(RPostgreSQL)
  require(rgdal)
  
  # Create well known text and add to spatial DF
  spatialwkt <- writeWKT(spatial.df, byid=TRUE)
  spatial.df$wkt <- spatialwkt
  
  # Add temporary unique ID to spatial DF
  spatial.df$spatial_id <- 1:nrow(spatial.df)
  
  # Set column names to lower case
  names(spatial.df) <- tolower(names(spatial.df))
  
  # Upload DF to DB
  data.df <- spatial.df@data
  rv <- dbWriteTable(con, c(schemaname, tablename), data.df, overwrite=replace, row.names=FALSE)
  
  # Create geometry column and clean up table
  schema.table <- paste(schemaname, ".", tablename, sep="")
  query1 <- paste("ALTER TABLE ", schema.table, " ADD COLUMN the_geom GEOMETRY;", sep="")
  query2 <- paste("UPDATE ", schema.table, " SET the_geom = ST_GEOMETRYFROMTEXT(t.wkt) FROM ", 
                  schema.table, " t  WHERE t.spatial_id = ", schema.table, ".spatial_id;", sep="")
  query3 <- paste("ALTER TABLE ", schema.table, " DROP COLUMN spatial_id;")
  query4 <- paste("ALTER TABLE ", schema.table, " DROP COLUMN wkt;")
  er <- dbSendQuery(con, statement=query1)
  er <- dbSendQuery(con, statement=query2)
  er <- dbSendQuery(con, statement=query3)
  er <- dbSendQuery(con, statement=query4)
  
  return(TRUE)
}

