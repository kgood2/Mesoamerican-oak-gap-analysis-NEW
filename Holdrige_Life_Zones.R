library("raster")
library("rasterVis")

setwd("/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points/inputs/gis_data/holdridge_life_zones/Life_zones_rasters")

raster1 <- raster("zv1.tif")
raster2 <- raster("zv2.tif")
raster3 <-raster("zv3.tif")
raster4 <-raster("zv4.tif")
raster5 <- raster("zv5.tif")
raster6 <- raster("zv6.tif")
raster7 <- raster("zv7.tif")
raster8 <- raster("zv8.tif") 
raster9 <- raster("zv9.tif") 
raster10 <- raster("zv10.tif") 
raster11 <- raster("zv11.tif") 
raster12 <- raster("zv12.tif") 
raster13 <- raster("zv13.tif") 
raster14 <- raster("zv14.tif") 
raster15 <- raster("zv15.tif") 
raster16 <- raster("zv16.tif") 
raster17 <- raster("zv17.tif") 
raster18 <- raster("zv18.tif") 
raster19 <- raster("zv19.tif") 
raster20 <- raster("zv20.tif") 
raster21 <- raster("zv21.tif") 
raster22 <- raster("zv22.tif") 
raster23 <- raster("zv23.tif") 
raster24 <- raster("zv24.tif") 
raster25 <- raster("zv25.tif") 
raster26 <- raster("zv26.tif") 
raster27 <- raster("zv27.tif") 


x <- list(raster1, raster2, raster3, raster4, raster5, raster6, raster7, raster8, raster9,
          raster10, raster11, raster12, raster13, raster14, raster15, raster16, raster17, raster18,
          raster19, raster20, raster21, raster22, raster23, raster24, raster25, raster26, raster27)

names(x)[1:2] <- c('x', 'y')
x$fun <- mean
x$na.rm <- TRUE

m <- do.call(mosaic, x)
plot(m)

#################
# trying to add color. Not working 
plot(raster1)
col_red <- colorRampPalette(c("white", "red"))
plot(raster1, col = col_red(100))
###################

########
#trying to turn into shapefiles, taking very long time
polygons <- rasterToPolygons(raster1, dissolve=TRUE)
data_values <- extract(raster1, polygons)
spatial_polygons_df <- SpatialPolygonsDataFrame(polygons, data = data.frame(value = unlist(data_values)))
#########











