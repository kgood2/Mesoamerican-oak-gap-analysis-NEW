#######Cartographic model for the life zone system according to Holdridge (1971)
rm(list=ls())
##Create by Isabel Trejo and Angela Cuervo (2016)
## Packages

library("raster")
library("rgdal")
library("sp")
library("grid")

####################INICIO###############################
main_dir <- "/Volumes/GoogleDrive/My Drive/Holdridge Life Zones/WorldClim"
# Read necessary data
ap<- raster(file.path(main_dir,"wc2.1_30s_bio_12.tif")) ##Raster precipitation (mm) bio_12
at <-raster(file.path(main_dir,"wc2.1_30s_bio_1.tif")) #annual mean temperature bio_01

dir.create("Presente_1980-2009_ZVH")

# Explore data
summary(at)
#plot(at)

summary(ap)
#plot(ap)


## Do same proyecting and extent ##Case Mexico
at <- projectRaster(at,ap,method="ngb")
extent(at) <- extent(ap)

##Verify if data and extent are the same ##Answer must be TRUE
compareRaster(at, ap)

##Save the new files

writeRaster(at, filename="Presente_1980-2009_ZVH/at.tif", format="GTiff", overwrite=TRUE)
writeRaster(ap, filename="Presente_1980-2009_ZVH/ap.tif", format="GTiff", overwrite=TRUE)

##Calculate lat 

x<-readGDAL("Presente_1980-2009_ZVH/at.tif")
coor<-coordinates(x) #extrae coordenadas x,y del raster
lat<-coor[,2] #extrae coordenada y
x_lat<-x #raster de latitud
x_lat$band1<-lat #sustitucion de values por latitud
writeGDAL(x_lat,"Presente_1980-2009_ZVH/lat.tif",drivername='GTiff') #raster de latitud
lat<- raster("Presente_1980-2009_ZVH/lat.tif") ##Raster latitude


##Calculate biotemperature with temperature and latitude grade

biotTem <- at ##Create a new file from the original
biotTem [biotTem < 0.0] <- 0 ##Classify data below to 0 into 0
#plot(biotTem)
biotTem [biotTem > 30.0] <- 0 ##Classify data up  to 30.0 into 0
bio0to30 <- biotTem ##Create a file with all the classified data
#plot(bio0to30)

##The data up to 24.0 need a correction: biot = temperature - [3 * latitudegrade/100) * (temperature - 24)^2

is.na (biotTem) <- biotTem >= 24.1 
bio0to24 <- biotTem
biotTem <- bio0to30
is.na (biotTem) <- biotTem <= 23.9
bio24to30 <- biotTem
bioupto24 <- (bio24to30 - (((3.0 * lat) / 100) * ((bio24to30 - 24.0)^2)))
biot <- merge(bioupto24,bio0to24)
writeRaster(biot, filename="Presente_1980-2009_ZVH/biot.tif", format="GTiff", overwrite=TRUE)

##Calculate evapotranspiration (pet)

pet <- biot * 58.93
writeRaster(pet, filename="Presente_1980-2009_ZVH/pet.tif", format="GTiff", overwrite=TRUE)

##Calculate potencial evapotranspiration ratio

per <- pet/ap
writeRaster(per, filename="Presente_1980-2009_ZVH/per.tif", format="GTiff", overwrite=TRUE)

## Calculate categories of annual precipitation (mm)

## [1]  < 125.0
## [2]  125.0 -   250.0
## [3]  250.0 -   500.0
## [4]  500.0 -  1000.0
## [5] 1000.0 -  2000.0
## [6] 2000.0 -  4000.0
## [7] 4000.0 -  8000.0
## [8]  >8000.0


apCat <- ap
apCat[apCat <= 125.0] <- 1
apCat[apCat >=  125.0 & apCat <=  250.0] <- 2
apCat[apCat >=  250.0 & apCat <=  500.0] <- 3
apCat[apCat >=  500.0 & apCat <= 1000.0] <- 4
apCat[apCat >= 1000.0 & apCat <= 2000.0] <- 5
apCat[apCat >= 2000.0 & apCat <= 4000.0] <- 6
apCat[apCat >= 4000.0 & apCat <= 8000.0] <- 7
apCat[apCat >= 8000] <- 8

writeRaster(apCat, filename="Presente_1980-2009_ZVH/apCat.tif", format="GTiff", overwrite=TRUE)

## Calculate latitudinal regions with biot
## Classification of regions
## [1] Polar
## [2] Subpolar
## [3] Boreal
## [4] Cool temperate
## [5] Warm temperate
## [6] Subtropical
## [7] Tropical

latr <- biot
latr[latr <= 1.5] <- 1
latr[latr >= 1.5  & latr <=  3.0] <- 2
latr[latr >= 3.0  & latr <=  6.0] <- 3
latr[latr >= 6.0  & latr <= 12.0] <- 4
latr[latr >= 12.0 & latr <= 18.0] <- 5
latr[latr >= 18.0 & latr <= 24.0] <- 6
latr[latr >= 24.0] <- 7

writeRaster(latr, filename="Presente_1980-2009_ZVH/latr.tif", format="GTiff", overwrite=TRUE)

## Calculate humidity provinces with potencial evapotranspiration ratio
## Classification of provinces

## [1] Semiparched # > 32.0
## [2] Superarid   # 16.0- 32.0
## [3] Perarid     #  8.0- 16.0
## [4] Arid        #  4.0- 8.0 
## [5] Semiarid    #  2.0- 4.0
## [6] Subhumid    #  1.0- 2.0
## [7] Humid       #  0.5- 1.0
## [8] Perhumid    #  0.25-0.5 
## [9] Superhumid  # < 0.25

provhum <- per*1000
provhum[provhum <=   250] <- 9
provhum[provhum >=   250 & provhum <=   500] <- 8
provhum[provhum >=   500 & provhum <=  1000] <- 7
provhum[provhum >=  1000 & provhum <=  2000] <- 6
provhum[provhum >=  2000 & provhum <=  4000] <- 5
provhum[provhum >=  4000 & provhum <=  8000] <- 4
provhum[provhum >=  8000 & provhum <= 16000] <- 3
provhum[provhum >= 16000 & provhum <= 32000] <- 2
provhum[provhum >= 32000] <- 1

writeRaster(provhum, filename="Presente_1980-2009_ZVH/provhum.tif", format="GTiff", overwrite=TRUE)

##Create life zone Holdridge

##  [1] Polar desert
##  [2] Subpolar dry tundra
##  [3] Subpolar moist tundra
##  [4] Subpolar wet tundra
##  [5] Subpolar rain tundra
##  [6] Boreal desert
##  [7] Boreal dry scrub
##  [8] Boreal moist forest
##  [9] Boreal wet forest
## [10] Boreal rain forest
## [11] Cool temperate desert
## [12] Cool temperate desert scrub
## [13] Cool temperate steppe
## [14] Cool temperate moist forest
## [15] Cool temperate wet forest
## [16] Cool temperate rain forest
## [17] Warm temperate desert
## [18] Warm temperate desert scrub
## [19] Warm temperate thorn scrub
## [20] Warm temperate dry forest
## [21] Warm temperate moist forest
## [22] Warm temperate wet forest
## [23] Warm temperate rain forest
## [24] Subtropical desert
## [25] Subtropical desert scrub
## [26] Subtropical thorn woodland
## [27] Subtropical dry forest
## [28] Subtropical moist forest
## [29] Subtropical wet forest
## [30] Subtropical rain forest
## [31] Tropical desert
## [32] Tropical desert scrub
## [33] Tropical thorn woodland
## [34] Tropical very dry forest
## [35] Tropical dry forest
## [36] Tropical moist forest
## [37] Tropical wet forest
## [38] Tropical rain forest


raster1 <- raster("Presente_1980-2009_ZVH/latr.tif") 
raster2 <- raster("Presente_1980-2009_ZVH/apCat.tif")

raster1[raster1 == 	1	 & raster2 == 	1	] <-	1
raster1[raster1 == 	1	 & raster2 == 	2	] <-	1
raster1[raster1 == 	1	 & raster2 == 	3	] <-	1
raster1[raster1 == 	2	 & raster2 == 	1	] <-	2
raster1[raster1 == 	2	 & raster2 == 	2	] <-	3
raster1[raster1 == 	2	 & raster2 == 	3	] <-	4
raster1[raster1 == 	2	 & raster2 == 	4	] <-	5
raster1[raster1 == 	3	 & raster2 == 	1	] <-	6
raster1[raster1 == 	3	 & raster2 == 	2	] <-	7
raster1[raster1 == 	3	 & raster2 == 	3	] <-	8
raster1[raster1 == 	3	 & raster2 == 	4	] <-	9
raster1[raster1 == 	3	 & raster2 == 	5	] <-	10
raster1[raster1 == 	3	 & raster2 == 	6	] <-	10
raster1[raster1 == 	4	 & raster2 == 	1	] <-	11
raster1[raster1 == 	4	 & raster2 == 	2	] <-	12
raster1[raster1 == 	4	 & raster2 == 	3	] <-	13
raster1[raster1 == 	4	 & raster2 == 	4	] <-	14
raster1[raster1 == 	4	 & raster2 == 	5	] <-	15
raster1[raster1 == 	4	 & raster2 == 	6	] <-	16
raster1[raster1 == 	5	 & raster2 == 	1	] <-	17
raster1[raster1 == 	5	 & raster2 == 	2	] <-	18
raster1[raster1 == 	5	 & raster2 == 	3	] <-	19
raster1[raster1 == 	5	 & raster2 == 	4	] <-	20
raster1[raster1 == 	5	 & raster2 == 	5	] <-	21
raster1[raster1 == 	5	 & raster2 == 	6	] <-	22
raster1[raster1 == 	5	 & raster2 == 	7	] <-	23
raster1[raster1 == 	6	 & raster2 == 	1	] <-	24
raster1[raster1 == 	6	 & raster2 == 	2	] <-	25
raster1[raster1 == 	6	 & raster2 == 	3	] <-	26
raster1[raster1 == 	6	 & raster2 == 	4	] <-	27
raster1[raster1 == 	6	 & raster2 == 	5	] <-	28
raster1[raster1 == 	6	 & raster2 == 	6	] <-	29
raster1[raster1 == 	6	 & raster2 == 	7	] <-	30
raster1[raster1 == 	7	 & raster2 == 	1	] <-	31
raster1[raster1 == 	7	 & raster2 == 	2	] <-	32
raster1[raster1 == 	7	 & raster2 == 	3	] <-	33
raster1[raster1 == 	7	 & raster2 == 	4	] <-	34
raster1[raster1 == 	7	 & raster2 == 	5	] <-	35
raster1[raster1 == 	7	 & raster2 == 	6	] <-	36
raster1[raster1 == 	7	 & raster2 == 	7	] <-	37
raster1[raster1 == 	7	 & raster2 == 	8	] <-	38

raster3 <- raster("Presente_1980-2009_ZVH/provHum.tif")

raster1[raster1 == 	1	 & raster3 == 	7	] <-	1
raster1[raster1 == 	1	 & raster3 == 	8	] <-	1
raster1[raster1 == 	1	 & raster3 == 	9	] <-	1
raster1[raster1 == 	2	 & raster3 == 	6	] <-	2
raster1[raster1 == 	3	 & raster3 == 	7	] <-	3
raster1[raster1 == 	4	 & raster3 == 	8	] <-	4
raster1[raster1 == 	5	 & raster3 == 	9	] <-	5
raster1[raster1 == 	6	 & raster3 == 	5	] <-	6
raster1[raster1 == 	7	 & raster3 == 	6	] <-	7
raster1[raster1 == 	8	 & raster3 == 	7	] <-	8
raster1[raster1 == 	9	 & raster3 == 	8	] <-	9
raster1[raster1 == 	10	 & raster3 == 	9	] <-	10
raster1[raster1 == 	11	 & raster3 == 	4	] <-	11
raster1[raster1 == 	12	 & raster3 == 	5	] <-	12
raster1[raster1 == 	13	 & raster3 == 	6	] <-	13
raster1[raster1 == 	14	 & raster3 == 	7	] <-	14
raster1[raster1 == 	15	 & raster3 == 	8	] <-	15
raster1[raster1 == 	16	 & raster3 == 	9	] <-	16
raster1[raster1 == 	17	 & raster3 == 	3	] <-	17
raster1[raster1 == 	18	 & raster3 == 	4	] <-	18
raster1[raster1 == 	19	 & raster3 == 	5	] <-	19
raster1[raster1 == 	20	 & raster3 == 	6	] <-	20
raster1[raster1 == 	21	 & raster3 == 	7	] <-	21
raster1[raster1 == 	22	 & raster3 == 	8	] <-	22
raster1[raster1 == 	23	 & raster3 == 	9	] <-	23
raster1[raster1 == 	24	 & raster3 == 	3	] <-	24
raster1[raster1 == 	25	 & raster3 == 	4	] <-	25
raster1[raster1 == 	26	 & raster3 == 	5	] <-	26
raster1[raster1 == 	27	 & raster3 == 	6	] <-	27
raster1[raster1 == 	28	 & raster3 == 	7	] <-	28
raster1[raster1 == 	29	 & raster3 == 	8	] <-	29
raster1[raster1 == 	30	 & raster3 == 	9	] <-	30
raster1[raster1 == 	31	 & raster3 == 	2	] <-	31
raster1[raster1 == 	32	 & raster3 == 	3	] <-	32
raster1[raster1 == 	33	 & raster3 == 	4	] <-	33
raster1[raster1 == 	34	 & raster3 == 	5	] <-	34
raster1[raster1 == 	35	 & raster3 == 	6	] <-	35
raster1[raster1 == 	36	 & raster3 == 	7	] <-	36
raster1[raster1 == 	37	 & raster3 == 	8	] <-	37
raster1[raster1 == 	38	 & raster3 == 	9	] <-	38

## Rename ZVH with others ID's
raster1[raster1 ==	1	] <-	117
raster1[raster1 == 	1	] <-	117
raster1[raster1 == 	1	] <-	117
raster1[raster1 == 	2	] <-	216
raster1[raster1 == 	3	] <-	227
raster1[raster1 == 	4	] <-	238
raster1[raster1 == 	5	] <-	249
raster1[raster1 == 	6	] <-	315
raster1[raster1 == 	7	] <-	326
raster1[raster1 == 	8	] <-	337
raster1[raster1 == 	9	] <-	348
raster1[raster1 == 	10	] <-	359
raster1[raster1 == 	11	] <-	414
raster1[raster1 == 	12	] <-	425
raster1[raster1 == 	13	] <-	436
raster1[raster1 == 	14	] <-	447
raster1[raster1 == 	15	] <-	458
raster1[raster1 == 	16	] <-	469
raster1[raster1 == 	17	] <-	513
raster1[raster1 == 	18	] <-	524
raster1[raster1 == 	19	] <-	535
raster1[raster1 == 	20	] <-	546
raster1[raster1 == 	21	] <-	557
raster1[raster1 == 	22	] <-	568
raster1[raster1 == 	23	] <-	579
raster1[raster1 == 	24	] <-	613
raster1[raster1 == 	25	] <-	624
raster1[raster1 == 	26	] <-	635
raster1[raster1 == 	27	] <-	646
raster1[raster1 == 	28	] <-	657
raster1[raster1 == 	29	] <-	668
raster1[raster1 == 	30	] <-	679
raster1[raster1 == 	31	] <-	712
raster1[raster1 == 	32	] <-	723
raster1[raster1 == 	33	] <-	734
raster1[raster1 == 	34	] <-	745
raster1[raster1 == 	35	] <-	756
raster1[raster1 == 	36	] <-	767
raster1[raster1 == 	37	] <-	778
raster1[raster1 == 	38	] <-	789

writeRaster(raster1, filename="Presente_1980-2009_ZVH/zvh.tif", format="GTiff", overwrite = TRUE)
#plot(raster1)

##END