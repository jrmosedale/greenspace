
library(rgdal)
library(rgeos)
library(raster)
library(ggplot2)
library(leaflet)
library(maptools)
library(dplyr)
library(sf)
library(magrittr)

root<-"~/Documents/Exeter/Eservices_Data/"; in.root<-"/Volumes/Pocket Sam/Data/"

dir_grnsp<-paste(in.root,"/OS/open-greenspace_1939061/",sep="")
dir_cornwall<-paste(in.root,"Boundaries/geog_areas/",sep="") 
dir_census<-paste(in.root,"ONS_Census/Population/",sep="")
dir_soa<-paste(in.root,"Boundaries/Lower_Layer_Super_Output_Areas_December_2011_Full_Clipped__Boundaries_in_England_and_Wales/",sep="")
dir_poppc<-paste(in.root,"ONS_Census/R2_5_postcode_estimates_revised_17_10_2013/",sep="")
dir_pc<-paste(in.root,"Boundaries/codepoint-poly_1912676/two_letter_pc_code/",sep="")
dir_shape<-paste(in.root,"SOA_shapefiles/", sep="")
dir_openac<-paste(in.root,"NaturalEngland/CROW_Access/", sep="")
dir_aonb<-paste(in.root,"NaturalEngland/AONB_boundary/", sep="")

####################################################################
# Calculate area of different types of greenspace by SOA 
####################################################################
# Read Orednance Survey greenspace and soa 
greensp.st<-st_read(paste(dir_grnsp,"GB_GreenspaceSite.shp",sep=""))
soa.st<-st_read(paste(dir_soa,"Lower_Layer_Super_Output_Areas_December_2011_Full_Clipped__Boundaries_in_England_and_Wales.shp",sep=""))
#st_crs(cornwall.sfc)
class(st_geometry(soa.st)); attributes(st_geometry(soa.st))
class(st_geometry(greensp.st))

# Read open access and AONB shape filse - already clipped to Cornwall & IoS
openac.st<-st_read(paste(dir_openac,"Cornwall_Access_Land_singleparts_27700.shp",sep=""))
aonb.st<-st_read(paste(dir_aonb,"Cornwall_AONB.shp",sep=""))
# set openaccess crs
st_crs(openac.st) <- st_set_crs(openac.st,27700)

plot(aonb.st$geometry); plot(openac.st$geometry, add=T, col="red") # plot gsp in red

# Subset OS greenspace to Cornwal and Isles of Scilly using SOA labels
cornwall_soa.st<-soa.st[substr(soa.st$lsoa11nm,1,8) == "Cornwall" | substr(soa.st$lsoa11nm,1,8) == "Isles of",]
cornwall.sfc<-st_union(cornwall_soa.st)

cornwall_gsp.st<-st_intersection(greensp.st, cornwall.sfc) 
plot(cornwall_soa.st$geometry); plot(cornwall_gsp.st$geometry, add=T, col="red") # plot gsp in red
plot(cornwall_soa.st$geometry[8:10],add=T,col="blue") # single add soa in blue

# Calculate area of different kinds of Greenspace by SOA
names(cornwall_gsp.st);names(aonb.st); names(openac.st)

# Calculate area of greenspace polygons and open access areas
cornwall_gsp.st$area<-st_area(cornwall_gsp.st)
openac.st$oa_area<-st_area(openac.st)

### Using SOA data - calculate sum of all greenspace by SOA
i<-st_intersection(cornwall_gsp.st,cornwall_soa.st)
aggregate(i$area,list(i$lsoa11cd), FUN="sum") # print out area by greenspace type
a<-aggregate(i$area,list(i$lsoa11cd), FUN="sum") # calculate area ogreenspace per SOA
names(a)<-c("lsoa11cd","area_allgsp")
result<-merge(cornwall_soa.st,a,by="lsoa11cd")

# Repeat calculation of area per SOA excluding Golf courses
i_nogolf.st <- i[i$function. != "Golf Course",]
b<-aggregate(i_nogolf.st$area,list(i_nogolf.st$lsoa11cd), FUN="sum")
names(b)<-c("lsoa11cd","area_nogolf")
result<-merge(result,b,all.x=TRUE,by="lsoa11cd")

# Calculate OpenAccess area by SOA
i_openac <- st_intersection(openac.st,cornwall_soa.st)
c<-aggregate(i_openac$oa_area,list(i_openac$lsoa11cd), FUN="sum")
names(c)<-c("lsoa11cd","area_openac")
result<-merge(result,c,all.x=TRUE,by="lsoa11cd")

# Load population data per soa
pop.df<-read.csv(file=paste(dir_census,"SAPE18DT2-mid-2015-lsoa-syoa-estimates-unformatted.csv",sep=""))
names(pop.df)<-c("lsoa11cd","Name","All.Ages","Ages.0.18","Ages.19.69","Ages.70.90")
result<-merge(result,pop.df,all.x=TRUE, by="lsoa11cd")

# result$gsm2_pop<-result$area/result$All.Ages
result$st_areasha<-round(result$st_areasha,0) # round off to allow file writing
result$area_allgsp<-round(result$area_allgsp,0)
result$area_nogolf<-round(result$area_nogolf,0)
result$area_openac<-round(result$area_openac,0)

# convert NA to 0 for areas calculated
sel<-which(is.na(result$area_openac))
result$area_openac[sel]<-0
sel<-which(is.na(result$area_nogolf))
result$area_nogolf[sel]<-0
if(length(which(is.na(result$area_allgsp)))>0) warning("SOA missing greenspace")

fileout<-paste(dir_shape,"GrnSpc_by_SOA.shp", sep="")
print(fileout)
write_sf(result,fileout, delete_layer = TRUE)

####### OR... Create test subset of 3 soa to TEST
#test.soa<-cornwall_soa.st[8:10,]
#plot(test.soa$geometry)
#plot(i$geometry,add=T)

# Calculate greenspace area within each soa
#i<-st_intersection(cornwall_gsp.st,test.soa)
#a<-aggregate(i$area,list(i$lsoa11cd), FUN="sum")
#names(a)<-c("lsoa11cd","area")
#result<-merge(test.soa,a,by="lsoa11cd")

# Load population data per soa
#pop.df<-read.csv(file=paste(dir_census,"SAPE18DT2-mid-2015-lsoa-syoa-estimates-unformatted.csv",sep=""))
#names(pop.df)<-c("lsoa11cd","Name","All.Ages","Ages.0.18","Ages.19.69","Ages.70.90")
#result<-merge(result,pop.df,by="lsoa11cd")

#result$gsm2_pop<-result$area/result$All.Ages

####################################################################
# Test plot in Leaflet- Map of Greenspace per SOA
result_map <- result %>%
  st_transform(4326) %>%
  as("Spatial") 
names(result_map)

# drop st_area as integer too large for writing
result_map<-result_map[,-5] # drop unused varioable - NOTE might want to keep st_areaha if gspace to be expressed as density of area not pop

# Write spatial polygons as shape file - ready for use in leaflet - convert 'units' type into numeric
result_map$area_allgsp<-as.numeric(result_map$area_allgsp)
result_map$area_nogolf<-as.numeric(result_map$area_nogolf)
result_map$area_openac<-as.numeric(result_map$area_openac)

writeOGR(obj=result_map, dsn=dir_shape, layer="GrnSpc_by_SOA_map", driver="ESRI Shapefile", overwrite_layer=TRUE)

# Print data 
#result_map@data[,c(1,6:10)]

# Define variables to be mapped HERE and calculate area/pop
mapdata<-result_map[,c("lsoa11cd","All.Ages","area_nogolf")]
names(mapdata)<-c("lsoa11cd","pop","area")
mapdata$area_per_pop<-round(mapdata$area/mapdata$pop,0)

# Define colours 
q<-quantile(mapdata$area_per_pop,c(0,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,1),names=FALSE,na.rm=TRUE)
bins <- unique(round(q))
pal <- colorBin("Greens", domain = mapdata$area_per_pop, bins = bins, pretty=TRUE)

# Add information by SOA
labels <- sprintf(
  "<strong>SOA %s<br/>has %s Ha of Greenspace for %s population</strong>",
  mapdata$lsoa11cd,round(mapdata$area/10000,0), mapdata$pop
) %>% lapply(htmltools::HTML)

leaflet(mapdata) %>%
  addTiles() %>%
  addPolygons(
  fillColor = ~pal(area_per_pop),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~area_per_pop, opacity = 0.7, title = NULL,
                position = "bottomright") 



####################################################################
# Postcode level analysis
# Load pop per postcode 2011 data - https://census.ukdataservice.ac.uk/use-data/guides/boundary-data 
plpop<-read.csv(file=paste(dir_poppc,"Postcode_Estimates_2_M_R.csv",sep=""))
plpop<-plpop[which(substr(plpop$Postcode,1,2)=="PL"),]
trpop<-read.csv(file=paste(dir_poppc,"Postcode_Estimates_2_S_Z.csv",sep=""))
trpop<-trpop[which(substr(trpop$Postcode,1,2)=="TR"),]
cornwall_pcpop<-rbind(plpop,trpop)



pl.spdf<-readOGR(paste(dir_pc,"pl",sep=""),"pl")
tr.spdf<-readOGR(paste(dir_pc,"tr",sep=""),"tr")
cornwallpc.spdf<-rbind(pl.spdf,tr.spdf,makeUniqueIDs = TRUE)
# Add population variable
# Refs: http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS2_MergingSpatialData_part1_Joins.html 
# Ref: https://stackoverflow.com/questions/3650636/how-to-attach-a-simple-data-frame-to-a-spatialpolygondataframe-in-r
# Remove white space from postcode
cornwallpc.spdf@data$POSTCODE<-gsub(" ", "", cornwallpc.spdf@data$POSTCODE, fixed = FALSE)
cornwall_pcpop$Postcode<-gsub(" ", "", cornwall_pcpop$Postcode, fixed = FALSE)

# Remove V* postcodes from shape dataframe
cornwallpc.spdf<-cornwallpc.spdf[substr(cornwallpc.spdf@data$POSTCODE,1,1)=="V" ,]

# Add pop data
cornwallpc.spdf <- merge(cornwallpc.spdf, cornwall_pcpop, by.x = "POSTCODE", by.y = "Postcode")
plot(cornwallpc.spdf)
