
library(rgdal)
library(rgeos)
library(raster)
library(ggplot2)
library(leaflet)
library(maptools)
library(dplyr)
library(sf)
library(magrittr)
library(plotly)

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
#st_crs(openac.st) <- st_set_crs(openac.st,27700)

plot(aonb.st$geometry); plot(openac.st$geometry, add=T, col="red") # plot gsp in red

# Subset OS greenspace to Cornwal and Isles of Scilly using SOA labels
cornwall_soa.st<-soa.st[substr(soa.st$lsoa11nm,1,8) == "Cornwall" | substr(soa.st$lsoa11nm,1,8) == "Isles of",]
cornwall.sfc<-st_union(cornwall_soa.st)

#st_crs(cornwall.sfc)<-27700
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
z<-aggregate(i$area,list(i$lsoa11cd,i$function.), FUN="sum")
names(z)<-c("soa","type","area")
unique(z$type)
levels(z$type) <- c("Allotments", "Bowling_Green", "Cemetery", "Golf_Course", "Sports", "Play_Space","Playing_Field",
                    "Park_Garden","Religious_Grounds","Tennis_Court")

# Calculate OpenAccess area by SOA
i_openac <- st_intersection(openac.st,cornwall_soa.st)
y<-aggregate(i_openac$oa_area,list(i_openac$lsoa11cd), FUN="sum")
y$type<-"Open Access"
names(y)<-c("soa","area","type")

# Load population data per soa
pop.df<-read.csv(file=paste(dir_census,"SAPE18DT2-mid-2015-lsoa-syoa-estimates-unformatted.csv",sep=""))
names(pop.df)<-c("soa","Name","All.Ages","Ages.0.18","Ages.19.69","Ages.70.90")
sel<-which(substr(pop.df$Name,1,5)=="Cornw" | substr(pop.df$Name,1,5)=="Isles")
cornwall_pop.df<-pop.df[sel,]

### Re-organise and merge data sets into single dataframe by SOA
# Transpose data to wide form so that area of each type of greenspace = column
gsp.types<-reshape(z, idvar = "soa", timevar = "type", direction = "wide")
names(gsp.types)<-c("soa",levels(z$type))

# add greenspace from open access data
result<-merge(gsp.types,y,all.x=TRUE,all.y=TRUE,by="soa")
result<-result[,-13] # drop var
names(result)<-c("soa",levels(z$type),"Open_Access")

# add population data 
result<-merge(result,cornwall_pop.df,all.x=TRUE, all.y=TRUE,by="soa")
result<-result[,-13]

# Round values - CAREFUL NOT TO ROUND TO 0 decimel
result[,2:12]<-round(result[,2:12],1) 

### Merge with SOA Polygons and convert any NA to 0
names(cornwall_soa.st)<-c("objectid" ,  "soa" ,  "lsoa11nm"  , "lsoa11nmw" , "st_areasha", "st_lengths" ,"geometry")
result<-merge(cornwall_soa.st, result, all.x=TRUE)
result[ is.na(result) ] <- 0

# Write file - note most variable = 'unit' type
fileout<-paste(dir_shape,"GrnSpc_by_SOA.shp", sep="")
print(fileout)
write_sf(result,fileout, delete_layer = TRUE)


####################################################################
# Test plot in Leaflet- Map of Greenspace per SOA
result_map <- result %>%
  st_transform(4326) %>%
  as("Spatial") 
names(result_map)

# drop st_area as integer too large for writing
result_map<-result_map[,-5] # drop unused varioable - NOTE might want to keep st_areaha if gspace to be expressed as density of area not pop

# Write spatial polygons as shape file - ready for use in leaflet - CONVERT 'units' type into numeric
for (n in 6:16){
  result_map@data[,n]<-as.numeric(result_map@data[,n])
}

writeOGR(result_map,dir_shape,"GrnSpc_by_SOA_map", driver="ESRI Shapefile",overwrite_layer=TRUE)

#####################################################################
# Map different kinds of greenspace
#####################################################################

calc_grnsp <- function(golf,sports,play,open, mapdata) {
  names(mapdata)<-c("soa","objectid","lsoa11nm","lsoa11nmw","st_lengths",   
                     "Allotments","Bowling_Green","Cemetery","Golf_Course","Sports" ,          
                     "Play_Space", "Playing_Field","Park_Garden","Religious_Grounds","Tennis_Court",     
                    "Open_Access", "All.Ages","Ages.0.18","Ages.19.69","Ages.70.90" )
  
  # sum basic greenspace (where no options = TRUE)
  area<-rowSums(mapdata@data[,c(6,8,13,14)], na.rm = TRUE)
  if(golf) area<-area + mapdata@data[,c(9)]
  if(sports) area<-area + rowSums(mapdata@data[,c(7,10,15)], na.rm = TRUE)
  if(play) area<-area + rowSums(mapdata@data[,c(11,12)], na.rm = TRUE)
  if(open) area<-area + mapdata@data[,c(16)]
  
  mapdata@data$area<-area
  mapdata@data$area_per_pop<-round(mapdata@data$area/mapdata@data$All.Ages,1)
  mapdata<-mapdata[,c(1,17,21,22)] # DO NOT USE @data !!
  return(mapdata)
}

# Define variables to be mapped HERE and calculate area/pop
golf<-TRUE
sports<-TRUE
play<-FALSE
open<-FALSE

mapdata<-calc_grnsp(golf,sports,play,open,result_map)

# Define colours 
q<-quantile(mapdata@data$area_per_pop,c(0,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,1),names=FALSE,na.rm=TRUE)
bins <- unique(q)
pal <- colorBin("Greens", domain = mapdata@data$area_per_pop, bins = bins, pretty=TRUE)

# Add information by SOA
labels <- sprintf(
  "<strong>SOA %s<br/>has %s Ha of Greenspace for %s population</strong>",
  mapdata@data$soa,round(mapdata@data$area/10000,0), mapdata@data$All.Ages
) %>% lapply(htmltools::HTML)

leaflet(r) %>%
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
# Bar plot of different kinds of grrenspace in map charts

calc_chartdata<-function(golf,sports,play,open,mapdata){
    names(mapdata)<-c("soa","objectid","lsoa11nm","lsoa11nmw","st_lengths",   
                      "Allotments","Bowling_Green","Cemetery","Golf_Course","Sports" ,          
                      "Play_Space", "Playing_Field","Park_Garden","Religious_Grounds","Tennis_Court",     
                      "Open_Access", "All.Ages","Ages.0.18","Ages.19.69","Ages.70.90" )
    
    # sum basic greenspace (where no options = TRUE)
    area.sums<-colSums(mapdata@data[,c(6:16)],na.rm=TRUE) # array
    # Calc chart data according to definition of greenspace
    chartdata<-data.frame(type=c("Allotments", "Golf Courses", "Open Access Land", "Parks & Gardens", "Playing fields", 
                                 "Religious & Cemetery", "Sports" ),
                                  area=c(area.sums[1],area.sums[4],area.sums[11],area.sums[8],sum(area.sums[c(7:8)]),
                                         sum(area.sums[c(3,9)]),sum(area.sums[c(2,5,10)])  ) ) 
    
    if(!golf) chartdata<-chartdata[-which(chartdata$type=="Golf Courses"),]
    if(!sports) chartdata<-chartdata[-which(chartdata$type=="Sports"),]
    if(!play) chartdata<-chartdata[-which(chartdata$type=="Playing fields"),] 
    if(!open) chartdata<-chartdata[-which(chartdata$type=="Open Access Land"),]
    
    chartdata[,"area"]<-chartdata[,"area"]/10000
    return(chartdata)
}

chartdata<-calc_chartdata(golf,sports,play,open,result_map)

p<-ggplot(data = chartdata, aes(x = type, y=area, fill=type)) +
  geom_bar(stat = "identity")  +
  labs(x="Greenspace", y = "Area (hectares)",
     #  title="Composition Greenspace in Cornwall", 
       fill='Types of Greenspace') +
  theme_classic() +
  theme(axis.text.x=element_blank(),legend.position="right") 

ggplotly(p)

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
