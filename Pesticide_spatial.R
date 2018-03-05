## ----warning = FALSE, error = FALSE, message = FALSE---------------------
library(raster)
library(dplyr)
setwd<-"G:/Team Drives/EOS3 PRACTICUM/data/"
counties <- shapefile("G:/Team Drives/EOS3 PRACTICUM/data/cb_2016_us_county_5m.shp")

# make a new variable that combines state and counties fips codes so they are unique
counties$statefips <- paste0(counties$STATEFP,counties$COUNTYFP)

radars <- shapefile("G:/Team Drives/EOS3 PRACTICUM/data/allnexrad_NAD11.shp")

# Use raster to download State boundaries
states <- raster::getData("GADM", country = "United States",level = 1)


## ------------------------------------------------------------------------
# Define new equal area projection
EqArea <- "+proj=aea 
           +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 
           +x_0=0 +y_0=0 
           +ellps=GRS80 
           +datum=NAD83 
           +units=m 
           +no_defs"

# Make radars into SpatialPoints that can be projected 
radarpts <- sp::SpatialPoints(cbind(radars$LONGITUDE_,radars$LATITUDE_N),
                CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# Make radars a SpatialPointsDataFrame that can be projected 
radars <- sp::SpatialPointsDataFrame(radarpts,radars@data)

# Project counties
counties <- sp::spTransform(counties,CRS(EqArea))

# Project radars
radars <- sp::spTransform(radars,CRS(EqArea))

# Project states 
states <- spTransform(states,CRS(EqArea))

## ------------------------------------------------------------------------
# width is in meters 
radar375 <- rgeos::gBuffer(radars, width = c(37500),byid = TRUE,id = radars$SITE)
radar35 <- rgeos::gBuffer(radars, width = c(35000),byid = TRUE,id = radars$SITE) 

## ------------------------------------------------------------------------
# this returns the row within the counties file then returns the statefips code
CountyRadar <- lapply(rgeos::gIntersects(radar375,counties,byid = TRUE,returnDense = FALSE),
                      FUN = function(x){counties$statefips[x]})

# because CountyRadar is returned as a list - this
# next bit converts it back to a data.frame and 
# fills the radar name with the name of the list
RadarCounty <- unlist(CountyRadar, use.names = TRUE)

# the use.names function adds ".1" ... .etc
# we just want the prefix so we save the 
# first three characters
Radarname <- substr(names(RadarCounty),1,3)

# then we combine two columns together
RadarCounty <- cbind(RadarCounty,Radarname)

# This just counts the number of counties under each radar
numCounties <- unlist(lapply(CountyRadar,length),use.names = TRUE)

## ----echo = FALSE--------------------------------------------------------
# Find the radars that aren't in Alaska or Hawaii. 
# the easiest way to do that is to search for Alaska, Hawaii and PR 
# then tell R to give us everything that's not those (invert = TRUE)
use <- grep(radars$STATE,pattern = "AK/*|HI/*|PR/*",invert = TRUE)

# This code plots the radars (only the ones that don't fall in AK,HI,or PR)
plot(radars[use,], #plot radars
     cex = numCounties[use]/6, # size of the point
     pch = 19, # make the points filled circles
     main = "Number of counties within 35km radius") # title
plot(states,add = TRUE) # add states to the current plot

## ------------------------------------------------------------------------
# Add the prefix for where to download the data from
site <- "https://water.usgs.gov/nawqa/pnsp/usage/maps/county-level/PesticideUseEstimates/EPest.county.estimates."

# specify the years that have that prefix
year <- 1992:2012

# paste them together to make a vector of urls to get data
files.to.get <- paste0(site,year,".txt")

# Use that list to download the data - this can take a while
Sys.time()
a<-Sys.time()
pest.years <- lapply(files.to.get,read.table,sep = "\t", header = TRUE)
pest.years <- lapply(pest.years, FUN = function(x){x$STATE_FIPS_CODE <- sprintf("%02d",x$STATE_FIPS_CODE)
                                                   x$COUNTY_FIPS_CODE <- sprintf("%03d",x$COUNTY_FIPS_CODE)
                                                   x$statefips <- paste0(x$STATE_FIPS_CODE,x$COUNTY_FIPS_CODE)
                                                   return(x)})
Sys.time()-a

## ------------------------------------------------------------------------
# The use the do.call function to stack "rbind" all years of data into a single document. 
pest.combine <- do.call('rbind',pest.years)

## ------------------------------------------------------------------------
# Create a vector with the compount names that are neonics
Neonic <- c("Acetamiprid","Clothianidin","Imidacloprid",
            "Nitenpyram","Nithiazine","Thiacloprid",
            "Thiamethoxam","Organophosphate","Carbamate")

# in the data they are all caps - the toupper() function
# converts them to all caps for us. 
Neonic <- toupper(Neonic)

# Convert from long-data format to wide-data format
NeoPest <- pest.combine[pest.combine$COMPOUND %in% Neonic,]

# Merge Radar and Neonic levels
RadarNeo <- merge(RadarCounty,NeoPest, by.x = "RadarCounty", by.y = "statefips")

# summarize by radar, compound and year #
Neonics <- data.frame(group_by(RadarNeo,Radarname,COMPOUND,YEAR) %>% 
                        summarize(SUM = sum(EPEST_HIGH_KG)) %>% 
                        tidyr::spread(YEAR,SUM))

# Save the data.frame to file
#write.csv(Neonics,"RadarNeonic.csv",row.names = FALSE)

## ----echo = FALSE--------------------------------------------------------
# Lets have a look at the data
str(Neonics)

## ---- fig.width = 8, fig.height = 15,echo = FALSE------------------------
# This next bit of code is sloppy (sorry) but it shows how I made the plots
# The code uses loops and if statements 

# Tell R where the put the plots using the par() call. 
# mfrow means draw c(rows,columns) by row. mfcol means
# draw c(rows,columns) by columns. 
par(mfrow = c(7,3))

# vector of years in the data
years <- 1994:2012

# This is the start of the for loop. I want to loop through all 19 years
for(i in 1:(length(years))){
# This is a check to make sure that not all values are NA in the column. If they are then DONT do the following:
if(!all(is.na(Neonics[,i+2]))){
# Plot a histogram 
hist(log(Neonics[,i+2]), 
     xlim = c(-10,15), # provide limits for the x axis c(min,max)
     ylim= c(0,150), # provide limits for the y axis c(min,max)
     xlab = "log(total neonics)", # give x a label
     main = years[i], # title of the plot
     col = "gray", # color (fill) of the bars in histogram
     border = "gray") # color (border) of the bars
# If it's the first plot - make a legend
if(i == 1){
legend(-10,150,bty="n", legend = c("Total","IMIDACLOPRID","THIAMETHOXAM","ACETAMIPRID","CLOTHIANIDIN"),
        fill = c("gray", # Total 
                 rgb(255,0,0,150,maxColorValue = 255), # IMID
                 rgb(0,0,255,150,maxColorValue = 255), # THIA
                 rgb(0,255,0,150,maxColorValue = 255), # ACET
                 rgb(125,150,50,150,maxColorValue = 255))) # CLOT
}
# Not we're only the next compound 
if(!all(is.na(Neonics[Neonics$COMPOUND=="IMIDACLOPRID",i+2]))){
# par(new = TRUE) means draw the plot on an existing plot - confusing I know. Poor choice for a parameter name. 
par(new = TRUE)
hist(log(Neonics[Neonics$COMPOUND=="IMIDACLOPRID",i+2]),
     xlim = c(-10,15),
     ylim= c(0,150),
     axes = FALSE, # don't draw the axis labels / or tick marks 
     col = rgb(255,0,0,150,maxColorValue = 255), # rgb allows you to set transparent values - I used those here
     border = rgb(255,0,0,150,maxColorValue = 255),
     main = "",
     xlab = "",
     ylab = "")
}
# Next compound
if(!all(is.na(Neonics[Neonics$COMPOUND=="THIAMETHOXAM",i+2]))){
par(new = TRUE)
hist(log(Neonics[Neonics$COMPOUND=="THIAMETHOXAM",i+2]),
     xlim = c(-10,15),
     ylim= c(0,150),
     axes = FALSE,
     col = rgb(0,0,255,150,maxColorValue = 255),
     border = rgb(0,0,255,150,maxColorValue = 255),
     main = "",
     xlab = "",
     ylab = "")
}
# Next compound
if(!all(is.na(Neonics[Neonics$COMPOUND=="ACETAMIPRID",i+2]))){
par(new = TRUE)
hist(log(Neonics[Neonics$COMPOUND=="ACETAMIPRID",i+2]),
     xlim = c(-10,15),
     ylim= c(0,150),
     axes = FALSE,
     col = rgb(0,255,0,150,maxColorValue = 255),
     border = rgb(0,255,0,150,maxColorValue = 255),
     main = "",
     xlab = "",
     ylab = "")
}
# Nex compound
if(!all(is.na(Neonics[Neonics$COMPOUND=="CLOTHIANIDIN",i+2]))){
par(new = TRUE)
hist(log(Neonics[Neonics$COMPOUND=="CLOTHIANIDIN",i+2]),
     xlim = c(-10,15),
     ylim= c(0,150),
     axes = FALSE,
     col = rgb(125,150,50,150,maxColorValue = 255),
     border = rgb(125,150,50,150,maxColorValue = 255),
     main = "",
     xlab = "",
     ylab = "")
} # end if
} # end all NA
} # end years


## ---- echo = FALSE, fig.height = 8---------------------------------------
# set up the way to plots look. 
# mar = margin = c(bottom,left,top,right)
par(mar = c(0,0,4,0))
# subset to the compound
IMID <- Neonics[Neonics$COMPOUND == "IMIDACLOPRID",]
# merge with radar
IMIDradar <- merge(radar35,IMID,by.x = "SITE",by.y = "Radarname")

# get the median value across years using the apply() function. 
# This code says - give me the median (remove na's) log value
# of each row in columns 13:31

medianYrs <- apply(log(IMIDradar@data[,13:31]),1,median,na.rm = TRUE)

# Create a vector of length 100 sequencing from min to max 
seqvals <- seq(min(medianYrs,na.rm = TRUE),max(medianYrs,na.rm = TRUE),,100)

# Find where the values fall within that sequence
intervals <- findInterval(medianYrs,seqvals)

# make a color palatte of 100 colors in the blue-purple-yellow colors
cols <- sp::bpy.colors(100)

# plot States - Alaksa and Hawaii
plot(states[states$NAME_1!="Alaska" & states$NAME_1!="Hawaii",],
     main = "IMIDACLOPRID")
# Plot the radars with size of the point relative to the median
plot(radars,
     cex = medianYrs/3,
     add = TRUE,
     pch = 19
     ,col = cols[intervals]) #colors have specific numeric values
# Start the legend - Make an empty raster with 100 cells
r<-raster(ncol = 100, nrow = 1)
# fill the raster with the range of values in the data
r[] <- seq(min(medianYrs,na.rm = TRUE),max(medianYrs,na.rm = TRUE),,100)
# plot just the legend of the raster and make it horizontal
plot(r, add = TRUE, legend.only=TRUE, horiz = TRUE,col = bpy.colors(100))


###### NEXT COMPOUND ######################
ACET<- Neonics[Neonics$COMPOUND == "ACETAMIPRID",]
ACETradar <- merge(radar35,ACET,by.x = "SITE",by.y = "Radarname")

medianYrs <- apply(log(ACETradar@data[,13:31]),1,median,na.rm = TRUE)

seqvals <- seq(min(medianYrs,na.rm = TRUE),max(medianYrs,na.rm = TRUE),,100)

intervals <- findInterval(medianYrs,seqvals)

cols <- sp::bpy.colors(100)

plot(states[states$NAME_1!="Alaska" & states$NAME_1!="Hawaii",],
     main = "ACETAMIPRID")

plot(radars,cex = medianYrs/3,add = TRUE,pch = 19,col = cols[intervals])

r<-raster(ncol = 100, nrow = 1)

r[] <- seq(min(medianYrs,na.rm = TRUE),max(medianYrs,na.rm = TRUE),,100)

plot(r, add = TRUE, legend.only=TRUE, horiz = TRUE,col = bpy.colors(100))

###### NEXT COMPOUND ######################
CLOT<- Neonics[Neonics$COMPOUND == "CLOTHIANIDIN",]

CLOTradar <- merge(radar35,CLOT,by.x = "SITE",by.y = "Radarname")

medianYrs <- apply(log(CLOTradar@data[,13:31]),1,median,na.rm = TRUE)

seqvals <- seq(min(medianYrs,na.rm = TRUE),max(medianYrs,na.rm = TRUE),,100)

intervals <- findInterval(medianYrs,seqvals)

cols <- sp::bpy.colors(100)

plot(states[states$NAME_1!="Alaska" & states$NAME_1!="Hawaii",],
     main = "CLOTHIANIDIN")

plot(radars,cex = medianYrs/3,add = TRUE,pch = 19,col = cols[intervals])

r<-raster(ncol = 100, nrow = 1)

r[] <- seq(min(medianYrs,na.rm = TRUE),max(medianYrs,na.rm = TRUE),,100)

plot(r, add = TRUE, legend.only=TRUE, horiz = TRUE,col = bpy.colors(100))

###### NEXT COMPOUND ######################
THIA<- Neonics[Neonics$COMPOUND == "THIAMETHOXAM",]

THIAradar <- merge(radar35,THIA,by.x = "SITE",by.y = "Radarname")

medianYrs <- apply(log(THIAradar@data[,13:31]),1,median,na.rm = TRUE)

seqvals <- seq(min(medianYrs,na.rm = TRUE),max(medianYrs,na.rm = TRUE),,100)

intervals <- findInterval(medianYrs,seqvals)

cols <- sp::bpy.colors(100)

plot(states[states$NAME_1!="Alaska" & states$NAME_1!="Hawaii",],
     main = "THIAMETHOXAM")

plot(radars,cex = medianYrs/3,add = TRUE,pch = 19,col = cols[intervals])

r<-raster(ncol = 100, nrow = 1)

r[] <- seq(min(medianYrs,na.rm = TRUE),max(medianYrs,na.rm = TRUE),,100)

plot(r, add = TRUE, legend.only=TRUE, horiz = TRUE,col = bpy.colors(100))

## ---- echo = FALSE-------------------------------------------------------
# sum across compounds within year within radar
yrsum <- data.frame(Neonics %>% select(-(COMPOUND))%>%group_by(Radarname)%>%
        replace(is.na(.), 0) %>%
        summarise_all(funs(sum)))

# add NA's back
yrsum[yrsum == 0]<-NA
# merge with the radar locations
tot.pest <- merge(radars,yrsum, by.x = "SITE",by.y = "Radarname")

# number of years
y <- 19
for(i in 1:y){
# get the median value across years using the apply() function. 
# This code says - give me the median (remove na's) log value
# of each row in columns 13:31

# Create a vector of length 100 sequencing from min to max 
seqvals <- seq(min(log(yrsum[,2:20]),na.rm = TRUE),max(log(yrsum[,2:20]),na.rm = TRUE),,100)

# Find where the values fall within that sequence
intervals <- findInterval(log(yrsum[,i+1]),seqvals)

# make a color palatte of 100 colors in the blue-purple-yellow colors
cols <- sp::bpy.colors(100)

# plot States - Alaksa and Hawaii
plot(states[states$NAME_1!="Alaska" & states$NAME_1!="Hawaii",],
     main = years[i])
# Plot the radars with size of the point relative to the median
plot(radars,
     cex = 2,
     add = TRUE,
     pch = 19
     ,col = cols[intervals]) #colors have specific numeric values
# Start the legend - Make an empty raster with 100 cells
r<-raster(ncol = 100, nrow = 1)
# fill the raster with the range of values in the data
r[] <- seq(min(log(yrsum[,2:20]),na.rm = TRUE),max(log(yrsum[,2:20]),na.rm = TRUE),,100)
# plot just the legend of the raster and make it horizontal
plot(r, add = TRUE, legend.only=TRUE, horiz = TRUE,col = bpy.colors(100))
}

## ---- echo = FALSE, fig.height = 24, fig.width = 12----------------------
# Subset 
IMID <- Neonics[Neonics$COMPOUND == "IMIDACLOPRID",]
# merge data
IMIDradar <- merge(radar35,IMID,by.x = "SITE",by.y = "Radarname")

# Set a variable equal to the number of years
y <- 19

# Set up the output plot - lots of columns with 3 rows. 
# 0 margins

par(mfcol = c(y,3),mar = c(0,0,0,0))
# Start the for loop to loop over years
for(i in 1:y){
# categorize the pest levels into thirds
# The quantile function returns the 'probs' that you're interested in
breaks <- quantile(log(IMIDradar@data[,i+12]),probs = c(0.33,0.66),na.rm = TRUE)

# plot states again
plot(states[states$NAME_1!="Alaska" & states$NAME_1!="Hawaii",],
main = ifelse(i == 1,"IMIDACLOPRID",""))

# here we use the quantiles as breaks 
cols <- findInterval(log(IMIDradar@data[,i+12]),breaks)+1

cols[cols == 1]<-"blue"      #lower third
cols[cols == 2]<-"yellow"    # middle third
cols[cols == 3]<-"firebrick" # upper third

plot(IMIDradar, col = cols,border = cols,add = TRUE)
}

############# NEXT COMPOUND ##########
ACET<- Neonics[Neonics$COMPOUND == "ACETAMIPRID",]

ACETradar <- merge(radar35,ACET,by.x = "SITE",by.y = "Radarname")

for(i in 1:y){
  
# categorize the pest levels into thirds
breaks <- quantile(log(ACETradar@data[,i+12]),probs = c(0.33,0.66),na.rm = TRUE)

plot(states[states$NAME_1!="Alaska" & states$NAME_1!="Hawaii",],
main = ifelse(i == 1,"ACETAMIPRID",""))

if(!all(is.na(breaks))){
cols <- findInterval(log(ACETradar@data[,i+12]),breaks)+1
cols[cols == 1]<-"blue"
cols[cols == 2]<-"yellow"
cols[cols == 3]<-"firebrick"
plot(ACETradar, col = cols,border = cols,add = TRUE)
}
}

############# NEXT COMPOUND ##########
CLOT<- Neonics[Neonics$COMPOUND == "CLOTHIANIDIN",]
CLOTradar <- merge(radar35,CLOT,by.x = "SITE",by.y = "Radarname")
for(i in 1:y){
# categorize the pest levels into thirds
breaks <- quantile(log(CLOTradar@data[,i+12]),probs = c(0.33,0.66),na.rm = TRUE)

plot(states[states$NAME_1!="Alaska" & states$NAME_1!="Hawaii",],
main = ifelse(i == 1,"CLOTHIANIDIN",""))
if(!all(is.na(breaks))){
cols <- findInterval(log(CLOTradar@data[,i+12]),breaks)+1
cols[cols == 1]<-"blue"
cols[cols == 2]<-"yellow"
cols[cols == 3]<-"firebrick"
plot(CLOTradar, col = cols,border = cols,add = TRUE)
}
}

