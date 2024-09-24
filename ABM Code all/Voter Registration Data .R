

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)


ncvoter_Statewide <- read.delim("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/ncvoter_Statewide.txt", header=FALSE)


ncvhis_Statewide <- read.delim("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/ncvhis_Statewide.txt")


names(ncvoter_Statewide) <- ncvoter_Statewide[1,]
ncvoter_Statewide <- ncvoter_Statewide[-1,]
table(ncvoter_Statewide$voter_status_desc)
table(ncvoter_Statewide$party_cd)




ncvoter_Statewide <- ncvoter_Statewide %>% filter(voter_status_desc == "ACTIVE")

### for distance between each other I would probably need the geodesic distance between two people 
### --> would need to get the coordinates from the addresses but only when i have made my sample smaller 


#### Google Maps API: AIzaSyCBzo8BkD9dQ94fW4oKDkQAUWEdmragCaU

######### looking at zipcode 27814 with population of 1,676	######

nc_27555 <- ncvoter_Statewide %>% filter(zip_code == 27555)
nc_28571 <- ncvoter_Statewide %>% filter(zip_code == 28571)


table(nc_27555$race_code)


register_google(key = "AIzaSyCBzo8BkD9dQ94fW4oKDkQAUWEdmragCaU")

# geocoded_data <- geocode("207  PILLOW LN   ", output = "latlon")

adress1 <- paste(nc_27555$res_street_address, nc_27555$res_city_desc)

adress_dataframe <- data.frame(address = adress1)

#adress <- nc_27555$res_street_address

adress <- unique(adress1)
length(adress)


nc_27555$full_adress <- adress1





geocoded_adress <- data.frame()

#Loop through each street name
for(street in adress){
  # Geocode the current street name
  geocoded_street <- geocode(street, output = "latlon")
  
  # Create a temporary dataframe to store geocoded information for the current street
  temp_df <- data.frame(
    streetname = street,
    lon = geocoded_street$lon,
    lat = geocoded_street$lat
  )
  
  # Append the temporary dataframe to the geocoded dataset
  geocoded_adress <- rbind(geocoded_adress, temp_df)
}

save(geocoded_adress, file = "geocoded_adress.RData")



######## using this function ##########

##### maybe scrape data from here: https://www.mapdevelopers.com/geocode_tool.php


library(RSQLite)
library(rvest)
library(RSelenium)
library(stringr)
library(dplyr)
library(readxl)
library(tmap)
library(sf)


# read in url

url <- "https://www.mapdevelopers.com/geocode_tool.php"

# setting up the port and connection to server

rD <- rsDriver(browser=c("firefox"), port = free_port(random = TRUE), chromever = NULL) 
driver <- rD$client
driver$navigate(url)


?rsDriver()

selector_list <- c()
# search button

selector_list$search_box <- '//*[@id="address"]'
selector_list$go_button <-'//*[@id="search-form"]/div[1]/span[2]/button'
selector_list$latitude <- '//*[@id="display_lat"]'
selector_list$longitude <- '//*[@id="display_lng"]'


## search for function: 


search_for <- function(term) {
  
  # Find the search field, clear it, and enter the new search term, e.g. "data science"
  search_field <- driver$findElement(using = "xpath", value = selector_list$search_box)
  search_field$clearElement()
  search_field$sendKeysToElement(list(term))
  
  # Wait for one second and then press the enter key
  Sys.sleep(1)
  # Find the "Go" button and click on it
  go_button <- driver$findElement(using = "xpath", value = selector_list$go_button)
  go_button$clickElement()
  
  longitude <- driver$findElement(using = "xpath",
                                  value = selector_list$longitude)$getElementText()[[1]]
  latitude <- driver$findElement(using = "xpath",
                                 value = selector_list$latitude)$getElementText()[[1]]
  
  Sys.sleep(1)
  
  # Create data frame with coordinates and address
  coords <- data.frame(longitude = longitude,
                       latitude =latitude,
                       address = term)
  
  return(coords)
}



###### calling function with loop over all adresses:

coords_adress <- data.frame(longitude = NA, latitude = NA, address = NA)

for(street in adress) {
  
  coords <- search_for(street)
  
coords_adress <- rbind(coords_adress, coords)
  
}




############## merging registration data with coords ####

nc_27555_small <- nc_27555 %>% select(ncid, race_code, ethnic_code, party_cd, gender_code, birth_year, birth_state, full_adress)

#### creating points for adresses:
coords_adress <- na.omit(coords_adress)

coords_adress_all <- left_join(adress_dataframe, coords_adress, by = c("address"))
points <- st_as_sf(coords_adress_all, coords = c("longitude", "latitude"), crs = 4269)

geo_nc_27555_points <- left_join(points, nc_27555_small, by =c("address" = "full_adress"))
geo_nc_27555_points <- unique(geo_nc_27555_points)


##### getting NC shp #####


nc_shapefile <- st_read("~/Desktop/LSE Term 2/Capstone Project/Data/NC Voter Registration Data/North_Carolina_State_and_County_Boundary_Polygons/North_Carolina_State_and_County_Boundary_Polygons.shp")

nc_johnston <- nc_shapefile %>% filter(FIPS == 101)

centerlines <- st_read("~/Desktop/LSE Term 2/Capstone Project/Data/NC Voter Registration Data/Centerlines/centerli.shp")

zipcodes <- st_read("~/Desktop/LSE Term 2/Capstone Project/Data/NC Voter Registration Data/zipcode shapefile/tl_2023_us_zcta520.shp")

zipcodes27555 <- zipcodes %>% filter(ZCTA5CE20 == 27555)


## test clustering 
library(dbscan)

coords <- coords_adress_all %>% select(longitude, latitude)
coords$longitude <- as.numeric(coords$longitude)
coords$latitude <- as.numeric(coords$latitude)

# Run DBSCAN clustering
dbscan_result <- dbscan(coords, eps = 0.1, minPts = 5)
# Add cluster labels to your points data frame
points$cluster <- dbscan_result$cluster

# Plot the clusters
tm_shape(points) +
  tm_symbols(col = "cluster", palette = "Set1", size = 0.1)


#############################################################

tmap_mode("view")

tm_shape(zipcodes27555) + 
  tm_polygons() +
  tm_shape(centerlines) + 
  tm_lines() + 
  tm_shape(geo_nc_27555_points)+
  tm_symbols(col = "party_cd", size = 0.1)


# Create clusters
library(leaflet)

clusters <- leaflet() %>%
  addTiles() %>%
  addMarkers(data = points, clusterOptions = markerClusterOptions())


################################-------------------------------

###### old code #####

##### getting lon lat for zipcodes #######

geocoded_zip <- geocode("27555", output = "latlon")


geocoded_zipcodes <- data.frame()

#Loop through each street name
for(zipcode in zipcodes){
  # Geocode the current street name
  geocoded_zip <- geocode(zipcode, output = "latlon")
  
  # Create a temporary dataframe to store geocoded information for the current street
  temp_df <- data.frame(
    zip = zipcode,
    lon = geocoded_zip$lon,
    lat = geocoded_zip$lat
  )
  
  # Append the temporary dataframe to the geocoded dataset
  geocoded_zipcodes <- rbind(geocoded_zipcodes, temp_df)
}


save(geocoded_zipcodes, file = "geocoded_zipcodes.RData")



#################################################---------------------------------

# create a neighbourhood contiguity matrix 
library(units)

# Calculate pairwise distance matrix
distance_matrix_27555 <- as.matrix(dist(coords, method = "euclidean"))


coords_sf <- st_as_sf(coords_adress_all, coords = c("longitude", "latitude"), crs = 4326)

# Project to a coordinate system that uses meters, e.g., UTM zone 17N
coords_sf_m <- st_transform(coords_sf, crs = 32617)

# Calculate pairwise distance matrix in meters
distance_matrix_27555 <- st_distance(coords_sf_m)

# Convert to numeric matrix
distance_matrix_27555 <- as.matrix(distance_matrix_27555)


threshold_distance <- 500

# Check if each distance is below the threshold
distance_matrix_27555_min <- distance_matrix_27555 < threshold_distance

diag(distance_matrix_27555_min) <- 0 




###############################################
#### checking out flaches code: 
##############################################



# 3) World generation __________________________________________________________
#
# Here we create a simulated world of agents calibrated on the demographics of a zipcode in NC
# using the dataset from the NC voter registration and calculate coordinates of agents as well as 
# distance and proximity matrices to calculate probability for interaction 

library("geosphere")
library(sp)


## creating additional variables: 

# create a variable for non white population or races as percentage of actual population

nc_27555_small <- nc_27555_small %>% mutate(non_white_perc = sum(race_code != "W") / nrow(nc_27555_small))


nc_27555_small <- nc_27555_small %>% mutate(race_numeric = case_when(race_code == "W" ~1,
                                                                 race_code == "B" ~ 2,
                                                                 race_code == "I" ~ 3,
                                                                 race_code == "O" | race_code == "U" ~ 4))


table(nc_27555_small$party_cd)

nc_27555_small <- nc_27555_small %>% mutate(party_numeric = case_when(party_cd == "REP" ~ 1,
                                                                      party_cd == "DEM" ~2,
                                                                      party_cd == "NLB" ~3,
                                                                      party_cd == "UNA" ~4))

                                                                                                                                    
table(nc_27555_small$party_numeric)

###### creating spatial dataframe with coords 

geo_nc_27555 <- left_join(nc_27555_small, coords_adress, by =c("full_adress" = "address"))

geo_nc_27555$longitude <- as.numeric(geo_nc_27555$longitude)
geo_nc_27555$latitude <- as.numeric(geo_nc_27555$latitude)



## coordinates() is a function from the sp package in R, which is used to specify which columns of a dataframe 
## should be used as the spatial coordinates. It then transforms the df into a spatial object

geo_nc_27555 <- geo_nc_2755_test
coordinates(geo_nc_27555) = c("longitude","latitude")

library(sp)

########## creating distance and proximity matrices ##########




ncells <- length(geo_nc_27555)
distmat <- matrix(NA, nrow=ncells, ncol=ncells)
proxmat1 <- matrix(NA, nrow=ncells, ncol=ncells)
proxmat2 <- matrix(NA, nrow=ncells, ncol=ncells)
proxmat3 <- matrix(NA, nrow=ncells, ncol=ncells)


# Next, we calculate the distance matrix, in meters.
# We start by creating an empty matrix. Then we fill it in, row by row.

for (i in 1:ncells) {
  distmat[i,] <- distVincentyEllipsoid(coordinates(geo_nc_27555)[i,], coordinates(geo_nc_27555)) 
}

# Inside the loop, the function distVincentyEllipsoid() is used to calculate the distance between the coordinates 
# of the current cell (coordinates(dat)[i,]) and the coordinates of all other cells in the city (coordinates(dat)).

#proxmat <- proxmat/10 #Here we can adjust the unit of measurement, if needed.
#printMat(proxmat)


# We apply the distance decay function, so that the matrix expresses proximity instead of distance.
# NOTE: For agents who live in the same cell, we want to avoid to assume their
# distance is 0, because their proximity would be maximal. Instead, we assume that their proximity is the average distance between all points
# in a square sized 100*100 meters. That would be about 52.14m. Therefore, we assume that the distance between a cell and itself is about 52.14m.

# By applying the exponential function to the negative distances (scaled by a factor of 10), 
# it converts distances into proximity values.

proxmat1 <- exp(-distmat/10) 
proxmat2 <- exp(-distmat/100)
proxmat3 <- exp(-distmat/1000)


# diag(proxmat1) <- exp(-52.140543316/10)   #######
# diag(proxmat2) <- exp(-52.140543316/100)  #######
# diag(proxmat3) <- exp(-52.140543316/1000) #######
#printMat(proxmat)

# Normalize rows:
#sum <- rowSums(proxmat)
for (i in 1:length(distmat[1,])){
  #proxmat1[i,] <- proxmat1[i,] / sum[i]
  #proxmat2[i,] <- proxmat2[i,] / sum[i]
  #proxmat3[i,] <- proxmat3[i,] / sum[i]
  proxmat1[i,] <- proxmat1[i,] / sum(proxmat1[i,])
  proxmat2[i,] <- proxmat2[i,] / sum(proxmat2[i,])
  proxmat3[i,] <- proxmat3[i,] / sum(proxmat3[i,])
}




#############################################
## distance matrix finished  ##
###########################################


# Function to plot matrix for quick visual inspection:
printMat <- function(x){
  return(
    image(x, col=heat.colors(10000), zlim=c(min(x, na.rm=T),max(x, na.rm=T)))
  )
}


# Let's try out distance decay functions:
plotDistanceDecay <- function(){
  plot(exp(- (c(1:1000) / 100)),
       type = "l",
       col = "blue",
       xlab = "distance in meters",
       ylab = "probability of interaction")
  lines(exp(- (c(1:1000) / 1000)), col = "green")
  lines(exp(- (c(1:1000) / 10)), col = "red")
  lines(exp(- c(1:1000)), col = "black")
  legend(600,
         1,
         legend=c(
           "p=exp(-distance/1000)",
           "p=exp(-distance/100)",
           "p=exp(-distance/10)",
           "p=exp(-distance)"
         ),
         col=c("green", "blue", "red", "black"),
         lty=1
  )
}
plotDistanceDecay()
rm(plotDistanceDecay)


#############################################################################

# Local Moran's I.
#
# We calculate the index at the level of the zipcode neighbourhood. Depending on "type", this function 
# returns the global value (type = "global"), or a vector of values of I measured at each spatial unit (type = "local")



# Local group clustering (local I_prop)
I <- MoranI(
  x=geo_nc_27555$race_code,
  proxmat=proxmat,
  type = "local"
)

# Local group segregation (local biv_I_count)
Is <- MoranI(
  x=cityData$inw2014 - cityData$nnwal2014,
  y=cityData$nnwal2014,
  proxmat=proxmat,
  type = "local"
)

# Local group clustering (local I_count)
Iec <- MoranI(
  x=cityData$nnwal2014,
  proxmat=proxmat,
  type = "local"
)


for (i in 1:populationSize){
  world$localI_prop[i] <- I[world$index[i]]
  world$localI_biv_count[i] <- Is[world$index[i]]
  world$localI_count[i] <- Iec[world$index[i]]
  world$p_nwa[i] <- cityData$pnwal2014[world$index[i]]
}
rm(I,Is,Iec)


agents <<- world
proxmat1 <<- proxmat1
proxmat2 <<- proxmat2
proxmat3 <<- proxmat3


###############################################o

### calculating exposure for different partisanships 

## not sure how to implement it or what is meant by exposure, maybe just calculate Moran's I or another
## similarity index for race and party ? 

table(nc_27555_small$race_code)


geo_nc_27555$index <- (1:277)

# function with three input parameter (agent data, aggregate data, proximity)
calcExposurePartisanship <- function(agents, proxmat){
  l <- nrow(proxmat)
  
  # Initialize matrices to store exposure values for same and different parties.
  
  eSameParty <- matrix(NA, nrow=l, ncol=l)
  eDiffParty <- matrix(NA, nrow=l, ncol=l)
  eSameParty_adj <- matrix(NA, nrow=l, ncol=l)
  eDiffParty_adj <- matrix(NA, nrow=l, ncol=l)
  
  # Iterate over each agent (i) and each other agent (j).
  
  for (i in 1:l){
    for (j in 1:l){
      
      # Determine if agents i and j belong to the same party or different party (excluding unaffiliated agents).
      
      same_party <- agents$party_cd[j] == agents$party_cd[i] & agents$party_cd[j] != "UNA"
      diff_party <- agents$party_cd[j] != agents$party_cd[i] & agents$party_cd[j] != "UNA"
      
      # Calculate exposure values for same-party and different-party based on proximity.
      
      eSameParty[i,j] <- proxmat[i,j] * same_party
      eDiffParty[i,j] <- proxmat[i,j] * diff_party
      
      # Adjust for in-group exposure by ensuring no self-influence.
      
      if (i == j){
        eSameParty_adj[i,j] <- proxmat[i,j] * (same_party - 1)
        eDiffParty_adj[i,j] <- proxmat[i,j] * (diff_party - 1)
      } else {
        eSameParty_adj[i,j] <- eSameParty[i,j]
        eDiffParty_adj[i,j] <- eDiffParty[i,j]
      }
    }
  }
  
  # Calculate row sums for the exposure matrices to normalize the values.
  
  vSameParty <- rowSums(eSameParty, na.rm = TRUE)
  vDiffParty <- rowSums(eDiffParty, na.rm = TRUE)
  vSameParty_adj <- rowSums(eSameParty_adj, na.rm = TRUE)
  vDiffParty_adj <- rowSums(eDiffParty_adj, na.rm = TRUE)
  
  # Calculate the total exposure sums for normalization.
  
  sums_egoSameParty <- vSameParty_adj + vDiffParty
  sums_egoDiffParty <- vSameParty + vDiffParty_adj
  
  # Normalize the exposure matrices by dividing by the total exposure sums.
  
  eSameParty_egoSameParty <- eSameParty_adj / matrix(sums_egoSameParty, nrow=l, ncol=l)
  eSameParty_egoDiffParty <- eSameParty / matrix(sums_egoDiffParty, nrow=l, ncol=l)
  eDiffParty_egoSameParty <- eDiffParty / matrix(sums_egoSameParty, nrow=l, ncol=l)
  eDiffParty_egoDiffParty <- eDiffParty_adj / matrix(sums_egoDiffParty, nrow=l, ncol=l)
  
  # Calculate final exposure values by summing the rows of the normalized matrices.
  
  vSameParty_egoSameParty <- rowSums(eSameParty_egoSameParty, na.rm = TRUE)
  vSameParty_egoDiffParty <- rowSums(eSameParty_egoDiffParty, na.rm = TRUE)
  vDiffParty_egoSameParty <- rowSums(eDiffParty_egoSameParty, na.rm = TRUE)
  vDiffParty_egoDiffParty <- rowSums(eDiffParty_egoDiffParty, na.rm = TRUE)
  
  # Assign calculated exposure values to each agent.
  
  for (i in 1:nrow(agents)){
    if (agents$party_cd[i] != "UNA"){
      print(paste("Processing row:", i))
      print(paste("ncid:", agents$index[i]))
      print(paste("Same party exposure:", vSameParty_egoSameParty[agents$index[i]]))
      print(paste("Different party exposure:", vDiffParty_egoSameParty[agents$index[i]]))
      
      agents$exposureIngroup[i] <- vSameParty_egoSameParty[agents$index[i]]
      agents$exposureOutgroup[i] <- vDiffParty_egoSameParty[agents$index[i]]
    } else {
      agents$exposureIngroup[i] <- 0
      agents$exposureOutgroup[i] <- 0
    }
  }

  return(as.data.frame(cbind(
    agents$party_cd,
    agents$index,
    agents$exposureIngroup,
    agents$exposureOutgroup
  )))
}

exposure <- calcExposurePartisanship(geo_nc_27555, proxmat1)
names(exposure) <- c("party_cd", "index", "exposureIngroup", "exposureOutgroup")


# merging it to dataframe 
# geo_nc_27555$exposureIngroup <- exposure$exposureIngroup
# geo_nc_27555$exposureOutgroup <- exposure$exposureOutgroup



##################################################-----------------------------------------------

library(sp)
install.packages("rgdal")
library(rgdal)
library(raster)


##### 1. Define Grids and Calculate Span and Convert geographic coordinates to UTM


## CRS Transformation: 
# The coordinates are transformed from the geographic coordinate system (longitude, latitude) 
# to the UTM coordinate system, which uses meters.

coords <- geo_nc_2755_test[, c("longitude", "latitude")]
coordinates(coords) <- ~longitude+latitude
proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")

coords_utm <- spTransform(coords, CRS("+proj=utm +zone=17 +datum=WGS84"))
utm_coords <- coordinates(coords_utm)


# Step 2: Define the extent and create raster
extent_raster <- extent(min(utm_coords[,1]), max(utm_coords[,1]), min(utm_coords[,2]), max(utm_coords[,2]))

# Calculate the number of rows and columns
ncols <- ceiling((extent_raster@xmax - extent_raster@xmin) / 200)
nrows <- ceiling((extent_raster@ymax - extent_raster@ymin) / 200)

# Create the raster
raster_200m <- raster(nrows = nrows, ncols = ncols, crs = CRS("+proj=utm +zone=17 +datum=WGS84"))

# Set the extent of the raster
extent(raster_200m) <- extent_raster

# Step 3: Rasterize each attribute


table(geo_nc_27555$race_numeric)
table(geo_nc_27555$party_numeric)


raster_race <- rasterize(utm_coords, raster_200m, geo_nc_27555$race_numeric, fun = mean)
raster_party <- rasterize(utm_coords, raster_200m, geo_nc_27555$party_numeric, fun = mean, na.rm = TRUE)


# Step 4: Combine rasters into a stack
raster_stack <- stack(raster_race, raster_party)
names(raster_stack) <- c("race_numeric", "party_affiliation")

plot(raster_stack)

# Step 5: Convert raster to SpatialPolygonsDataFrame
grid_polygons <- rasterToPolygons(raster_stack)
grid_polygons@data$ID <- 1:nrow(grid_polygons)

# Step 6: Assign agents to grid cells
overlay <- over(coords_utm, grid_polygons)
geo_nc_27555$grid_cell_id <- overlay$ID

# View the updated data with grid cell IDs
head(geo_nc_27555)








############################################------------------------------------------------
# not needed anymore 

# Span Calculation: 
# The minimum and maximum coordinates are used to calculate the span of the area in meters.

minX <- min(utm_coords[, 1])
minY <- min(utm_coords[, 2])
maxX <- max(utm_coords[, 1])
maxY <- max(utm_coords[, 2])

spanX <- maxX - minX
spanY <- maxY - minY


#Number of Cells: The span is divided by 100m to get the number of 100m cells required to cover the area, 
# with adjustments for any remainder to ensure all points are covered.

# Calculate the number of 100m cells
nCells100m_X <- spanX %/% 100
if (spanX %% 100 > 0) {nCells100m_X <- nCells100m_X + 1}

nCells100m_Y <- spanY %/% 100
if (spanY %% 100 > 0) {nCells100m_Y <- nCells100m_Y + 1}


# Print the results
print(paste("Span in X direction (meters):", spanX))
print(paste("Span in Y direction (meters):", spanY))
print(paste("Number of 100m cells in X direction:", nCells100m_X))
print(paste("Number of 100m cells in Y direction:", nCells100m_Y))


#### adjust raster dimensions 

rastMaxX100m <- nCells100m_X * 100 + minX
rastMaxY100m <- nCells100m_Y * 100 + minY

## create raster 

raster100m <- raster(
  xmn=minX,
  xmx=rastMaxX100m,
  ymn=minY,
  ymx=rastMaxY100m,
  resolution=100,
  crs=coords_utm
)

if (
  raster100m@ncols != nCells100m_X |
  raster100m@nrows != nCells100m_Y)
{warning("Error with raster generation.")}


# overlay raster data with NC zipcode data 
# using the utm coords as the raster is created via meters 

grid100m <- rasterize(
  utm_coords,
  raster100m,
  geo_nc_27555$race_numeric,
  fun=mean
)


plot(grid100m)

###########################################------------------------------------------------------











######### Dissimilarity Index Calculation ########

# Define the dissimilarity index function
dissimilarityIndex <- function(grid, inputWorld) {
  
  # We calculate the denominators of the dissimilarity index:
  sumWestern <- sum(inputWorld$western)
  sumNonWestern <- sum (inputWorld$nonWestern)
  
  # We force R to interpret "grid" as the name of the variable we pass to the 
  # function as argument. For example, if we request dissimilarityIndex(cell500m, world)
  # the script will interpret "inputWorld$grid" as "world$cell500m".
  inputWorld$grid <- eval(substitute(grid), inputWorld)
  
  # Next, we measure the composition of each individual locality.
  diss <- c()
  for (i in unique(inputWorld$grid)){
    w <- subset(inputWorld, inputWorld$grid == i)
    localWestern <- sum (w$western)
    localNonWestern <- sum (w$nonWestern)
    diss <- append(
      diss,
      abs((localWestern / sumWestern) - (localNonWestern / sumNonWestern))
    )
  }
  
  # And finally we have all the ingredients to calculate the index:
  return (sum(diss) * 0.5)
}







#############################################--------------------------------------------




####### Blau’s Index of Heterogeneity: ###########

# Formula: (1- square of proportion) for each cell to get the Blau Index for each cell 

geo_nc_27555_df <-  as.data.frame(geo_nc_27555)
  
# Calculate the proportion of each race within each grid cell
race_proportions <- geo_nc_27555_df %>%
    group_by(grid_cell_id, race_code) %>%
    summarise(count = n()) %>%
    mutate(race_proportion = count / sum(count)) %>%
    mutate(race_proportion_sq = race_proportion^2) %>%
    mutate(Blau_race = 1- sum(race_proportion_sq)) %>%
    ungroup()


  # Calculate the proportion of each party within each grid cell
party_proportions <- geo_nc_27555_df %>%
  group_by(grid_cell_id, party_cd) %>%
  summarise(count = n()) %>%
  mutate(party_proportion = count / sum(count)) %>%
  mutate(party_proportion_sq = party_proportion^2) %>%
  mutate(Blau_party = 1- sum(party_proportion_sq)) %>%
  ungroup()


race_proportions$race_code <- NULL
race_proportions$race_proportion <- NULL
race_proportions$race_proportion_sq <- NULL
race_proportions$count <- NULL


party_proportions$party_cd <- NULL
party_proportions$party_proportion <- NULL
party_proportions$party_proportion_sq <- NULL
party_proportions$count <- NULL

proportions <- left_join(race_proportions, party_proportions, by = "grid_cell_id")
proportions <- unique(proportions)

geo_nc_27555_blau <- left_join(geo_nc_27555_df, proportions, by = "grid_cell_id")


######### rasterizing the blau indices for race and party per cell 

raster_blau_race <- rasterize(utm_coords, raster_200m, geo_nc_27555_blau$Blau_race, fun = mean)
raster_blau_party <- rasterize(utm_coords, raster_200m, geo_nc_27555_blau$Blau_party, fun = mean)

raster_blau_stack <- stack(raster_blau_race, raster_blau_party)
names(raster_blau_stack) <- c("race_numeric", "party_affiliation")

plot(raster_blau_stack)



#################################################----------------------------------




sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}


geo_nc_2755_test <- geo_nc_27555_df

geo_nc_2755_test$birth_year <- as.numeric(geo_nc_2755_test$birth_year)
geo_nc_2755_test <- geo_nc_2755_test %>% mutate(age = 2020-birth_year)


hist(geo_nc_2755_test$age)


geo_nc_2755_test <- geo_nc_2755_test %>% mutate(age_binned = case_when(age > 13 & age < 20 ~"14-19 years",
                                                                       age > 19 & age < 36 ~"20-35 years",
                                                                       age > 35 & age < 51 ~"36-50 years",
                                                                       age > 50 & age < 70 ~"51-69 years",
                                                                       age > 69 ~"70 + years"))


#### creating probability vote variable for each party in the neighbourhood and append them to the dataset

# List of possible parties
parties <- unique(geo_nc_2755_test$party_cd)

# Initialize probability columns for each party
for (party in parties) {
  if(party != "UNA"){
    geo_nc_2755_test[[paste0(party, "_prob")]] <- 0
  }
  else {
    geo_nc_2755_test[[paste0(party, "_prob")]] <-NA
  }
}

# Set initial probabilities
for (i in 1:nrow(geo_nc_2755_test)) {
  current_party <- geo_nc_2755_test$party_cd[i]
  if (current_party == "UNA") {
    for (party in parties) {
      geo_nc_2755_test[[paste0(party, "_prob")]][i] <- 0.5
    }
  } else {
    geo_nc_2755_test[[paste0(current_party, "_prob")]][i] <- 1
  }
}


######### Vote Preference Update #############

sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}

# Helper function to convert probability to logit
logit <- function(p) {
  return(log(p / (1 - p)))
}


updateVoteProbability <- function(agents, ego, alter, alpha, beta, total_attributes) {
  
  # Calculate Kronecker delta for party difference
  party_diff <- ifelse(agents$party_cd[ego] == agents$party_cd[alter], 0, 1)
  
  if(agents$party_cd[alter] == "UNA") {
    delta_P <- 0
  } else {
    
  # Calculate the number of shared sociodemographic attributes
  shared_attributes <- sum(
    agents$age_binned[ego] == agents$age_binned[alter], 
    agents$race_code[ego] == agents$race_code[alter], 
    agents$gender_code[ego] == agents$gender_code[alter]
  )
  
  influence_weight <- sigmoid(shared_attributes / total_attributes)
  
  # Calculate the change in probability based on party difference
  if (party_diff == 1) {
    # Different party: decrease probability
    delta_P <- -alpha * influence_weight
  } else {
    # Same party: increase probability
    delta_P <- beta * influence_weight
  }
  }
  
  # Update the voting probability
  # Get the party columns to update
  ego_party_col <- paste0(agents$party_cd[ego], "_prob")
  alter_party_col <- paste0(agents$party_cd[alter], "_prob")
  
  # Update the voting probability
  agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
  agents[[alter_party_col]][ego] <- agents[[alter_party_col]][ego] - delta_P
  
  # Ensure the probability stays within bounds [0, 1]
  #agents[[ego_party_col]][ego] <- max(0, min(1, agents[[ego_party_col]][ego]))
  #agents[[alter_party_col]][ego] <- max(0, min(1, agents[[alter_party_col]][ego]))
  
  return(agents)
  
}

updateVoteProbability(geo_nc_2755_test, 1, 9, 0.1, 0.05, 3)



## check tomorrow how to use sigmoid function for vote probabilities
## and download the whole dataset again



########################################-----------------------------------

head(geo_nc_27555)
str(ncvhis_Statewide$election_lbl)


# getting list of 2024 voters ncid to check which ones exist in the historical data (but maybe taking 2020)
# as starting point for ABM cause then I have enough previous records for vote_intensity based on previous
# elections and enough years for the ABM to run 


ncid_list <- nc_27555$ncid


ncvhis_Statewide_27555 <- ncvhis_Statewide %>% filter(ncid %in% ncid_list)

ncvhis_Statewide_27555_2024 <- left_join(ncvhis_Statewide_27555, ncvoter_Statewide, by = "ncid")

length(ncvhis_Statewide_27555_2024$ncid)

voter_data_2020_27555 <- ncvhis_Statewide_27555 %>% filter(election_lbl == "11/03/2020")

table(voter_data_2020_27555$voted_party_cd)

# prepare data for regression of vote behavior


voter_data_2016 <- voter_data_2016 %>% select(election_lbl, voted_party_cd, voter_reg_num, ncid)


# get date time into correct format using lubridate 

ncvhis_Statewide_27555$election_lbl <- lubridate::mdy(ncvhis_Statewide_27555$election_lbl)


voter_data_2020 <- ncvhis_Statewide_27555 %>% filter(election_lbl < "2021-11-02")

table(ncvhis_Statewide_27555$election_lbl)


voter_data_2020 <- voter_data_2020 %>% 
  group_by(ncid) %>%
  mutate(DEM_prob = sum(voted_party_cd == "DEM")/length(ncid)) %>%
  mutate(UNA_prob = sum(voted_party_cd == "UNA")/length(ncid)) %>%
  mutate(REP_prob = sum(voted_party_cd == "REP")/length(ncid))




### find out tomorrow how markovchain is working and if it gets that the transition happen 
# between different voters

library(markovchain)

transitions <- voter_data_2020 %>%
  group_by(ncid) %>%
  arrange(election_lbl) %>%
  summarise(transitions = list(voted_party_cd)) %>%
  pull(transitions) %>%
  unlist()



mc_transition <- new("markovchain", states = colnames(transition_matrix), transitionMatrix = transition_matrix)


mc <- markovchainFit(data = voter_data_2020$voted_party_cd[[1]])

mc$estimate
plot(mc$estimate)


?markovchainFit()


##############################
  # estimate stubbornness #
##############################

# Calculate stubbornness for each voter
voter_data_2020 <- voter_data_2020 %>%
  group_by(ncid) %>%
  mutate(stubbornness = rle(as.character(voted_party_cd))$lengths[1]) %>%
  mutate(st_stubbornness = stubbornness/length(ncid))




voter_data_2020 <- voter_data_2020 %>%
  group_by(ncid) %>%
  mutate(
    rle_votes = list(rle(as.character(voted_party_cd))),
    max_len = max(rle_votes[[1]]$lengths),
    max_len2 = min(rle_votes[[1]]$lengths),
    max_party = rle_votes[[1]]$values[which.max(rle_votes[[1]]$lengths)]
  ) %>%
  mutate(
    stubbornness = ifelse(max_party == "UNA", max_len2, max_len),
    st_stubbornness = stubbornness / length(ncid)
  ) %>%
  ungroup() %>%
  dplyr::select(-rle_votes, -max_len, -max_party, -max_len2)



voter_data_2020_test2 <- voter_data_2020 %>%
  group_by(ncid) %>%
  mutate(stubbornness = ifelse(voted_party_cd != "UNA", rle(as.character(voted_party_cd))$lengths[1], 0)) %>%
  mutate(st_stubbornness = stubbornness / length(ncid))


# Summarize stubbornness
stubbornness_summary <- voter_data_2020 %>%
  group_by(ncid) %>%
  summarize(mean_stubbornness = mean(stubbornness, na.rm = TRUE))



voter_data_2020 %>% group_by(ncid) %>%
  mutate()


###### merging data from 2016 to 2024 based on identifier ######

merged_data <- left_join(voter_data_2016, ncvoter_Statewide, by = "ncid")

merged_data <- merged_data %>% select(election_lbl, voted_party_cd, ncid, res_street_address, zip_code, party_cd, race_code, ethnic_code, gender_code, birth_year, age_at_year_end, birth_state)

merged_data <- merged_data %>% rename("party_voted_2016" = voted_party_cd)

merged_data <- merged_data %>% rename("party_voted_2024" = party_cd)



###### creating variable for voter change ######

merged_data <- merged_data %>% mutate(party_change = case_when(party_voted_2016 == party_voted_2024 ~ 0,
                                                               party_voted_2016 != party_voted_2024 ~ 1))


table(merged_data$party_change)

# 518959/3215287 = 0.1614036

merged_data_27814 <- merged_data %>% filter(zip_code == 27814)
table(merged_data_27814$party_change)

# 97/746 = 0.1300268


merged_data_long <- pivot_longer(merged_data, cols = c(party_voted_2016, party_voted_2024),
                                 names_to = "year",
                                 values_to = "party_affiliation"
                                 )









load("/Users/charlottekuberka/Downloads/ICPSR_38606/DS0001/38606-0001-Data.rda")

library(haven)
gss2018 <- read_sas("Desktop/2018/gss2018.sas7bdat", NULL)

sum(is.na(gss2018$SOCOMMUN)== FALSE)


table(gss2018$XNORCSIZ)
sum(is.na(gss2018$XNORCSIZ)== TRUE)

gss2020panel_r1a <- read_dta("Desktop/GSS_2020_panel_stata_1a/gss2020panel_r1a.dta")

table(gss2020panel_r1a$yearid)

