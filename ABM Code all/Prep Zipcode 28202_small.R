

###############################################
####### Prepping data 28202_precinct for ABM ###########
###############################################


library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)

library(RSQLite)
library(rvest)
library(RSelenium)
library(stringr)

library(readxl)
library(tmap)
library(sf)
library(DBI)
library("netstat")
library(jsonlite)

library(readr)
library(tidyr)
library(tidycensus)

library(tigris)
library(measurements)

library("geosphere")
library(sp)
library(units)
library(raster)


###################################
######## loading data 
###################################

ncvoter_Statewide <- read.delim("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/ncvoter_Statewide.txt", header=FALSE)
names(ncvoter_Statewide) <- ncvoter_Statewide[1,]
ncvoter_Statewide <- ncvoter_Statewide[-1,]
ncvoter_Statewide <- ncvoter_Statewide %>% filter(voter_status_desc == "ACTIVE")

ncvhis_Statewide <- read.delim("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/ncvhis_Statewide.txt")


### using 28202_precinct zipcode in Charlotte

nc_28202_all <- ncvoter_Statewide %>% filter(zip_code == 28202)
nc_28202 <- nc_28202_all %>% filter(ward_abbrv == 1)
nc28202_precinct <- nc_28202_all %>% filter(precinct_abbrv == "009")

# smallest level of data for city 

# check if ncid is in dataset from 2020 

ncid_list_28202_precinct <- nc28202_precinct$ncid
ncvhis_Statewide_28202_precinct <- ncvhis_Statewide %>% filter(ncid %in% ncid_list_28202_precinct)
ncvhis_Statewide_2020_28202_precinct <- ncvhis_Statewide_28202_precinct %>%  filter(election_lbl == "11/03/2020")
ncvhis_Statewide_2020_28202_precinct <- left_join(ncvhis_Statewide_2020_28202_precinct, nc28202_precinct, by="ncid")



### transforming 2020 set ncvhis_Statewide_2020_28202_precinct to nc_28202_precinct
nc_28202_precinct <- ncvhis_Statewide_2020_28202_precinct


table(nc_28202_precinct$party_cd)
table(nc28202_precinct_precinct$party_cd)



####################################################################
#### 1. Preparing the addresses of zipcode for scraping coordinates
####################################################################

adress_28202_precinct <- paste(nc_28202_precinct$res_street_address, nc_28202_precinct$res_city_desc,", NC")

adress_df_28202_precinct <- data.frame(address = adress_28202_precinct)

nc_28202_precinct$full_adress <- adress_28202_precinct

#adress <- nc_27555$res_street_address

adress_28202_precinct <- unique(adress_28202_precinct)
length(adress_28202_precinct)



###############################################
#### 2. Using google API: 
###############################################

register_google(key = "AIzaSyCBzo8BkD9dQ94fW4oKDkQAUWEdmragCaU")


geocoded_adress <- data.frame()


#Loop through each street name
for(street in adress_28202_precinct){
  # Geocode the current street name
  geocoded_street <- geocode(street, output = "latlon")
  
  # Create a temporary dataframe to store geocoded information for the current street
  temp_df <- data.frame(
    address = street,
    lon = geocoded_street$lon,
    lat = geocoded_street$lat
  )
  
  # Append the temporary dataframe to the geocoded dataset
  geocoded_adress <- rbind(geocoded_adress, temp_df)
}



### removing the NAs where the coords haven't been retrieved

coords_adress_28202_precinct <- na.omit(geocoded_adress)


###############################################
#### 3. merging registration data with coords
###############################################


nc_28202_precinct_small <- nc_28202_precinct %>% dplyr::select(ncid, race_code, ethnic_code, voted_party_cd, party_cd, gender_code, birth_year, birth_state, full_adress, precinct_abbrv)

#### creating points for adresses:


coords_adress_28202_precinct <- left_join(adress_df_28202_precinct, coords_adress_28202_precinct, by = c("address"))
coords_adress_28202_precinct <- na.omit(coords_adress_28202_precinct)


save(coords_adress_28202_precinct, file = "coords_adress_28202_precinct.RData")


points_28202_precinct <- st_as_sf(coords_adress_28202_precinct, coords = c("lon", "lat"), crs = 4269)

geo_nc_28202_precinct_points <- left_join(points_28202_precinct, nc_28202_precinct_small, by =c("address" = "full_adress"))
geo_nc_28202_precinct_points <- unique(geo_nc_28202_precinct_points)



###############################################
##### 4. getting NC shp file data #####
###############################################


nc_shapefile <- st_read("~/Desktop/LSE Term 2/Capstone Project/Data/NC Voter Registration Data/North_Carolina_State_and_County_Boundary_Polygons/North_Carolina_State_and_County_Boundary_Polygons.shp")

nc_johnston <- nc_shapefile %>% filter(FIPS == 101)

centerlines <- st_read("~/Desktop/LSE Term 2/Capstone Project/Data/NC Voter Registration Data/Centerlines/centerli.shp")

zipcodes <- st_read("~/Desktop/LSE Term 2/Capstone Project/Data/NC Voter Registration Data/zipcode shapefile/tl_2023_us_zcta520.shp")

zipcodes28202_precinct <- zipcodes %>% filter(ZCTA5CE20 == 28202_precinct)



coords_28202_precinct <- coords_adress_28202_precinct %>% dplyr::select(lon, lat)
coords_28202_precinct$lon <- as.numeric(coords_28202_precinct$lon)
coords_28202_precinct$lat <- as.numeric(coords_28202_precinct$lat)



######## plotting the coords with agents

tmap_mode("view")

tm_shape(zipcodes28202_precinct) + 
  tm_polygons() +
  #tm_shape(centerlines) + 
  #tm_lines() + 
  tm_shape(geo_nc_28202_precinct_points) +
  tm_symbols(col = "party_cd", size = 0.01)


table(nc_28202_precinct$party_cd)



###############################################
### 5. creating additional variables
###############################################


# create a variable for non white population or races as percentage of actual population,
# mostly make them numeric to calculate statistics and Blau Index

nc_28202_precinct_small <- nc_28202_precinct_small %>% mutate(non_white_perc = sum(race_code != "W") / nrow(nc_28202_precinct_small))


nc_28202_precinct_small <- nc_28202_precinct_small %>% mutate(race_numeric = case_when(race_code == "W" ~1,
                                                                     race_code == "B" ~ 2,
                                                                     race_code == "I" ~ 3,
                                                                     race_code == "A" ~ 4,
                                                                     race_code == "O" | race_code == "U" | race_code == "M" ~ 5))




nc_28202_precinct_small <- nc_28202_precinct_small %>% mutate(party_numeric = case_when(voted_party_cd == "REP" ~ 1,
                                                                      voted_party_cd == "DEM" ~2,
                                                                      voted_party_cd == "LIB" ~3,
                                                                      voted_party_cd == "LIB" ~ 4,
                                                                      voted_party_cd == "GRE" ~ 5,
                                                                      voted_party_cd == "UNA" ~6))

##### age


nc_28202_precinct_small$birth_year <- as.numeric(nc_28202_precinct_small$birth_year)
nc_28202_precinct_small <- nc_28202_precinct_small %>% mutate(age = 2020-birth_year)


hist(nc_28202_precinct_small$age)


nc_28202_precinct_small <- nc_28202_precinct_small %>% mutate(age_binned = case_when(age > 13 & age < 20 ~"14-19 years",
                                                                   age > 19 & age < 36 ~"20-35 years",
                                                                   age > 35 & age < 51 ~"36-50 years",
                                                                   age > 50 & age < 70 ~"51-69 years",
                                                                   age > 69 ~"70 + years"))



######################################################
##### 6. creating distance and proximity matrices
######################################################

###### creating spatial data frame with coords 

geo_nc_28202_precinct <- left_join(nc_28202_precinct_small, coords_adress_28202_precinct, by =c("full_adress" = "address"))
geo_nc_28202_precinct <- unique(geo_nc_28202_precinct)

geo_nc_28202_precinct$lon <- as.numeric(geo_nc_28202_precinct$lon)
geo_nc_28202_precinct$lat <- as.numeric(geo_nc_28202_precinct$lat)


## coordinates() is a function from the sp package in R, which is used to specify which columns of a dataframe 
## should be used as the spatial coordinates. It then transforms the df into a spatial object




#geo_nc_28202_precinct <- na.omit(geo_nc_28202_precinct)
coordinates(geo_nc_28202_precinct) = c("lon","lat")



ncells_28202_precinct <- length(geo_nc_28202_precinct)
distmat_28202_precinct <- matrix(NA, nrow=ncells_28202_precinct, ncol=ncells_28202_precinct)
proxmat1_28202_precinct <- matrix(NA, nrow=ncells_28202_precinct, ncol=ncells_28202_precinct)
proxmat2_28202_precinct <- matrix(NA, nrow=ncells_28202_precinct, ncol=ncells_28202_precinct)
proxmat3_28202_precinct <- matrix(NA, nrow=ncells_28202_precinct, ncol=ncells_28202_precinct)

# Next, we calculate the distance matrix, in meters.
# We start by creating an empty matrix. Then we fill it in, row by row.

for (i in 1:ncells_28202_precinct) {
  distmat_28202_precinct[i,] <- distVincentyEllipsoid(coordinates(geo_nc_28202_precinct)[i,], coordinates(geo_nc_28202_precinct)) 
}

# Inside the loop, the function distVincentyEllipsoid() is used to calculate the distance between the coordinates 
# of the current cell (coordinates(dat)[i,]) and the coordinates of all other cells in the city (coordinates(dat)).




# We apply the distance decay function, so that the matrix expresses proximity instead of distance.

# By applying the exponential function to the negative distances (scaled by a factor of 10), 
# it converts distances into proximity values.

proxmat1_28202_precinct <- exp(-distmat_28202_precinct/10) 
proxmat2_28202_precinct <- exp(-distmat_28202_precinct/100)
proxmat3_28202_precinct <- exp(-distmat_28202_precinct/1000)


# diag(proxmat1) <- exp(-52.140543316/10)   #######
# diag(proxmat2) <- exp(-52.140543316/100)  #######
# diag(proxmat3) <- exp(-52.140543316/1000) #######
#printMat(proxmat)

# Normalize rows:
#sum <- rowSums(proxmat)
for (i in 1:length(distmat_28202_precinct[1,])){
  #proxmat1[i,] <- proxmat1[i,] / sum[i]
  #proxmat2[i,] <- proxmat2[i,] / sum[i]
  #proxmat3[i,] <- proxmat3[i,] / sum[i]
  proxmat1_28202_precinct[i,] <- proxmat1_28202_precinct[i,] / sum(proxmat1_28202_precinct[i,])
  proxmat2_28202_precinct[i,] <- proxmat2_28202_precinct[i,] / sum(proxmat2_28202_precinct[i,])
  proxmat3_28202_precinct[i,] <- proxmat3_28202_precinct[i,] / sum(proxmat3_28202_precinct[i,])
}



# Function to plot matrix for quick visual inspection:
printMat <- function(x){
  return(
    image(x, col=heat.colors(10000), zlim=c(min(x, na.rm=T),max(x, na.rm=T)))
  )
}


save(distmat_28202_precinct, file= "distmat_28202_precinct.RData")

################################################################
### 7. Calculating Group Statistics
################################################################


### calculating Exposure for Different Partisanships 

## not sure how to implement it or what is meant by exposure, maybe just calculate Moran's I or another
## similarity index for race and party ? 


geo_nc_28202_precinct$index <- (1:1630)


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





exposure <- calcExposurePartisanship(geo_nc_28202_precinct, proxmat1_28202_precinct)
names(exposure) <- c("party_cd", "index", "exposureIngroup", "exposureOutgroup")

save(exposure, file= "exposure_28202_precinct.RData") 


#######################################################
##### creating world like Flache et al.
#######################################################


##### 1. Define Grids and Calculate Span and Convert geographic coordinates to UTM


## CRS Transformation: 
# The coordinates are transformed from the geographic coordinate system (longitude, latitude) 
# to the UTM coordinate system, which uses meters.


coordinates(coords_28202_precinct) <- ~lon+lat

proj4string(coords_28202_precinct) <- CRS("+proj=longlat +datum=WGS84")

# Check the CRS
print(proj4string(coords_28202_precinct))



coords_utm_28202_precinct_data <- spTransform(coords_28202_precinct, CRS("+proj=utm +zone=17 +datum=WGS84"))
coords_utm_28202_precinct <- coordinates(coords_utm_28202_precinct_data)

# Check the transformed coordinates
print(head(coordinates(coords_utm_28202_precinct)))

# Step 2: Define the extent and create raster
extent_raster_28202_precinct <- extent(min(coords_utm_28202_precinct[,1]), max(coords_utm_28202_precinct[,1]), min(coords_utm_28202_precinct[,2]), max(coords_utm_28202_precinct[,2]))


# Calculate the number of rows and columns and number of Cells: 
# The span is divided by 500m to get the number of 100m cells required to cover the area, 
# with adjustments for any remainder to ensure all points are covered.
ncols_28202_precinct <- ceiling((extent_raster_28202_precinct@xmax - extent_raster_28202_precinct@xmin) / 100)
nrows_28202_precinct <- ceiling((extent_raster_28202_precinct@ymax - extent_raster_28202_precinct@ymin) / 100)

# Create the raster
raster_100m_28202_precinct <- raster(nrows = nrows_28202_precinct, ncols = ncols_28202_precinct, crs = CRS("+proj=utm +zone=17 +datum=WGS84"))

# Set the extent of the raster
extent(raster_100m_28202_precinct) <- extent_raster_28202_precinct

# Step 3: Rasterize each attribute

raster_race_28202_precinct <- rasterize(coords_utm_28202_precinct, raster_100m_28202_precinct, geo_nc_28202_precinct$race_numeric, fun = mean)
raster_party_28202_precinct <- rasterize(coords_utm_28202_precinct, raster_100m_28202_precinct, geo_nc_28202_precinct$party_numeric, fun = mean)


# Step 4: Combine rasters into a stack
raster_stack_28202_precinct <- stack(raster_race_28202_precinct, raster_party_28202_precinct)
names(raster_stack_28202_precinct) <- c("race_numeric", "party_affiliation")

plot(raster_stack_28202_precinct)


# Step 5: Convert raster to SpatialPolygonsDataFrame
grid_polygons_28202_precinct <- rasterToPolygons(raster_stack_28202_precinct)
grid_polygons_28202_precinct@data$ID <- 1:nrow(grid_polygons_28202_precinct)

# Step 6: Assign agents to grid cells
overlay_28202_precinct <- over(coords_utm_28202_precinct_data, grid_polygons_28202_precinct)
geo_nc_28202_precinct$grid_cell_id <- overlay_28202_precinct$ID


overlay_28202_precinct <- na.omit(overlay_28202_precinct)




# View the updated data with grid cell IDs
head(geo_nc_28202_precinct)
table(geo_nc_28202_precinct$grid_cell_id)



####### Blau’s Index of Heterogeneity: ###########

# Formula: (1- square of proportion) for each cell to get the Blau Index for each cell 

geo_nc_28202_precinct_df <-  as.data.frame(geo_nc_28202_precinct)

# Calculate the proportion of each race within each grid cell
race_proportions_28202_precinct <- geo_nc_28202_precinct_df %>%
  group_by(grid_cell_id, race_code) %>%
  summarise(count = n()) %>%
  mutate(race_proportion = count / sum(count)) %>%
  mutate(race_proportion_sq = race_proportion^2) %>%
  mutate(Blau_race = 1- sum(race_proportion_sq)) %>%
  ungroup()


# Calculate the proportion of each party within each grid cell
party_proportions_28202_precinct <- geo_nc_28202_precinct_df %>%
  group_by(grid_cell_id, party_cd) %>%
  summarise(count = n()) %>%
  mutate(party_proportion = count / sum(count)) %>%
  mutate(party_proportion_sq = party_proportion^2) %>%
  mutate(Blau_party = 1- sum(party_proportion_sq)) %>%
  ungroup()


race_proportions_28202_precinct$race_code <- NULL
race_proportions_28202_precinct$race_proportion <- NULL
race_proportions_28202_precinct$race_proportion_sq <- NULL
race_proportions_28202_precinct$count <- NULL


party_proportions_28202_precinct$party_cd <- NULL
party_proportions_28202_precinct$party_proportion <- NULL
party_proportions_28202_precinct$party_proportion_sq <- NULL
party_proportions_28202_precinct$count <- NULL

proportions_28202_precinct <- left_join(race_proportions_28202_precinct, party_proportions_28202_precinct, by = "grid_cell_id")
proportions_28202_precinct <- unique(proportions_28202_precinct)

geo_nc_28202_precinct_blau <- left_join(geo_nc_28202_precinct_df, proportions_28202_precinct, by = "grid_cell_id")



######### rasterizing the blau indices for race and party per cell 

raster_blau_race_28202_precinct <- rasterize(coords_utm_28202_precinct, raster_100m_28202_precinct, geo_nc_28202_precinct_blau$Blau_race, fun = mean)
raster_blau_party_28202_precinct <- rasterize(coords_utm_28202_precinct, raster_100m_28202_precinct, geo_nc_28202_precinct_blau$Blau_party, fun = mean)

raster_blau_stack_28202_precinct <- stack(raster_blau_race_28202_precinct, raster_blau_party_28202_precinct)
names(raster_blau_stack_28202_precinct) <- c("race_numeric", "party_affiliation")

plot(raster_blau_stack_28202_precinct)


save(geo_nc_28202_precinct_blau, file="geo_nc_28202_precinct_blau.RData")
save(geo_nc_28202_precinct_df, file="geo_nc_28202_precinct_df.RData")
save(geo_nc_28202_precinct, file="geo_nc_28202_precinct.RData")




load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/distmat_28202_precinct.RData")
load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/geo_nc_28202_precinct_blau.RData")
load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/geo_nc_28202_precinct_df.RData")
load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/geo_nc_28202_precinct.RData")




################################################################
### 8. Finding voters in 2020
################################################################


# getting list of 2024 voters ncid to check which ones exist in the historical data (but maybe taking 2020)
# as starting point for ABM cause then I have enough previous records for vote_intensity based on previous
# elections and enough years for the ABM to run 



nc_28202_precinct_all <- ncvoter_Statewide %>% filter(zip_code == 28202_precinct)
nc_28202_precinct_2024 <- nc_28202_precinct_all %>% filter(ward_abbrv == 1)


ncid_list_28202_precinct <- nc_28202_precinct$ncid
ncvhis_Statewide_28202_precinct <- ncvhis_Statewide %>% filter(ncid %in% ncid_list_28202_precinct)

geo_nc_28202_precinct_abm <- ncvhis_Statewide_28202_precinct %>% filter(election_lbl < "2021-11-02")

table(geo_nc_28202_precinct_abm$voted_party_cd)


geo_nc_28202_precinct_abm <- geo_nc_28202_precinct_abm %>% 
  group_by(ncid) %>%
  mutate(DEM_prob = sum(voted_party_cd == "DEM")/length(ncid)) %>%
  mutate(UNA_prob = sum(voted_party_cd == "UNA")/length(ncid)) %>%
  mutate(LIB_prob = sum(voted_party_cd == "LIB")/length(ncid)) %>%
  mutate(GRE_prob = sum(voted_party_cd == "GRE")/length(ncid)) %>%
  mutate(REP_prob = sum(voted_party_cd == "REP")/length(ncid))



##############################
# estimate stubbornness #
##############################

# Calculate stubbornness for each voter
geo_nc_28202_precinct_abm <- geo_nc_28202_precinct_abm %>%
  group_by(ncid) %>%
  mutate(stubbornness = rle(as.character(voted_party_cd))$lengths[1]) %>%
  mutate(st_stubbornness = stubbornness/length(ncid))


geo_nc_28202_precinct_abm <- geo_nc_28202_precinct_abm %>%
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



# rearrange voter probability to make sure that the voted party last is the one with the highest probability
# so we dont accidentally use the party with the highest probability which is != voted_party_cd

geo_nc_28202_precinct_abm <- geo_nc_28202_precinct_abm %>%
  rowwise() %>%
  mutate(
    DEM_prob = ifelse(voted_party_cd == "DEM", 1, DEM_prob),
    UNA_prob = ifelse(voted_party_cd == "UNA", 1, UNA_prob),
    REP_prob = ifelse(voted_party_cd == "REP", 1, REP_prob),
    LIB_prob = ifelse(voted_party_cd == "LIB", 1, LIB_prob),
    GRE_prob = ifelse(voted_party_cd == "GRE", 1, GRE_prob)
  ) %>%
  ungroup()



geo_nc_28202_precinct_blau_small <- geo_nc_28202_precinct_blau %>% dplyr::select(ncid, Blau_race, Blau_party, age_binned, race_code, gender_code, grid_cell_id)

geo_nc_28202_precinct_blau_abm <- left_join(geo_nc_28202_precinct_abm, geo_nc_28202_precinct_blau_small, by = "ncid")

geo_nc_28202_precinct_blau_abm <- geo_nc_28202_precinct_blau_abm %>% filter(election_lbl == "11/03/2020")







# prepare data for regression of vote behavior

### find out tomorrow how markovchain is working and if it gets that the transition happen 
# between different voters

library(markovchain)

transitions_28202_precinct <- geo_nc_28202_precinct_blau_abm %>%
  group_by(ncid) %>%
  arrange(election_lbl) %>%
  summarise(transitions = list(voted_party_cd)) %>%
  pull(transitions) %>%
  unlist()



mc_transition_28202_precinct <- new("markovchain", states = colnames(transition_matrix), transitionMatrix = transition_matrix)


mc <- markovchainFit(data = geo_nc_27915_abm$voted_party_cd[[1]])

mc$estimate
plot(mc$estimate)



###################################################
############### Vote Switches #####################
###################################################

###### merging data from 2020 to 2024 based on identifier ######


nc_28202_precinct_2024 <- geo_nc_28202_precinct_df



nc_28202_precinct_2024 <- nc_28202_precinct_2024 %>% rename("party_voted_2020" = voted_party_cd)

nc_28202_precinct_2024 <- nc_28202_precinct_2024 %>% rename("party_voted_2024" = party_cd)



###### creating variable for voter change ######

nc_28202_precinct_2024 <- nc_28202_precinct_2024 %>% mutate(party_change = case_when(party_voted_2020 == party_voted_2024 ~ 0,
                                                                   party_voted_2020 != party_voted_2024 ~ 1))


table(nc_28202_precinct_2024$party_change)

nc_28202_precinct_2024_long <- pivot_longer(nc_28202_precinct_2024, cols = c(party_voted_2020, party_voted_2024),
                                   names_to = "year",
                                   values_to = "party_affiliation")

