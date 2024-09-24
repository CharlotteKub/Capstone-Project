


###############################################
####### Prepping data 27915 for ABM ###########
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
#ncvoter_Statewide <- ncvoter_Statewide %>% filter(voter_status_desc == "ACTIVE")

ncvhis_Statewide <- read.delim("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/ncvhis_Statewide.txt")


### using 27915 zipcode in Charlotte

nc_27915 <- ncvoter_Statewide %>% filter(zip_code == 27915)

# smallest level of data for city 

# check if ncid is in dataset from 2020 

ncid_list_27915 <- nc_27915$ncid
ncvhis_Statewide_27915 <- ncvhis_Statewide %>% filter(ncid %in% ncid_list_27915)
ncvhis_Statewide_2016_27915 <- ncvhis_Statewide_27915 %>%  filter(election_lbl == "11/08/2016")
ncvhis_Statewide_2016_27915 <- left_join(ncvhis_Statewide_2016_27915, nc_27915, by="ncid")



### transforming 2020 set ncvhis_Statewide_2020_27915 to nc_27915
nc_27915 <- ncvhis_Statewide_2016_27915


table(nc_27915$party_cd)




####################################################################
#### 1. Preparing the addresses of zipcode for scraping coordinates
####################################################################

adress_27915 <- paste(nc_27915$res_street_address, nc_27915$res_city_desc,", NC")

adress_df_27915 <- data.frame(address = adress_27915)

nc_27915$full_adress <- adress_27915

#adress <- nc_27555$res_street_address

adress_27915 <- unique(adress_27915)
length(adress_27915)



###############################################
#### 2. Using google API: 
###############################################

register_google(key = "AIzaSyCBzo8BkD9dQ94fW4oKDkQAUWEdmragCaU")


geocoded_adress <- data.frame()


#Loop through each street name
for(street in adress_27915){
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

coords_adress_27915 <- na.omit(geocoded_adress)


###############################################
#### 3. merging registration data with coords
###############################################


nc_27915_small <- nc_27915 %>% dplyr::select(ncid, race_code, ethnic_code, voted_party_cd, party_cd, gender_code, birth_year, birth_state, full_adress, precinct_abbrv)

#### creating points for adresses:


coords_adress_27915 <- left_join(adress_df_27915, coords_adress_27915, by = c("address"))
coords_adress_27915 <- na.omit(coords_adress_27915)


save(coords_adress_27915, file = "coords_adress_27915.RData")


points_27915 <- st_as_sf(coords_adress_27915, coords = c("lon", "lat"), crs = 4269)

geo_nc_27915_points <- left_join(points_27915, nc_27915_small, by =c("address" = "full_adress"))
geo_nc_27915_points <- unique(geo_nc_27915_points)



###############################################
##### 4. getting NC shp file data #####
###############################################


nc_shapefile <- st_read("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/North_Carolina_State_and_County_Boundary_Polygons/North_Carolina_State_and_County_Boundary_Polygons.shp")

nc_johnston <- nc_shapefile %>% filter(FIPS == 101)

centerlines <- st_read("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/Centerlines/centerli.shp")

zipcodes <- st_read("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/zipcode shapefile/tl_2023_us_zcta520.shp")

zipcodes27915 <- zipcodes %>% filter(ZCTA5CE20 == 27915)



coords_27915 <- coords_adress_27915 %>% dplyr::select(lon, lat)
coords_27915$lon <- as.numeric(coords_27915$lon)
coords_27915$lat <- as.numeric(coords_27915$lat)



######## plotting the coords with agents

tmap_mode("view")

tm_shape(zipcodes27915) + 
  tm_polygons() +
  #tm_shape(centerlines) + 
  #tm_lines() + 
  tm_shape(geo_nc_27915_points) +
  tm_symbols(col = "party_cd", size = 0.01)


table(nc_27915$party_cd)



###############################################
### 5. creating additional variables
###############################################


# create a variable for non white population or races as percentage of actual population,
# mostly make them numeric to calculate statistics and Blau Index

nc_27915_small <- nc_27915_small %>% mutate(non_white_perc = sum(race_code != "W") / nrow(nc_27915_small))


nc_27915_small <- nc_27915_small %>% mutate(race_numeric = case_when(race_code == "W" ~1,
                                                                     race_code == "B" ~ 2,
                                                                     race_code == "I" ~ 3,
                                                                     race_code == "A" ~ 4,
                                                                     race_code == "O" | race_code == "U" | race_code == "M" ~ 5))




nc_27915_small <- nc_27915_small %>% mutate(party_numeric = case_when(voted_party_cd == "REP" ~ 1,
                                                                      voted_party_cd == "DEM" ~2,
                                                                      voted_party_cd == "LIB" ~3,
                                                                      voted_party_cd == "LIB" ~ 4,
                                                                      voted_party_cd == "GRE" ~ 5,
                                                                      voted_party_cd == "UNA" ~6))

##### age


nc_27915_small$birth_year <- as.numeric(nc_27915_small$birth_year)
nc_27915_small <- nc_27915_small %>% mutate(age = 2020-birth_year)


hist(nc_27915_small$age)


nc_27915_small <- nc_27915_small %>% mutate(age_binned = case_when(age > 13 & age < 20 ~"14-19 years",
                                                                   age > 19 & age < 36 ~"20-35 years",
                                                                   age > 35 & age < 51 ~"36-50 years",
                                                                   age > 50 & age < 70 ~"51-69 years",
                                                                   age > 69 ~"70 + years"))



######################################################
##### 6. creating distance and proximity matrices
######################################################

###### creating spatial data frame with coords 

geo_nc_27915 <- left_join(nc_27915_small, coords_adress_27915, by =c("full_adress" = "address"))
geo_nc_27915 <- unique(geo_nc_27915)

geo_nc_27915$lon <- as.numeric(geo_nc_27915$lon)
geo_nc_27915$lat <- as.numeric(geo_nc_27915$lat)


save(geo_nc_27915, file = "geo_nc_27915.RData")
save(nc_27915, file = "nc_27915.RData")

## coordinates() is a function from the sp package in R, which is used to specify which columns of a dataframe 
## should be used as the spatial coordinates. It then transforms the df into a spatial object




#geo_nc_27915 <- na.omit(geo_nc_27915)
#coordinates(geo_nc_27915) = c("lon","lat")



ncells_27915 <- length(geo_nc_27915)
distmat_27915 <- matrix(NA, nrow=ncells_27915, ncol=ncells_27915)
proxmat1_27915 <- matrix(NA, nrow=ncells_27915, ncol=ncells_27915)
proxmat2_27915 <- matrix(NA, nrow=ncells_27915, ncol=ncells_27915)
proxmat3_27915 <- matrix(NA, nrow=ncells_27915, ncol=ncells_27915)

# Next, we calculate the distance matrix, in meters.
# We start by creating an empty matrix. Then we fill it in, row by row.

for (i in 1:ncells_27915) {
  distmat_27915[i,] <- distVincentyEllipsoid(coordinates(geo_nc_27915)[i,], coordinates(geo_nc_27915)) 
}

# Inside the loop, the function distVincentyEllipsoid() is used to calculate the distance between the coordinates 
# of the current cell (coordinates(dat)[i,]) and the coordinates of all other cells in the city (coordinates(dat)).

load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/distmat_27915_2016.RData")


# We apply the distance decay function, so that the matrix expresses proximity instead of distance.

# By applying the exponential function to the negative distances (scaled by a factor of 10), 
# it converts distances into proximity values.

proxmat1_27915 <- exp(-distmat_27915/10) 
proxmat2_27915 <- exp(-distmat_27915/100)
proxmat3_27915 <- exp(-distmat_27915/1000)


# diag(proxmat1) <- exp(-52.140543316/10)   #######
# diag(proxmat2) <- exp(-52.140543316/100)  #######
# diag(proxmat3) <- exp(-52.140543316/1000) #######
#printMat(proxmat)

# Normalize rows:
#sum <- rowSums(proxmat)
for (i in 1:length(distmat_27915[1,])){
  #proxmat1[i,] <- proxmat1[i,] / sum[i]
  #proxmat2[i,] <- proxmat2[i,] / sum[i]
  #proxmat3[i,] <- proxmat3[i,] / sum[i]
  proxmat1_27915[i,] <- proxmat1_27915[i,] / sum(proxmat1_27915[i,])
  proxmat2_27915[i,] <- proxmat2_27915[i,] / sum(proxmat2_27915[i,])
  proxmat3_27915[i,] <- proxmat3_27915[i,] / sum(proxmat3_27915[i,])
}



# Function to plot matrix for quick visual inspection:
printMat <- function(x){
  return(
    image(x, col=heat.colors(10000), zlim=c(min(x, na.rm=T),max(x, na.rm=T)))
  )
}


save(distmat_27915, file= "distmat_27915.RData")

save(proxmat2_27915, file= "proxmat2_27915_2016.RData")

################################################################
### 7A. Calculating Average Number of Interaction Partners
################################################################

median(distmat_27915)


# Set the proximity threshold
proximity_threshold <- 300

# Calculate the number of neighbors for each individual
geo_nc_27915$num_neighbors <- apply(distmat_27915, 1, function(row) {
  sum(row < proximity_threshold & row > 0)
})


geo_nc_27915_df <- as.data.frame(geo_nc_27915)


neighbors_27915 <- geo_nc_27915_df %>% dplyr::select(ncid, voted_party_cd, num_neighbors)
neighbors_27915$location <- "Rural"

save(neighbors_27915, file = "neighbors_27915.RData")



##### calculate partisan exposure ####

library(FNN)  # For finding nearest neighbors

# Find nearest neighbors
k <- 50  # Number of nearest neighbors

coords_27915 <- coords_adress_27915 %>% dplyr::select(lon, lat)
# Extract coordinates
coords_27915

# Find nearest neighbors
knn_27915 <- get.knn(coords_27915, k)

#geo_nc_28203 <- st_as_sf(geo_nc_28203, coords = c("lon", "lat"), crs = 4269)

# Calculate partisan exposure
# Initialize exposure columns
geo_nc_27915$DEM_exposure <- 0
geo_nc_27915$REP_exposure <- 0
geo_nc_27915$UNA_exposure <- 0

# Calculate exposure based on neighbors
for (i in 1:nrow(geo_nc_27915)) {
  neighbors <- knn_27915$nn.index[i, ]
  neighbor_parties <- geo_nc_27915$voted_party_cd[neighbors]
  
  geo_nc_27915$DEM_exposure[i] <- mean(neighbor_parties == "DEM")
  geo_nc_27915$REP_exposure[i] <- mean(neighbor_parties == "REP")
  geo_nc_27915$UNA_exposure[i] <- mean(neighbor_parties == "UNA")
}




# Define a function to assign colors based on the highest exposure


geo_nc_27915$color <- "purple" # Default for non-partisans
geo_nc_27915$color[geo_nc_27915$DEM_exposure > geo_nc_27915$REP_exposure & geo_nc_27915$DEM_exposure > geo_nc_27915$UNA_exposure] <- "blue"
geo_nc_27915$color[geo_nc_27915$REP_exposure > geo_nc_27915$DEM_exposure & geo_nc_27915$REP_exposure > geo_nc_27915$UNA_exposure] <- "red"

geo_nc_27915$color <- factor(geo_nc_27915$color, levels = c("blue", "red", "purple"), labels = c("Dem", "Rep", "Una"))


# Assuming df has 'longitude' and 'latitude' columns for coordinates
ggplot(geo_nc_27915, aes(x = lon, y = lat, color = color)) +
  geom_point(alpha = 0.6) +
  theme_bw() +
  scale_color_manual(values = c(Dem = "blue", Rep = "red", Una = "purple"), name = "Partisan Exposure") +
  labs(title = "Local Partisan Exposure Map: Rural Zipcode, KNN = 50",
       x = "Longitude",
       y = "Latitude")+  
  theme(axis.text.x = element_blank(), 
  axis.text.y = element_blank()) 


ggsave("knn_partisan_exposure_27915.png", dpi = 95)


#################################################
################## Blau Index ###################
#################################################


knn_27915 <- get.knn(coords_27915, 50)

party <- geo_nc_27915$voted_party_cd  # Extract party affiliation

# Calculate Proportions
geo_nc_27915$blau_index <- NA  # Initialize a column for Blau Index

for (i in 1:nrow(geo_nc_27915)) {
  neighbors_indices <- knn_27915$nn.index[i, ]
  neighbors_parties <- party[neighbors_indices]
  
  party_proportions <- table(neighbors_parties) / 50
  party_proportions <- as.numeric(party_proportions)  # Convert to numeric
  
  # Step 3: Blau Index Calculation
  blau_index <- 1 - sum(party_proportions^2)
  geo_nc_27915$blau_index[i] <- blau_index
}



ggplot(geo_nc_27915, aes(x = lon, y = lat, color = blau_index)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "cornflowerblue", high = "red") +
  theme_bw() +
  labs(title = "Blau Index of Partisan Diversity: Rural Neighborhood",
       x = "Longitude",
       y = "Latitude",
       color = "Blau Index")+  
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank()) 


ggsave("blau_index_27915.png", dpi = 95)





####### calculating Blau Index for Race #######


race <- geo_nc_27915$race_code  # Extract party affiliation

# Calculate Proportions
geo_nc_27915$blau_index_race <- NA  # Initialize a column for Blau Index

for (i in 1:nrow(geo_nc_27915)) {
  neighbors_indices <- knn_27915$nn.index[i, ]
  neighbors_race <- race[neighbors_indices]
  
  race_proportions <- table(neighbors_race) / 50
  race_proportions <- as.numeric(race_proportions)  # Convert to numeric
  
  # Step 3: Blau Index Calculation
  blau_index_race <- 1 - sum(race_proportions^2)
  geo_nc_27915$blau_index_race[i] <- blau_index_race
}



ggplot(geo_nc_27915, aes(x = lon, y = lat, color = blau_index_race)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "cornflowerblue", high = "chartreuse3") +
  theme_bw() +
  labs(title = "Blau Index of Racial Diversity: Rural Neighborhood",
       x = "Longitude",
       y = "Latitude",
       color = "Blau Index")+  
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank()) 


ggsave("blau_index_race_27915.png", dpi = 95)

save(geo_nc_27915, file="geo_nc_27915.RData")

################################################################
### 7. Calculating Group Statistics
################################################################


### calculating Exposure for Different Partisanships 

## not sure how to implement it or what is meant by exposure, maybe just calculate Moran's I or another
## similarity index for race and party ? 


geo_nc_27915$index <- (1:436)


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





exposure <- calcExposurePartisanship(geo_nc_27915, proxmat1_27915)
names(exposure) <- c("party_cd", "index", "exposureIngroup", "exposureOutgroup")

save(exposure, file= "exposure_27915.RData") 


#######################################################
##### creating world like Flache et al.
#######################################################


##### 1. Define Grids and Calculate Span and Convert geographic coordinates to UTM


## CRS Transformation: 
# The coordinates are transformed from the geographic coordinate system (longitude, latitude) 
# to the UTM coordinate system, which uses meters.


coordinates(coords_27915) <- ~lon+lat

proj4string(coords_27915) <- CRS("+proj=longlat +datum=WGS84")

# Check the CRS
print(proj4string(coords_27915))



coords_utm_27915_data <- spTransform(coords_27915, CRS("+proj=utm +zone=17 +datum=WGS84"))
coords_utm_27915 <- coordinates(coords_utm_27915_data)

# Check the transformed coordinates
print(head(coordinates(coords_utm_27915)))

# Step 2: Define the extent and create raster
extent_raster_27915 <- extent(min(coords_utm_27915[,1]), max(coords_utm_27915[,1]), min(coords_utm_27915[,2]), max(coords_utm_27915[,2]))


# Calculate the number of rows and columns and number of Cells: 
# The span is divided by 500m to get the number of 100m cells required to cover the area, 
# with adjustments for any remainder to ensure all points are covered.
ncols_27915 <- ceiling((extent_raster_27915@xmax - extent_raster_27915@xmin) / 200)
nrows_27915 <- ceiling((extent_raster_27915@ymax - extent_raster_27915@ymin) / 200)

# Create the raster
raster_200m_27915 <- raster(nrows = nrows_27915, ncols = ncols_27915, crs = CRS("+proj=utm +zone=17 +datum=WGS84"))

# Set the extent of the raster
extent(raster_200m_27915) <- extent_raster_27915

# Step 3: Rasterize each attribute

raster_race_27915 <- rasterize(coords_utm_27915, raster_200m_27915, geo_nc_27915$race_numeric, fun = mean)
raster_party_27915 <- rasterize(coords_utm_27915, raster_200m_27915, geo_nc_27915$party_numeric, fun = mean)


# Step 4: Combine rasters into a stack
raster_stack_27915 <- stack(raster_race_27915, raster_party_27915)
names(raster_stack_27915) <- c("Distribution of Race per grid", "Distribution of Parties per grid")

plot_raster_stack_27915 <- plot(raster_stack_27915)

ggsave("plot_raster_stack_27915.png")



# Step 5: Convert raster to SpatialPolygonsDataFrame
grid_polygons_27915 <- rasterToPolygons(raster_stack_27915)
grid_polygons_27915@data$ID <- 1:nrow(grid_polygons_27915)

# Step 6: Assign agents to grid cells
overlay_27915 <- over(coords_utm_27915_data, grid_polygons_27915)
geo_nc_27915$grid_cell_id <- overlay_27915$ID


overlay_27915 <- na.omit(overlay_27915)




# View the updated data with grid cell IDs
head(geo_nc_27915)
table(geo_nc_27915$grid_cell_id)



####### Blau’s Index of Heterogeneity: ###########

# Formula: (1- square of proportion) for each cell to get the Blau Index for each cell 

geo_nc_27915_df <-  as.data.frame(geo_nc_27915)

# Calculate the proportion of each race within each grid cell
race_proportions_27915 <- geo_nc_27915_df %>%
  group_by(grid_cell_id, race_code) %>%
  summarise(count = n()) %>%
  mutate(race_proportion = count / sum(count)) %>%
  mutate(race_proportion_sq = race_proportion^2) %>%
  mutate(Blau_race = 1- sum(race_proportion_sq)) %>%
  ungroup()


# Calculate the proportion of each party within each grid cell
party_proportions_27915 <- geo_nc_27915_df %>%
  group_by(grid_cell_id, party_cd) %>%
  summarise(count = n()) %>%
  mutate(party_proportion = count / sum(count)) %>%
  mutate(party_proportion_sq = party_proportion^2) %>%
  mutate(Blau_party = 1- sum(party_proportion_sq)) %>%
  ungroup()


race_proportions_27915$race_code <- NULL
race_proportions_27915$race_proportion <- NULL
race_proportions_27915$race_proportion_sq <- NULL
race_proportions_27915$count <- NULL


party_proportions_27915$party_cd <- NULL
party_proportions_27915$party_proportion <- NULL
party_proportions_27915$party_proportion_sq <- NULL
party_proportions_27915$count <- NULL

proportions_27915 <- left_join(race_proportions_27915, party_proportions_27915, by = "grid_cell_id")
proportions_27915 <- unique(proportions_27915)

geo_nc_27915_blau <- left_join(geo_nc_27915_df, proportions_27915, by = "grid_cell_id")



######### rasterizing the blau indices for race and party per cell 

raster_blau_race_27915 <- rasterize(coords_utm_27915, raster_200m_27915, geo_nc_27915_blau$Blau_race, fun = mean)
raster_blau_party_27915 <- rasterize(coords_utm_27915, raster_200m_27915, geo_nc_27915_blau$Blau_party, fun = mean)

raster_blau_stack_27915 <- stack(raster_blau_race_27915, raster_blau_party_27915)
names(raster_blau_stack_27915) <- c("Blau Index Race", "Blau Index Party")

plot_raster_blau_stack_27915 <- plot(raster_blau_stack_27915)

ggsave("plot_raster_blau_stack_27915.pdf", plot = plot_raster_blau_stack_27915)

save(geo_nc_27915_blau, file="geo_nc_27915_blau.RData")
save(geo_nc_27915_df, file="geo_nc_27915_df.RData")
save(geo_nc_27915, file="geo_nc_27915.RData")




load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/distmat_27915.RData")
load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/geo_nc_27915_blau.RData")
load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/geo_nc_27915_df.RData")
load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/geo_nc_27915.RData")
load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/geo_nc_27915_blau_abm.RData")



# leaving BLAU index and EXPOSURE calculations out here as they are being calculated in the function 
# just using variables created before 



################################################################
### 8. Finding voters in 2020
################################################################


# getting list of 2024 voters ncid to check which ones exist in the historical data (but maybe taking 2020)
# as starting point for ABM cause then I have enough previous records for vote_intensity based on previous
# elections and enough years for the ABM to run 



nc_27915_2024 <- ncvoter_Statewide %>% filter(zip_code == 27915)


ncid_list_27915 <- nc_27915$ncid
ncvhis_Statewide_27915 <- ncvhis_Statewide %>% filter(ncid %in% ncid_list_27915)

geo_nc_27915_abm <- ncvhis_Statewide_27915 %>% filter(election_lbl < "2022-11-08")

table(geo_nc_27915_abm$voted_party_cd)


geo_nc_27915_abm <- geo_nc_27915_abm %>% 
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
geo_nc_27915_abm <- geo_nc_27915_abm %>%
  group_by(ncid) %>%
  mutate(stubbornness = rle(as.character(voted_party_cd))$lengths[1]) %>%
  mutate(st_stubbornness = stubbornness/length(ncid))


geo_nc_27915_abm <- geo_nc_27915_abm %>%
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

geo_nc_27915_abm <- geo_nc_27915_abm %>%
  rowwise() %>%
  mutate(
    DEM_prob = ifelse(voted_party_cd == "DEM", 1, DEM_prob),
    UNA_prob = ifelse(voted_party_cd == "UNA", 1, UNA_prob),
    REP_prob = ifelse(voted_party_cd == "REP", 1, REP_prob),
    LIB_prob = ifelse(voted_party_cd == "LIB", 1, LIB_prob),
    GRE_prob = ifelse(voted_party_cd == "GRE", 1, GRE_prob)
  ) %>%
  ungroup()



geo_nc_27915_small <- geo_nc_27915 %>% dplyr::select(party_cd, ncid, num_neighbors , age_binned, age, race_code, gender_code)

geo_nc_27915_abm_small <- left_join(geo_nc_27915_abm, geo_nc_27915_small, by = "ncid")

geo_nc_27915_abm_small <- geo_nc_27915_abm_small %>% filter(election_lbl == "11/08/2016")
table(geo_nc_27915_abm_small$election_lbl)

save(geo_nc_27915_abm_small, file = "geo_nc_27915_abm_small.RData")










# prepare data for regression of vote behavior

### find out tomorrow how markovchain is working and if it gets that the transition happen 
# between different voters

library(markovchain)

transitions_27915 <- geo_nc_27915_blau_abm %>%
  group_by(ncid) %>%
  arrange(election_lbl) %>%
  summarise(transitions = list(voted_party_cd)) %>%
  pull(transitions) %>%
  unlist()



mc_transition_27915 <- new("markovchain", states = colnames(transition_matrix), transitionMatrix = transition_matrix)


mc <- markovchainFit(data = geo_nc_27915_abm$voted_party_cd[[1]])

mc$estimate
plot(mc$estimate)



###################################################
############### Vote Switches #####################
###################################################

###### merging data from 2020 to 2024 based on identifier ######


nc_27915_2024 <- geo_nc_27915_blau_abm

nc_27915_2024_2016 <- geo_nc_27915_blau_abm_2016
nc_27915_2024_2016 <- nc_27915_2024_2016 %>% rename("party_voted_2016" = voted_party_cd)
nc_27915_2024_2016 <- nc_27915_2024_2016 %>% rename("party_voted_2024" = party_cd)


nc_27915_2024_2016 <- nc_27915_2024_2016 %>% mutate(party_change = case_when(party_voted_2016 == party_voted_2024 ~ 0,
                                                                             party_voted_2016 != party_voted_2024 ~ 1))

table(nc_27915_2024_2016$party_change)



nc_27915_2024 <- nc_27915_2024 %>% rename("party_voted_2020" = voted_party_cd)

nc_27915_2024 <- nc_27915_2024 %>% rename("party_voted_2024" = party_cd)



###### creating variable for voter change ######

nc_27915_2024 <- nc_27915_2024 %>% mutate(party_change = case_when(party_voted_2020 == party_voted_2024 ~ 0,
                                                                   party_voted_2020 != party_voted_2024 ~ 1))


table(nc_27915_2024$party_change)

nc_27915_2024_long <- pivot_longer(nc_27915_2024, cols = c(party_voted_2020, party_voted_2024),
                                   names_to = "year",
                                   values_to = "party_affiliation")



table(nc_27915_2024$party_voted_2020)
table(nc_27915_2024$party_voted_2024)




#####################################################
### voter change for all elections in df
#####################################################



# get date time into correct format using lubridate 

geo_nc_27915_blau_abm_16$election_lbl <- lubridate::mdy(geo_nc_27915_blau_abm_16$election_lbl)

geo_nc_27915_blau_abm_16 <- geo_nc_27915_blau_abm

geo_nc_27915_blau_abm_16 <- geo_nc_27915_blau_abm_16 %>% arrange(ncid, election_lbl)


geo_nc_27915_blau_abm_16 <- geo_nc_27915_blau_abm_16 %>%
  group_by(ncid) %>%
  mutate(party_change = ifelse(voted_party_cd != lag(voted_party_cd), 1, 0)) %>%
  ungroup()


geo_nc_27915_blau_abm_16_na <- na.omit(geo_nc_27915_blau_abm_16)
table(geo_nc_27915_blau_abm_16_na$party_change)

Y <- geo_nc_27915_blau_abm_16_na$party_change
x <-geo_nc_27915_blau_abm_16_na %>% dplyr::select(-party_change)
X <- as.matrix(x)



library(glmnet)


library(caret)
library(randomForest)
library(rpart) 
library(vip)

glmnet::cv.glmnet(X, Y, alpha = 1)
glmnet(X, Y, alpha = 1)

??rpart()
tree <- rpart(Y~., data=x)
plot(tree$variable.importance)

vip(tree)


################## Summary Statistics 


geo_nc_27915_blau$race_code <- as.factor(geo_nc_27915_blau$race_code)
geo_nc_27915_blau$gender_code <- as.factor(geo_nc_27915_blau$gender_code)
geo_nc_27915_blau$party_cd <- as.factor(geo_nc_27915_blau$party_cd)

### get the summary statistics for each zipcode: Age, Gender, Race, Party distribution, Density and Size

vtable::sumtable(geo_nc_27915_blau,vars = c("gender_code", "race_code", "party_cd", "age"),
                 out = "latex")



