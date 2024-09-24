
############################-------------------------
# Voter Registration Data cleaned 
###########################--------------------------



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


######### looking at zipcode 27814 with population of 1,676	######

nc_27555 <- ncvoter_Statewide %>% filter(zip_code == 27555)
nc_28571 <- ncvoter_Statewide %>% filter(zip_code == 28571)



adress1 <- paste(nc_27555$res_street_address, nc_27555$res_city_desc)
adress <- unique(adress1)
nc_27555$full_adress <- adress1


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


save(coords_adress, file = "coords_adress.RData")

load("coords_adress.RData")

############## merging registrationd data with coords ####

library(dplyr)

nc_27555 <- as.data.frame(nc_27555)
nc_27555$full_adress <- adress1

nc_27555_small <- nc_27555 %>% dplyr::select(ncid, race_code, ethnic_code, party_cd, gender_code, birth_year, birth_state, full_adress)





#### creating points for adresses:
coords_adress <- na.omit(coords_adress)

adress_dataframe <- data.frame(address = adress1)

coords_adress_all <- left_join(adress_dataframe, coords_adress, by = c("address"))

points <- sf::st_as_sf(coords_adress_all, coords = c("longitude", "latitude"), crs = 4269)

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


coordinates(geo_nc_27555) = c("longitude","latitude")



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


# We apply the distance decay function, so that the matrix expresses
# proximity instead of distance.
# NOTE: For agents who live in the same cell, we want to avoid to assume their
# distance is 0, because their proximity would be maximal. Instead, we
# assume that their proximity is the average distance between all points
# in a square sized 100*100 meters. That would be about 52.14m. Therefore,
# we assume that the distance between a cell and itself is about 52.14m.
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


### calculating exposure for different partisanships 

## not sure how to implement it or what is meant by exposure, maybe just calculate Moran's I or another
## similarity index for race and party ? 

table(nc_27555_small$race_code)


geo_nc_27555$index <- (1:228)

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
    geo_nc_2755_test[[paste0(party, "_prob")]] <- sigmoid(0)
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
      geo_nc_2755_test[[paste0(party, "_prob")]][i] <- sigmoid(0.5)
    }
  } else {
    geo_nc_2755_test[[paste0(current_party, "_prob")]][i] <- sigmoid(1)
  }
}




#################################-------------------------
######### Vote Preference Update #############
#################################-------------------------



################# Version 1 ######################

updateVoteProbability <- function(agents, proximity_matrix, alpha, beta, total_attributes, interaction_prob, epochs) {
  
  # create dataframe for interactions 
  interactions <- data.frame(epoch = integer(),agent = integer(), party = character(), vote_prob = numeric())
  
  num_agents <- nrow(agents)
  for (epoch in 1:epochs) {
  for (ego in 1:num_agents) {
    for (alter in 1:num_agents) {
      
      # Check proximity before interaction and add randomness
      if (proximity_matrix[ego, alter] > 0.0000001 && runif(1) < interaction_prob) { 
        
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
            
            # Update the voting probability
            # Get the party columns to update
            ego_party_col <- paste0(agents$party_cd[ego], "_prob")
            alter_party_col <- paste0(agents$party_cd[alter], "_prob")
            
            # Update the voting probability for ego
            agents[[ego_party_col]][ego] <- sigmoid(agents[[ego_party_col]][ego] + delta_P)
            agents[[alter_party_col]][ego] <- sigmoid(agents[[alter_party_col]][ego] - delta_P)
            
            # Update the voting probability for alter
            agents[[ego_party_col]][alter] <- sigmoid(agents[[ego_party_col]][alter] - delta_P)
            agents[[alter_party_col]][alter] <- sigmoid(agents[[alter_party_col]][alter] + delta_P)
            
            
          } else {
            # Same party: increase probability
            delta_P <- beta * influence_weight
            
            # Update the voting probability
            # Get the party columns to update
            ego_party_col <- paste0(agents$party_cd[ego], "_prob")
            alter_party_col <- paste0(agents$party_cd[alter], "_prob")
            
            # Update the voting probability
            agents[[ego_party_col]][ego] <- sigmoid(agents[[ego_party_col]][ego] + delta_P)
            # Update the voting probability for alter
            agents[[ego_party_col]][alter] <- sigmoid(agents[[ego_party_col]][alter] + delta_P)
          }
        }
      }
    }
  }
        # Log voting probabilities after each epoch
        for (agent in 1:num_agents) {
          party_prob_cols <- grep("_prob$", colnames(agents), value = TRUE)
          for (party_col in party_prob_cols) {
            interactions <- rbind(interactions, data.frame(
              epoch = epoch,
              agent = agent,
              party = gsub("_prob", "", party_col),
              vote_prob = agents[[party_col]][agent]
           ))
        }
      }
    }
    return(interactions)
}


interactions <- updateVoteProbability(geo_nc_2755_test, proxmat1, 0.1, 0.05, 3, 0.5, 100)



ggplot(interactions, aes(x = epoch, y = vote_prob, color = agent, group = agent)) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs",
       x = "Epoch",
       y = "Voting Probability") +
  facet_wrap(~party)+
  theme_minimal()

save(geo_nc_2755_test, file = "geo_nc_2755_test.RData")




########################################------------------------------------


load("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/geo_nc_2755_test.RData")


################# Version 2 ######################

# we want to actually limit the number of interactions per day 

### TEST with plotting interactions of agents
#### randomizing interaction agents by proximity 

updateVoteProbability2 <- function(agents, proximity_matrix, alpha, beta, total_attributes, epochs) {
  
  num_agents <- nrow(agents)
  
  # Create a dataframe for interactions and logging
  interactions <- data.frame(epoch = integer(), agent = integer(), alter = integer(), party = character(), vote_prob = numeric(), delta_P = numeric())
  interactions_epoch <- data.frame(epoch = integer(), agent = integer(), party = character(), vote_prob = numeric())
  
  for (epoch in 1:epochs) {
    for (ego in 1:num_agents) {
      for (alter in 1:num_agents) {
        if (ego != alter) {  # Ensure ego does not interact with itself
          
          # Interaction probability based on proximity
          interaction_prob <- proximity_matrix[ego, alter]
          
          # Random interaction based on the calculated probability
          if (runif(1) < interaction_prob) {
            # Calculate Kronecker delta for party difference
            party_diff <- ifelse(agents$party_cd[ego] == agents$party_cd[alter], 0, 1)
            
            if (agents$party_cd[alter] == "UNA") {
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
                
                ego_party_col <- paste0(agents$party_cd[ego], "_prob")
                alter_party_col <- paste0(agents$party_cd[alter], "_prob")
                
                # Update the voting probability for ego
                agents[[ego_party_col]][ego] <- sigmoid(agents[[ego_party_col]][ego] + delta_P)
                agents[[alter_party_col]][ego] <- sigmoid(agents[[alter_party_col]][ego] - delta_P)
                
                # Update the voting probability for alter
                agents[[ego_party_col]][alter] <- sigmoid(agents[[ego_party_col]][alter] - delta_P)
                agents[[alter_party_col]][alter] <- sigmoid(agents[[alter_party_col]][alter] + delta_P)
                
              } else {
                # Same party: increase probability
                delta_P <- beta * influence_weight
                
                ego_party_col <- paste0(agents$party_cd[ego], "_prob")
                alter_party_col <- paste0(agents$party_cd[alter], "_prob")
                
                # Update the voting probability for ego
                agents[[ego_party_col]][ego] <- sigmoid(agents[[ego_party_col]][ego] + delta_P)
                # Update the voting probability for alter
                agents[[alter_party_col]][alter] <- sigmoid(agents[[alter_party_col]][alter] + delta_P)
              }
            }
            
            # Record this interaction
            interactions <- rbind(interactions, data.frame(
              epoch = epoch,
              agent = ego,
              alter = alter,
              party = agents$party_cd[ego],
              vote_prob = agents[[paste0(agents$party_cd[ego], "_prob")]][ego],
              delta_P = delta_P
            ))
          }
        }
      }
    }
    
    # Log voting probabilities after each epoch
    for (agent in 1:num_agents) {
      party_prob_cols <- grep("_prob$", colnames(agents), value = TRUE)
      for (party_col in party_prob_cols) {
        interactions_epoch <- rbind(interactions_epoch, data.frame(
          epoch = epoch,
          agent = agent,
          party = gsub("_prob", "", party_col),
          vote_prob = agents[[party_col]][agent]
        ))
      }
    }
  }
  
  return(list(interactions = interactions, interactions_epoch = interactions_epoch))
}




################## Example usage:

# Assuming geo_nc_2755_test is my data frame and proximity_matrix is defined

total_attributes <- 3 # Number of sociodemographic attributes considered
alpha <- 0.01 # Base influence factor for different parties
beta <- 0.01 # Base influence factor for same party
epochs <- 2 # Number of epochs

# Update probabilities based on proximity and interactions over multiple epochs
interactions <- updateVoteProbability2(geo_nc_2755_test, proxmat1, alpha, beta, total_attributes, epochs)

################# Plot the results

library(ggplot2)
ggplot(interactions, aes(x = epoch, y = vote_prob, color = agent, group = agent)) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs",
       x = "Epoch",
       y = "Voting Probability") +
  facet_wrap(~party)+
  theme_minimal()


interactions_na <- na.omit(interactions)
max(interactions_na$vote_prob)



##########################################-----------------------------------------------


################# Version 3 ######################

# Update voting probability function with proximity-based interactions
# interaction between agents only faciliated when within proximity threshold
# number of interactions is randomized between 0:5 per agent 


updateVoteProbability_3 <- function(agents, proximity_matrix, alpha, beta, total_attributes, epochs, proximity_threshold) {
  
  num_agents <- nrow(agents)
  
  # Create a dataframe for interactions and logging
  interactions <- data.frame(epoch = integer(), agent = integer(), alter = integer(), party = character(), vote_prob = numeric(), delta_P = numeric())
  interactions_epoch <- data.frame(epoch = integer(), agent = integer(), party = character(), vote_prob = numeric())
  
  for (epoch in 1:epochs) {
    for (ego in 1:num_agents) {
      # Randomly assign the number of interactions for this agent for this day
      num_interactions <- sample(0:5, 1)
      for (interaction in 1:num_interactions) {
        # Filter agents within the proximity threshold
        potential_alters <- which(proximity_matrix[ego, ] > proximity_threshold & proximity_matrix[ego, ] < 1)
        
        if (length(potential_alters) > 0) {
          alter <- sample(potential_alters, 1)  # Randomly select one of the agents within proximity
          
          if (ego != alter) {  # Ensure ego does not interact with itself
            # Interaction probability based on proximity
            interaction_prob <- proximity_matrix[ego, alter]
            
            # Calculate Kronecker delta for party difference
            party_diff <- ifelse(agents$party_cd[ego] == agents$party_cd[alter], 0, 1)
            
            if (agents$party_cd[alter] == "UNA") {
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
                
                ego_party_col <- paste0(agents$party_cd[ego], "_prob")
                alter_party_col <- paste0(agents$party_cd[alter], "_prob")
                
                # Update the voting probability for ego
                agents[[ego_party_col]][ego] <- sigmoid(agents[[ego_party_col]][ego] + delta_P)
                agents[[alter_party_col]][ego] <- sigmoid(agents[[alter_party_col]][ego] - delta_P)
                
                # Update the voting probability for alter
                agents[[ego_party_col]][alter] <- sigmoid(agents[[ego_party_col]][alter] - delta_P)
                agents[[alter_party_col]][alter] <- sigmoid(agents[[alter_party_col]][alter] + delta_P)
                
              } else {
                # Same party: increase probability
                delta_P <- beta * influence_weight
                
                ego_party_col <- paste0(agents$party_cd[ego], "_prob")
                alter_party_col <- paste0(agents$party_cd[alter], "_prob")
                
                # Update the voting probability for ego
                agents[[ego_party_col]][ego] <- sigmoid(agents[[ego_party_col]][ego] + delta_P)
                # Update the voting probability for alter
                agents[[alter_party_col]][alter] <- sigmoid(agents[[alter_party_col]][alter] + delta_P)
              }
            }
            
            # Record this interaction
            interactions <- rbind(interactions, data.frame(
              epoch = epoch,
              agent = ego,
              alter = alter,
              party = agents$party_cd[ego],
              vote_prob = agents[[paste0(agents$party_cd[ego], "_prob")]][ego],
              delta_P = delta_P
            ))
          }
        }
      }
    }
    
    # Log voting probabilities after each epoch
    for (agent in 1:num_agents) {
      party_prob_cols <- grep("_prob$", colnames(agents), value = TRUE)
      for (party_col in party_prob_cols) {
        interactions_epoch <- rbind(interactions_epoch, data.frame(
          epoch = epoch,
          agent = agent,
          party = gsub("_prob", "", party_col),
          vote_prob = agents[[party_col]][agent]
        ))
      }
    }
  }
  
  return(list(interactions = interactions, interactions_epoch = interactions_epoch))
}


proximity_threshold <- 0.000000001

interactions_random <-updateVoteProbability_3(geo_nc_2755_test, proxmat1, alpha, beta, total_attributes, epochs = 300, proximity_threshold)

interactions_random1 <- interactions_random[[2]]
interactions_random2 <- interactions_random[[1]]



interactions_random2_agent1 <- interactions_random2 %>% filter(agent == 1)
interactions_random1_agent1 <- interactions_random1 %>% filter(agent == 1)



# Plot for individual vote probability changes over time

ggplot(interactions_random1, aes(x = epoch, y = vote_prob, color = agent, group = factor(agent))) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs",
       x = "Epoch",
       y = "Voting Probability") +
  facet_wrap(~party)+
  ylim(0,1)+
  theme_minimal()


ggplot(interactions_random1_agent1, aes(x = epoch, y = vote_prob, color = party, group = party)) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs for Agent 1",
       x = "Epoch",
       y = "Voting Probability") +
  theme_minimal()


##################################################
################# Version 4 ######################
##################################################


#### Bounded Confidence Model ###
# creating homophily threshold by either sharing party affiliation or having 2/3 of shared attributes

updateVoteProbability_4 <- function(agents, proximity_matrix, alpha, beta, total_attributes, epochs, proximity_threshold) {
  
  num_agents <- nrow(agents)
  
  # Create a dataframe for interactions and logging
  interactions <- data.frame(epoch = integer(), agent = integer(), alter = integer(), party = character(), vote_prob = numeric(), delta_P = numeric())
  interactions_epoch <- data.frame(epoch = integer(), agent = integer(), party = character(), vote_prob = numeric())
  
  for (epoch in 1:epochs) {
    for (ego in 1:num_agents) {
      # Randomly assign the number of interactions for this agent for this day
      num_interactions <- sample(0:5, 1)
      for (interaction in 1:num_interactions) {
        # Filter agents within the proximity threshold
        potential_alters <- which(proximity_matrix[ego, ] > proximity_threshold & proximity_matrix[ego, ] < 1)
        
        if (length(potential_alters) > 0) {
          alter <- sample(potential_alters, 1)  # Randomly select one of the agents within proximity
          
          if (ego != alter) {  # Ensure ego does not interact with itself
            # Interaction probability based on proximity
            interaction_prob <- proximity_matrix[ego, alter]
            
            # Calculate Kronecker delta for party difference
            party_diff <- ifelse(agents$party_cd[ego] == agents$party_cd[alter], 0, 1)
            
            if (agents$party_cd[alter] == "UNA") {
              delta_P <- 0
            } else {
              
              # Calculate the change in probability based on party difference
              if (party_diff == 1) {
                
                # Calculate the number of shared sociodemographic attributes
                shared_attributes <- sum(
                  agents$age_binned[ego] == agents$age_binned[alter], 
                  agents$race_code[ego] == agents$race_code[alter], 
                  agents$gender_code[ego] == agents$gender_code[alter]
                )
                
                influence_weight <- shared_attributes / total_attributes
                
                if (influence_weight > 0.5) {
                  # Different party: decrease probability
                  delta_P <- -alpha * influence_weight 
                  
                  ego_party_col <- paste0(agents$party_cd[ego], "_prob")
                  alter_party_col <- paste0(agents$party_cd[alter], "_prob")
                  
                  # Update the voting probability for ego
                  agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                  agents[[alter_party_col]][ego] <- agents[[alter_party_col]][ego] - delta_P
                  
                  # Update the voting probability for alter
                  agents[[ego_party_col]][alter] <- agents[[ego_party_col]][alter] - delta_P
                  agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P
                  
                } else {
                  delta_P <- 0
                }
                
              } else {
                # Same party: increase probability
                 
                 # Calculate the number of shared sociodemographic attributes
                 shared_attributes <- sum(
                   agents$age_binned[ego] == agents$age_binned[alter], 
                   agents$race_code[ego] == agents$race_code[alter], 
                   agents$gender_code[ego] == agents$gender_code[alter]
                 )
                 
                 influence_weight <- shared_attributes / total_attributes
                 
                  delta_P <- beta * influence_weight 
                
                ego_party_col <- paste0(agents$party_cd[ego], "_prob")
                alter_party_col <- paste0(agents$party_cd[alter], "_prob")
                
                # Update the voting probability for ego
                agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                # Update the voting probability for alter
                agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P
              }
            }
            
            # Record this interaction
            interactions <- rbind(interactions, data.frame(
              epoch = epoch,
              agent = ego,
              alter = alter,
              party = agents$party_cd[ego],
              vote_prob = agents[[paste0(agents$party_cd[ego], "_prob")]][ego],
              delta_P = delta_P
            ))
          }
        }
      }
    }
    
    # Log voting probabilities after each epoch
    for (agent in 1:num_agents) {
      party_prob_cols <- grep("_prob$", colnames(agents), value = TRUE)
      for (party_col in party_prob_cols) {
        interactions_epoch <- rbind(interactions_epoch, data.frame(
          epoch = epoch,
          agent = agent,
          party = gsub("_prob", "", party_col),
          vote_prob = agents[[party_col]][agent]
        ))
      }
    }
  }
  
  return(list(interactions = interactions, interactions_epoch = interactions_epoch))
}




proximity_threshold <- 0.000000001
alpha <- 1e-13
beta <-  1e-13
scaling_factor = 1e-6

interactions_bounded_confidence <-updateVoteProbability_4(geo_nc_2755_test, proxmat1, alpha = 0.001, beta= 0.001, total_attributes, epochs = 50, proximity_threshold)


interactions_epoch <- interactions_bounded_confidence$interactions_epoch
interactions <- interactions_bounded_confidence$interactions


interactions_conf2_agent1 <- interactions %>% filter(agent == 1)
interactions_conf1_agent1 <- interactions_epoch %>% filter(agent == 2)


###################################################################
## Visualizing the interactions ##
###################################################################


# Plot for individual vote probability changes over time

ggplot(interactions_epoch, aes(x = epoch, y = vote_prob, color = agent, group = factor(agent))) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs",
       x = "Epoch",
       y = "Voting Probability") +
  facet_wrap(~party)+
  ylim(0,1)+
  theme_minimal()


ggplot(interactions_conf1_agent1, aes(x = epoch, y = vote_prob, color = party, group = party)) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs for Agent 2",
       x = "Epoch",
       y = "Voting Probability") +
  theme_minimal()



######## more visualization:




plot_data <- interactions_epoch %>%
  tidyr::pivot_wider(names_from = party, values_from = vote_prob) 

plot_data_all <- interactions_epoch %>%
  tidyr::pivot_wider(names_from = party, values_from = vote_prob) %>% 
  tidyr::pivot_longer(cols = c(DEM, REP, UNA, NLB), names_to = "party", values_to = "vote_prob")



# Plot vote probabilities over time for each party
ggplot(plot_data, aes(x = epoch)) +
  geom_smooth(aes(y = DEM, color = "DEM")) +
  geom_smooth(aes(y = REP, color = "REP")) +
  geom_smooth(aes(y = UNA, color = "UNA")) +
  geom_smooth(aes(y = NLB, color = "NLB")) +
  labs(title = "Vote Probabilities Over Time",
       x = "Epoch",
       y = "Vote Probability",
       color = "Party") +
  theme_minimal()



# Aggregate the number of interactions by agent and epoch
interaction_counts <- interactions %>%
  group_by(epoch, agent) %>%
  summarise(count = n(), .groups = 'drop')

interaction_counts_small <- interaction_counts %>% filter(agent < 100)

# Plot the number of interactions over time
ggplot(interaction_counts, aes(x = epoch, y = count, group = agent, color = factor(agent))) +
  geom_smooth(se = FALSE, linewidth = .5) +
  labs(title = "Number of Interactions Over Time",
       x = "Epoch",
       y = "Number of Interactions",
       color = "Agent") +
  theme_minimal() +
  theme(legend.position = "none")



# Plot for overall vote probability distribution at the final epoch

final_epoch <- max(plot_data$epoch)
middle_epoch <- round(median(plot_data$epoch))
first_epoch <- min(plot_data$epoch)

plot_start <- interactions_epoch %>%
  filter(epoch == first_epoch) %>%
  ggplot(aes(x = vote_prob, fill = party)) +
  geom_histogram(position = 'identity', alpha = 0.6, bins = 30) +
  labs(title = 'Vote Probability Distribution at First Epoch', x = 'Vote Probability', y = 'Count', fill = 'Party') +
  theme_minimal()

plot_middle <- interactions_epoch %>%
  filter(epoch == middle_epoch) %>%
  ggplot(aes(x = vote_prob, fill = party)) +
  geom_histogram(position = 'identity', alpha = 0.6, bins = 30) +
  labs(title = 'Vote Probability Distribution at Middle Epoch', x = 'Vote Probability', y = 'Count', fill = 'Party') +
  theme_minimal()

plot_end <- interactions_epoch %>%
  filter(epoch == final_epoch) %>%
  ggplot(aes(x = vote_prob, fill = party)) +
  geom_histogram(position = 'identity', alpha = 0.6, bins = 30) +
  labs(title = 'Vote Probability Distribution at Final Epoch', x = 'Vote Probability', y = 'Count', fill = 'Party') +
  theme_minimal()

library(patchwork)


patchwork::wrap_plots(ncol = 2, plot_start, plot_middle, plot_end)





###############################################
#### Version 4 but with 2020 data ######
###############################################



## problem: i need to make a new proximity matrix as I have slightly different data which is why my code doesnt run

## cannot use all of the 2020 voter data as the demographic data only is available at the 2024 dataset which means I need
## to have the ncid in 2024 to connect it for the demographics

## then create the dataset I have now with the vote probabilities, get coordinates for them and create proximity matrices

## then run the code



voter_data_2020_abm <- voter_data_2020_abm %>%
  rowwise() %>%
  mutate(
    DEM_prob = ifelse(voted_party_cd == "DEM", 1, DEM_prob),
    UNA_prob = ifelse(voted_party_cd == "UNA", 1, UNA_prob),
    REP_prob = ifelse(voted_party_cd == "REP", 1, REP_prob)
  ) %>%
  ungroup()


##### create a new proximity matrix based on the 2020 data 

coordinates(voter_data_2020_abm) = c("longitude","latitude")


########## creating distance and proximity matrices ##########

ncells_2020 <- length(voter_data_2020_abm)
distmat_2020 <- matrix(NA, nrow=ncells_2020, ncol=ncells_2020)
proxmat1_2020 <- matrix(NA, nrow=ncells_2020, ncol=ncells_2020)
proxmat2_2020 <- matrix(NA, nrow=ncells_2020, ncol=ncells_2020)
proxmat3_2020 <- matrix(NA, nrow=ncells_2020, ncol=ncells_2020)


for (i in 1:ncells_2020) {
  distmat_2020[i,] <- distVincentyEllipsoid(coordinates(voter_data_2020_abm)[i,], coordinates(voter_data_2020_abm)) 
}

proxmat1_2020 <- exp(-distmat_2020/10) 
proxmat2_2020 <- exp(-distmat_2020/100)
proxmat3_2020 <- exp(-distmat_2020/1000)



# Normalize rows:
#sum <- rowSums(proxmat)
for (i in 1:length(distmat_2020[1,])){
  #proxmat1[i,] <- proxmat1[i,] / sum[i]
  #proxmat2[i,] <- proxmat2[i,] / sum[i]
  #proxmat3[i,] <- proxmat3[i,] / sum[i]
  proxmat1_2020[i,] <- proxmat1_2020[i,] / sum(proxmat1_2020[i,])
  proxmat2_2020[i,] <- proxmat2_2020[i,] / sum(proxmat2_2020[i,])
  proxmat3_2020[i,] <- proxmat3_2020[i,] / sum(proxmat3_2020[i,])
}




# initializing vote probabilities by previous elections using 2020 votes: 


voter_data_2020_abm <- voter_data_2020 %>% 
  filter(election_lbl == "2020-11-03")

# merge 2024 data and demographics to it


geo_nc_2755_small <- geo_nc_2755_test %>% dplyr::select(-DEM_prob,-REP_prob, -UNA_prob, -NLB_prob)

voter_data_2020_abm <- left_join(voter_data_2020_abm, geo_nc_2755_small, by = "ncid")

voter_data_2020_abm <- na.omit(voter_data_2020_abm)

voter_data_2020_abm <- voter_data_2020_abm %>% dplyr::select(-DEM_votes, -REP_votes, -UNA_votes, -DEM_votes_prob, -REP_votes_prob, -UNA_votes_prob)




### running the function


updateVoteProbability_4a <- function(agents, proximity_matrix, alpha, beta, total_attributes, epochs, proximity_threshold) {
  
  num_agents <- nrow(agents)
  
  # Create a dataframe for interactions and logging
  interactions <- data.frame(epoch = integer(), agent = integer(), alter = integer(), party = character(), vote_prob = numeric(), delta_P = numeric())
  interactions_epoch <- data.frame(epoch = integer(), agent = integer(), party = character(), vote_prob = numeric())
  
  for (epoch in 1:epochs) {
    cat("Epoch:", epoch, "\n")
    for (ego in 1:num_agents) {
      # Randomly decide if the agent will be chosen for interaction (60% chance)
      if (runif(1) < 0.6) {
      # Randomly assign the number of interactions for this agent for this day
      num_interactions <- sample(0:5, 1)
      for (interaction in 1:num_interactions) {
        # Filter agents within the proximity threshold
        potential_alters <- which(proximity_matrix[ego, ] > proximity_threshold & proximity_matrix[ego, ] < 1)
        
        if (length(potential_alters) > 0) {
          alter <- sample(potential_alters, 1)  # Randomly select one of the agents within proximity
          
          if (ego != alter) {  # Ensure ego does not interact with itself
            # Interaction probability based on proximity
            interaction_prob <- proximity_matrix[ego, alter]
            
            # Calculate Kronecker delta for party difference
            party_diff <- ifelse(agents$voted_party_cd[ego] == agents$voted_party_cd[alter], 0, 1)
            
            if (agents$voted_party_cd[alter] == "UNA") {
              delta_P <- 0
            } else {
              
              # Calculate the change in probability based on party difference
              if (party_diff == 1) {
                
                # Calculate the number of shared sociodemographic attributes
                shared_attributes <- sum(
                  agents$age_binned[ego] == agents$age_binned[alter], 
                  agents$race_code[ego] == agents$race_code[alter], 
                  agents$gender_code[ego] == agents$gender_code[alter]
                )
                
                influence_weight <- shared_attributes / total_attributes
                
                if (influence_weight > 0.5) {
                  # Different party: decrease probability
                  delta_P <- -alpha * influence_weight 
                  
                  ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
                  alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
                  
                  # Update the voting probability for ego
                  agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                  agents[[alter_party_col]][ego] <- agents[[alter_party_col]][ego] - delta_P
                  
                  # Update the voting probability for alter
                  agents[[ego_party_col]][alter] <- agents[[ego_party_col]][alter] - delta_P
                  agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P
                  
                } else {
                  delta_P <- 0
                }
                
              } else {
                # Same party: increase probability
                
                # Calculate the number of shared sociodemographic attributes
                shared_attributes <- sum(
                  agents$age_binned[ego] == agents$age_binned[alter], 
                  agents$race_code[ego] == agents$race_code[alter], 
                  agents$gender_code[ego] == agents$gender_code[alter]
                )
                
                influence_weight <- shared_attributes / total_attributes
                
                delta_P <- beta * influence_weight 
                
                ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
                alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
                
                # Update the voting probability for ego
                agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                # Update the voting probability for alter
                agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P
              }
            }
            
            # Record this interaction
            interactions <- rbind(interactions, data.frame(
              epoch = epoch,
              agent = ego,
              alter = alter,
              party = agents$voted_party_cd[ego],
              vote_prob = agents[[paste0(agents$voted_party_cd[ego], "_prob")]][ego],
              delta_P = delta_P
            ))
          }
        }
      }
    }
  }  
    
    # Log voting probabilities after each epoch
    for (agent in 1:num_agents) {
      party_prob_cols <- grep("_prob$", colnames(agents), value = TRUE)
      for (party_col in party_prob_cols) {
        interactions_epoch <- rbind(interactions_epoch, data.frame(
          epoch = epoch,
          agent = agent,
          party = gsub("_prob", "", party_col),
          vote_prob = agents[[party_col]][agent]
        ))
      }
    }
    
    # Plot after every 10 epochs
    if (epoch %% 10 == 0) {
      plot_current <- interactions_epoch %>%
        filter(epoch == epoch) %>%
        ggplot(aes(x = vote_prob, fill = party)) +
        geom_histogram(position = 'stack', alpha = 0.6, bins = 30) +
        labs(title = paste('Vote Probability Distribution at Epoch', epoch), x = 'Vote Probability', y = 'Count', fill = 'Party') +
        theme_minimal()
      print(plot_current)
      
    }
  }  
  
  return(list(interactions = interactions, interactions_epoch = interactions_epoch))
}


#### test 

for(agent in 1:nrow(voter_data_2020_abm)) {
  party_probs <- c(voter_data_2020_abm$DEM_prob[agent], voter_data_2020_abm$UNA_prob[agent], voter_data_2020_abm$REP_prob[agent])
  
  # Get the names of the party probability columns
  party_names <- c("DEM_prob", "UNA_prob", "REP_prob")
  
  # Find the index of the maximum probability
  max_index <- which.max(party_probs)
  
  # Get the name of the party with the highest probability
  max_party <- party_names[max_index]
  max_party <- stringr::str_remove(max_party, pattern = "_prob")
  
  if (voter_data_2020_abm$voted_party_cd[agent] != max_party) {
  
    print(agent)
  }
}


names(which.max(c(voter_data_2020_abm$DEM_prob[1], voter_data_2020_abm$UNA_prob[1], voter_data_2020_abm$REP_prob[1])))






#### test 2




proximity_threshold <- 0.000000001
alpha <- 1e-13
beta <-  1e-13
scaling_factor = 1e-6

interactions_4_2020 <-updateVoteProbability_4a(voter_data_2020_abm, proxmat1_2020, alpha = 0.001, beta= 0.001, total_attributes, epochs = 100, proximity_threshold)

interactions_plot <-updateVoteProbability_4a(voter_data_2020_abm, proxmat1_2020, alpha = 0.001, beta= 0.001, total_attributes, epochs = 100, proximity_threshold)


interactions_epoch_2020 <- interactions_4_2020$interactions_epoch
interactions_2020 <- interactions_4_2020$interactions


interactions_conf2_agent1_2020 <- interactions_2020 %>% filter(agent == 1)
interactions_conf1_agent1_2020 <- interactions_epoch_2020 %>% filter(agent == 1)



# Plot for individual vote probability changes over time

ggplot(interactions_epoch_2020, aes(x = epoch, y = vote_prob, color = agent, group = factor(agent))) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs",
       x = "Epoch",
       y = "Voting Probability") +
  facet_wrap(~party)+
  ylim(0,1)+
  theme_minimal()


ggplot(interactions_conf1_agent1_2020, aes(x = epoch, y = vote_prob, color = party, group = party)) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs for Agent 2",
       x = "Epoch",
       y = "Voting Probability") +
  theme_minimal()





plot_data_2020 <- interactions_epoch_2020 %>%
  tidyr::pivot_wider(names_from = party, values_from = vote_prob) 


# Plot vote probabilities over time for each party
ggplot(plot_data_2020, aes(x = epoch)) +
  geom_smooth(aes(y = DEM, color = "DEM")) +
  geom_smooth(aes(y = REP, color = "REP")) +
  geom_smooth(aes(y = UNA, color = "UNA")) +
  #geom_smooth(aes(y = NLB, color = "NLB")) +
  labs(title = "Vote Probabilities Over Time",
       x = "Epoch",
       y = "Vote Probability",
       color = "Party") +
  theme_minimal()




# Aggregate the number of interactions by agent and epoch
interaction_counts_2020 <- interactions_2020 %>%
  group_by(epoch, agent) %>%
  summarise(count = n(), .groups = 'drop')


# Plot the number of interactions over time
ggplot(interaction_counts_2020, aes(x = epoch, y = count, group = agent, color = factor(agent))) +
  geom_smooth(se = FALSE, linewidth = .5) +
  labs(title = "Number of Interactions Over Time",
       x = "Epoch",
       y = "Number of Interactions",
       color = "Agent") +
  theme_minimal() +
  theme(legend.position = "none")




# Plot for overall vote probability distribution at the final epoch

final_epoch <- max(plot_data_2020$epoch)
middle_epoch <- round(median(plot_data_2020$epoch))
first_epoch <- min(plot_data_2020$epoch)

plot_start <- interactions_epoch_2020 %>%
  filter(epoch == first_epoch) %>%
  ggplot(aes(x = vote_prob, fill = party)) +
  geom_histogram(position = 'stack', alpha = 0.6, bins = 30) +
  labs(title = 'Vote Probability Distribution at First Epoch', x = 'Vote Probability', y = 'Count', fill = 'Party') +
  theme_minimal()

?geom_histogram()

plot_middle <- interactions_epoch_2020 %>%
  filter(epoch == middle_epoch) %>%
  ggplot(aes(x = vote_prob, fill = party)) +
  geom_histogram(position = 'stack', alpha = 0.6, bins = 30) +
  labs(title = 'Vote Probability Distribution at Middle Epoch', x = 'Vote Probability', y = 'Count', fill = 'Party') +
  theme_minimal()

plot_end <- interactions_epoch_2020 %>%
  filter(epoch == final_epoch) %>%
  ggplot(aes(x = vote_prob, fill = party)) +
  geom_histogram(position = 'stack', alpha = 0.6, bins = 30) +
  labs(title = 'Vote Probability Distribution at Final Epoch', x = 'Vote Probability', y = 'Count', fill = 'Party') +
  theme_minimal()

library(patchwork)


patchwork::wrap_plots(ncol = 2, plot_start, plot_middle, plot_end)



interactions_epoch_2020_final_epoch <- interactions_epoch_2020 %>% filter(epoch == 1450)



##########################################################################


###############################################
#### Version 5  with 2020 data ######
###############################################


## changes ##
# replacing alpha and beta with mu as intensity of interaction

updateVoteProbability_5 <- function(agents, proximity_matrix, mu, total_attributes, epochs, proximity_threshold) {
  
  num_agents <- nrow(agents)
  
  # Create a dataframe for interactions and logging
  interactions <- data.frame(epoch = integer(), agent = integer(), alter = integer(), party = character(), vote_prob = numeric(), delta_P = numeric())
  interactions_epoch <- data.frame(epoch = integer(), agent = integer(), party = character(), vote_prob = numeric())
  
  for (epoch in 1:epochs) {
    cat("Epoch:", epoch, "\n")
    for (ego in 1:num_agents) {
      # Randomly decide if the agent will be chosen for interaction (60% chance)
      if (runif(1) < 0.6) {
        # Randomly assign the number of interactions for this agent for this day
        num_interactions <- sample(0:5, 1)
        for (interaction in 1:num_interactions) {
          # Filter agents within the proximity threshold
          potential_alters <- which(proximity_matrix[ego, ] > proximity_threshold & proximity_matrix[ego, ] < 1)
          
          if (length(potential_alters) > 0) {
            alter <- sample(potential_alters, 1)  # Randomly select one of the agents within proximity
            
            if (ego != alter) {  # Ensure ego does not interact with itself
              # Interaction probability based on proximity
              interaction_prob <- proximity_matrix[ego, alter]
              
              # Determine the party with the highest vote probability for ego and alter
              ego_party_probs <- c(agents$DEM_prob[ego], agents$UNA_prob[ego], agents$REP_prob[ego])
              
              # Get the names of the party probability columns
              ego_party_names <- c("DEM_prob", "UNA_prob", "REP_prob")
              
              # Find the index of the maximum probability
              ego_party_index <- which.max(ego_party_probs)
              
              # Get the name of the party with the highest probability
              ego_party <- party_names[ego_party_index]
              ego_party <- stringr::str_remove(ego_party, pattern = "_prob")
              
              
              alter_party_probs <- c(agents$DEM_prob[alter], agents$UNA_prob[alter], agents$REP_prob[alter])
              
              # Get the names of the party probability columns
              alter_party_names <- c("DEM_prob", "UNA_prob", "REP_prob")
              
              # Find the index of the maximum probability
              alter_party_index <- which.max(alter_party_probs)
              
              # Get the name of the party with the highest probability
              alter_party <- party_names[alter_party_index]
              alter_party <- stringr::str_remove(alter_party, pattern = "_prob")
              
              # Calculate Kronecker delta for party difference
              party_diff <- ifelse(ego_party == alter_party, 0, 1)
              
              
              if (agents$voted_party_cd[alter] == "UNA") {
                delta_P <- 0
              } else {
                
                # Calculate the change in probability based on party difference
                if (party_diff == 1) {
                  
                  # Calculate the number of shared sociodemographic attributes
                  shared_attributes <- sum(
                    agents$age_binned[ego] == agents$age_binned[alter], 
                    agents$race_code[ego] == agents$race_code[alter], 
                    agents$gender_code[ego] == agents$gender_code[alter]
                  )
                  
                  similarity_weight <- shared_attributes / total_attributes
                  
                  if (similarity_weight > 0.5) {
                    # Different party: decrease probability
                    delta_P <- -mu 
                    
                    ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
                    alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
                    
                    # Update the voting probability for ego
                    agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                    agents[[alter_party_col]][ego] <- agents[[alter_party_col]][ego] - delta_P
                    
                    # Update the voting probability for alter
                    agents[[ego_party_col]][alter] <- agents[[ego_party_col]][alter] - delta_P
                    agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P
                    
                  } else {
                    delta_P <- 0
                  }
                  
                } else {
                  # Same party: increase probability
                  
                  # Calculate the number of shared sociodemographic attributes
                  shared_attributes <- sum(
                    agents$age_binned[ego] == agents$age_binned[alter], 
                    agents$race_code[ego] == agents$race_code[alter], 
                    agents$gender_code[ego] == agents$gender_code[alter]
                  )
                  
                  similarity_weight <- shared_attributes / total_attributes
                  
                  delta_P <- mu 
                  
                  ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
                  alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
                  
                  # Update the voting probability for ego
                  agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                  # Update the voting probability for alter
                  agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P
                }
              }
              
              # Record this interaction
              interactions <- rbind(interactions, data.frame(
                epoch = epoch,
                agent = ego,
                alter = alter,
                party = agents$voted_party_cd[ego],
                vote_prob = agents[[paste0(agents$voted_party_cd[ego], "_prob")]][ego],
                delta_P = delta_P
              ))
            }
          }
        }
      }
    }  
    
    # Log voting probabilities after each epoch
    for (agent in 1:num_agents) {
      party_prob_cols <- grep("_prob$", colnames(agents), value = TRUE)
      for (party_col in party_prob_cols) {
        interactions_epoch <- rbind(interactions_epoch, data.frame(
          epoch = epoch,
          agent = agent,
          party = gsub("_prob", "", party_col),
          vote_prob = agents[[party_col]][agent]
        ))
      }
    }
    
    # Plot after every 10 epochs
    if (epoch %% 10 == 0) {
      plot_current <- interactions_epoch %>%
        filter(epoch == epoch) %>%
        ggplot(aes(x = vote_prob, fill = party)) +
        geom_histogram(position = 'stack', alpha = 0.6, bins = 30) +
        labs(title = paste('Vote Probability Distribution at Epoch', epoch), x = 'Vote Probability', y = 'Count', fill = 'Party') +
        theme_minimal()
      print(plot_current)
      
    }
  }  
  
  return(list(interactions = interactions, interactions_epoch = interactions_epoch))
}




interaction_5 <- updateVoteProbability_5(voter_data_2020_abm, proxmat1_2020, mu = 0.001,  total_attributes, epochs = 10, proximity_threshold)




#### tomorrow: define opinion threshold dynamically before each interaction (e.g. like CI) 



##########################################################################


###############################################
#### Version 6  with 2020 data ######
###############################################


## changes ##
# remove similarity weight from opinion update
# include Homophily H as third interaction possibility

updateVoteProbability_6 <- function(agents, proximity_matrix, mu, total_attributes, epochs, proximity_threshold) {
  
  num_agents <- nrow(agents)
  
  # Create a dataframe for interactions and logging
  interactions <- data.frame(epoch = integer(), agent = integer(), alter = integer(), party = character(), vote_prob = numeric(), delta_P = numeric())
  interactions_epoch <- data.frame(epoch = integer(), agent = integer(), party = character(), vote_prob = numeric())
  
  for (epoch in 1:epochs) {
    cat("Epoch:", epoch, "\n")
    for (ego in 1:num_agents) {
      # Randomly decide if the agent will be chosen for interaction (60% chance)
      if (runif(1) < 0.6) {
        # Randomly assign the number of interactions for this agent for this day
        num_interactions <- sample(0:5, 1)
        for (interaction in 1:num_interactions) {
          # Filter agents within the proximity threshold
          potential_alters <- which(proximity_matrix[ego, ] > proximity_threshold & proximity_matrix[ego, ] < 1)
          
          if (length(potential_alters) > 0) {
            alter <- sample(potential_alters, 1)  # Randomly select one of the agents within proximity
            
            if (ego != alter) {  # Ensure ego does not interact with itself
              # Interaction probability based on proximity
              interaction_prob <- proximity_matrix[ego, alter]
              
              # Determine the party with the highest vote probability for ego and alter
              ego_party_probs <- c(agents$DEM_prob[ego], agents$UNA_prob[ego], agents$REP_prob[ego])
              
              # Get the names of the party probability columns
              ego_party_names <- c("DEM_prob", "UNA_prob", "REP_prob")
              
              # Find the index of the maximum probability
              ego_party_index <- which.max(ego_party_probs)
              
              # Get the name of the party with the highest probability
              ego_party <- party_names[ego_party_index]
              ego_party <- stringr::str_remove(ego_party, pattern = "_prob")
              
              
              alter_party_probs <- c(agents$DEM_prob[alter], agents$UNA_prob[alter], agents$REP_prob[alter])
              
              # Get the names of the party probability columns
              alter_party_names <- c("DEM_prob", "UNA_prob", "REP_prob")
              
              # Find the index of the maximum probability
              alter_party_index <- which.max(alter_party_probs)
              
              # Get the name of the party with the highest probability
              alter_party <- party_names[alter_party_index]
              alter_party <- stringr::str_remove(alter_party, pattern = "_prob")
              
              # Calculate Kronecker delta for party difference
              party_diff <- ifelse(ego_party == alter_party, 0, 1)
              
              
              if (agents$voted_party_cd[alter] == "UNA") {
                delta_P <- 0
              } else {
                
                # Calculate the change in probability based on party difference
                if (party_diff == 1) {
                  
                  # Calculate the number of shared sociodemographic attributes
                  shared_attributes <- sum(
                    agents$age_binned[ego] == agents$age_binned[alter], 
                    agents$race_code[ego] == agents$race_code[alter], 
                    agents$gender_code[ego] == agents$gender_code[alter]
                  )
                  
                  similarity_weight <- shared_attributes / total_attributes
                  
                  if (similarity_weight > 0.5) {
                    # Different party: decrease probability
                    delta_P <- -mu 
                    
                    ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
                    alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
                    
                    # Update the voting probability for ego
                    agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                    agents[[alter_party_col]][ego] <- agents[[alter_party_col]][ego] - delta_P
                    
                    # Update the voting probability for alter
                    agents[[ego_party_col]][alter] <- agents[[ego_party_col]][alter] - delta_P
                    agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P
                    
                  } else {
                    
                    if (agents$Blau_party[ego] < 0.3 ) {
                      
                      delta_P <- -mu 
                      
                      ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
                      alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
                      
                      # Update the voting probability for ego
                      agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                      agents[[alter_party_col]][ego] <- agents[[alter_party_col]][ego] - delta_P
                      
                      # Update the voting probability for alter
                      agents[[ego_party_col]][alter] <- agents[[ego_party_col]][alter] - delta_P
                      agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P  
                      
                    } else {
                    delta_P <- 0
                  }
                }
                  
                } else {
                  # Same party: increase probability
                  
                  # Calculate the number of shared sociodemographic attributes
                  shared_attributes <- sum(
                    agents$age_binned[ego] == agents$age_binned[alter], 
                    agents$race_code[ego] == agents$race_code[alter], 
                    agents$gender_code[ego] == agents$gender_code[alter]
                  )
                  
                  similarity_weight <- shared_attributes / total_attributes
                  
                  delta_P <- mu 
                  
                  ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
                  alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
                  
                  # Update the voting probability for ego
                  agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                  # Update the voting probability for alter
                  agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P
                }
              }
              
              # Record this interaction
              interactions <- rbind(interactions, data.frame(
                epoch = epoch,
                agent = ego,
                alter = alter,
                party = agents$voted_party_cd[ego],
                vote_prob = agents[[paste0(agents$voted_party_cd[ego], "_prob")]][ego],
                delta_P = delta_P
              ))
            }
          }
        }
      }
    }  
    
    # Log voting probabilities after each epoch
    for (agent in 1:num_agents) {
      party_prob_cols <- grep("_prob$", colnames(agents), value = TRUE)
      for (party_col in party_prob_cols) {
        interactions_epoch <- rbind(interactions_epoch, data.frame(
          epoch = epoch,
          agent = agent,
          party = gsub("_prob", "", party_col),
          vote_prob = agents[[party_col]][agent]
        ))
      }
    }
    
    # Plot after every 10 epochs
    if (epoch %% 10 == 0) {
      plot_current <- interactions_epoch %>%
        filter(epoch == epoch) %>%
        ggplot(aes(x = vote_prob, fill = party)) +
        geom_histogram(position = 'stack', alpha = 0.6, bins = 30) +
        labs(title = paste('Vote Probability Distribution at Epoch', epoch), x = 'Vote Probability', y = 'Count', fill = 'Party') +
        theme_minimal()
      print(plot_current)
      
    }
  }  
  
  return(list(interactions = interactions, interactions_epoch = interactions_epoch))
}




geo_nc_27555_blau_small <- geo_nc_27555_blau %>%
  dplyr::select(ncid, Blau_party, Blau_race)

voter_data_2020_abm <- left_join(voter_data_2020_abm, geo_nc_27555_blau_small, by = "ncid")

table(voter_data_2020_abm$Blau_party)

interaction_6 <- updateVoteProbability_6(voter_data_2020_abm, proxmat1_2020, mu = 0.001,  total_attributes, epochs = 100, proximity_threshold)

voter_data_2020_abm$Blau_party



######################################################################################



###############################################
#### Version 7  with 2020 data ######
###############################################


## changes ##
# include dynamcially changing blau index per grid cell 

updateVoteProbability_7 <- function(agents, proximity_matrix, mu, total_attributes, epochs, proximity_threshold) {
  
  num_agents <- nrow(agents)
  
  # Create a dataframe for interactions and logging
  interactions <- data.frame(epoch = integer(), agent = integer(), alter = integer(), party = character(), vote_prob = numeric(), delta_P = numeric())
  interactions_epoch <- data.frame(epoch = integer(), agent = integer(), party = character(), vote_prob = numeric())
  
  for (epoch in 1:epochs) {
    cat("Epoch:", epoch, "\n")
    for (ego in 1:num_agents) {
      # Randomly decide if the agent will be chosen for interaction (60% chance)
      if (runif(1) < 0.6) {
        # Randomly assign the number of interactions for this agent for this day
        num_interactions <- sample(0:5, 1)
        for (interaction in 1:num_interactions) {
          
          
          # Filter agents within the proximity threshold
          potential_alters <- which(proximity_matrix[ego, ] > proximity_threshold & proximity_matrix[ego, ] < 1)
          
          if (length(potential_alters) > 0) {
            alter <- sample(potential_alters, 1)  # Randomly select one of the agents within proximity
            
            if (ego != alter) {  # Ensure ego does not interact with itself
              # Interaction probability based on proximity
              interaction_prob <- proximity_matrix[ego, alter]
              
              # Calculate Kronecker delta for party difference using current voted_party_cd
              party_diff <- ifelse(agents$voted_party_cd[ego] == agents$voted_party_cd[alter], 0, 1)
              
              if (agents$voted_party_cd[alter] == "UNA") {
                delta_P <- 0
              } else {
                # Calculate the change in probability based on party difference
                if (party_diff == 1) {
                  shared_attributes <- sum(
                    agents$age_binned[ego] == agents$age_binned[alter], 
                    agents$race_code[ego] == agents$race_code[alter], 
                    agents$gender_code[ego] == agents$gender_code[alter]
                  )
                  
                  similarity_weight <- shared_attributes / total_attributes
                  
                  if (similarity_weight > 0.5) {
                    # Different party: decrease probability
                    delta_P <- -mu
                    
                    ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
                    alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
                    
                    # Update the voting probability for ego
                    agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                    agents[[alter_party_col]][ego] <- agents[[alter_party_col]][ego] - delta_P
                    
                    # Update the voting probability for alter
                    agents[[ego_party_col]][alter] <- agents[[ego_party_col]][alter] - delta_P
                    agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P
                  } else {
                    if (agents$Blau_party[ego] < 0.3) {
                      delta_P <- -mu
                      
                      ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
                      alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
                      
                      # Update the voting probability for ego
                      agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                      agents[[alter_party_col]][ego] <- agents[[alter_party_col]][ego] - delta_P
                      
                      # Update the voting probability for alter
                      agents[[ego_party_col]][alter] <- agents[[ego_party_col]][alter] - delta_P
                      agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P  
                    } else {
                      delta_P <- 0
                    }
                  }
                } else {
                  # Same party: increase probability
                  shared_attributes <- sum(
                    agents$age_binned[ego] == agents$age_binned[alter], 
                    agents$race_code[ego] == agents$race_code[alter], 
                    agents$gender_code[ego] == agents$gender_code[alter]
                  )
                  
                  similarity_weight <- shared_attributes / total_attributes
                  
                  delta_P <- mu
                  
                  ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
                  alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
                  
                  # Update the voting probability for ego
                  agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                  # Update the voting probability for alter
                  agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P
                }
              }
              
              # Record this interaction
              interactions <- rbind(interactions, data.frame(
                epoch = epoch,
                agent = ego,
                alter = alter,
                party = agents$voted_party_cd[ego],
                vote_prob = agents[[paste0(agents$voted_party_cd[ego], "_prob")]][ego],
                delta_P = delta_P
              ))
              
              # Determine the party with the highest vote probability for ego and alter after interaction
              ego_party_probs <- c(agents$DEM_prob[ego], agents$UNA_prob[ego], agents$REP_prob[ego])
              ego_party_names <- c("DEM_prob", "UNA_prob", "REP_prob", "LIB_prob", "GRE_prob")
              ego_party_index <- which.max(ego_party_probs)
              ego_party <- ego_party_names[ego_party_index]
              ego_party <- stringr::str_remove(ego_party, pattern = "_prob")
              
              alter_party_probs <- c(agents$DEM_prob[alter], agents$UNA_prob[alter], agents$REP_prob[alter])
              alter_party_names <- c("DEM_prob", "UNA_prob", "REP_prob", "LIB_prob", "GRE_prob")
              alter_party_index <- which.max(alter_party_probs)
              alter_party <- alter_party_names[alter_party_index]
              alter_party <- stringr::str_remove(alter_party, pattern = "_prob")
              
              # Update voted_party_cd based on the highest probability after interaction
              agents$voted_party_cd[ego] <- ego_party
              agents$voted_party_cd[alter] <- alter_party
              
              # Update Blau index for the respective grid cells
              agents <- agents %>%
                group_by(grid_cell_id) %>%
                mutate(
                  party_proportion_DEM = sum(voted_party_cd == "DEM") / n(),
                  party_proportion_UNA = sum(voted_party_cd == "UNA") / n(),
                  party_proportion_REP = sum(voted_party_cd == "REP") / n(),
                  party_proportion_LIB = sum(voted_party_cd == "LIB") / n(),
                  party_proportion_GRE = sum(voted_party_cd == "GRE") / n(),
                  party_proportion_sq = party_proportion_DEM^2 + party_proportion_UNA^2 + party_proportion_REP^2,
                  Blau_party = 1 - party_proportion_sq
                ) %>%
                ungroup()
            }
          }
        }
      }
    }  
    
    # Log voting probabilities after each epoch
    for (agent in 1:num_agents) {
      party_prob_cols <- grep("_prob$", colnames(agents), value = TRUE)
      for (party_col in party_prob_cols) {
        interactions_epoch <- rbind(interactions_epoch, data.frame(
          epoch = epoch,
          agent = agent,
          party = gsub("_prob", "", party_col),
          vote_prob = agents[[party_col]][agent]
        ))
      }
    }
    
    # Plot after every 10 epochs
    if (epoch %% 10 == 0) {
      plot_current <- interactions_epoch %>%
        filter(epoch == epoch) %>%
        ggplot(aes(x = vote_prob, fill = party)) +
        geom_histogram(position = 'stack', alpha = 0.6, bins = 30) +
        labs(title = paste('Vote Probability Distribution at Epoch', epoch), x = 'Vote Probability', y = 'Count', fill = 'Party') +
        theme_minimal()
      print(plot_current)
    }
  }  
  
  return(list(interactions = interactions, interactions_epoch = interactions_epoch))
}


geo_nc_27555_blau_small <- geo_nc_27555_blau %>%
  dplyr::select(ncid, Blau_party, Blau_race)

voter_data_2020_abm <- left_join(voter_data_2020_abm, geo_nc_27555_blau_small, by = "ncid")

table(voter_data_2020_abm$Blau_party)

interaction_7 <- updateVoteProbability_7(voter_data_2020_abm, proxmat1_2020, mu = 0.001,  total_attributes, epochs = 100, proximity_threshold)



#########################################################################################################


##############################################
#### Version 8  with 2020 data ######
###############################################


## changes ##
# penalty rate for divergence based on Out group Exposure within proximity but needs to be updated after every interaction

updateVoteProbability_8 <- function(agents, proximity_matrix, mu, total_attributes, epochs, proximity_threshold, p) {
  num_agents <- nrow(agents)
  
  # Create a dataframe for interactions and logging
  interactions <- data.frame(epoch = integer(), agent = integer(), alter = integer(), party = character(), vote_prob = numeric(), delta_P = numeric())
  interactions_epoch <- data.frame(epoch = integer(), agent = integer(), party = character(), vote_prob = numeric())
  
  for (epoch in 1:epochs) {
    cat("Epoch:", epoch, "\n")
    
    # Update exposure values at the start of each epoch
    exposure_values <- calcExposurePartisanship(agents, proximity_matrix, proximity_threshold)
    agents$exposureIngroup <- exposure_values$exposureIngroup
    agents$exposureOutgroup <- exposure_values$exposureOutgroup
    agents$party_majority <- exposure_values$party_majority
    
    
    for (ego in 1:num_agents) {
      # Randomly decide if the agent will be chosen for interaction (60% chance)
      if (runif(1) < 0.6) {
        # Randomly assign the number of interactions for this agent for this day
        num_interactions <- sample(0:5, 1)
        for (interaction in 1:num_interactions) {
          # Filter agents within the proximity threshold
          potential_alters <- which(proximity_matrix[ego, ] > proximity_threshold & proximity_matrix[ego, ] < 1)
          
          if (length(potential_alters) > 0) {
            alter <- sample(potential_alters, 1)  # Randomly select one of the agents within proximity
            
            if (ego != alter) {  # Ensure ego does not interact with itself
              # Interaction probability based on proximity
              interaction_prob <- proximity_matrix[ego, alter]
              
              # Calculate Kronecker delta for party difference using current voted_party_cd
              party_diff <- ifelse(agents$voted_party_cd[ego] == agents$voted_party_cd[alter], 0, 1)
              
              if (agents$voted_party_cd[alter] == "UNA") {
                delta_P <- 0
              } else {
                # Calculate the change in probability based on party difference
                if (party_diff == 1) {
                  shared_attributes <- sum(
                    agents$age_binned[ego] == agents$age_binned[alter], 
                    agents$race_code[ego] == agents$race_code[alter], 
                    agents$gender_code[ego] == agents$gender_code[alter]
                  )
                  
                  similarity_weight <- shared_attributes / total_attributes
                  
                  if (similarity_weight > 0.5) {
                    if(agents$voted_party_cd[alter] == agents$party_majority[ego] & agents$exposureOutgroup[ego] > 0.5) {
                      # Different party: decrease probability with penalty
                      delta_P <- -mu * p
                      
                      ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
                      alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
                      
                      # Update the voting probability for ego
                      agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                      agents[[alter_party_col]][ego] <- agents[[alter_party_col]][ego] - delta_P
                      
                      # Update the voting probability for alter
                      agents[[ego_party_col]][alter] <- agents[[ego_party_col]][alter] - delta_P
                      agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P
                    } else {
                      # Different party: decrease probability
                      delta_P <- -mu
                      
                      ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
                      alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
                      
                      # Update the voting probability for ego
                      agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                      agents[[alter_party_col]][ego] <- agents[[alter_party_col]][ego] - delta_P
                      
                      # Update the voting probability for alter
                      agents[[ego_party_col]][alter] <- agents[[ego_party_col]][alter] - delta_P
                      agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P
                    }
                  } else {
                    if (agents$Blau_party[ego] < 0.3) {
                      delta_P <- -mu
                      
                      ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
                      alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
                      
                      # Update the voting probability for ego
                      agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                      agents[[alter_party_col]][ego] <- agents[[alter_party_col]][ego] - delta_P
                      
                      # Update the voting probability for alter
                      agents[[ego_party_col]][alter] <- agents[[ego_party_col]][alter] - delta_P
                      agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P  
                    } else {
                      delta_P <- 0
                    }
                  }
                } else {
                  # Same party: increase probability
                  shared_attributes <- sum(
                    agents$age_binned[ego] == agents$age_binned[alter], 
                    agents$race_code[ego] == agents$race_code[alter], 
                    agents$gender_code[ego] == agents$gender_code[alter]
                  )
                  
                  similarity_weight <- shared_attributes / total_attributes
                  
                  delta_P <- mu
                  
                  ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
                  alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
                  
                  # Update the voting probability for ego
                  agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                  # Update the voting probability for alter
                  agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P
                }
              }
              
              # Record this interaction
              interactions <- rbind(interactions, data.frame(
                epoch = epoch,
                agent = ego,
                alter = alter,
                party = agents$voted_party_cd[ego],
                vote_prob = agents[[paste0(agents$voted_party_cd[ego], "_prob")]][ego],
                delta_P = delta_P
              ))
              
              # Determine the party with the highest vote probability for ego and alter after interaction
              ego_party_probs <- c(agents$DEM_prob[ego], agents$UNA_prob[ego], agents$REP_prob[ego])
              ego_party_names <- c("DEM_prob", "UNA_prob", "REP_prob", "LIB_prob", "GRE_prob")
              ego_party_index <- which.max(ego_party_probs)
              ego_party <- ego_party_names[ego_party_index]
              ego_party <- stringr::str_remove(ego_party, pattern = "_prob")
              
              alter_party_probs <- c(agents$DEM_prob[alter], agents$UNA_prob[alter], agents$REP_prob[alter])
              alter_party_names <- c("DEM_prob", "UNA_prob", "REP_prob", "LIB_prob", "GRE_prob")
              alter_party_index <- which.max(alter_party_probs)
              alter_party <- alter_party_names[alter_party_index]
              alter_party <- stringr::str_remove(alter_party, pattern = "_prob")
              
              # Update voted_party_cd based on the highest probability after interaction
              agents$voted_party_cd[ego] <- ego_party
              agents$voted_party_cd[alter] <- alter_party
              
              # Update Blau index for the respective grid cells
              agents <- agents %>%
                group_by(grid_cell_id) %>%
                mutate(
                  party_proportion_DEM = sum(voted_party_cd == "DEM") / n(),
                  party_proportion_UNA = sum(voted_party_cd == "UNA") / n(),
                  party_proportion_REP = sum(voted_party_cd == "REP") / n(),
                  party_proportion_LIB = sum(voted_party_cd == "LIB") / n(),
                  party_proportion_GRE = sum(voted_party_cd == "GRE") / n(),
                  party_proportion_sq = party_proportion_DEM^2 + party_proportion_UNA^2 + party_proportion_REP^2 + party_proportion_LIB^2 + party_proportion_GRE^2,
                  Blau_party = 1 - party_proportion_sq
                ) %>%
                ungroup()
            }
          }
        }
      }
    }  
    
    # Log voting probabilities after each epoch
    for (agent in 1:num_agents) {
      party_prob_cols <- grep("_prob$", colnames(agents), value = TRUE)
      for (party_col in party_prob_cols) {
        interactions_epoch <- rbind(interactions_epoch, data.frame(
          epoch = epoch,
          agent = agent,
          party = gsub("_prob", "", party_col),
          vote_prob = agents[[party_col]][agent]
        ))
      }
    }
    
    # Plot after every 10 epochs
    if (epoch %% 10 == 0) {
      plot_current <- interactions_epoch %>%
        filter(epoch == epoch) %>%
        ggplot(aes(x = vote_prob, fill = party)) +
        geom_histogram(position = 'stack', alpha = 0.6, bins = 30) +
        labs(title = paste('Vote Probability Distribution at Epoch', epoch), x = 'Vote Probability', y = 'Count', fill = 'Party') +
        theme_minimal()
      print(plot_current)
    }
  }  
  
  return(list(interactions = interactions, interactions_epoch = interactions_epoch))
}


# Example usage
interaction_8 <- updateVoteProbability_8(agents, proximity_matrix, mu, total_attributes, epochs, proximity_threshold, p)




#########################################################################################################








library(av)
library(gganimate)

# Animated plot of voting probabilities over epochs
anim <- ggplot(interactions_random1, aes(x = epoch, y = vote_prob, color = party)) +
  geom_line(aes(group = agent), alpha = 0.3) +
  facet_wrap(~ party, scales = "free_y") +
  labs(title = "Voting Probabilities Over Time",
       x = "Epoch",
       y = "Voting Probability") +
  theme_minimal() +
  transition_reveal(epoch)

animate(anim, nframes = 200, fps = 10)





# Extract coordinates and create layout matrix
coords <- as.matrix(geo_nc_2755_test[, c("longitude", "latitude")])


library(igraph)

# Create an interaction network
edges <- interactions_conf2 %>%
  dplyr::select(agent, alter, epoch) %>%
  dplyr::filter(epoch == max(epoch)) %>%
  distinct()


g <- graph_from_data_frame(edges, directed = TRUE)
V(g)$party <- interactions_max_party$party

V(g)$party <- as.factor(V(g)$party)
V(g)$party_num  <- as.numeric(V(g)$party)

# Plot the network
plot(g, vertex.label = NA, vertex.size = 3, vertex.color = V(g)$party, 
     edge.arrow.size = 0.2,
     edge.width = 0.2,
     edge.curved = TRUE,
     layout = coords,
     main = "Interaction Network of Agents in Epoch 50")+
  legend("topright", 
         legend = c("DEM" ,"NLB", "REP", "UNA"), 
         pch = 19, 
         col = categorical_pal(4)
  )

library(ggnetwork)
library(ggplot2)
library(dplyr)

# Prepare the network plot using ggplot2 and ggnetwork
network_plot <- ggplot(ggnetwork(g, layout = coords), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey", linewidth = .5) +
  geom_nodes(aes(color = factor(party)), size = 3) +
  scale_color_manual(values = c("blue", "black", "red", "green")) +
  theme_void() +
  theme(legend.position = "right")

######################################--------------------------------------




#### TEST TEST TEST ##########













########################################-----------------------------------



### lets see who switched:

# probably get it to wide format

interactions_random1_300 <- interactions_random1 %>% filter(epoch == 300)

# Identify the party with the highest probability for each agent at epoch 100
interactions_max_party <- interactions_random1_300 %>%
  group_by(agent) %>%
  filter(vote_prob == max(vote_prob, na.rm = TRUE)) %>%
  #slice(1) %>%  # In case of ties, take the first one
  ungroup()

table(interactions_max_party$party)


logit <- function(p) {
  log(p / (1 - p))
}


logit(0.6730887)


geo_nc_2755_test$party_new <- interactions_max_party$party


geo_nc_2755_test <- geo_nc_2755_test %>% mutate(switch = case_when(party_cd == party_new ~ 0,
                                                                           party_cd != party_new ~1))
table(geo_nc_2755_test$switch)





########################################-----------------------------------

###### election data from 2016 for initial configuration of model when model is finished

head(geo_nc_27555)
str(ncvhis_Statewide$election_lbl)

voter_data_2016 <- ncvhis_Statewide %>% filter(election_lbl == "11/08/2016")
table(voter_data_2016$voted_party_cd)

voter_data_2016 <- voter_data_2016 %>% select(election_lbl, voted_party_cd, voter_reg_num, ncid)

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


