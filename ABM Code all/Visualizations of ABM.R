


#################################################
############## VISUALIZATIONS ###################
#################################################



######## Mapping North Carolina #############
###############################################



nc_shapefile <- st_read("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/North_Carolina_State_and_County_Boundary_Polygons/North_Carolina_State_and_County_Boundary_Polygons.shp")

nc_johnston <- nc_shapefile %>% filter(FIPS == 101)

centerlines <- st_read("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/Centerlines/centerli.shp")

zipcodes <- st_read("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/zipcode shapefile/tl_2023_us_zcta520.shp")



RUCA <- read_excel("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/RUCA_US.xlsx")

RUCA_NC <- RUCA %>% filter(STATE == "NC")
RUCA_NC <- RUCA_NC %>% dplyr::select(ZIP_CODE, RUCA1, ZIP_TYPE)

RUCA_NC <- RUCA_NC %>% filter(ZIP_TYPE == "Zip Code Area")

# spatial join with RUCA and zipcode data 



RUCA_zip <- RUCA_NC$ZIP_CODE
zipcodes_NC <- zipcodes %>% filter(GEOID20 %in% RUCA_zip)

zipcodes_Ruca <- left_join(zipcodes_NC, RUCA_NC, by = c("GEOID20" = "ZIP_CODE"))


str(zipcodes_Ruca)


zipcodes_Ruca$RUCA1 <- as.factor(zipcodes_Ruca$RUCA1)

######## plotting the coords with agents


tmap_mode("plot")
tmap_mode("view")

map_RUCA <- tm_shape(nc_shapefile) + 
  tm_polygons() +
  tm_shape(zipcodes_Ruca) +
  tm_polygons(fill = "RUCA1", palette = "Blues") +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right") +
  tm_title ("Rural Urban Classification of Zipcodes in NC")


tmap_save(map_RUCA, "map_RUCA.png")




############ Mapping Zipcodes #################
###############################################


##################### 28202 #######################


zipcodes28202 <- zipcodes %>% filter(ZCTA5CE20 == 28202)
load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/coords_adress_28202.RData")


coords_28202 <- coords_adress_28202 %>% dplyr::select(lon, lat)
coords_28202$lon <- as.numeric(coords_28202$lon)
coords_28202$lat <- as.numeric(coords_28202$lat)

points_28202 <- st_as_sf(coords_adress_28202, coords = c("lon", "lat"), crs = 4269)

geo_nc_28202_points <- left_join(points_28202, geo_nc_28202_df, by =c("address" = "full_adress"))
geo_nc_28202_points <- unique(geo_nc_28202_points)


######## plotting the coords with agents

tmap_mode("view")

tm_shape(zipcodes28202) + 
  tm_polygons() +
  #tm_shape(centerlines) + 
  #tm_lines() + 
  tm_shape(geo_nc_28202_points) +
  tm_symbols(col = "party_cd", size = 0.1)

str(geo_nc_27915_blau_abm)


##################### 27915 #######################

zipcodes27915 <- zipcodes %>% filter(ZCTA5CE20 == 27915)

load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/coords_adress_27915.RData")


coords_27915 <- coords_adress_27915 %>% dplyr::select(lon, lat)
coords_27915$lon <- as.numeric(coords_27915$lon)
coords_27915$lat <- as.numeric(coords_27915$lat)

points_27915 <- st_as_sf(coords_adress_27915, coords = c("lon", "lat"), crs = 4269)

geo_nc_27915_points <- left_join(points_27915, geo_nc_27915_df, by =c("address" = "full_adress"))
geo_nc_27915_points <- unique(geo_nc_27915_points)



######## plotting the coords with agents

tmap_mode("view")

tm_shape(zipcodes27915) + 
  tm_polygons() +
  #tm_shape(centerlines) + 
  #tm_lines() + 
  tm_shape(geo_nc_27915_points) +
  tm_symbols(col = "party_cd", size = 0.1)


table(nc_27915$party_cd)





#####################################################################
########### Plotting Partisan Homogenization in Zipcodes ############
#####################################################################


# getting ncid from zipcodes


nc_all <- ncvoter_Statewide %>% filter(zip_code == 28203 | zip_code == 27915)
nc_all <- nc_all %>% filter(precinct_abbrv == "010" | precinct_abbrv == "AVON")

ncid_list_all <- nc_all$ncid

ncvhis_zipcodes <- ncvhis_Statewide %>% filter(ncid %in% ncid_list_all)


##### deciding which elections and parties to keep 


nc_voter_hist_zipcodes <- ncvhis_zipcodes %>% filter(election_lbl %in% c("11/04/2014","11/08/2016", "11/06/2018","11/03/2020", "11/08/2022")) %>% 
  filter(voted_party_cd %in% c("DEM", "REP", "GRE", "UNA", "NLB"))

nc_voter_hist_zipcodes$election_lbl <- lubridate::mdy(nc_voter_hist_zipcodes$election_lbl)

nc_voter_hist_zipcodes <- nc_voter_hist_zipcodes %>% dplyr::select(ncid, election_lbl, voted_party_cd)


nc_all_zip <- nc_all %>% dplyr::select(ncid, zip_code)
nc_voter_hist_zipcodes <- left_join(nc_voter_hist_zipcodes,nc_all_zip, by ="ncid")


nc_all_zipcodes <- nc_all %>% dplyr::select(zip_code, ncid, party_cd)
nc_all_zipcodes$election_lbl <- "2024-07-25"

nc_all_zipcodes <- nc_all_zipcodes %>% rename("voted_party_cd" = party_cd)



# merging both 


nc_all_zips_merged <- rbind(nc_voter_hist_zipcodes, nc_all_zipcodes)

nc_all_zips_merged_no_UNA <- nc_all_zips_merged %>% filter(voted_party_cd != "UNA")

nc_stats_zipcode<- nc_all_zips_merged %>%
  group_by(voted_party_cd, zip_code, election_lbl) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(election_lbl, zip_code) %>%
  mutate(perc = count / sum(count))



ggplot(nc_stats_zipcode, aes(election_lbl, perc, group = voted_party_cd, col = voted_party_cd)) +
  geom_line() + scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple", "GRE" = "darkgreen", "LIB" = "yellow")) +
  facet_wrap(~zip_code)+
  labs(x = "Election", y = "Percentage", color = "Voted Party") +
  theme_bw() + ylim(0,1)


ggsave("nc_stats_all_ruca.png",dpi = 95)




# second way of visualizing: 
### using 27915 zipcode in outerbanks

nc_27915 <- ncvoter_Statewide %>% filter(zip_code == 27915)

ncid_list_27915 <- nc_27915$ncid



nc_stats_2024 <- nc_27915 %>% group_by(party_cd) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count))

nc_stats_2024$year <- 2024


nc_stats_2024 <- nc_stats_2024 %>% rename("voted_party_cd" = party_cd)

# ATTENTION: i cannot get history voting data per zipcode as this information is not available in the data file, so I have to filter via ncid

ncvhis_Statewide_27915 <- ncvhis_Statewide %>% filter(ncid %in% ncid_list_27915)

participation <- ncvhis_Statewide_27915 %>%group_by(election_lbl) %>%
  count()


participation <- arrange(participation, by_group = n)




####### 2020 ########

nc_voter_2020 <- ncvhis_Statewide_27915 %>% filter(election_lbl
                                                   == "11/03/2020")


nc_stats_2020 <- nc_voter_2020 %>% group_by(voted_party_cd) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count))

nc_stats_2020$year <- 2020

###### 2016 #########

nc_voter_2016 <- ncvhis_Statewide_27915 %>% filter(election_lbl
                                                   == "11/08/2016")


nc_stats_2016 <- nc_voter_2016 %>% group_by(voted_party_cd) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count))

nc_stats_2016$year <- 2016


##### 2014 ######

nc_voter_2014 <- ncvhis_Statewide_27915 %>% filter(election_lbl=="11/04/2014")


nc_stats_2014 <- nc_voter_2014 %>% group_by(voted_party_cd) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count))

nc_stats_2014$year <- 2014



table(nc_voter_2014$voted_party_cd)
table(nc_voter_2016$voted_party_cd)
table(nc_voter_2020$voted_party_cd)
table(nc_27915$party_cd)



nc_stats_all <- rbind(nc_stats_2014, nc_stats_2016)

nc_stats_all <- rbind(nc_stats_all, nc_stats_2020)

nc_stats_all <- rbind(nc_stats_all, nc_stats_2024)

nc_stats_all$location <- "R"

library(ggplot2)


ggplot(nc_stats_all, aes(year, perc, group = voted_party_cd, col = voted_party_cd)) +
  geom_line() + theme_bw() + ylim(0,1)

########## using other zipcode ######



nc_28202_all <- ncvoter_Statewide %>% filter(zip_code == 28202)
nc_28202 <- nc_28202_all %>% filter(ward_abbrv == 1)
ncid_list_28202 <- nc_28202$ncid

ncvhis_Statewide_28202 <- ncvhis_Statewide %>% filter(ncid %in% ncid_list_28202)


participation_28202 <- ncvhis_Statewide_28202 %>%group_by(election_lbl) %>%
  count() %>% arrange(by_group = n)




nc_stats_2024_1 <- nc_28202 %>% group_by(party_cd) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count))

nc_stats_2024_1$year <- 2024


nc_stats_2024_1 <- nc_stats_2024_1 %>% rename("voted_party_cd" = party_cd)

##### 2020

nc_voter_2020_1 <- ncvhis_Statewide_28202 %>% filter(election_lbl
                                                     == "11/03/2020")


nc_stats_2020_1 <- nc_voter_2020_1 %>% group_by(voted_party_cd) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count))

nc_stats_2020_1$year <- 2020


###### 2016 #########

nc_voter_2016_1 <- ncvhis_Statewide_28202 %>% filter(election_lbl
                                                     == "11/08/2016")
nc_stats_2016_1 <- nc_voter_2016_1 %>% group_by(voted_party_cd) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count))

nc_stats_2016_1$year <- 2016


##### 2014 ######

nc_voter_2014_1 <- ncvhis_Statewide_28202 %>% filter(election_lbl=="11/04/2014")


nc_stats_2014_1 <- nc_voter_2014_1 %>% group_by(voted_party_cd) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count))

nc_stats_2014_1$year <- 2014



nc_stats_all_1 <- rbind(nc_stats_2014_1, nc_stats_2016_1)

nc_stats_all_1 <- rbind(nc_stats_all_1, nc_stats_2020_1)

nc_stats_all_1 <- rbind(nc_stats_all_1, nc_stats_2024_1)


nc_stats_all_1$location <- "U"


nc_stats <- rbind(nc_stats_all, nc_stats_all_1)



library(ggplot2)


ggplot(nc_stats, aes(year, perc, group = voted_party_cd, col = voted_party_cd)) +
  geom_line() + scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple", "GRE" = "darkgreen", "LIB" = "yellow")) +
  facet_wrap(~location)+
  theme_bw() + ylim(0,1)




#############################################
##### plotting average number of neighbors ##
##############################################

# merging the zipcodes and their number of neighbors 

load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/neighbors_28203.RData")
load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/neighbors_27915.RData")


# Select relevant columns for plotting
plot_data <- rbind(neighbors_27915, neighbors_28203)

# Plot the data
ggplot(plot_data, aes(x = location, y = num_neighbors)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  theme_bw() +
  labs(x = "Environment",
       y = "Number of Neighbors")


ggsave("number_neighbors.png", dpi = 95)
# proximity threshold < 500m



ggplot(plot_data, aes(x = location, y = num_neighbors, fill = location)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  theme_bw() +
  labs(title = "Number of Neighbors in Urban vs Rural Environments",
       x = "Environment",
       y = "Number of Neighbors") +
  scale_fill_manual(values = c("Urban" = "skyblue", "Rural" = "lightgreen"))


ggplot(plot_data, aes(x = num_neighbors)) +
  geom_density(alpha = 0.5, fill = "skyblue") +
  theme_bw() +
  labs(title = "Density Plot of Number of Neighbors",
       x = "Number of Neighbors",
       y = "Density") +
  facet_wrap(~ location)


ggplot(plot_data, aes(x = num_neighbors, fill = location)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  theme_bw() +
  labs(title = "Histogram of Number of Neighbors",
       x = "Number of Neighbors",
       y = "Count") +
  scale_fill_manual(values = c("Urban" = "skyblue", "Rural" = "lightgreen"))




#############################################
####### Visualizing results of ABM ##########
#############################################

# Plot for individual vote probability changes over time

ggplot(interactions_epoch_2020, aes(x = epoch, y = vote_prob, color = agent, group = factor(agent))) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs",
       x = "Epoch",
       y = "Voting Probability") +
  facet_wrap(~party)+
  ylim(0,1)+
  theme_minimal()


# Plot individual agents changes over time 

ggplot(interactions_conf1_agent1_2020, aes(x = epoch, y = vote_prob, color = party, group = party)) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs for Agent 2",
       x = "Epoch",
       y = "Voting Probability") +
  theme_minimal()
######## Plot changes in party vote probabilities across epochs

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




##### Plot overall vote probability distribution at the first, middle and final epoch

final_epoch <- max(plot_data_2020$epoch)
middle_epoch <- round(median(plot_data_2020$epoch))
first_epoch <- min(plot_data_2020$epoch)

plot_start <- interactions_epoch_2020 %>%
  filter(epoch == first_epoch) %>%
  ggplot(aes(x = vote_prob, fill = party)) +
  geom_histogram(position = 'stack', alpha = 0.6, bins = 30) +
  labs(title = 'Vote Probability Distribution at First Epoch', x = 'Vote Probability', y = 'Count', fill = 'Party') +
  theme_minimal()


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






####### Visualize Number of Interactions per agent and epoch:

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





###### Network Visualization of both Zipcodes and their interactions


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