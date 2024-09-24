
# utilities



###################################################
########## Similarity between Agents ##############
###################################################


dem_sim <- sum(
  agents$age_binned_factor[ego] == agents$age_binned_factor[alter], 
  agents$race_code_factor[ego] == agents$race_code_factor[alter], 
  agents$gender_code_factor[ego] == agents$gender_code_factor[alter]
) / total_attributes

opin_attrs <- c("DEM_prob", "UNA_prob", "LIB_prob", "GRE_prob", "REP_prob")
opin_diffs <- abs(agents[ego, opin_attrs, with = FALSE] - agents[alter, opin_attrs, with = FALSE])
opin_sim <- sum(1 - opin_diffs) / length(opin_attrs)

sim_ij <- (dem_sim + opin_sim) / 2





###################################################
## Compute number of interactions and interaction partners
###################################################

for (ego in 1:num_agents) {
  # Randomly assign the number of interactions for this agent for this day using a Poisson distribution
  num_interactions <- rpois(1, lambda = lambda)
  num_interactions <- min(num_interactions, 7)  # Ensure the value does not exceed 7
  
  # If no interactions, skip to the next agent
  if (num_interactions == 0) next
  
  potential_alters <- potential_alters_list[[ego]]
  
  if (length(potential_alters) > 0) {
    for (interaction in 1:num_interactions) {
      alter <- sample(potential_alters, 1)
      
      if (ego != alter) {
        interaction_prob <- proximity_matrix[ego, alter]
        party_diff <- ifelse(agents$voted_party_cd[ego] == agents$voted_party_cd[alter], 0, 1)
      }
      }
    }
  }



###################################################
# Calculate Blau Index for each agent using k-NN #
###################################################


party <- agents$voted_party_cd  # Extract party affiliation
agents$Blau_party <- NA 

for (i in 1:num_agents) {
  neighbors_indices <- knn_matrix$nn.index[i, ]
  neighbors_parties <- party[neighbors_indices]
  
  party_proportions <- table(neighbors_parties) / length(neighbors_indices)
  party_proportions <- as.numeric(party_proportions)  # Convert to numeric
  
  blau_index <- 1 - sum(party_proportions^2)
  agents$Blau_party[i] <- blau_index
}



###################################################
###### Calculate Partisan Outgroup Exposure #######
###################################################

# Function to calculate exposure to partisanship and majority party within proximity
calcExposurePartisanship <- function(agents, proxmat, proximity_threshold) {
  num_agents <- nrow(proxmat)
  
  # Initialize columns for exposure values and majority party
  exposureIngroup <- numeric(num_agents)
  exposureOutgroup <- numeric(num_agents)
  party_majority <- character(num_agents)
  
  # Initialize matrices to store exposure values for same and different parties.
  eSameParty <- matrix(NA, nrow = num_agents, ncol = num_agents)
  eDiffParty <- matrix(NA, nrow = num_agents, ncol = num_agents)
  eSameParty_adj <- matrix(NA, nrow = num_agents, ncol = num_agents)
  eDiffParty_adj <- matrix(NA, nrow = num_agents, ncol = num_agents)
  
  # Iterate over each agent (i) and each other agent (j).
  for (i in 1:num_agents) {
    # Filter agents within the proximity threshold
    potential_alters <- which(proxmat[i, ] > proximity_threshold & proxmat[i, ] < 1)
    
    # Initialize counters for each party
    party_counts <- table(factor(agents$voted_party_cd[potential_alters], levels = unique(agents$voted_party_cd)))
    
    # Determine the majority party within proximity
    majority_party <- names(which.max(party_counts))
    party_majority[i] <- ifelse(length(majority_party) > 0, majority_party, NA)
    
    for (j in potential_alters) { 
      # Determine if agents i and j belong to the same party or different party (excluding unaffiliated agents).
      same_party <- agents$voted_party_cd[j] == agents$voted_party_cd[i]
      diff_party <- agents$voted_party_cd[j] != agents$voted_party_cd[i]
      
      # Calculate exposure values for same-party and different-party based on proximity.
      eSameParty[i, j] <- proxmat[i, j] * same_party
      eDiffParty[i, j] <- proxmat[i, j] * diff_party
      
      # Adjust for in-group exposure by ensuring no self-influence.
      if (i == j) {
        eSameParty_adj[i, j] <- proxmat[i, j] * (same_party - 1)
        eDiffParty_adj[i, j] <- proxmat[i, j] * (diff_party - 1)
      } else {
        eSameParty_adj[i, j] <- eSameParty[i, j]
        eDiffParty_adj[i, j] <- eDiffParty[i, j]
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
  eSameParty_egoSameParty <- eSameParty_adj / matrix(sums_egoSameParty, nrow = num_agents, ncol = num_agents)
  eSameParty_egoDiffParty <- eSameParty / matrix(sums_egoDiffParty, nrow = num_agents, ncol = num_agents)
  eDiffParty_egoSameParty <- eDiffParty / matrix(sums_egoSameParty, nrow = num_agents, ncol = num_agents)
  eDiffParty_egoDiffParty <- eDiffParty_adj / matrix(sums_egoDiffParty, nrow = num_agents, ncol = num_agents)
  
  # Calculate final exposure values by summing the rows of the normalized matrices.
  vSameParty_egoSameParty <- rowSums(eSameParty_egoSameParty, na.rm = TRUE)
  vSameParty_egoDiffParty <- rowSums(eSameParty_egoDiffParty, na.rm = TRUE)
  vDiffParty_egoSameParty <- rowSums(eDiffParty_egoSameParty, na.rm = TRUE)
  vDiffParty_egoDiffParty <- rowSums(eDiffParty_egoDiffParty, na.rm = TRUE)
  
  # Assign calculated exposure values to each agent.
  for (i in 1:num_agents) {
    if (agents$voted_party_cd[i] != "UNA") {
      exposureIngroup[i] <- vSameParty_egoSameParty[i]
      exposureOutgroup[i] <- vDiffParty_egoSameParty[i]
    } else {
      exposureIngroup[i] <- vSameParty_egoSameParty[i]
      exposureOutgroup[i] <- vDiffParty_egoSameParty[i]
    }
  }
  
  return(data.frame(
    ncid = agents$ncid,
    exposureIngroup = exposureIngroup,
    exposureOutgroup = exposureOutgroup,
    party_majority = party_majority
  ))
}


#### Example usage 
geo_nc_27915_blau_abm_test <- geo_nc_27915_blau_abm
exposure <- calcExposurePartisanship(geo_nc_27915_blau_abm, proxmat1_27915, proximity_threshold = 0.05)
names(exposure) <- c("ncid", "exposureIngroup", "exposureOutgroup", "party_majority")






#########################################################
############# RUCA Codes for US #########################
#########################################################

#### loading RUCA codes for zipcodes

RUCA_US <- readxl::read_xlsx("~/Desktop/RUCA_US.xlsx")

RUCA_NC <- RUCA_US %>% filter(STATE=="NC") %>% 
  filter(ZIP_TYPE=="Zip Code Area") %>%
  filter(RUCA1 == 10)



RUCA_NC_zip <- RUCA_NC$ZIP_CODE

ncvoter_zips <- ncvoter_Statewide %>% filter(zip_code %in% RUCA_NC_zip)


ncvoter_charlotte <- ncvoter_zips %>% filter(mail_city == "CHARLOTTE")

RUCA_NC <- RUCA_NC %>% rename("zip_code" = "ZIP_CODE")

RUCA_NC_charlotte <- left_join(RUCA_NC, ncvoter_charlotte, by = "zip_code")


rural_charlotte <- RUCA_NC_charlotte %>% filter(RUCA1 == 10)
rural_charlotte <- RUCA_NC_charlotte$zip_code[RUCA_NC_charlotte$RUCA1 == 10]

rural_charlotte <- unique(rural_charlotte)


rural_numbers <- rural_charlotte %>% group_by(zip_code) %>%
  count()

nc_numbers_charlotte <- ncvoter_charlotte %>% group_by(zip_code) %>%
  count()


ggplot(nc_numbers_charlotte, aes(x = zip_code, y=n)) + 
  geom_col(position = "identity", fill = "darkblue", alpha = 0.5)



### using 28203 zipcode in Charlotte as most urban

nc_28203 <- ncvoter_Statewide %>% filter(zip_code == 28203)
nc28203_precinct <- nc_28203 %>% filter(precinct_abbrv == "010")

# smallest level of data for city 

table(nc_28203$party_cd)
table(nc28203_precinct$party_cd)




## using 27915 as the most rural district in Charlotte

nc_27915 <- ncvoter_Statewide %>% filter(zip_code == 27915)
table(nc_27915$party_cd)


################################
#### Using PEW Data on 2018 ####
################################


pew_2018 <- readr::read_csv("~/Desktop/LSE Term 2/Capstone-Project/Data/PEW Data 2018/31103019.csv")


## urban/ rural  

table(pew_2018$COMTYPE2_INS_W32)


# neighbors 
table(pew_2018$NEIGHINTERA_W32)

table(pew_2018$NEIGHKNOW_W32)

  
str(pew_2018$NEIGHINTERA_W32)


pew_2018 <- pew_2018 %>% filter(NEIGHINTERA_W32 != "Refused") %>%
  filter(COMTYPE2_W32 !="Refused")





pew_2018$NEIGHINTERA_fac <- factor(pew_2018$NEIGHINTERA_W32,
                                   levels =c("Never","Less than once a month", 
                                             "About once a month",
                                             "About once a week", 
                                             "Several times a week", 
                                             "Every day"))

table(pew_2018$NEIGHINTERA_fac)



neighborhood_interaction <- pew_2018 %>% group_by(COMTYPE2_W32) %>% 
  count(NEIGHINTERA_fac) %>%
  mutate(st_n = n/length(COMTYPE2_W32))
  


library(ggplot2)
plot_neighbors <- neighborhood_interaction %>% 
  ggplot(aes(x = NEIGHINTERA_fac, y =st_n, fill = NEIGHINTERA_fac)) +
  geom_col(position = 'identity', alpha = 0.8) +
  facet_wrap(~COMTYPE2_W32)+
  scale_fill_brewer(palette = "Paired")+
  labs(title = 'Neighborhood Interaction Histogram', x = 'Number of Interactions', y = 'Count', fill = "Number of Interactions") +
  theme_minimal()


ggsave("plot_interaction_neighbors.png", dpi = 95)

##### quantifying character values of interactions


interaction_mapping <- c("Never" = 0,
                         "Less than once a month" = 0.5,
                         "About once a month" = 1,
                         "About once a week" = 4,
                         "Several times a week" = 12,
                         "Every day" = 30)

# Apply the mapping to the dataset
pew_2018 <- pew_2018 %>%
  mutate(NEIGHINTERA_quantified = interaction_mapping[as.character(NEIGHINTERA_W32)])





neighborhood_interaction_quantified <- pew_2018 %>% group_by(COMTYPE2_W32) %>%
  summarize(mean_interaction = mean(NEIGHINTERA_quantified))



#### using a regression 

lm(NEIGHINTERA_quantified ~ COMTYPE2_W32, data =pew_2018)




### maybe making argument that more interactions but less influence as less trust and knowledge of neighbors



neighborhood_interaction_quantified %>% 
  ggplot(aes(x = COMTYPE2_W32, y =mean_interaction, fill = COMTYPE2_W32)) +
  geom_col(position = 'identity', alpha = 0.8, width = 0.3) +
  geom_text(aes(label = round(mean_interaction, 2)), vjust = -0.3) + # Add text labels
  scale_fill_brewer(palette = "Paired")+
  ylim(0,10) +
  labs(title = 'Mean Number of interaction per Month', x = 'Area of Residence', y = 'Mean Number', fill = "Area of Residence") +
  theme_bw()


ggsave("neighbor_interaction_context.png", dpi = 95) 



################################
############ BES ###############
################################


BES_2019 <- haven::read_dta("~/Desktop/LSE Term 2/Capstone-Project/Data/BES_2019.dta")

table(BES_2019$u01)

BES_2019$LA_UA_Code




# upload urban/ rural classification

RUCA_UK <- readxl::read_xls("~/Desktop/LSE Term 2/Capstone-Project/Data/RUCA_UK.xls")

RUCA_uk_small <- RUCA_UK %>% dplyr::select(LA_UA_Code, RUC11CD, RUC11)

BES_2019 <- left_join(BES_2019, RUCA_uk_small, by = "LA_UA_Code")

BES_2019 <- BES_2019 %>% mutate(urban_rural = case_when(RUC11CD < 3 ~ 1,
                                                        RUC11CD == 3 ~2,
                                                        RUC11CD >3 ~ 3))




table(BES_2019$RUC11CD)
table(BES_2019$RUC11)
table(BES_2019$urban_rural)

BES_2019$u01[BES_2019$u01 == -1] <- NA



BES_2019_politics <- BES_2019 %>% dplyr::select(LA_UA_Code, urban_rural,u01)

BES_2019_politics <- na.omit(BES_2019_politics)

talk_politics <- BES_2019_politics %>% group_by(urban_rural) %>%
  count(u01)

talk_politics$urban_rural

talk_politics %>% ggplot(aes(x = u01, y = n)) +
  geom_col(position = 'identity', alpha = 0.6, fill = "darkblue")+
  facet_wrap(~urban_rural)


talk_politics_mean <- BES_2019_politics %>% group_by(urban_rural) %>%
  summarize(mean_talk = mean(u01))



################################
########### ANES ##############
################################
ANES <- read_csv("~/Desktop/LSE Term 2/Capstone-Project/Data/anes_timeseries_2020_csv_20220210/anes_timeseries_2020.csv")


ANES$V202023[ANES$V202023 < 0] <- NA
ANES$V202355[ANES$V202355 < 0] <- NA


table(ANES$V202023)

# Rural/Urban
table(ANES$V202355)

# 1= Rural
# 2 = small town
# 3 = suburb
# 4 = urban


ANES_small <- ANES %>% dplyr::select(V202023,V202355)
ANES_small <- na.omit(ANES_small)

Anes_talk <- ANES_small %>% group_by(V202355) %>%
  count(V202023)

# V202023 = POST: HOW MANY DAYS IN PAST WEEK DISCUSSED POLITICS WITH FAMILY OR FRIENDS

urban_rural_Anes <- (c("1" = "Rural", "2" = "Small Town",
                     "3" = "Suburb", "4" = "Urban"))


Anes_talk %>% ggplot(aes(x=V202023, y=n)) +
  geom_col(position = "identity", alpha = 0.6, fill = "darkblue") +
  facet_wrap(~V202355, labeller = as_labeller(urban_rural_Anes)) +
  labs(title = "How many day in past week discussed Politics with Family or Friends?", y = "Number of People", 
       x = "Number of Days") +
  theme_bw()

ggsave("ANES_talk_politics.png", dpi = 95)


ANES_talk_mean <- ANES_small %>% group_by(V202355) %>%
  summarize(mean_talk = mean(V202023))


ANES_talk_mean %>% ggplot(aes(x=V202355, y=mean_talk, fill = as.factor(V202355))) +
  geom_col(position = "identity", alpha = 0.8) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "cornflowerblue", "3" = "yellowgreen", "4" = "seagreen"), labels = urban_rural_Anes) +
  labs(title = "How many day in past week discussed Politics with Family or Friends?", y = "Number of People", 
       x = "Context", fill = "Context") +
  ylim(0,8)+ theme_bw()


ggsave("Anes_talk_mean.png", dpi = 95)

## South Bend Study

X06522_0007_Data <- read_por("Desktop/ICPSR_06522 3/DS0007/06522-0007-Data.por")

