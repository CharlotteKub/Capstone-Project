

###### test test test


load("~/Desktop/Data Test 27915/distmat_27915_2020.RData")
load("~/Desktop/Data Test 27915/geo_nc_27915.RData")
load("~/Desktop/Data Test 27915/geo_nc_27915_blau.RData")
load("~/Desktop/Data Test 27915/geo_nc_27915_df.RData")





ncid_list_27915 <- geo_nc_27915_blau$ncid
ncvhis_Statewide_27915 <- ncvhis_Statewide %>% filter(ncid %in% ncid_list_27915)

geo_nc_27915_abm <- ncvhis_Statewide_27915 %>% filter(election_lbl < "2021-11-02")

table(geo_nc_27915_abm$voted_party_cd)


geo_nc_27915_abm <- geo_nc_27915_abm %>% 
  group_by(ncid) %>%
  mutate(DEM_prob = sum(voted_party_cd == "DEM")/length(ncid)) %>%
  mutate(UNA_prob = sum(voted_party_cd == "UNA")/length(ncid)) %>%
  mutate(LIB_prob = sum(voted_party_cd == "LIB")/length(ncid)) %>%
  mutate(GRE_prob = sum(voted_party_cd == "GRE")/length(ncid)) %>%
  mutate(REP_prob = sum(voted_party_cd == "REP")/length(ncid))


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


geo_nc_27915_blau_small <- geo_nc_27915_blau %>% dplyr::select(ncid, Blau_race, Blau_party, age_binned, race_code, gender_code, grid_cell_id)

geo_nc_27915_blau_abm <- left_join(geo_nc_27915_abm, geo_nc_27915_blau_small, by = "ncid")

geo_nc_27915_blau_abm <- geo_nc_27915_blau_abm %>% filter(election_lbl == "11/03/2020")

ncells_27915 <- length(geo_nc_27915_blau_abm)
proxmat1_27915 <- matrix(NA, nrow=ncells_27915, ncol=ncells_27915)
proxmat1_27915 <- exp(-distmat_27915/100) 



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

interaction_7 <- updateVoteProbability_7(geo_nc_27915_blau_abm, proxmat1_27915, mu = 0.001, total_attributes = 3, epochs = 100, proximity_threshold = 0.000000001)


interaction_7_1 <- interaction_7[[2]]
interaction_7_2 <- interaction_7[[1]]



interaction_7_2_agent1 <- interaction_7_2 %>% filter(agent == 1)
interaction_7_1_agent1 <- interaction_7_1 %>% filter(agent == 1)



# Plot for individual vote probability changes over time

ggplot(interaction_7_1, aes(x = epoch, y = vote_prob, color = agent, group = factor(agent))) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs",
       x = "Epoch",
       y = "Voting Probability") +
  facet_wrap(~party)+
  ylim(0,1)+
  theme_minimal()


ggplot(interaction_7_1_agent1, aes(x = epoch, y = vote_prob, color = party, group = party)) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs for Agent 1",
       x = "Epoch",
       y = "Voting Probability") +
  theme_minimal()





#### what to do tomorrow: calculate party out group exposure for new penalty parameter by grid cell id /
# // or by whole zipcode????
#### update the function accordingly
#### check if code correctly updates the blau index and the exposure dynamically


## implement penalty: based on exposure to outgroup for ego: if alter has party affiliation of majority in neighborhood,
## then calculate exposure to outgroup  and if that is higher than 0.6 or smth,
## implement some penalty value that so that influence weight is even stronger for ego  



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

# Example run


geo_nc_27915_blau_abm_test <- geo_nc_27915_blau_abm
exposure <- calcExposurePartisanship(geo_nc_27915_blau_abm, proxmat1_27915, proximity_threshold = 0.05)
names(exposure) <- c("ncid", "exposureIngroup", "exposureOutgroup", "party_majority")

geo_nc_27915_blau_abm <- geo_nc_27915_blau_abm [,-30:-32]
geo_nc_27915_blau_abm <- left_join(geo_nc_27915_blau_abm, exposure, by = "ncid")


###########################################################



geo_nc_test <- geo_nc_27915_blau_abm
geo_nc_test$party_majority
# Iterate over each agent (i) and each other agent (j).

num_agents <- nrow(geo_nc_test)
proximity_threshold = 0.05
party_majority_list <- vector("list", num_agents)

for (i in 1:num_agents) {
  # Filter agents within the proximity threshold
  potential_alters <- which(proxmat1_27915[i, ] > proximity_threshold & proxmat1_27915[i, ] < 1)
  
  if (length(potential_alters) > 0) {
    # Initialize counters for each party
    party_counts <- table(factor(geo_nc_test$voted_party_cd[potential_alters], levels = unique(geo_nc_test$voted_party_cd)))
    
    # Determine the majority party within proximity
    majority_party <- names(which.max(party_counts))
    geo_nc_test$party_majority[i] <- ifelse(length(majority_party) > 0, majority_party, "NA")
    
    # Store the party counts in the list
    party_majority_list[[i]] <- party_counts
  } else {
    # If no potential alters, set the party majority to UNA
    geo_nc_test$party_majority[i] <- "NA"
    party_majority_list[[i]] <- table(factor("NA", levels = unique(geo_nc_test$voted_party_cd)))
  }
}

# Convert the list to a dataframe (optional, for further analysis)
party_majority_df <- do.call(rbind, lapply(party_majority_list, as.data.frame))


updateInteractionPartnersAndMajorityParty <- function(agents, proxmat, proximity_threshold) {
  num_agents <- nrow(agents)
  
  # Initialize new columns in agents dataframe
  agents$interaction_partners <- numeric(num_agents)
  agents$party_majority <- character(num_agents)
  
  for (i in 1:num_agents) {
    # Filter agents within the proximity threshold
    potential_alters <- which(proxmat[i, ] > proximity_threshold & proxmat[i, ] < 1)
    
    # Store the number of interaction partners
    agents$interaction_partners[i] <- length(potential_alters)
    
    if (length(potential_alters) > 0) {
      # Initialize counters for each party
      party_counts <- table(factor(agents$voted_party_cd[potential_alters], levels = unique(agents$voted_party_cd)))
      
      # Determine the majority party within proximity
      majority_party <- names(which.max(party_counts))
      agents$party_majority[i] <- ifelse(length(majority_party) > 0, majority_party, NA)
    } else {
      # If no potential alters, set the party majority to NA
      agents$party_majority[i] <- NA
    }
  }
  
  return(agents)
}

# Example usage
updated_agents <- updateInteractionPartnersAndMajorityParty(geo_nc_27915_blau_abm, proxmat1_27915, proximity_threshold=0.05)

# Check the result
table(updated_agents$party_majority)


###################################################

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

# Example usage for 27915
interaction_8 <- updateVoteProbability_8(geo_nc_27915_blau_abm, proxmat1_27915, mu = 0.001, total_attributes = 3, epochs = 100, proximity_threshold = 0.05, p = 1.5)




# Example usage for 28202
interaction_7_28202 <- updateVoteProbability_7(geo_nc_28202_blau_abm, proxmat2_28202, mu = 0.001, total_attributes = 3, epochs = 100, proximity_threshold = 0.05)





######################################################################################

## regressions of voter switches for both df merged and identify factors (or maybe even for all years together????) using ML 


nc_28202_2024$location <- 1

nc_27915_2024$location <- 0

nc_2024_merged <- rbind(nc_28202_2024, nc_27915_2024)



## regression

glm(party_change ~location + race_code + gender_code+ age + st_stubbornness, data = nc_2024_merged)


## using ML?


library(glmnet)
library(caret)
library(randomForest)
library(rpart) 
library(vip)


Y_2024 <-  nc_2024_merged$party_change
X_2024_df <- nc_2024_merged %>% dplyr::select(-party_change, - voted_county_desc, -voted_party_desc, -vtd_label, -vtd_description,
                                             -pct_label, -pct_description, -election_desc, -voter_reg_num, -county_id, -county_id)

X_2024 <- as.matrix(X_2024_df)


glmnet::cv.glmnet(X_2024, Y_2024, alpha = 1)
glmnet(X_2024, Y_2024, alpha = 0)

??rpart()
tree_2024 <- rpart(Y_2024~., data=X_2024_df)
plot(tree_2024$variable.importance)

vip(tree_2024)






