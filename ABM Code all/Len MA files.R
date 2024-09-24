

install.packages("dplyr")
install.packages("FNN")
install.packages("tidyr")


library(dplyr)
library(FNN) 
library(tidyr)



load("~/Desktop/coords_adress_28203_2016.RData")
load("~/Desktop/distmat_28203_2016.RData")
load("~/Desktop/geo_nc_28203_abm_small.RData")

k <- 200  # Number of nearest neighbors
coords_28203 <- coords_adress_28203 %>% dplyr::select(lon, lat)
# Find nearest neighbors
knn_28203 <- get.knn(coords_28203, k)

proxmat2_28203 <- exp(-distmat_28203/100)




### Exposure Calculation #####

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




############## Function #############


uupdateVoteProbability_15_urban <- function(agents, proximity_matrix, mu, total_attributes, epochs, proximity_threshold, p, C, knn_matrix, lambda) {
  
  num_agents <- nrow(agents)
  
  # Convert to data.table for efficiency
  setDT(agents)
  interactions <- data.table(epoch = integer(), ncid = integer(), agent = integer(), alter = integer(), party = character(), vote_prob = numeric(), delta_P = numeric())
  interactions_epoch <- data.table(epoch = integer(),ncid = integer(), agent = integer(), party = character(), vote_prob = numeric(), voted_party_cd = character())
  blau_indices <- data.table(epoch = integer(), blau_index = numeric())
  
  # Precompute potential alters for each agent
  potential_alters_list <- lapply(1:num_agents, function(ego) {
    which(proximity_matrix[ego, ] > proximity_threshold & proximity_matrix[ego, ] < 1)
  })
  
  for (epoch in 1:epochs) {
    # Print statement for monitoring epochs
    cat("Epoch:", epoch, "\n")
    
    # Update exposure values at the start of each epoch
    exposure_values <- calcExposurePartisanship(agents, proximity_matrix, proximity_threshold)
    agents[, `:=`(
      exposureIngroup = exposure_values$exposureIngroup,
      exposureOutgroup = exposure_values$exposureOutgroup,
      party_majority = exposure_values$party_majority
    )]
    
    # Calculate Blau Index for each agent using k-NN
    party <- agents$voted_party_cd
    agents[, Blau_party := {
      neighbors_indices <- knn_matrix$nn.index[.I, ]
      neighbors_parties <- party[neighbors_indices]
      party_proportions <- table(neighbors_parties) / length(neighbors_indices)
      party_proportions <- as.numeric(party_proportions)
      1 - sum(party_proportions^2)
    }, by = 1:num_agents]
    
    agents[, `:=`(age_binned_factor = as.factor(age_binned),
                  race_code_factor = as.factor(race_code),
                  gender_code_factor = as.factor(gender_code))]
    
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
            
            dem_sim <- sum(
              agents$age_binned_factor[ego] == agents$age_binned_factor[alter], 
              agents$race_code_factor[ego] == agents$race_code_factor[alter], 
              agents$gender_code_factor[ego] == agents$gender_code_factor[alter]
            ) / total_attributes
            
            opin_attrs <- c("DEM_prob", "UNA_prob", "LIB_prob", "GRE_prob", "REP_prob")
            opin_diffs <- abs(agents[ego, opin_attrs, with = FALSE] - agents[alter, opin_attrs, with = FALSE])
            opin_sim <- sum(1 - opin_diffs) / length(opin_attrs)
            
            sim_ij <- (dem_sim + opin_sim) / 2
            
            if (agents$Blau_party[ego] <= 0.3) {
              sim_ij <- sim_ij * C
            }
            
            delta_P <- if (sim_ij < 0.6) {
              0
            } else {
              if (agents$voted_party_cd[alter] == "UNA" |agents$voted_party_cd[ego] == "UNA") {
                0
              } else {
                if (party_diff == 1) {
                  if (agents$voted_party_cd[alter] == agents$party_majority[ego] & agents$exposureOutgroup[ego] > 0.5) {
                    -mu * sim_ij * p
                  } else {
                    -mu * sim_ij
                  }
                } else {
                  mu * sim_ij
                }
              }
            }
            
            # Calculate adjusted delta_P considering the stubbornness of ego and alter
            delta_P_ego <- delta_P *  exp(-agents$st_stubbornness[ego])
            delta_P_alter <- delta_P * exp(-agents$st_stubbornness[alter])
            
            #delta_P_ego <- delta_P
            #delta_P_alter <- delta_P 
            delta_P_una <- mu * sim_ij /4
            
            ego_party_col <- paste0(agents$voted_party_cd[ego], "_prob")
            alter_party_col <- paste0(agents$voted_party_cd[alter], "_prob")
            
            agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P_ego
            agents[[alter_party_col]][ego] <- agents[[alter_party_col]][ego] - delta_P_ego
            
            agents[[ego_party_col]][alter] <- agents[[ego_party_col]][alter] - delta_P_alter
            agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P_alter
            
            # Increase UNA probability if party_diff == 1 and the other agent's party is REP
            if (party_diff == 1) {
              if (agents$voted_party_cd[ego] == "DEM" && agents$voted_party_cd[alter] == "REP") {
                agents[["UNA_prob"]][ego] <- agents[["UNA_prob"]][ego] + delta_P_una
                agents[["UNA_prob"]][alter] <- agents[["UNA_prob"]][alter] + delta_P_una
                
              } else if (agents$voted_party_cd[ego] == "REP" && agents$voted_party_cd[alter] == "DEM") {
                agents[["UNA_prob"]][alter] <- agents[["UNA_prob"]][alter] + delta_P_una
                agents[["UNA_prob"]][ego] <- agents[["UNA_prob"]][ego] + delta_P_una
                
              }
            }
            
            interactions <- rbind(interactions, data.table(
              epoch = epoch,
              ncid = agents$ncid[ego],
              agent = ego,
              alter = alter,
              party = agents$voted_party_cd[ego],
              vote_prob = agents[[paste0(agents$voted_party_cd[ego], "_prob")]][ego],
              delta_P = delta_P_ego
            ))
            
            # Determine the party with the highest probability for both ego and alter
            ego_party_probs <- unlist(agents[ego, .(DEM_prob, UNA_prob, REP_prob)])
            ego_party_index <- which.max(ego_party_probs)
            ego_party <- c("DEM", "UNA", "REP")[ego_party_index]
            
            alter_party_probs <- unlist(agents[alter, .(DEM_prob, UNA_prob, REP_prob)])
            alter_party_index <- which.max(alter_party_probs)
            alter_party <- c("DEM", "UNA", "REP")[alter_party_index]
            
            agents$voted_party_cd[ego] <- ego_party
            agents$voted_party_cd[alter] <- alter_party
          }
        }
      }
    }
    
    # Log voting probabilities and party preferences after each epoch
    for (agent in 1:num_agents) {
      party_prob_cols <- grep("_prob$", colnames(agents), value = TRUE)
      for (party_col in party_prob_cols) {
        alter_party_cd <- NA  # Initialize with NA if no interaction
        # Get the alter's party if any interactions happened
        if (agent %in% interactions$agent) {
          alter_party_cd <- interactions$party[interactions$agent == agent & interactions$epoch == epoch]
        }
        interactions_epoch <- rbind(interactions_epoch, data.table(
          epoch = epoch,
          ncid = agents$ncid[ego],
          agent = agent,
          party = gsub("_prob", "", party_col),
          vote_prob = agents[[party_col]][agent],
          voted_party_cd = agents$voted_party_cd[agent]
        ), fill = TRUE)
      }
    }
    
    # Calculate and store Blau Index for the entire neighborhood after each epoch
    blau_index_epoch <- agents[, mean(Blau_party, na.rm = TRUE)]
    blau_indices <- rbind(blau_indices, data.table(epoch = epoch, blau_index = blau_index_epoch))
    
    # Plot after every 10 epochs
    if (epoch %% 10 == 0) {
      plot_current <- interactions_epoch[epoch == epoch] %>%
        ggplot(aes(x = vote_prob, fill = party)) +
        geom_histogram(position = 'stack', alpha = 0.6, bins = 30) +
        labs(title = paste('Vote Probability Distribution at Epoch', epoch), x = 'Vote Probability', y = 'Count', fill = 'Party') +
        theme_minimal()
      print(plot_current)
    }
  }
  
  return(list(interactions = interactions, interactions_epoch = interactions_epoch, blau_indices = blau_indices))
}





interaction_urban_with_rural_parameter <- uupdateVoteProbability_15_urban(geo_nc_28203_abm_small, proxmat2_28203, 
                                                                     mu = 0.005, total_attributes = 3, epochs = 400, 
                                                                     proximity_threshold = 0.05, p = 1.5, 
                                                                     C = 1.1,  knn_28203,  lambda = 1.7)
