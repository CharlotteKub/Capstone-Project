

##########################################################################
######## Without using sigmoid function to bound values between 0 and 1
##########################################################################


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



updateVoteProbability_3_test <- function(agents, proximity_matrix, alpha, beta, total_attributes, epochs, proximity_threshold) {
  
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
                agents[[ego_party_col]][ego] <- agents[[ego_party_col]][ego] + delta_P
                agents[[alter_party_col]][ego] <- agents[[alter_party_col]][ego] - delta_P
                
                # Update the voting probability for alter
                agents[[ego_party_col]][alter] <- agents[[ego_party_col]][alter] - delta_P
                agents[[alter_party_col]][alter] <- agents[[alter_party_col]][alter] + delta_P
                
              } else {
                # Same party: increase probability
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
interactions_test <-updateVoteProbability_3_test(geo_nc_2755_test, proxmat1, alpha = 0.001, beta = 0.001, total_attributes, epochs = 50, proximity_threshold)


interactions_test1 <- interactions_test[[2]]
interactions_test2 <- interactions_test[[1]]

interactions_test2_agent1 <- interactions_test2 %>% filter(agent == 1)
interactions_test1_agent1 <- interactions_test1 %>% filter(agent == 1)


ggplot(interactions_test1, aes(x = epoch, y = vote_prob, color = agent, group = agent)) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs",
       x = "Epoch",
       y = "Voting Probability") +
  facet_wrap(~party)+
  theme_minimal()



ggplot(interactions_test1_agent1, aes(x = epoch, y = vote_prob, color = party, group = party)) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs",
       x = "Epoch",
       y = "Voting Probability") +
  theme_minimal()


### lets see who switched:

# probably get it to wide format

interactions_test1_100 <- interactions_test1 %>% filter(epoch == 100)

# Identify the party with the highest probability for each agent at epoch 100
interactions_max_party_test <- interactions_test1_100 %>%
  group_by(agent) %>%
  filter(vote_prob == max(vote_prob, na.rm = TRUE)) %>%
  #slice(1) %>%  # In case of ties, take the first one
  ungroup()

table(interactions_max_party_test$party)


geo_nc_2755_test$party_new <- interactions_max_party_test$party


geo_nc_2755_test <- geo_nc_2755_test %>% mutate(switch = case_when(party_cd == party_new ~ 0,
                                                                   party_cd != party_new ~1))
table(geo_nc_2755_test$switch)


################################################################



computeWeight <- function(agents, ego, alter, H){
  return(
    1 - (abs(agents$party_numeric[ego] - agents$party_numeric[alter]) * H +
           abs(agents$party_numeric[ego] - agents$party_numeric[alter]) * (1 - H)) / 1
  )
}


computeWeight(geo_nc_2755_test, 1, 6, 0.6)

