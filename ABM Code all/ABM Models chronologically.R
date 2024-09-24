

#######################################
##### Different Stages of the ABM #####
#######################################


#####################
# Getting the data 
#####################

ncvoter_Statewide <- read.delim("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/ncvoter_Statewide.txt", header=FALSE)
ncvhis_Statewide <- read.delim("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/ncvhis_Statewide.txt")


names(ncvoter_Statewide) <- ncvoter_Statewide[1,]
ncvoter_Statewide <- ncvoter_Statewide[-1,]
table(ncvoter_Statewide$voter_status_desc)
table(ncvoter_Statewide$party_cd)


ncvoter_Statewide <- ncvoter_Statewide %>% filter(voter_status_desc == "ACTIVE")



##################################################
################# Version 0 ######################
##################################################

# no similarity bias, just assimilation model 

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
            
            # Calculate the change in probability based on party difference
            if (party_diff == 1) {
              # Different party: decrease probability
              delta_P <- -alpha 
              
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
              delta_P <- beta 
              
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





##################################################
################# Version 1 ######################
##################################################

updateVoteProbability1 <- function(agents, proximity_matrix, alpha, beta, total_attributes, interaction_prob, epochs) {
  
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


interactions1 <- updateVoteProbability1(geo_nc_2755_test, proxmat1, 0.1, 0.05, 3, 0.5, 100)



ggplot(interactions1, aes(x = epoch, y = vote_prob, color = agent, group = agent)) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs",
       x = "Epoch",
       y = "Voting Probability") +
  facet_wrap(~party)+
  theme_minimal()


##################################################
################# Version 2 ######################
##################################################

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
interaction_prob <- 0.5 # Probability of interaction within proximity
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



##################################################
################# Version 3 ######################
##################################################

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



interactions_3 <-updateVoteProbability_3(geo_nc_2755_test, proxmat1, alpha, beta, total_attributes, epochs = 300, proximity_threshold)


interactions_3_1 <- interactions_3[[2]]
interactions_3_2 <- interactions_3[[1]]



interactions_3_2_agent1 <- interactions_3_2 %>% filter(agent == 1)
interactions_3_1_agent1 <- interactions_3_1 %>% filter(agent == 1)



# Plot for individual vote probability changes over time

ggplot(interactions_3_1, aes(x = epoch, y = vote_prob, color = agent, group = factor(agent))) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs",
       x = "Epoch",
       y = "Voting Probability") +
  facet_wrap(~party)+
  ylim(0,1)+
  theme_minimal()


ggplot(interactions_3_1_agent1, aes(x = epoch, y = vote_prob, color = party, group = party)) +
  geom_line(alpha = 0.5) +
  labs(title = "Change in Voting Probabilities over Epochs for Agent 1",
       x = "Epoch",
       y = "Voting Probability") +
  theme_minimal()

##########################################################################



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



proximity_threshold <- 0.05
alpha <- 1e-13
beta <-  1e-13
scaling_factor = 1e-6

interactions_4 <-updateVoteProbability_4(geo_nc_2755_test, proxmat1, alpha = 0.001, beta= 0.001, total_attributes, epochs = 50, proximity_threshold)


interactions_4_epoch <- interactions_4$interactions_epoch
interactions_4_all <- interactions_4$interactions


interactions_4_all_agent1 <- interactions_4_all %>% filter(agent == 1)
interactions_4_epoch_agent1 <- interactions_4_epoch %>% filter(agent == 2)


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




##################################################
################# Version 4a ######################
##################################################



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

interactions_4a <-updateVoteProbability_4a(voter_data_2020_abm, proxmat1_2020, alpha = 0.001, beta= 0.001, total_attributes, epochs = 100, proximity_threshold)



###############################################
################ Version 5 ####################
###############################################


## changes ##
# replacing alpha and beta with mu as intensity of interaction
# replaced 'voted_party' with a dynamic variable to document the party with the highest vote probability
# as 'party[ego]' and 'party[alter]' dynamically

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






###############################################
################## Version 6 ##################
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


interaction_6 <- updateVoteProbability_6(voter_data_2020_abm, proxmat1_2020, mu = 0.001,  total_attributes, epochs = 100, proximity_threshold)



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


interaction_7 <- updateVoteProbability_7(voter_data_2020_abm, proxmat1_2020, mu = 0.001,  total_attributes, epochs = 100, proximity_threshold)







##################################################
################# Version 8 ######################
##################################################

# penalty rate for divergence based on Out group Exposure within proximity but updated after every interaction


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
interaction_8 <- updateVoteProbability_8(geo_nc_27915_blau_abm, proxmat1_27915, mu = 0.001, total_attributes = 3, epochs = 10, proximity_threshold = 0.05, p = 1.5)




# Calculate similarity based on demographics and opinions
dem_attrs <- c("age_binned", "race_code", "gender_code")
opin_attrs <- c("DEM_prob", "UNA_prob", "LIB_prob", "GRE_prob", "REP_prob")

opin_diffs <- abs(geo_nc_27915_blau_abm[2, opin_attrs] - geo_nc_27915_blau_abm[4, opin_attrs])
opin_sim <- sum(1 - opin_diffs)
opin_sim <- opin_sim / length(opin_attrs)  # Normalize by the number of opinion attributes

# 0 = max dissimilarity, 1 = max similarity 

dem_sim <- sum(
  geo_nc_27915_blau_abm$age_binned[2] == geo_nc_27915_blau_abm$age_binned[5], 
  geo_nc_27915_blau_abm$race_code[2] == geo_nc_27915_blau_abm$race_code[5], 
  geo_nc_27915_blau_abm$gender_code[2] == geo_nc_27915_blau_abm$gender_code[5]
)/total_attributes


total_attributes = 3


# 0 = max dissimilarity, 1 = max similarity 

sim_ij <- (dem_sim + opin_sim) / 2


