


geo_nc_2755_test <- geo_nc_27555

geo_nc_2755_test$index <- (1:228)

calcExposurePartisanship <- function(agents, proxmat){
  
  l <- nrow(proxmat1)
  eSameParty <- matrix(NA, nrow=l, ncol=l)
  eDiffParty <- matrix(NA, nrow=l, ncol=l)
  eSameParty_ingr <- matrix(NA, nrow=l, ncol=l)
  eDiffParty_ingr <- matrix(NA, nrow=l, ncol=l)
  
  # Nested Loop to iterate over values in proxmat 
  for (i in 1:l){
    for (j in 1:l){
      
      # same_party is TRUE if agents i and j have the same party and are not unaffiliated (UA).
      # diff_party is TRUE if agents i and j have different parties and are not unaffiliated.
      
      same_party <- geo_nc_2755_test$party_cd[j] == geo_nc_2755_test$party_cd[i] & geo_nc_2755_test$party_cd[j] != "UNA"
      diff_party <- geo_nc_2755_test$party_cd[j] != geo_nc_2755_test$party_cd[i] & geo_nc_2755_test$party_cd[j] != "UNA"
      
      # eSameParty and eDiffParty matrices are filled with proximity values scaled by party similarity (TRUE = 1, FALSE = 0)
      
      eSameParty[i,j] <- proxmat1[i,j] * same_party
      eDiffParty[i,j] <- proxmat1[i,j] * diff_party
      
      # for diagonal elements (whenb i == j), the values are adjusted to avoid self-influence
      
      if (i == j){
        eSameParty_ingr[i,j] <- proxmat1[i,j] * (same_party - 1)
        eDiffParty_ingr[i,j] <- proxmat1[i,j] * (diff_party - 1)
      } else {
        eSameParty_ingr[i,j] <- eSameParty[i,j]
        eDiffParty_ingr[i,j] <- eDiffParty[i,j]
      }
    }
  }
  
  vSameParty <- rowSums(eSameParty, na.rm = TRUE)
  vDiffParty <- rowSums(eDiffParty, na.rm = TRUE)
  vSameParty_ingr <- rowSums(eSameParty_ingr, na.rm = TRUE)
  vDiffParty_ingr <- rowSums(eDiffParty_ingr, na.rm = TRUE)
  sums_egoSameParty <- vSameParty_ingr + vDiffParty
  sums_egoDiffParty <- vSameParty + vDiffParty_ingr
  
  eSameParty_egoSameParty <- eSameParty_ingr / matrix(sums_egoSameParty, nrow=l, ncol=l)
  eSameParty_egoDiffParty <- eSameParty / matrix(sums_egoDiffParty, nrow=l, ncol=l)
  eDiffParty_egoSameParty <- eDiffParty / matrix(sums_egoSameParty, nrow=l, ncol=l)
  eDiffParty_egoDiffParty <- eDiffParty_ingr / matrix(sums_egoDiffParty, nrow=l, ncol=l)
  
  vSameParty_egoSameParty <- rowSums(eSameParty_egoSameParty, na.rm = TRUE)
  vSameParty_egoDiffParty <- rowSums(eSameParty_egoDiffParty, na.rm = TRUE)
  vDiffParty_egoSameParty <- rowSums(eDiffParty_egoSameParty, na.rm = TRUE)
  vDiffParty_egoDiffParty <- rowSums(eDiffParty_egoDiffParty, na.rm = TRUE)
  
  # Debugging loop
  for (i in 1:nrow(geo_nc_2755_test)) {
    if (geo_nc_2755_test$party_cd[i] != "UNA") {
      print(paste("Processing row:", i))
      print(paste("ncid:", geo_nc_2755_test$index[i]))
      print(paste("Same party exposure:", vSameParty_egoSameParty[geo_nc_2755_test$index[i]]))
      print(paste("Different party exposure:", vDiffParty_egoSameParty[geo_nc_2755_test$index[i]]))
      
      geo_nc_2755_test$exposureIngroup[i] <- vSameParty_egoSameParty[geo_nc_2755_test$index[i]]
      geo_nc_2755_test$exposureOutgroup[i] <- vDiffParty_egoSameParty[geo_nc_2755_test$index[i]]
    } else {
      geo_nc_2755_test$exposureIngroup[i] <- NA
      geo_nc_2755_test$exposureOutgroup[i] <- NA
    }
  }
  
  # Return the updated data frame
  result <- as.data.frame(cbind(
    geo_nc_2755_test$party_cd,
    geo_nc_2755_test$exposureIngroup,
    geo_nc_2755_test$exposureOutgroup
  ))
  
}



geo_nc_2755_test$birth_year <- as.numeric(geo_nc_2755_test$birth_year)
geo_nc_2755_test <- geo_nc_2755_test %>% mutate(age = 2020-birth_year)


hist(geo_nc_2755_test$age)
which(min(geo_nc_2755_test$age))

geo_nc_2755_test <- geo_nc_2755_test %>% mutate(age_binned = case_when(age > 13 & age < 20 ~"14-19 years",
                                                                       age > 19 & age < 36 ~"20-35 years",
                                                                       age > 35 & age < 51 ~"36-50 years",
                                                                       age > 50 & age < 70 ~"51-69 years",
                                                                       age > 69 ~"70 + years"))




str(geo_nc_2755_test$birth_year)

shared_attributes <- sum(
  geo_nc_2755_test$age_binned[1] == geo_nc_2755_test$age_binned[2], 
  geo_nc_2755_test$race_code[1] == geo_nc_2755_test$race_code[2], 
  geo_nc_2755_test$gender_code[1] == geo_nc_2755_test$gender_code[2]
)




###### testing update function:

#### creating probability vote variable for each party in the neighbourhood and append them to the dataset

# List of possible parties
parties <- unique(geo_nc_2755_test$party_cd)

# Initialize probability columns for each party
for (party in parties) {
  if(party != "UNA"){
  geo_nc_2755_test[[paste0(party, "_prob")]] <- 0
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



############ using those new variables for the voting perference update function

total_attributes <- 3
beta <- 0.1
alpha <- 0.1


  # Calculate Kronecker delta for party difference
  party_diff <- ifelse(geo_nc_2755_test$party_cd[1] == geo_nc_2755_test$party_cd[6], 0, 1)
  
  if(geo_nc_2755_test$party_cd[6] == "UNA") {
    delta_P <- 0
  } else {
  
  # Calculate the number of shared sociodemographic attributes
  shared_attributes <- sum(
    geo_nc_2755_test$age_binned[1] == geo_nc_2755_test$age_binned[6], 
    geo_nc_2755_test$race_code[1] == geo_nc_2755_test$race_code[6], 
    geo_nc_2755_test$gender_code[1] == geo_nc_2755_test$gender_code[6]
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
  ego_party_col <- paste0(geo_nc_2755_test$party_cd[1], "_prob")
  alter_party_col <- paste0(geo_nc_2755_test$party_cd[6], "_prob")
  
  # Update the voting probability
  geo_nc_2755_test[[ego_party_col]][1] <- sigmoid(geo_nc_2755_test[[ego_party_col]][1] + delta_P)
  geo_nc_2755_test[[alter_party_col]][1] <- sigmoid(geo_nc_2755_test[[alter_party_col]][1] - delta_P)
  
  # Ensure the probability stays within bounds [0, 1]
  # geo_nc_2755_test[[ego_party_col]][1] <- max(0, min(1, geo_nc_2755_test[[ego_party_col]][1]))
  # geo_nc_2755_test[[alter_party_col]][1] <- max(0, min(1, geo_nc_2755_test[[alter_party_col]][1]))



#######
  
  sigmoid <- function(x) {
    return(1 / (1 + exp(-x)))
  }

  
  

####################################

ego_test = 1
  
  
agents_test <- geo_nc_2755_test
### interaction probability:
  
  # Randomly assign the number of interactions for this agent for this day
  num_interactions_test <- sample(0:5, 1)
  for (interaction in 1:num_interactions_test) {
    # Filter agents within the proximity threshold
    potential_alters_test <- which(proxmat1[ego_test, ] > proximity_threshold & proxmat1[ego_test, ] < 1)
    
    if (length(potential_alters_test) > 0) {
      alter <- sample(potential_alters_test, 1)  # Randomly select one of the agents within proximity
      
      if (ego_test != alter) {  # Ensure ego does not interact with itself
        # Interaction probability based on proximity
        interaction_prob_test <- proxmat1[ego_test, alter]
        
        # Random interaction based on the calculated probability
        if (runif(1) < interaction_prob) {
        # Calculate Kronecker delta for party difference
        party_diff <- ifelse(agents_test$party_cd[ego_test] == agents_test$party_cd[alter], 0, 1)
        #party_diff <- 0
        if (agents_test$party_cd[alter] == "UNA") {
          delta_P <- 0
        } else {
          # Calculate the number of shared sociodemographic attributes
          shared_attributes <- sum(
            agents_test$age_binned[ego_test] == agents_test$age_binned[alter], 
            agents_test$race_code[ego_test] == agents_test$race_code[alter], 
            agents_test$gender_code[ego_test] == agents_test$gender_code[alter]
          )
          
          influence_weight <- sigmoid(shared_attributes / total_attributes)
          
          # Calculate the change in probability based on party difference
          if (party_diff == 1) {
            # Different party: decrease probability
            delta_P <- -alpha * influence_weight
            
            ego_party_col <- paste0(agents_test$party_cd[ego_test], "_prob")
            alter_party_col <- paste0(agents_test$party_cd[alter], "_prob")
            
            # Update the voting probability for ego
            agents_test[[ego_party_col]][ego_test] <- sigmoid(logit(agents_test[[ego_party_col]][ego_test]) + delta_P)
            agents_test[[alter_party_col]][ego_test] <- agents_test[[alter_party_col]][ego_test] - delta_P
            
            
            # Update the voting probability for alter
            agents_test[[ego_party_col]][alter] <- agents_test[[ego_party_col]][alter] - delta_P
            agents_test[[alter_party_col]][alter] <- sigmoid(logit(agents_test[[alter_party_col]][alter]) + delta_P)
            
            
          } else {
            # Same party: increase probability
            delta_P <- beta * influence_weight
            
            ego_party_col <- paste0(agents_test$party_cd[ego_test], "_prob")
            alter_party_col <- paste0(agents_test$party_cd[alter], "_prob")
            
            # Update the voting probability for ego
            agents_test[[ego_party_col]][ego_test] <- sigmoid(logit(agents_test[[ego_party_col]][ego_test]) + delta_P)
            
            # Update the voting probability for alter
            agents_test[[alter_party_col]][alter] <- sigmoid(logit(agents_test[[alter_party_col]][alter]) + delta_P)
            
          }
        }
      }
    }
  }
  }
  
  
median(proxmat1)
###########################################
          
          
          

          
          updateVoteProbability_random <- function(agents, proximity_matrix, alpha, beta, total_attributes, epochs) {
            
            # initialize list with number of agents 
            num_agents <- nrow(agents)
            
            # create dataframe for interactions 
            interactions <- data.frame(epoch = integer(), agent = integer(), party = character(), vote_prob = numeric())
            
            # 3 times nested loop iterating over epochs first and then over i and j from num_agents list 
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
                          agents[[ego_party_col]][ego] <- sigmoid(logit(agents[[ego_party_col]][ego]) + delta_P)
                          agents[[alter_party_col]][ego] <- agents[[alter_party_col]][ego] - delta_P
                          
                          
                          # Update the voting probability for alter
                          agents[[ego_party_col]][alter] <- agents[[ego_party_col]][alter] - delta_P
                          agents[[alter_party_col]][alter] <- sigmoid(logit(agents[[alter_party_col]][alter]) + delta_P)
                          
                        } else {
                          # Same party: increase probability
                          delta_P <- beta * influence_weight
                          
                          ego_party_col <- paste0(agents$party_cd[ego], "_prob")
                          alter_party_col <- paste0(agents$party_cd[alter], "_prob")
                          
                          # Update the voting probability for ego
                          agents[[ego_party_col]][ego] <- sigmoid(logit(agents[[ego_party_col]][ego]) + delta_P)
                          # Update the voting probability for alter
                          agents[[alter_party_col]][alter] <- sigmoid(logit(agents[[alter_party_col]][alter]) + delta_P)
                        }
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
          
          