
library(dplyr)
library(ggplot2)
library(data.table)
library(FNN)
library(tidyr)

#############################################################
################ Analyzing Results ##########################
#############################################################



##### 1. Getting empirical voter data from 2016 to 2024 ########

ncvoter_Statewide <- read.delim("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/ncvoter_Statewide.txt")
ncvhis_Statewide <- read.delim("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/ncvhis_Statewide.txt")




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


count(nc_voter_hist_zipcodes %>% filter(election_lbl == 	
                                          "2016-11-08", zip_code == 27915))

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


nc_stats_zipcode_no_UNA<- nc_all_zips_merged_no_UNA %>%
  group_by(voted_party_cd, zip_code, election_lbl) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(election_lbl, zip_code) %>%
  mutate(perc = count / sum(count))

nc_stats_zipcode$zip_code <- factor(nc_stats_zipcode$zip_code, levels = c("28203", "27915"))
nc_stats_zipcode_no_UNA$zip_code <- factor(nc_stats_zipcode_no_UNA$zip_code, levels = c("28203", "27915"))

custom_labels <- c(`28203` = "Urban Area", `27915` = "Rural Area")

ggplot(nc_stats_zipcode, aes(election_lbl, perc, group = voted_party_cd, col = voted_party_cd)) +
  geom_line() +
  scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple", "GRE" = "darkgreen", "LIB" = "yellow")) +
  facet_wrap(~zip_code, labeller = labeller(zip_code = custom_labels)) +
  labs(x = "Election", y = "Percentage", color = "Voted Party") +
  theme_bw() +
  ylim(0, 1)


ggsave("nc_stats_all_ruca.png",dpi = 95)


ggplot(nc_stats_zipcode_no_UNA, aes(election_lbl, perc, group = voted_party_cd, col = voted_party_cd)) +
  geom_line() +
  scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "GRE" = "darkgreen", "LIB" = "yellow")) +
  facet_wrap(~zip_code, labeller = labeller(zip_code = custom_labels)) +
  labs(x = "Election", y = "Percentage", color = "Voted Party") +
  theme_bw() +
  ylim(0, 1)

ggsave("nc_stats_all_ruca_no_una.png",dpi = 95)







###################################################
############### Vote Switches #####################
###################################################

library(tidyr)

################## urban area ##################

###### merging data from 2020 to 2024 based on identifier ######

load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/geo_nc_28203_abm_small.RData")
load("~/Desktop/LSE Term 2/Capstone-Project/DATA ABM R/geo_nc_27915_abm_small.RData")

nc_28203_2024 <- geo_nc_28203_abm_small



nc_28203_2024 <- nc_28203_2024 %>% rename("party_voted_2016" = voted_party_cd)

nc_28203_2024 <- nc_28203_2024 %>% rename("party_voted_2024" = party_cd)



###### creating variable for voter change ######


nc_28203_2024 <- nc_28203_2024 %>% mutate(party_change = case_when(party_voted_2016 == party_voted_2024 ~ 0,
                                                                   party_voted_2016 != party_voted_2024 ~ 1))


table(nc_28203_2024$party_change)

nc_28203_2024_long <- pivot_longer(nc_28203_2024, cols = c(party_voted_2016, party_voted_2024),
                                   names_to = "year",
                                   values_to = "party_affiliation")



table(nc_28203_2024$party_voted_2016)
table(nc_28203_2024$party_voted_2024)


save(nc_28203_2024, file = "~/nc_28203_2024.RData")

################## rural area ##################


nc_27915_2024 <- geo_nc_27915_abm_small



nc_27915_2024 <- nc_27915_2024 %>% rename("party_voted_2016" = voted_party_cd)

nc_27915_2024 <- nc_27915_2024 %>% rename("party_voted_2024" = party_cd)



###### creating variable for voter change ######


nc_27915_2024 <- nc_27915_2024 %>% mutate(party_change = case_when(party_voted_2016 == party_voted_2024 ~ 0,
                                                                   party_voted_2016 != party_voted_2024 ~ 1))


table(nc_27915_2024$party_change)

nc_27915_2024_long <- pivot_longer(nc_27915_2024, cols = c(party_voted_2016, party_voted_2024),
                                   names_to = "year",
                                   values_to = "party_affiliation")



table(nc_27915_2024$party_voted_2016)
table(nc_27915_2024$party_voted_2024)

save(nc_27915_2024, file = "~/nc_27915_2024.RData")


###########################################
# calculating Blau Index for empirical data
############################################


rural_zipcode_2016 <- nc_stats_zipcode %>% filter(election_lbl == "2016-11-08", zip_code == "27915")
rural_zipcode_2024 <- nc_stats_zipcode %>% filter(election_lbl == "2024-07-25", zip_code == "27915")

urban_zipcode_2016 <- nc_stats_zipcode %>% filter(election_lbl == "2016-11-08", zip_code == "28203")
urban_zipcode_2024 <- nc_stats_zipcode %>% filter(election_lbl == "2024-07-25", zip_code == "28203")




blau_index_data <- nc_stats_zipcode %>% group_by(election_lbl, zip_code) %>%
  mutate(blau_index = 1-(sum(perc^2)))

ggplot(blau_index_data, aes(x=election_lbl, y = blau_index))+
  facet_wrap(~zip_code, labeller = labeller(zip_code = custom_labels)) +
  geom_line() + theme_bw()




####### without UNA #########

rural_zipcode_2016_no_una <- nc_stats_zipcode_no_UNA %>% filter(election_lbl == "2016-11-08", zip_code == "27915")
rural_zipcode_2024_no_una <- nc_stats_zipcode_no_UNA %>% filter(election_lbl == "2024-07-25", zip_code == "27915")

urban_zipcode_2016_no_una <- nc_stats_zipcode_no_UNA %>% filter(election_lbl == "2016-11-08", zip_code == "28203")
urban_zipcode_2024_no_una <- nc_stats_zipcode_no_UNA %>% filter(election_lbl == "2024-07-25", zip_code == "28203")



blau_index_data_no_una <- nc_stats_zipcode_no_UNA %>% group_by(election_lbl, zip_code) %>%
  mutate(blau_index = 1-(sum(perc^2)))

ggplot(blau_index_data_no_una, aes(x=election_lbl, y = blau_index))+
  facet_wrap(~zip_code, labeller = labeller(zip_code = custom_labels)) +
  geom_line() + theme_bw()






################################################
###### Calculate Entropy and Blau again ########
################################################


nc_stats_ruca_BLAU <- nc_stats_zipcode %>%
  group_by(zip_code, election_lbl) %>%
  summarise(blau_index = 1 - sum(perc^2))

nc_stats_ruca_entropy <- nc_stats_zipcode %>%
  group_by(zip_code, election_lbl) %>%
  summarise(entropy = -sum(perc * log(perc)))


## no UNA

nc_stats_ruca_BLAU_noUNA <- nc_stats_zipcode_no_UNA %>%
  group_by(zip_code, election_lbl) %>%
  summarise(blau_index = 1 - sum(perc^2))

nc_stats_ruca_entropy_noUNA <- nc_stats_zipcode_no_UNA %>%
  group_by(zip_code, election_lbl) %>%
  summarise(entropy = -sum(perc * log(perc)))


###### plotting Blau Index and Entropy Balancing


ggplot() +
  geom_line(data = blau_index_data, aes(x = election_lbl, y = blau_index, color ="Blau Index", linetype = "including UNA voters")) + 
  geom_line(data = blau_index_data_no_una, aes(x = election_lbl, y = blau_index, , color ="Blau Index", linetype = "without UNA voters")) + 
  geom_line(data = nc_stats_ruca_entropy, aes(x = election_lbl, y = entropy, , color ="Entropy", linetype = "including UNA voters")) + 
  geom_line(data = nc_stats_ruca_entropy_noUNA, aes(x = election_lbl, y = entropy, , color ="Entropy", linetype = "without UNA voters")) + 
  scale_linetype_manual(values = c("including UNA voters" = "solid", "without UNA voters" = "dashed")) +
  scale_color_manual(values = c("Blau Index" = "blue", "Entropy" = "black")) +
  facet_wrap(~zip_code, labeller = labeller(zip_code = custom_labels)) +
  labs(x="Election Date", y="Blau Index", linetype = "Data", color = "Index") +
  theme_bw()


ggsave("blau_index_entropy_combined.png", dpi = 95)



#######################
#### Rural Zipcode ####
#######################




load("~/Desktop/LSE Term 2/Capstone-Project/ABM/Results ABM/interaction_mu.RData")



interactions_mu_0.01 <- interaction_mu$mu_0.01
interactions_mu_0.005 <- interaction_mu$mu_0.005

blau_index_rural_0.01 <- interactions_mu_0.01$blau_indices
interaction_epoch_0.01_rural <- interactions_mu_0.01$interactions_epoch


##############################
#### Plot Blau Index
###############################



ggplot(blau_index_rural_0.01, aes(x=epoch,y= blau_index))+
  geom_line(linewidth = .7) +theme_bw()

ggsave("blau_index_rural_0.01.png", dpi = 95)

voted_parties_rural_0.01 <- interaction_epoch_0.01_rural %>% group_by(epoch) %>%
  count(voted_party_cd) %>% mutate(prop = n/sum(n))

ggplot(voted_parties_rural_0.01, aes(x=epoch, y = prop, group = voted_party_cd, color = voted_party_cd))+  geom_line(linewidth = .7)  +theme_bw()  + ylim(.25,.45)+
  labs(title="Change in Partisan Affiliation: Rural Neighborhood", y = "Percentage", x = "Epoch", color = "Party Affiliation") + 
  scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple"))+
  ylim(0,0.8) +theme_bw()


ggsave("voted_parties_rural_0.01.png", dpi = 95)



###################################################################################################################

########## does not work #########


load("~/Desktop/LSE Term 2/Capstone-Project/ABM/Results ABM/interaction_rural_test_una_div_4.RData")


interaction_rural_test_blau <- interaction_rural_test$blau_indices
interaction_rural_test_epochs <- interaction_rural_test$interactions_epoch


ggplot(interaction_rural_test_blau, aes(x=epoch,y= blau_index))+
  geom_line(linewidth = .7) +theme_bw()


voted_parties_rural_0.01 <- interaction_rural_test_epochs %>% group_by(epoch) %>%
  count(voted_party_cd) %>% mutate(prop = n/sum(n))

ggplot(voted_parties_rural_0.01, aes(x=epoch, y = prop, group = voted_party_cd, color = voted_party_cd))+  geom_line()  +theme_bw()  +   scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple"))+
  labs(title="Change in Partisan Affiliation: Rural Neighborhood", y = "Number", x = "Epoch", color = "Party Affiliation") + ylim(0,0.8)


###### does not work #######

#######################################################################################################








blau_index_rural_0.005 <- interactions_mu_0.005$blau_indices
interaction_epoch_0.005_rural <- interactions_mu_0.005$interactions_epoch

ggplot(blau_index_rural_0.005, aes(x=epoch,y= blau_index))+
  geom_line(linewidth = .7) + ylim(0.5,0.7) +theme_bw()


voted_parties_rural_0.005 <- interaction_epoch_0.005_rural %>% group_by(epoch) %>%
  count(voted_party_cd) %>% mutate(prop = n/sum(n))


blau_index_data_rural_0.005 <- voted_parties_rural_0.005 %>% group_by(epoch) %>%
  mutate(blau_index = 1-(sum(prop^2)))

ggplot(blau_index_data_rural_0.005, aes(x=epoch,y= blau_index))+
  geom_line(linewidth = .7) + ylim(0.5,0.7) +theme_bw()

ggsave("blau_index_rural_0.005_new.png", dpi = 95)




ggplot(voted_parties_rural_0.005, aes(x=epoch, y = prop, group = voted_party_cd, color = voted_party_cd))+  geom_line(linewidth = .7)  +theme_bw()  + ylim(.25,.45)+
  labs(title="Change in Partisan Affiliation: Rural Neighborhood", y = "Percentage", x = "Epoch", color = "Party Affiliation") + 
  scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple"))+
  ylim(0,0.8) +theme_bw()


ggsave("voted_parties_rural_0.005.png", dpi = 95)


########## combining empirical and simulation data in one plot ###################


blau_index_data_27915 <- blau_index_data %>% filter(zip_code== "27915", voted_party_cd == "DEM", election_lbl!= "2014-11-04")
blau_index_data_27915$epoch <- c(0,100,200,300,400)


ggplot()+
  geom_line(data= blau_index_data_rural_0.005, aes(x=epoch,y= blau_index, color = "Simulation Data")) +
  geom_line(data=blau_index_data_27915, aes(x=epoch,y= blau_index, color = "Empirical Data"))+
  scale_color_manual(values = c("Simulation Data" = "chartreuse3","Empirical Data" = "black"))+
  theme_bw() +ylim(0.5,0.7) + labs(y="Blau Index", x = "Epoch", color = "Data")


ggsave("blau_index_verification_rural.0.005.png", dpi = 95)


##### Entropy ####

nc_stats_ruca_entropy <- nc_stats_zipcode %>%
  group_by(zip_code, election_lbl) %>%
  summarise(entropy = -sum(perc * log(perc)))

entropy_data_27915 <- nc_stats_ruca_entropy %>% filter(zip_code== "27915", election_lbl!= "2014-11-04")
entropy_data_27915$epoch <- c(0,100,200,300,400)


entropy_data_rural_0.005 <- voted_parties_rural_0.005 %>% group_by(epoch) %>%
  mutate(entropy = -sum(prop * log(prop)))


ggplot()+
  geom_line(data= entropy_data_rural_0.005, aes(x=epoch,y= entropy, color = "Simulation Data")) +
  geom_line(data=entropy_data_27915, aes(x=epoch,y= entropy, color = "Empirical Data"))+
  scale_color_manual(values = c("Simulation Data" = "chartreuse3","Empirical Data" = "black"))+
  theme_bw() +ylim(0.8,1.2) + labs(y="Entropy", x = "Epoch", color = "Data")



ggsave("entropy_verification_rural.png", dpi = 95)


### voted parties ##

voted_parties_data_27915 <- nc_stats_zipcode %>% filter(zip_code== "27915", election_lbl!= "2014-11-04")
voted_parties_data_27915 <- voted_parties_data_27915 %>% mutate(epoch = case_when(election_lbl == "2016-11-08" ~ 0,
                                                                                  election_lbl == "2018-11-06" ~ 100,
                                                                                  election_lbl == "2020-11-03" ~ 200,
                                                                                  election_lbl == "2022-11-08" ~ 300,
                                                                                  election_lbl == "2024-07-25" ~ 400))


ggplot()+
  geom_line(data = voted_parties_rural_0.005, aes(x=epoch, y = prop, group = voted_party_cd, color = voted_party_cd, linetype = "Simulation Data")) +
  geom_line(data=voted_parties_data_27915, aes(x=epoch,y= perc,group = voted_party_cd, color = voted_party_cd, linetype = "Empirical Data"))+
  scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple", "GRE" = "green"))+
  scale_linetype_manual(values = c("Simulation Data" = "longdash","Empirical Data" = "solid"))+
  theme_bw()+ labs(y="Blau Index", x = "Epoch", color = "Party", linetype = "Data")


ggsave("voted_parties_verification_rural.png", dpi = 95)


##############################
#### Plot voting probabilities
###############################

vote_probs_rural_0.005 <-interaction_epoch_0.005_rural %>%
  tidyr::pivot_wider(names_from = party, values_from = vote_prob) 


# Plot vote probabilities over time for each party
ggplot(vote_probs_rural_0.005, aes(x = epoch)) +
  geom_smooth(aes(y = DEM, color = "DEM")) +
  geom_smooth(aes(y = REP, color = "REP")) +
  geom_smooth(aes(y = UNA, color = "UNA")) +
  #geom_smooth(aes(y = NLB, color = "NLB")) +
  labs(title = "Vote Probabilities Over Time: Rural Neighborhood",
       x = "Epoch",
       y = "Vote Probability",
       color = "Party") +
  scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple"))+
  ylim(0,1)+
  theme_bw()

ggsave("vote_probs_rural_0.005.png", dpi = 95)




#################################
#### Plot individual changes
#################################


epoch1_voting_rural <- interaction_epoch_0.005_rural %>% filter(epoch ==1) %>% dplyr::select(agent, voted_party_cd)
epoch1_voting_rural <- unique(epoch1_voting_rural)

epoch1_voting_rural <- epoch1_voting_rural %>% rename("party_voted_epoch1" = voted_party_cd)
table(epoch1_voting_rural$voted_party_cd)


epoch400_voting_rural <- interaction_epoch_0.005_rural %>% filter(epoch ==400) %>% dplyr::select(agent, voted_party_cd)
epoch400_voting_rural <- unique(epoch400_voting_rural)

table(epoch400_voting_rural$voted_party_cd)


epoch400_voting_rural <- epoch400_voting_rural %>% rename("party_voted_epoch400" = voted_party_cd)
simulated_party_rural <- left_join(epoch1_voting_rural, epoch400_voting_rural, by = "agent")

simulated_party_rural <- simulated_party_rural %>% mutate(party_change_simulated = case_when(party_voted_epoch1 == party_voted_epoch400 ~ 0,
                                                                                             party_voted_epoch1 != party_voted_epoch400 ~ 1))
table(simulated_party_rural$party_change_simulated)




###### creating variable for voter change ######


nc_27915_2024 <- nc_27915_2024 %>% mutate(party_change = case_when(party_voted_2016 == party_voted_2024 ~ 0,
                                                                   party_voted_2016 != party_voted_2024 ~ 1))


table(nc_27915_2024$party_change)

nc_27915_2024_long <- pivot_longer(nc_27915_2024, cols = c(party_voted_2016, party_voted_2024),
                                   names_to = "year",
                                   values_to = "party_affiliation")



table(nc_27915_2024$party_voted_2016)
table(nc_27915_2024$party_voted_2024)





######### Verifiying individual switches ############


empirical_party_2024 <- nc_27915_2024$party_voted_2024
simulation_party_2024 <- epoch400_voting_rural$party_voted_epoch400


# Calculate the accuracy
accuracy_rural_0.005 <- sum(empirical_party_2024 == simulation_party_2024) / length(empirical_party_2024)

# Print the accuracy as a percentage
cat("Prediction Accuracy:", accuracy_rural_0.005 * 100, "%\n")


######## precision for switching and staying ##########

empirical_vote_switches_rural <- nc_27915_2024$party_change
simulated_vote_switches_rural <- simulated_party_rural$party_change_simulated



F1(empirical_vote_switches_rural, simulated_vote_switches_rural)
Precision(empirical_vote_switches_rural, simulated_vote_switches_rural)

accuracy_rural_0.005 <- sum(empirical_vote_switches_rural == simulated_vote_switches_rural) / length(empirical_vote_switches_rural)

Recall(empirical_vote_switches_rural, simulated_vote_switches_rural)


############################# mu 0.004 ############################################


load("~/Desktop/LSE Term 2/Capstone-Project/ABM/Results ABM/interaction_rural_0.004.RData")


blau_index_rural_0.004 <- interaction_rural_0.004$blau_indices
interaction_epoch_0.004_rural <- interaction_rural_0.004$interactions_epoch

ggplot(blau_index_rural_0.004, aes(x=epoch,y= blau_index))+
  geom_line(linewidth = .7) + ylim(0.5,0.7) +theme_bw()


voted_parties_rural_0.004 <- interaction_epoch_0.004_rural %>% group_by(epoch) %>%
  count(voted_party_cd) %>% mutate(prop = n/sum(n))


blau_index_data_rural_0.004 <- voted_parties_rural_0.004 %>% group_by(epoch) %>%
  mutate(blau_index = 1-(sum(prop^2)))

ggplot(blau_index_data_rural_0.004, aes(x=epoch,y= blau_index))+
  geom_line(linewidth = .7) + ylim(0.5,0.7) +theme_bw()

ggsave("blau_index_rural_0.004_new.png", dpi = 95)




ggplot(voted_parties_rural_0.004, aes(x=epoch, y = prop, group = voted_party_cd, color = voted_party_cd))+  geom_line(linewidth = .7)  +theme_bw()  + ylim(.25,.45)+
  labs(title="Change in Partisan Affiliation: Rural Neighborhood", y = "Percentage", x = "Epoch", color = "Party Affiliation") + 
  scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple"))+
  ylim(0,0.8) +theme_bw()


ggsave("voted_parties_rural_0.004.png", dpi = 95)


########## combining empirical and simulation data in one plot ###################


blau_index_data_27915 <- blau_index_data %>% filter(zip_code== "27915", voted_party_cd == "DEM", election_lbl!= "2014-11-04")
blau_index_data_27915$epoch <- c(0,100,200,300,400)


ggplot()+
  geom_line(data= blau_index_data_rural_0.004, aes(x=epoch,y= blau_index, color = "Simulation Data")) +
  geom_line(data=blau_index_data_27915, aes(x=epoch,y= blau_index, color = "Empirical Data"))+
  scale_color_manual(values = c("Simulation Data" = "chartreuse3","Empirical Data" = "black"))+
  theme_bw() +ylim(0.5,0.7) + labs(y="Blau Index", x = "Epoch", color = "Data")


ggsave("blau_index_verification_rural.0.004.png", dpi = 95)


##### Entropy ####

nc_stats_ruca_entropy <- nc_stats_zipcode %>%
  group_by(zip_code, election_lbl) %>%
  summarise(entropy = -sum(perc * log(perc)))

entropy_data_27915 <- nc_stats_ruca_entropy %>% filter(zip_code== "27915", election_lbl!= "2014-11-04")
entropy_data_27915$epoch <- c(0,100,200,300,400)


entropy_data_rural_0.004 <- voted_parties_rural_0.004 %>% group_by(epoch) %>%
  mutate(entropy = -sum(prop * log(prop)))


ggplot()+
  geom_line(data= entropy_data_rural_0.004, aes(x=epoch,y= entropy, color = "Simulation Data")) +
  geom_line(data=entropy_data_27915, aes(x=epoch,y= entropy, color = "Empirical Data"))+
  scale_color_manual(values = c("Simulation Data" = "chartreuse3","Empirical Data" = "black"))+
  theme_bw() +ylim(0.8,1.2) + labs(y="Entropy", x = "Epoch", color = "Data")



ggsave("entropy_verification_rural.png", dpi = 95)


### voted parties ##

voted_parties_data_27915 <- nc_stats_zipcode %>% filter(zip_code== "27915", election_lbl!= "2014-11-04")
voted_parties_data_27915 <- voted_parties_data_27915 %>% mutate(epoch = case_when(election_lbl == "2016-11-08" ~ 0,
                                                                                  election_lbl == "2018-11-06" ~ 100,
                                                                                  election_lbl == "2020-11-03" ~ 200,
                                                                                  election_lbl == "2022-11-08" ~ 300,
                                                                                  election_lbl == "2024-07-25" ~ 400))


ggplot()+
  geom_line(data = voted_parties_rural_0.004, aes(x=epoch, y = prop, group = voted_party_cd, color = voted_party_cd, linetype = "Simulation Data")) +
  geom_line(data=voted_parties_data_27915, aes(x=epoch,y= perc,group = voted_party_cd, color = voted_party_cd, linetype = "Empirical Data"))+
  scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple", "GRE" = "green"))+
  scale_linetype_manual(values = c("Simulation Data" = "longdash","Empirical Data" = "solid"))+
  theme_bw()+ labs(y="Blau Index", x = "Epoch", color = "Party", linetype = "Data")


ggsave("voted_parties_verification_rural.png", dpi = 95)


##############################
#### Plot voting probabilities
###############################

vote_probs_rural_0.004 <-interaction_epoch_0.004_rural %>%
  tidyr::pivot_wider(names_from = party, values_from = vote_prob) 


# Plot vote probabilities over time for each party
ggplot(vote_probs_rural_0.004, aes(x = epoch)) +
  geom_smooth(aes(y = DEM, color = "DEM")) +
  geom_smooth(aes(y = REP, color = "REP")) +
  geom_smooth(aes(y = UNA, color = "UNA")) +
  #geom_smooth(aes(y = NLB, color = "NLB")) +
  labs(title = "Vote Probabilities Over Time: Rural Neighborhood",
       x = "Epoch",
       y = "Vote Probability",
       color = "Party") +
  scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple"))+
  ylim(0,1)+
  theme_bw()

ggsave("vote_probs_rural_0.004.png", dpi = 95)




#################################
#### Plot individual changes
#################################


epoch1_voting_rural <- interaction_epoch_0.004_rural %>% filter(epoch ==1) %>% dplyr::select(agent, voted_party_cd)
epoch1_voting_rural <- unique(epoch1_voting_rural)

table(epoch1_voting_rural$voted_party_cd)


epoch400_voting_rural <- interaction_epoch_0.004_rural %>% filter(epoch ==400) %>% dplyr::select(agent, voted_party_cd)
epoch400_voting_rural <- unique(epoch400_voting_rural)

table(epoch400_voting_rural$voted_party_cd)



###### creating variable for voter change ######


nc_27915_2024 <- nc_27915_2024 %>% mutate(party_change = case_when(party_voted_2016 == party_voted_2024 ~ 0,
                                                                   party_voted_2016 != party_voted_2024 ~ 1))


table(nc_27915_2024$party_change)

nc_27915_2024_long <- pivot_longer(nc_27915_2024, cols = c(party_voted_2016, party_voted_2024),
                                   names_to = "year",
                                   values_to = "party_affiliation")



table(nc_27915_2024$party_voted_2016)
table(nc_27915_2024$party_voted_2024)




############################# mu 0.001 ############################################


blau_index_rural_0.001 <- interactions_mu_0.001$blau_indices
interaction_epoch_0.001_rural <- interactions_mu_0.001$interactions_epoch

ggplot(blau_index_rural_0.001, aes(x=epoch,y= blau_index))+
  geom_line(linewidth = .7) + ylim(0.5,0.7) +theme_bw()


voted_parties_rural_0.001 <- interaction_epoch_0.001_rural %>% group_by(epoch) %>%
  count(voted_party_cd) %>% mutate(prop = n/sum(n))


blau_index_data_rural_0.001 <- voted_parties_rural_0.001 %>% group_by(epoch) %>%
  mutate(blau_index = 1-(sum(prop^2)))

ggplot(blau_index_data_rural_0.001, aes(x=epoch,y= blau_index))+
  geom_line(linewidth = .7) + ylim(0.5,0.7) +theme_bw()

ggsave("blau_index_rural_0.001_new.png", dpi = 95)




ggplot(voted_parties_rural_0.001, aes(x=epoch, y = prop, group = voted_party_cd, color = voted_party_cd))+  geom_line(linewidth = .7)  +theme_bw()  + ylim(.25,.45)+
  labs(title="Change in Partisan Affiliation: Rural Neighborhood", y = "Percentage", x = "Epoch", color = "Party Affiliation") + 
  scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple"))+
  ylim(0,0.8) +theme_bw()


ggsave("voted_parties_rural_0.001.png", dpi = 95)


########## combining empirical and simulation data in one plot ###################


blau_index_data_27915 <- blau_index_data %>% filter(zip_code== "27915", voted_party_cd == "DEM", election_lbl!= "2014-11-04")
blau_index_data_27915$epoch <- c(0,100,200,300,400)


ggplot()+
  geom_line(data= blau_index_data_rural_0.001, aes(x=epoch,y= blau_index, color = "Simulation Data")) +
  geom_line(data=blau_index_data_27915, aes(x=epoch,y= blau_index, color = "Empirical Data"))+
  scale_color_manual(values = c("Simulation Data" = "chartreuse3","Empirical Data" = "black"))+
  theme_bw() +ylim(0.5,0.7) + labs(y="Blau Index", x = "Epoch", color = "Data")


ggsave("blau_index_verification_rural.0.001png", dpi = 95)


##### Entropy ####

nc_stats_ruca_entropy <- nc_stats_zipcode %>%
  group_by(zip_code, election_lbl) %>%
  summarise(entropy = -sum(perc * log(perc)))

entropy_data_27915 <- nc_stats_ruca_entropy %>% filter(zip_code== "27915", election_lbl!= "2014-11-04")
entropy_data_27915$epoch <- c(0,100,200,300,400)


entropy_data_rural_0.001 <- voted_parties_rural_0.001 %>% group_by(epoch) %>%
  mutate(entropy = -sum(prop * log(prop)))


ggplot()+
  geom_line(data= entropy_data_rural_0.001, aes(x=epoch,y= entropy, color = "Simulation Data")) +
  geom_line(data=entropy_data_27915, aes(x=epoch,y= entropy, color = "Empirical Data"))+
  scale_color_manual(values = c("Simulation Data" = "chartreuse3","Empirical Data" = "black"))+
  theme_bw() +ylim(0.8,1.2) + labs(y="Entropy", x = "Epoch", color = "Data")



ggsave("entropy_verification_rural.0.001.png", dpi = 95)


### voted parties ##

voted_parties_data_27915 <- nc_stats_zipcode %>% filter(zip_code== "27915", election_lbl!= "2014-11-04")
voted_parties_data_27915 <- voted_parties_data_27915 %>% mutate(epoch = case_when(election_lbl == "2016-11-08" ~ 0,
                                                                                  election_lbl == "2018-11-06" ~ 100,
                                                                                  election_lbl == "2020-11-03" ~ 200,
                                                                                  election_lbl == "2022-11-08" ~ 300,
                                                                                  election_lbl == "2024-07-25" ~ 400))


ggplot()+
  geom_line(data = voted_parties_rural_0.001, aes(x=epoch, y = prop, group = voted_party_cd, color = voted_party_cd, linetype = "Simulation Data")) +
  geom_line(data=voted_parties_data_27915, aes(x=epoch,y= perc,group = voted_party_cd, color = voted_party_cd, linetype = "Empirical Data"))+
  scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple", "GRE" = "green"))+
  scale_linetype_manual(values = c("Simulation Data" = "longdash","Empirical Data" = "solid"))+
  theme_bw()+ labs(y="Blau Index", x = "Epoch", color = "Party", linetype = "Data")


ggsave("voted_parties_verification_rural.0.001.png", dpi = 95)


##############################
#### Plot voting probabilities
###############################

vote_probs_rural_0.001 <-interaction_epoch_0.001_rural %>%
  tidyr::pivot_wider(names_from = party, values_from = vote_prob) 


# Plot vote probabilities over time for each party
ggplot(vote_probs_rural_0.001, aes(x = epoch)) +
  geom_smooth(aes(y = DEM, color = "DEM")) +
  geom_smooth(aes(y = REP, color = "REP")) +
  geom_smooth(aes(y = UNA, color = "UNA")) +
  #geom_smooth(aes(y = NLB, color = "NLB")) +
  labs(title = "Vote Probabilities Over Time: Rural Neighborhood",
       x = "Epoch",
       y = "Vote Probability",
       color = "Party") +
  scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple"))+
  ylim(0,1)+
  theme_bw()

ggsave("vote_probs_rural_0.001.png", dpi = 95)


#################################
#### Plot individual changes
#################################

epoch1_voting_rural_0.001 <- interaction_epoch_0.001_rural %>% filter(epoch ==1) %>% dplyr::select(agent, voted_party_cd)
epoch1_voting_rural_0.001 <- unique(epoch1_voting_rural_0.001)

table(epoch1_voting_rural_0.001$voted_party_cd)


epoch400_voting_rural_0.001 <- interaction_epoch_0.001_rural %>% filter(epoch ==400) %>% dplyr::select(agent, voted_party_cd)
epoch400_voting_rural_0.001 <- unique(epoch400_voting_rural_0.001)

table(epoch400_voting_rural$voted_party_cd)






############### interaction test UNA = mu/3, mu = 0.01
load("~/Desktop/LSE Term 2/Capstone-Project/ABM/Results ABM/Rural/interaction_rural_test_una_3.0.01.RData")
load("~/Desktop/LSE Term 2/Capstone-Project/ABM/Results ABM/Rural/interaction_rural_test_una_div_4.RData")



blau_index_rural_0.01 <- interaction_rural_test_una_3$blau_indices
interaction_epoch_rural_0.01 <- interaction_rural_test_una_3$interactions_epoch

ggplot(blau_index_rural_0.01, aes(x=epoch,y= blau_index))+
  geom_line()

voted_parties_rural_0.01 <- interaction_epoch_rural_0.01 %>% group_by(epoch) %>%
  count(voted_party_cd) %>% mutate(prop = n/sum(n))


blau_index_data_rural_0.01 <- voted_parties_rural_0.01 %>% group_by(epoch) %>%
  mutate(blau_index = 1-(sum(prop^2)))



ggplot(blau_index_data_rural_0.01, aes(x=epoch,y= blau_index))+
  geom_line() +theme_bw() +ylim(0.5,0.7) + labs(y="Blau Index", x = "Epoch")


ggplot(voted_parties_rural_0.01, aes(x=epoch, y = prop, group = voted_party_cd, color = voted_party_cd))+  geom_line()  +theme_bw()  +   scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple"))+
  labs(title="Change in Partisan Affiliation: Rural Neighborhood", y = "Number", x = "Epoch", color = "Party Affiliation") + ylim(0,0.8)


################# interaction test UNA = mu/4, mu = 0.01


##### wrong doesnt work !!!!!! #######


blau_index_rural_0.01_2 <- interaction_rural_test$blau_indices
interaction_epoch_rural_0.01_2 <- interaction_rural_test$interactions_epoch



voted_parties_rural_0.01_2 <- interaction_epoch_rural_0.01_2 %>% group_by(epoch) %>%
  count(voted_party_cd) %>% mutate(prop = n/sum(n))


blau_index_data_rural_0.01_2 <- voted_parties_rural_0.01_2 %>% group_by(epoch) %>%
  mutate(blau_index = 1-(sum(prop^2)))



ggplot(blau_index_data_rural_0.01_2, aes(x=epoch,y= blau_index))+
  geom_line() +theme_bw() +ylim(0.5,0.7) + labs(y="Blau Index", x = "Epoch")


ggplot(voted_parties_rural_0.01_2, aes(x=epoch, y = prop, group = voted_party_cd, color = voted_party_cd))+  geom_line()  +theme_bw()  +   scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple"))+
  labs(title="Change in Partisan Affiliation: Rural Neighborhood", y = "Number", x = "Epoch", color = "Party Affiliation") + ylim(0,0.8)


##### wrong doesnt work !!!!!! #######


#######################
#### Urban Zipcode ####
#######################



load("~/Desktop/LSE Term 2/Capstone-Project/ABM/Results ABM/interaction_15urban_0.01.RData")

load("~/Desktop/LSE Term 2/Capstone-Project/ABM/Results ABM/interaction_urban_0.005.RData")


##############################
#### Plot Blau Index
###############################



##### this model is perfect !!!!!!!! 


load("~/Desktop/LSE Term 2/Capstone-Project/ABM/Results ABM/interaction_urban_0.005.RData")


blau_index_urban_0.005 <- interaction_urban_0.005$blau_indices
interaction_epoch_0.005_urban <- interaction_urban_0.005$interactions_epoch

ggplot(blau_index_urban_0.005, aes(x=epoch,y= blau_index))+
  geom_line() +theme_bw() +ylim(0.5,0.7) + labs(y="Blau Index", x = "Epoch")

ggsave("blau_index_urban_0.005.png", dpi = 95)




voted_parties_urban_0.005 <- interaction_epoch_0.005_urban %>% group_by(epoch) %>%
  count(voted_party_cd) %>% mutate(prop = n/sum(n))

blau_index_data_urban_0.005 <- voted_parties_urban_0.005 %>% group_by(epoch) %>%
  mutate(blau_index = 1-(sum(prop^2)))



ggplot(blau_index_data_urban_0.005, aes(x=epoch,y= blau_index))+
  geom_line() +theme_bw() +ylim(0.5,0.7) + labs(y="Blau Index", x = "Epoch")

ggsave("blau_index_urban_0.005.png", dpi = 95)



ggplot(voted_parties_urban_0.005, aes(x=epoch, y = prop, group = voted_party_cd, color = voted_party_cd))+  geom_line()  +theme_bw()  +   scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple"))+
  labs(title="Change in Partisan Affiliation: Urban Neighborhood", y = "Number", x = "Epoch", color = "Party Affiliation") + ylim(0,0.8)


ggsave("voted_parties_urban_0.005.png", dpi = 95)


save(interaction_urban_0.005, file = "interaction_urban_0.005.RData")




########## combining empirical and simulation data in one plot ###################


blau_index_data_28203 <- blau_index_data %>% filter(zip_code== "28203", voted_party_cd == "DEM", election_lbl!= "2014-11-04")
blau_index_data_28203$epoch <- c(0,100,200,300,400)

ggplot()+
  geom_line(data= blau_index_data_urban_0.005, aes(x=epoch,y= blau_index, color = "Simulation Data")) +
  geom_line(data=blau_index_data_28203, aes(x=epoch,y= blau_index, color = "Empirical Data"))+
  scale_color_manual(values = c("Simulation Data" = "chartreuse3","Empirical Data" = "black"))+
  theme_bw() +ylim(0.5,0.7) + labs(y="Blau Index", x = "Epoch", color = "Data")

ggsave("blau_index_verification_urban.png", dpi = 95)





##### Entropy ####

nc_stats_ruca_entropy <- nc_stats_zipcode %>%
  group_by(zip_code, election_lbl) %>%
  summarise(entropy = -sum(perc * log(perc)))

entropy_data_28203 <- nc_stats_ruca_entropy %>% filter(zip_code== "28203", election_lbl!= "2014-11-04")
entropy_data_28203$epoch <- c(0,100,200,300,400)


entropy_data_urban_0.005 <- voted_parties_urban_0.005 %>% group_by(epoch) %>%
  mutate(entropy = -sum(prop * log(prop)))


ggplot()+
  geom_line(data= entropy_data_urban_0.005, aes(x=epoch,y= entropy, color = "Simulation Data")) +
  geom_line(data=entropy_data_28203, aes(x=epoch,y= entropy, color = "Empirical Data"))+
  scale_color_manual(values = c("Simulation Data" = "chartreuse3","Empirical Data" = "black"))+
  theme_bw() +ylim(0.8,1.2) + labs(y="Entropy", x = "Epoch", color = "Data")



ggsave("entropy_verification_urban.png", dpi = 95)


######## voted parties #########

voted_parties_data_28203 <- nc_stats_zipcode %>% filter(zip_code== "28203", election_lbl!= "2014-11-04")
voted_parties_data_28203 <- voted_parties_data_28203 %>% mutate(epoch = case_when(election_lbl == "2016-11-08" ~ 0,
                                                                                  election_lbl == "2018-11-06" ~ 100,
                                                                                  election_lbl == "2020-11-03" ~ 200,
                                                                                  election_lbl == "2022-11-08" ~ 300,
                                                                                  election_lbl == "2024-07-25" ~ 400))


ggplot()+
  geom_line(data = voted_parties_urban_0.005, aes(x=epoch, y = prop, group = voted_party_cd, color = voted_party_cd, linetype = "Simulation Data")) +
  geom_line(data=voted_parties_data_28203, aes(x=epoch,y= perc,group = voted_party_cd, color = voted_party_cd, linetype = "Empirical Data"))+
  scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple", "GRE" = "green"))+
  scale_linetype_manual(values = c("Simulation Data" = "longdash","Empirical Data" = "solid"))+
  theme_bw()+ labs(y="Blau Index", x = "Epoch", color = "Party", linetype = "Data")


ggsave("voted_parties_verification_urban.png", dpi = 95)




##############################
#### Plot voting probabilities
###############################


vote_probs_urban_0.005 <-interaction_epoch_0.005_urban %>%
  tidyr::pivot_wider(names_from = party, values_from = vote_prob) 


# Plot vote probabilities over time for each party
ggplot(vote_probs_urban_0.005, aes(x = epoch)) +
  geom_smooth(aes(y = DEM, color = "DEM")) +
  geom_smooth(aes(y = REP, color = "REP")) +
  geom_smooth(aes(y = UNA, color = "UNA")) +
  #geom_smooth(aes(y = NLB, color = "NLB")) +
  labs(title = "Vote Probabilities Over Time: Urban Neighborhood",
       x = "Epoch",
       y = "Vote Probability",
       color = "Party") +
  scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple"))+
  ylim(0,1)+
  theme_bw()

ggsave("vote_probs_urban_0.005.png", dpi = 95)




################################################################
#### Plot individual changes and compare it with empirical data 
################################################################


epoch1_voting_urban <- interaction_epoch_0.005_urban %>% filter(epoch ==1) %>% dplyr::select(agent, voted_party_cd)
epoch1_voting_urban <- unique(epoch1_voting_urban)

table(epoch1_voting_urban$voted_party_cd)


epoch1_voting_urban <- epoch1_voting_urban %>% rename("party_voted_epoch1" = voted_party_cd)

epoch400_voting_urban <- interaction_epoch_0.005_urban %>% filter(epoch ==400) %>% dplyr::select(agent, voted_party_cd)
epoch400_voting_urban <- unique(epoch400_voting_urban)

table(epoch400_voting_urban$voted_party_cd)


epoch400_voting_urban <- epoch400_voting_urban %>% rename("party_voted_epoch400" = voted_party_cd)
simulated_party_urban <- left_join(epoch1_voting_urban, epoch400_voting_urban, by = "agent")

simulated_party_urban <- simulated_party_urban %>% mutate(party_change_simulated = case_when(party_voted_epoch1 == party_voted_epoch400 ~ 0,
                                                                                   party_voted_epoch1 != party_voted_epoch400 ~ 1))
table(simulated_party_urban$party_change_simulated)



### empirical data ##

###### creating variable for voter change ######


nc_28203_2024 <- nc_28203_2024 %>% mutate(party_change = case_when(party_voted_2016 == party_voted_2024 ~ 0,
                                                                   party_voted_2016 != party_voted_2024 ~ 1))


table(nc_28203_2024$party_change)

nc_28203_2024_long <- pivot_longer(nc_28203_2024, cols = c(party_voted_2016, party_voted_2024),
                                   names_to = "year",
                                   values_to = "party_affiliation")



table(nc_28203_2024$party_voted_2016)
table(nc_28203_2024$party_voted_2024)





######### Verifiying individual switches ############


empirical_party_2024_urban <- nc_28203_2024$party_voted_2024
simulation_party_2024_urban <- epoch400_voting_urban$party_voted_epoch400




# Calculate the accuracy
accuracy_urban_0.005 <- sum(empirical_party_2024_urban == simulation_party_2024_urban) / length(empirical_party_2024_urban)

# Print the accuracy as a percentage
cat("Prediction Accuracy:", accuracy_urban_0.005 * 100, "%\n")




######## precision for switching and staying ##########

empirical_vote_switches_urban <- nc_28203_2024$party_change
simulated_vote_switches_urban <- simulated_party_urban$party_change_simulated



F1(empirical_vote_switches_urban, simulated_vote_switches_urban)
Precision(empirical_vote_switches_urban, simulated_vote_switches_urban)

accuracy_urban_0.005 <- sum(empirical_vote_switches_urban == simulated_vote_switches_urban) / length(empirical_vote_switches_urban)

Recall(empirical_vote_switches_urban, simulated_vote_switches_urban)





########## mu = 0.01 ################

load("~/Desktop/LSE Term 2/Capstone-Project/ABM/Results ABM/interaction_urban_0.01.RData")


interaction_urban_0.01 <- interaction_15urban_0.01

blau_index_urban_0.01 <- interaction_urban_0.01$blau_indices
interaction_epoch_0.0_urban <- interaction_urban_0.01$interactions_epoch

ggplot(blau_index_urban_0.01, aes(x=epoch,y= blau_index))+
  geom_line() +theme_bw() +ylim(0.3,0.7) + labs(y="Blau Index", x = "Epoch")

ggsave("blau_index_urban_0.01.png", dpi = 95)




voted_parties_urban_0.01 <- interaction_epoch_0.0_urban %>% group_by(epoch) %>%
  count(voted_party_cd) %>% mutate(prop = n/sum(n))

blau_index_data_urban_0.01 <- voted_parties_urban_0.01 %>% group_by(epoch) %>%
  mutate(blau_index = 1-(sum(prop^2)))



ggplot(blau_index_data_urban_0.01, aes(x=epoch,y= blau_index))+
  geom_line() +theme_bw() +ylim(0.4,0.7) + labs(y="Blau Index", x = "Epoch")

ggsave("blau_index_urban_0.01.png", dpi = 95)



ggplot(voted_parties_urban_0.01, aes(x=epoch, y = prop, group = voted_party_cd, color = voted_party_cd))+  geom_line()  +theme_bw()  +   scale_color_manual(values = c("DEM" = "blue", "REP" = "red", "UNA" = "purple"))+
  labs(title="Change in Partisan Affiliation: Urban Neighborhood", y = "Number", x = "Epoch", color = "Party Affiliation") + ylim(0,0.8)


ggsave("voted_parties_urban_0.01.png", dpi = 95)


save(interaction_urban_0.01, file = "interaction_urban_0.01.RData")









