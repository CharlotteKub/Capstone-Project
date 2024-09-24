
library(dplyr)


ncvoter_Statewide <- read.delim("~/Desktop/Charlotte MA/ncvoter_Statewide.txt")

ncvhis_Statewide <- read.delim("~/Desktop/Charlotte MA/ncvhis_Statewide.txt")

voter_stats_2012 <- read.delim("~/Desktop/Charlotte MA/voter_stats_20121106.txt")

remove(ncvhis_Statewide_28202)

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


participation <-arrange(participation, by_group = n)




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




table(nc_voter_2014_1$voted_party_cd)
table(nc_voter_2016_1$voted_party_cd)
table(nc_28202$party_cd)



nc_stats_all_1 <- rbind(nc_stats_2014_1, nc_stats_2016_1)

nc_stats_all_1 <- rbind(nc_stats_all_1, nc_stats_2020_1)

nc_stats_all_1 <- rbind(nc_stats_all_1, nc_stats_2024_1)


nc_stats_all_1$location <- "U"


nc_stats <- rbind(nc_stats_all, nc_stats_all_1)



library(ggplot2)


ggplot(nc_stats, aes(year, perc, group = voted_party_cd, col = voted_party_cd)) +
  geom_line() + facet_wrap(~location)+
  theme_bw() + ylim(0,1)


########## doing the same thing for all zipcodes differentiatet by rural/urban ######


library(readr)

ruca <- readxl::read_xlsx("~/Desktop/LSE Term 2/Capstone-Project/Data/NC Voter Registration Data/RUCA_US.xlsx")

remove(ruca)

ruca_nc <-ruca %>% filter(STATE=="NC")

ruca_nc <- ruca_nc %>% rename("zip_code" = ZIP_CODE)

ruca_nc$zip_code <- as.integer(ruca_nc$zip_code)
ncvoter_Statewide$zip_code <- as.integer(ncvoter_Statewide$zip_code)

nc_voter_all <- left_join(ncvoter_Statewide, ruca_nc, by= "zip_code")

remove(nc_voter_all)

nc_ncid_ruca <- nc_voter_all %>% dplyr::select(ncid, RUCA1, zip_code, party_cd)


nc_voter_all_ncid <- nc_voter_all$ncid


nc_voter_hist <- ncvhis_Statewide %>% filter(ncid%in% nc_voter_all_ncid)

nc_voter_hist <- nc_voter_hist %>% dplyr::select(ncid, voted_party_cd, election_lbl)

remove(nc_voter_hist)


table(nc_voter_hist$election_lbl)

nc_voter_hist_ruca <- left_join(nc_voter_hist, nc_ncid_ruca, by="ncid")

nc_voter_hist_ruca <- nc_voter_hist_ruca %>% filter(election_lbl %in% c("11/04/2014","11/08/2016", "11/06/2018","11/03/2020", "11/08/2022")) %>% 
  filter(voted_party_cd %in% c("DEM", "REP", "GRE", "UNA", "NLB"))

nc_voter_hist_ruca$election_lbl <- lubridate::mdy(nc_voter_hist_ruca$election_lbl)

nc_voter_hist_ruca_1_10 <- nc_voter_hist_ruca %>% filter(RUCA1 == 1 | RUCA1 == 10)

nc_stats_all_overall <- nc_voter_hist_ruca %>%
  group_by(voted_party_cd, RUCA1, election_lbl) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(election_lbl, RUCA1) %>%
  mutate(perc = count / sum(count))



nc_stats_all_ruca <- nc_voter_hist_ruca_1_10 %>% group_by(voted_party_cd, RUCA1, election_lbl) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(election_lbl, RUCA1) %>%
  mutate(perc = count / sum(count))

table(nc_stats_all_overall$voted_party_cd)


ggplot(nc_stats_all_ruca, aes(election_lbl, perc, group = voted_party_cd, col = voted_party_cd)) +
  geom_line() + facet_wrap(~RUCA1)+
  theme_bw() + ylim(0,1)


########### now get the 2024 data as well ######

nc_stats_2024 <- nc_ncid_ruca %>% group_by(party_cd,RUCA1) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(RUCA1) %>%
  mutate(perc = count / sum(count))


nc_stats_2024$election_lbl <- "07/25/2024"

nc_stats_2024$election_lbl <- lubridate::mdy(nc_stats_2024$election_lbl)
nc_stats_2024 <- nc_stats_2024 %>% rename("voted_party_cd" = party_cd)


nc_stats_merged <- rbind(nc_stats_all_overall, nc_stats_2024)

ggplot(nc_stats_merged, aes(election_lbl, perc, group = voted_party_cd, col = voted_party_cd)) +
  geom_line() + facet_wrap(~RUCA1)+
  theme_bw() + ylim(0,1)






###### same but without including UNA

nc_voter_hist_ruca_no_UNA <- nc_voter_hist_ruca %>% filter(election_lbl %in% c("2014-11-04","2016-11-08", "2018-11-06","2020-11-03", "2022-11-08")) %>% 
  filter(voted_party_cd %in% c("DEM", "REP", "GRE", "NLB", "LIB"))

# nc_voter_hist_ruca_no_UNA$election_lbl <- lubridate::mdy(nc_voter_hist_ruca_no_UNA$election_lbl)

nc_voter_hist_ruca_no_UNA_1_10 <- nc_voter_hist_ruca_no_UNA %>% filter(RUCA1 == 1 | RUCA1 == 10)


nc_stats_all_overall_no_UNA <- nc_voter_hist_ruca_no_UNA_1_10 %>%
  group_by(voted_party_cd, RUCA1, election_lbl) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(election_lbl, RUCA1) %>%
  mutate(perc = count / sum(count))



nc_stats_2024_no_UNA <- nc_ncid_ruca %>% filter(party_cd != "UNA") %>% filter(RUCA1 == 1 | RUCA1 == 10)
table(nc_stats_2024_no_UNA$party_cd)



nc_stats_2024_no_UNA <- nc_stats_2024_no_UNA %>% group_by(party_cd,RUCA1) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(RUCA1) %>%
  mutate(perc = count / sum(count))


nc_stats_2024_no_UNA$election_lbl <- "07/25/2024"

nc_stats_2024_no_UNA$election_lbl <- lubridate::mdy(nc_stats_2024_no_UNA$election_lbl)
nc_stats_2024_no_UNA <- nc_stats_2024_no_UNA %>% rename("voted_party_cd" = party_cd)

nc_stats_merged_no_UNA <- rbind(nc_stats_all_overall_no_UNA, nc_stats_2024_no_UNA)

ggplot(nc_stats_merged_no_UNA, aes(election_lbl, perc, group = voted_party_cd, col = voted_party_cd)) +
  geom_line() + facet_wrap(~RUCA1)+
  theme_bw() + ylim(0,1)




####### only RUCA 1 AND 10 #########

nc_ncid_ruca_1_10 <- nc_ncid_ruca %>% filter(RUCA1 == 1 | RUCA1 == 10)

nc_stats_2024_ruca <- nc_ncid_ruca_1_10 %>% group_by(party_cd,RUCA1) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(RUCA1) %>%
  mutate(perc = count / sum(count))


nc_stats_2024_ruca$election_lbl <- "07/25/2024"

nc_stats_2024_ruca$election_lbl <- lubridate::mdy(nc_stats_2024_ruca$election_lbl)
nc_stats_2024_ruca <- nc_stats_2024_ruca %>% rename("voted_party_cd" = party_cd)


nc_stats_merged_ruca <- rbind(nc_stats_all_ruca, nc_stats_2024_ruca)

ggplot(nc_stats_merged_ruca, aes(election_lbl, perc, group = voted_party_cd, col = voted_party_cd)) +
  geom_line() + facet_wrap(~RUCA1)+
  theme_bw() + ylim(0,1)





### we can see a homogenization in the rural areas which is due to the increase in rep and decrease in dem, and no such thing in urban areas
#### this supports my hypothesis


#########################################################
#### Calculating Entropy and Blau Index for Homogeneity
#########################################################



calculate_blau_index <- function(data, party_column) {
  party_counts <- table(data[[party_column]])
  total_count <- sum(party_counts)
  proportions <- party_counts / total_count
  blau_index <- 1 - sum(proportions^2)
  return(blau_index)
}

# Example usage
blau_index <- calculate_blau_index(your_data, "party_column_name")


calculate_entropy <- function(data, party_column) {
  party_counts <- table(data[[party_column]])
  total_count <- sum(party_counts)
  proportions <- party_counts / total_count
  entropy <- -sum(proportions * log(proportions))
  return(entropy)
}

# Example usage
entropy <- calculate_entropy(your_data, "party_column_name")



nc_stats_merged_ruca_BLAU <- nc_stats_merged_ruca %>%
  group_by(RUCA1, election_lbl) %>%
  summarise(blau_index = 1 - sum(perc^2))

nc_stats_merged_ruca_entropy <- nc_stats_merged_ruca %>%
  group_by(RUCA1, election_lbl) %>%
  summarise(entropy = -sum(perc * log(perc)))




#######################################
### calculate stats without UNA ######

nc_stats_merged_no_UNA


nc_stats_merged_no_UNA_BLAU <- nc_stats_merged_no_UNA %>%
  group_by(RUCA1, election_lbl) %>%
  summarise(blau_index = 1 - sum(perc^2))

nc_stats_merged_no_UNA_entropy <- nc_stats_merged_no_UNA %>%
  group_by(RUCA1, election_lbl) %>%
  summarise(entropy = -sum(perc * log(perc)))
