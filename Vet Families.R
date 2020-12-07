#' Original Script by Christina, early 2019, which pulls and approximates the
#' average family size of a vet's family (how many people on average make up a
#' veteran's family in X region?)
#' 
#' Updated by Michael, December 2020; changed used packages considering we only needed one out of 
#' the entire tidyverse that was being loaded. 
#' 
#' STOP USING UNECESSARY LIBRARIES


library(dplyr)
# PUMAs Requested

# puma <- c(05301, 05309) # Travis County
# puma <- c(05201, 05202, 05203, 05204) # Williamson County

puma <- c(05301, 05309, # Travis County
          05201, 05202, 05203, 05204) # Williamson County

# load PUMS housing data
housing <- read.csv("D:/MMHPI/Master File and Prevelance Estimates/2018 Prevelence Run/Holzer Prevalence/data/psam_h48.csv", stringsAsFactors = F)

# load PUMS person data
person <- read.csv("D:/MMHPI/Master File and Prevelance Estimates/2018 Prevelence Run/Holzer Prevalence/data/psam_p48.csv", stringsAsFactors = F)


# subset to lubbock PUMAs
loc.house <- subset(housing, PUMA %in% puma)

# subset to harris county pop
loc.person <- subset(person, PUMA %in% puma)
remove(housing)
remove(person)

#subset of housing variables I need
lubh_sub <- select(loc.house, SERIALNO, NP, NPF,WGTP)
#SERIALNO: Housing unit/GQ person serial number
#NP: Number of persons associated with this housing record
#NPF: Number of persons in family (unweighted)
#WGTP: Housing Unit Weight

#merge in household data
lubmerged <- inner_join(loc.person,lubh_sub, by = "SERIALNO")

#check for empty values
table(lubmerged$NP)

#get subset of data needed for further analysis
lubsub <- select(lubmerged, SERIALNO,NPF,PWGTP,WGTP, VPS)
#PWGTP: Person weight
#VPS: Veteran period of service, bb .N/A (less than 17 years old, no active duty)

#get vets in subset
lubvets <- na.omit(lubsub, cols="VPS")

#We now have a df with one row for each vet. It is a person level DF.
#Now convert this to a household DF by counting up the number of rows per household, and make this a new variable.
#count the number of rows per serialno
vetsph <- data.frame(table(lubvets$SERIALNO))
colnames(vetsph) <- c("SERIALNO","VPH")
#join the tables
vetsbyhouse <- inner_join(lubvets,vetsph, by = "SERIALNO")
#This gives us the number of vets per household for households with any vets. There should
#be one row per household. We need to retain the household weight WTGP, but not the person weight PWGTP.
#get unique households by removing duplicate serialno
vetsbyhu <- vetsbyhouse[!duplicated(vetsbyhouse$SERIALNO), ]

#Then calculate the number of non-vets per household by subtracting from the number of persons in the family NPF
# the number of vets.
vetsbyhu$NONVPH <- vetsbyhu$NPF - vetsbyhu$VPH

#average this number, using the household weights.
#group the records by non veterans per household and sum up each number with household weights 
avgfs_nonv <- vetsbyhu %>% group_by(NONVPH) %>% count(wt = WGTP)

#multiple number of non veteran household members by the sum of total weights for each number 
#divide the sum of the weighted numbers of non veterans in households by the sum of the weights (which because we used household weights should = total # of households )
avgfsuu <- sum(avgfs_nonv$NONVPH*avgfs_nonv$n)/sum(avgfs_nonv$n)

avgfsuu
