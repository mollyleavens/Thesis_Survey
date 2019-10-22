# Script written by Molly Leavens 
# Last edited:

library(tidyverse)

# This dataset is formed in "Drafting_Dataset.R"
all_data_hh <- read_csv("individualHH_cleaned_both_waves.csv")

### This is the actual on-farm practices from the farm questionaire
# and questions from b3 on if farmers think they are doing something
# note: this is just wave 1

# --------- SHADE TREES ---------
# How many farmers have shade trees? (P15)
# 150 no 769 yes
table(all_data_hh$shadetree_farm)

# How many farmers say they have shade trees? (C35)
# 74% say yes in wave 1
# 78% say yes in wave 2
table(all_data_hh$ggshadetree.1)
table(all_data_hh$ggshadetree.2)

# How closed is the canopy? (P18)
# 237 canopy closed, 569 canopy partially open, 112 canopy wide open 
table(all_data_hh$canopy_farm)

# is there a relationship between those with shade trees and those who say they have them? 
# FIGURE OUT THESE RESULTS AND PUT INTO PAPER 
shade_model <- glm(shadetree_farm ~ ggshadetree.1, data=all_data_hh, family = binomial)
summary(shade_model)

# is there a relationship between those with shade trees and canopy cover? 
# THIS DID NOT WORK FOR SOME REASON 
canopy_model <- glm(canopy_farm ~ shadetree_farm, data=all_data_hh, family = binomial)
summary(canopy_model)

# --------- PRUNING ---------
# How many farmers prune?
# 254 yes 669 no 
table(all_data_hh$prune_farm)

# How many farmers say they prune? (b3)
# This is reported as how many household members engaged in pruning
# 74% say yes in wave 1
# 57% say yes in wave 2
table(all_data_hh$b3_prune_all.1)
table(all_data_hh$b3_prune_all.2)

# is there a relationship between those who acutally prune and those who say they do? 
#no relationship between self-reported and actual farm 
prune_model <- glm(prune_farm ~ b3_prune_all.1, data=all_data_hh, family = binomial)
summary(prune_model)

# BUT, there is an association if I recategorize the pruning variable as a dumi varible
all_data_hh$b3_prune_all.1[all_data_hh$b3_prune_all.1>1] <- 1
prune_model <- glm(prune_farm ~ b3_prune_all.1, data=all_data_hh, family = binomial)
summary(prune_model)

# --------- WEEDING ---------
# How many farmers weed?
# 431 is clean, 333 has weeds 1 foot high, and 137 higher than 1 foot.
table(all_data_hh$weed_farm)

all_data_hh$weed_farm <- as.numeric(all_data_hh$weed_farm)


# How many farmers say they weed? (b3)
# 94% say yes in wave 1
# 54% say yes in wave 2
table(all_data_hh$b3_weed_all.1)
table(all_data_hh$b3_weed_all.2)

# is there a relationship between those who acutally weed and those who say they do? 
# THIS IS NOT WORKING
weed_model <- glm(weed_farm ~ b3_weed_all.1, data=all_data_hh, family = binomial)
summary(weed_model)

# THEN recategorize the pruning variable as a dumi varible and try again
all_data_hh$b3_weed_all.1[all_data_hh$b3_weed_all.1>1] <- 1
weed_model <- glm(weed_farm ~ b3_weed_all.1, data=all_data_hh, family = binomial)
summary(weed_model)

# --------- ROWS ---------
# How many farmers have rows? (P13)
# 858 no rows, 72 yes
table(all_data_hh$row_farm)

# How many farmers say they have rows? (C34)
# 12% say yes in wave 1
# 20% say yes in wave 2
table(all_data_hh$ggrows.1)
table(all_data_hh$ggrows.2)

# Are shade tree planted in rows? (P16)
# 760 no and 22 yes
table(all_data_hh$shadetree_row_farm)

# How many farmers plant trees too close together or too far apart?
# 441 are too close, 410 are good, 74 are too far 
table(all_data_hh$tree_dist_farm)

# ARE SHADE TREE ROWS AND TREE ROWS RELATED?
# IS THIS A PERFECT CORRELATION? HOW DO YOU TEST FOR THAT?
rows_model <- glm(row_farm ~ shadetree_row_farm, data=all_data_hh, family = binomial)
summary(rows_model)

# is there a relationship between those who acutally have rows and those who say they do? 
# IS THIS A PERFECT CORRELATION? HOW DO YOU TEST FOR THAT?
row_model <- glm(row_farm ~ ggrows.1, data=all_data_hh, family = binomial)
summary(row_model)

# IF NOT TESTED ELSEWHERE, SEE IF ROWS IS CORRELATED WITH SHADE TREES GENERALLY

# --------- HYBRIDS ---------
table(wave1Data$gghybrid)
# 2094/(715+2094) 75% have hybrids

table(wave2Data$c29yesno_1) #717 1784
table(wave2Data$c29yesno_2) #404 1022 
table(wave2Data$c29yesno_3) #153 451 
table(wave2Data$c29yesno_4) #54 127
table(wave2Data$c29yesno_5) #14 37
#w2 72% have hybrids

# --------- FERTILIZER ---------
# How many farmers apply fertilzer (yes or no question D35)?
# wave 1 23% say yes
# wave 2 35% say yes
table(all_data_hh$ggdumfert.1)
table(all_data_hh$ggdumfert.2)

# How many household members apply fertilizer? (b3)
# 22% have at least 1 in wave 1 and 25% say yes in wave 2
table(all_data_hh$b3_fertilizer_all.1)
summary(all_data_hh$b3_fertilizer_all.1)

# --------- PESTICIDES ---------
# How many apply pesticides? (b3)
# 39% say yes in wave 1
# 50% say yes in wave 2
table(all_data_hh$b3_pesticides_all.1)
table(all_data_hh$b3_pesticides_all.2)




