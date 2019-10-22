library(tidyverse)
library(car)
library(mice)

# This dataset is formed in "Drafting_Dataset.R"
all_data_hh <- read_csv("individualHH_cleaned_both_waves.csv")

# ----------------------------
# This script assess which variables are most associated
# with each farming practice

# MICE
# I am making assumption that missingness is not correlated with responses
# I COULD CHECK VALIDITY OF ASSUMPTION
# This code chuck takes a couple minutes to run

# ----------------------------

# ------ PRUNING ------
# Select variables for this analysis
data <- 
  all_data_hh %>% 
  select(prune_farm,
    # Household Demographics 
    gghhsize.1, ggagehhhead.1, gggenderhhhead.1, propFemaleCocoaLabor.1,           
    ggliterate.1, ggschoolinghhhead.1, ggborninvillage.1, 
    gghealthinsurance.1, ggwouldlosefallow.1, 
    # Income and Finances
    ggincometotalcap.1, ggsavings.1, ggreceivedloan.1, 
    ggbank.1, ggsusu.1, ggSpouseAccount.1,
    # Cocoa Production and Losses
    ggfsizecocoaacres.1, ggyieldall_cocarea.1, 
    ggfraclost.1, ggyoungtrees.1,
    # Equipment
    c41cutlass.1, c41pruner.1,
    # Inputs
    ggdumfert.1, gginsect.1, ggherb.1, ggfungi.1,
    # Certifications and Groups
    ggorggroup.1, ggcertifiedfarmerorg.1,
    ggknowsft.1, ggorganic.1, ggOrgLeader.1, 
    ggFarmGroupOrCoop.1,
    # Training
    ggTrainingNum.1)

# Select rows with data for farm pruning
prune_data <- data[complete.cases(data$prune_farm),]

# MICE to fill NAs
mice_data <- mice(prune_data)
prune_data <- complete(mice_data,1)

# Test to ensure all there are no NAs
na_count <-sapply(prune_data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
# MICE was sucessful. There are no NAs in new dataframe. 

# Pruning model
prune_model <- 
  glm(prune_farm ~ 
       # Household Demographics 
       gghhsize.1+ ggagehhhead.1+ gggenderhhhead.1+ propFemaleCocoaLabor.1+           
       ggliterate.1+ ggschoolinghhhead.1+ ggborninvillage.1+ 
       gghealthinsurance.1+ ggwouldlosefallow.1+ 
       # Income and Finances
       ggincometotalcap.1+ ggsavings.1+ ggreceivedloan.1+ 
       ggbank.1+ ggsusu.1+ ggSpouseAccount.1+
       # Cocoa Production and Losses
       ggfsizecocoaacres.1+ ggyieldall_cocarea.1+ 
       ggfraclost.1+ ggyoungtrees.1+
       # Equipment
       c41cutlass.1+ c41pruner.1+
       # Inputs
       ggdumfert.1+ gginsect.1+ ggherb.1+ ggfungi.1+
       # Certifications and Groups
       ggorggroup.1+ ggcertifiedfarmerorg.1+
       ggknowsft.1+ ggorganic.1+ ggOrgLeader.1+ 
       ggFarmGroupOrCoop.1+
       # Training
       ggTrainingNum.1,
    family = "binomial",
    data=prune_data)

# Nested models
# Make models with grouping for different variables
# Likihood ratio test
Household_Demographics_model <- 
  glm(prune_farm ~ 
        gghhsize.1+ ggagehhhead.1+ gggenderhhhead.1+ 
        propFemaleCocoaLabor.1+ ggliterate.1+ ggschoolinghhhead.1+
        ggborninvillage.1+ gghealthinsurance.1+ ggwouldlosefallow.1,
      family = "binomial",
      data=prune_data)
        
Income_Finances_model <-
  glm(prune_farm ~
        ggincometotalcap.1+ ggsavings.1+ ggreceivedloan.1+ 
        ggbank.1+ ggsusu.1+ ggSpouseAccount.1,
      family = "binomial",
      data=prune_data)

Production_Losses_model <-
  glm(prune_farm ~
        ggfsizecocoaacres.1+ ggyieldall_cocarea.1+ 
        ggfraclost.1+ ggyoungtrees.1,
      family = "binomial",
      data=prune_data)

Tools_model <-
  glm(prune_farm ~
        c41cutlass.1+ c41pruner.1,
      family = "binomial",
      data=prune_data)

Inputs_model <-
  glm(prune_farm ~
        ggdumfert.1+ gginsect.1+ ggherb.1+ ggfungi.1,
      family = "binomial",
      data=prune_data)

Groups_model <-
  glm(prune_farm ~ 
        ggorggroup.1+ ggcertifiedfarmerorg.1+
        ggknowsft.1+ ggorganic.1+ ggOrgLeader.1+ ggFarmGroupOrCoop.1,
      family = "binomial",
      data=prune_data)

Training_model <-
  glm(prune_farm ~ 
        ggTrainingNum.1,
      family = "binomial",
      data=prune_data)

anova(prune_model, Household_Demographics_model, test = "Chi") 
anova(prune_model, Income_Finances_model, test = "Chi")
anova(prune_model, Production_Losses_model, test = "Chi")
anova(prune_model, Tools_model, test = "Chi")
anova(prune_model, Inputs_model, test = "Chi")
anova(prune_model, Groups_model, test = "Chi")
anova(prune_model, Training_model, test = "Chi")

# mutliple comparison adjustments 
# need to add a penality to p-value with so many variables. 
pvals <- coef(summary(prune_model))[,4]
p.adjust(pvals, "holm")

# ------ SHADETREES ------

# Select variables for this analysis
data <- 
  all_data_hh %>% 
  select(shadetree_farm,
         # Household Demographics 
         gghhsize.1, ggagehhhead.1, gggenderhhhead.1, propFemaleCocoaLabor.1,           
         ggliterate.1, ggschoolinghhhead.1, ggborninvillage.1, 
         gghealthinsurance.1, ggwouldlosefallow.1, 
         # Income and Finances
         ggincometotalcap.1, ggsavings.1, ggreceivedloan.1, 
         ggbank.1, ggsusu.1, ggSpouseAccount.1,
         # Cocoa Production and Losses
         ggfsizecocoaacres.1, ggyieldall_cocarea.1, 
         ggfraclost.1, ggyoungtrees.1,
         # Equipment
         c41cutlass.1, c41pruner.1,
         # Inputs
         ggdumfert.1, gginsect.1, ggherb.1, ggfungi.1,
         # Certifications and Groups
         ggorggroup.1, ggcertifiedfarmerorg.1,
         ggknowsft.1, ggorganic.1, ggOrgLeader.1, 
         ggFarmGroupOrCoop.1,
         # Training
         ggTrainingNum.1)

# Select rows with data for farm pruning
shadetree_data <- data[complete.cases(data$shadetree_farm),]

# MICE to fill NAs
mice_data <- mice(shadetree_data)
shadetree_data <- complete(mice_data,1)

# Shadetree model
shadetree_model <- 
  glm(shadetree_farm ~ 
        # Household Demographics 
        gghhsize.1+ ggagehhhead.1+ gggenderhhhead.1+ propFemaleCocoaLabor.1+           
        ggliterate.1+ ggschoolinghhhead.1+ ggborninvillage.1+ 
        gghealthinsurance.1+ ggwouldlosefallow.1+ 
        # Income and Finances
        ggincometotalcap.1+ ggsavings.1+ ggreceivedloan.1+ 
        ggbank.1+ ggsusu.1+ ggSpouseAccount.1+
        # Cocoa Production and Losses
        ggfsizecocoaacres.1+ ggyieldall_cocarea.1+ 
        ggfraclost.1+ ggyoungtrees.1+
        # Equipment
        c41cutlass.1+ c41pruner.1+
        # Inputs
        ggdumfert.1+ gginsect.1+ ggherb.1+ ggfungi.1+
        # Certifications and Groups
        ggorggroup.1+ ggcertifiedfarmerorg.1+
        ggknowsft.1+ ggorganic.1+ ggOrgLeader.1+ 
        ggFarmGroupOrCoop.1+
        # Training
        ggTrainingNum.1,
      family = "binomial",
      data=shadetree_data)

# Nested models
# Make models with grouping for different variables
# Likihood ratio test
Household_Demographics_model <- 
  glm(shadetree_farm ~ 
        gghhsize.1+ ggagehhhead.1+ gggenderhhhead.1+ 
        propFemaleCocoaLabor.1+ ggliterate.1+ ggschoolinghhhead.1+
        ggborninvillage.1+ gghealthinsurance.1+ ggwouldlosefallow.1,
      family = "binomial",
      data=shadetree_data)

Income_Finances_model <-
  glm(shadetree_farm ~
        ggincometotalcap.1+ ggsavings.1+ ggreceivedloan.1+ 
        ggbank.1+ ggsusu.1+ ggSpouseAccount.1,
      family = "binomial",
      data=shadetree_data)

Production_Losses_model <-
  glm(shadetree_farm ~
        ggfsizecocoaacres.1+ ggyieldall_cocarea.1+ 
        ggfraclost.1+ ggyoungtrees.1,
      family = "binomial",
      data=shadetree_data)

Tools_model <-
  glm(shadetree_farm ~
        c41cutlass.1+ c41pruner.1,
      family = "binomial",
      data=shadetree_data)

Inputs_model <-
  glm(shadetree_farm ~
        ggdumfert.1+ gginsect.1+ ggherb.1+ ggfungi.1,
      family = "binomial",
      data=shadetree_data)

Groups_model <-
  glm(shadetree_farm ~ 
        ggorggroup.1+ ggcertifiedfarmerorg.1+
        ggknowsft.1+ ggorganic.1+ ggOrgLeader.1+ ggFarmGroupOrCoop.1,
      family = "binomial",
      data=shadetree_data)

Training_model <-
  glm(shadetree_farm ~ 
        ggTrainingNum.1,
      family = "binomial",
      data=shadetree_data)

anova(shadetree_model, Household_Demographics_model, test = "Chi") 
anova(shadetree_model, Income_Finances_model, test = "Chi")
anova(shadetree_model, Production_Losses_model, test = "Chi")
anova(shadetree_model, Tools_model, test = "Chi")
anova(shadetree_model, Inputs_model, test = "Chi")
anova(shadetree_model, Groups_model, test = "Chi")
anova(shadetree_model, Training_model, test = "Chi")

# mutliple comparison adjustments 
# need to add a penality to p-value with so many variables. 
pvals <- coef(summary(shadetree_model))[,4]
p.adjust(pvals, "holm")

# ------ ROWS ------
# Select variables for this analysis
data <- 
  all_data_hh %>% 
  select(row_farm,
         # Household Demographics 
         gghhsize.1, ggagehhhead.1, gggenderhhhead.1, propFemaleCocoaLabor.1,           
         ggliterate.1, ggschoolinghhhead.1, ggborninvillage.1, 
         gghealthinsurance.1, ggwouldlosefallow.1, 
         # Income and Finances
         ggincometotalcap.1, ggsavings.1, ggreceivedloan.1, 
         ggbank.1, ggsusu.1, ggSpouseAccount.1,
         # Cocoa Production and Losses
         ggfsizecocoaacres.1, ggyieldall_cocarea.1, 
         ggfraclost.1, ggyoungtrees.1,
         # Equipment
         c41cutlass.1, c41pruner.1,
         # Inputs
         ggdumfert.1, gginsect.1, ggherb.1, ggfungi.1,
         # Certifications and Groups
         ggorggroup.1, ggcertifiedfarmerorg.1,
         ggknowsft.1, ggorganic.1, ggOrgLeader.1, 
         ggFarmGroupOrCoop.1,
         # Training
         ggTrainingNum.1)

# Select rows with data for farm pruning
row_data <- data[complete.cases(data$row_farm),]

# MICE to fill NAs
mice_data <- mice(row_data)
row_data <- complete(mice_data,1)

# row model
row_model <- 
  glm(row_farm ~ 
        # Household Demographics 
        gghhsize.1+ ggagehhhead.1+ gggenderhhhead.1+ propFemaleCocoaLabor.1+           
        ggliterate.1+ ggschoolinghhhead.1+ ggborninvillage.1+ 
        gghealthinsurance.1+ ggwouldlosefallow.1+ 
        # Income and Finances
        ggincometotalcap.1+ ggsavings.1+ ggreceivedloan.1+ 
        ggbank.1+ ggsusu.1+ ggSpouseAccount.1+
        # Cocoa Production and Losses
        ggfsizecocoaacres.1+ ggyieldall_cocarea.1+ 
        ggfraclost.1+ ggyoungtrees.1+
        # Equipment
        c41cutlass.1+ c41pruner.1+
        # Inputs
        ggdumfert.1+ gginsect.1+ ggherb.1+ ggfungi.1+
        # Certifications and Groups
        ggorggroup.1+ ggcertifiedfarmerorg.1+
        ggknowsft.1+ ggorganic.1+ ggOrgLeader.1+ 
        ggFarmGroupOrCoop.1+
        # Training
        ggTrainingNum.1,
      family = "binomial",
      data=row_data)

# Nested models
# Make models with grouping for different variables
# Likihood ratio test
Household_Demographics_model <- 
  glm(row_farm ~ 
        gghhsize.1+ ggagehhhead.1+ gggenderhhhead.1+ 
        propFemaleCocoaLabor.1+ ggliterate.1+ ggschoolinghhhead.1+
        ggborninvillage.1+ gghealthinsurance.1+ ggwouldlosefallow.1,
      family = "binomial",
      data=row_data)

Income_Finances_model <-
  glm(row_farm ~
        ggincometotalcap.1+ ggsavings.1+ ggreceivedloan.1+ 
        ggbank.1+ ggsusu.1+ ggSpouseAccount.1,
      family = "binomial",
      data=row_data)

Production_Losses_model <-
  glm(row_farm ~
        ggfsizecocoaacres.1+ ggyieldall_cocarea.1+ 
        ggfraclost.1+ ggyoungtrees.1,
      family = "binomial",
      data=row_data)

Tools_model <-
  glm(row_farm ~
        c41cutlass.1+ c41pruner.1,
      family = "binomial",
      data=row_data)

Inputs_model <-
  glm(row_farm ~
        ggdumfert.1+ gginsect.1+ ggherb.1+ ggfungi.1,
      family = "binomial",
      data=row_data)

Groups_model <-
  glm(row_farm ~ 
        ggorggroup.1+ ggcertifiedfarmerorg.1+
        ggknowsft.1+ ggorganic.1+ ggOrgLeader.1+ ggFarmGroupOrCoop.1,
      family = "binomial",
      data=row_data)

Training_model <-
  glm(row_farm ~ 
        ggTrainingNum.1,
      family = "binomial",
      data=row_data)

anova(row_model, Household_Demographics_model, test = "Chi") 
anova(row_model, Income_Finances_model, test = "Chi")
anova(row_model, Production_Losses_model, test = "Chi")
anova(row_model, Tools_model, test = "Chi")
anova(row_model, Inputs_model, test = "Chi")
anova(row_model, Groups_model, test = "Chi")
anova(row_model, Training_model, test = "Chi")

# mutliple comparison adjustments 
# need to add a penality to p-value with so many variables. 
pvals <- coef(summary(row_model))[,4]
p.adjust(pvals, "holm")

# ------ FERTILIZER ------
# Select variables for this analysis
data <- 
  all_data_hh %>% 
  select(ggdumfert.1,
         # Household Demographics 
         gghhsize.1, ggagehhhead.1, gggenderhhhead.1, propFemaleCocoaLabor.1,           
         ggliterate.1, ggschoolinghhhead.1, ggborninvillage.1, 
         gghealthinsurance.1, ggwouldlosefallow.1, 
         # Income and Finances
         ggincometotalcap.1, ggsavings.1, ggreceivedloan.1, 
         ggbank.1, ggsusu.1, ggSpouseAccount.1,
         # Cocoa Production and Losses
         ggfsizecocoaacres.1, ggyieldall_cocarea.1, 
         ggfraclost.1, ggyoungtrees.1,
         # Certifications and Groups
         ggorggroup.1, ggcertifiedfarmerorg.1,
         ggknowsft.1, ggorganic.1, ggOrgLeader.1, 
         ggFarmGroupOrCoop.1,
         # Training
         ggTrainingNum.1)

# Select rows with data for farm pruning
fert_data <- data[complete.cases(data$ggdumfert.1),]

# MICE to fill NAs
mice_data <- mice(fert_data)
fert_data <- complete(mice_data,1)

# fert model
fert_model <- 
  glm(ggdumfert.1 ~ 
        # Household Demographics 
        gghhsize.1+ ggagehhhead.1+ gggenderhhhead.1+ propFemaleCocoaLabor.1+           
        ggliterate.1+ ggschoolinghhhead.1+ ggborninvillage.1+ 
        gghealthinsurance.1+ ggwouldlosefallow.1+ 
        # Income and Finances
        ggincometotalcap.1+ ggsavings.1+ ggreceivedloan.1+ 
        ggbank.1+ ggsusu.1+ ggSpouseAccount.1+
        # Cocoa Production and Losses
        ggfsizecocoaacres.1+ ggyieldall_cocarea.1+ 
        ggfraclost.1+ ggyoungtrees.1+
        # Certifications and Groups
        ggorggroup.1+ ggcertifiedfarmerorg.1+
        ggknowsft.1+ ggorganic.1+ ggOrgLeader.1+ 
        ggFarmGroupOrCoop.1+
        # Training
        ggTrainingNum.1,
      family = "binomial",
      data=fert_data)

# Nested models
# Make models with grouping for different variables
# Likihood ratio test
Household_Demographics_model <- 
  glm(fert_farm ~ 
        gghhsize.1+ ggagehhhead.1+ gggenderhhhead.1+ 
        propFemaleCocoaLabor.1+ ggliterate.1+ ggschoolinghhhead.1+
        ggborninvillage.1+ gghealthinsurance.1+ ggwouldlosefallow.1,
      family = "binomial",
      data=fert_data)

Income_Finances_model <-
  glm(fert_farm ~
        ggincometotalcap.1+ ggsavings.1+ ggreceivedloan.1+ 
        ggbank.1+ ggsusu.1+ ggSpouseAccount.1,
      family = "binomial",
      data=fert_data)

Production_Losses_model <-
  glm(fert_farm ~
        ggfsizecocoaacres.1+ ggyieldall_cocarea.1+ 
        ggfraclost.1+ ggyoungtrees.1,
      family = "binomial",
      data=fert_data)

Tools_model <-
  glm(fert_farm ~
        c41cutlass.1+ c41pruner.1,
      family = "binomial",
      data=fert_data)

Inputs_model <-
  glm(fert_farm ~
        ggdumfert.1+ gginsect.1+ ggherb.1+ ggfungi.1,
      family = "binomial",
      data=fert_data)

Groups_model <-
  glm(fert_farm ~ 
        ggorggroup.1+ ggcertifiedfarmerorg.1+
        ggknowsft.1+ ggorganic.1+ ggOrgLeader.1+ ggFarmGroupOrCoop.1,
      family = "binomial",
      data=fert_data)

Training_model <-
  glm(fert_farm ~ 
        ggTrainingNum.1,
      family = "binomial",
      data=fert_data)

anova(fert_model, Household_Demographics_model, test = "Chi") 
anova(fert_model, Income_Finances_model, test = "Chi")
anova(fert_model, Production_Losses_model, test = "Chi")
anova(fert_model, Tools_model, test = "Chi")
anova(fert_model, Inputs_model, test = "Chi")
anova(fert_model, Groups_model, test = "Chi")
anova(fert_model, Training_model, test = "Chi")

# mutliple comparison adjustments 
# need to add a penality to p-value with so many variables. 
pvals <- coef(summary(fert_model))[,4]
p.adjust(pvals, "holm")

# ------ INSECTICIDE ------
data <- 
  all_data_hh %>% 
  select(gginsect.1,
         # Household Demographics 
         gghhsize.1, ggagehhhead.1, gggenderhhhead.1, propFemaleCocoaLabor.1,           
         ggliterate.1, ggschoolinghhhead.1, ggborninvillage.1, 
         gghealthinsurance.1, ggwouldlosefallow.1, 
         # Income and Finances
         ggincometotalcap.1, ggsavings.1, ggreceivedloan.1, 
         ggbank.1, ggsusu.1, ggSpouseAccount.1,
         # Cocoa Production and Losses
         ggfsizecocoaacres.1, ggyieldall_cocarea.1, 
         ggfraclost.1, ggyoungtrees.1,
         # Certifications and Groups
         ggorggroup.1, ggcertifiedfarmerorg.1,
         ggknowsft.1, ggorganic.1, ggOrgLeader.1, 
         ggFarmGroupOrCoop.1,
         # Tools
        # ggmistblower.1, ggknap.1,
         # Training
         ggTrainingNum.1)

# Select rows with data for insectice use
insect_data <- data[complete.cases(data$gginsect.1),]

# MICE to fill NAs
mice_data <- mice(insect_data)
insect_data <- complete(mice_data,1)

# insecticide model
insect_model <- 
  glm(gginsect.1 ~ 
        # Household Demographics 
        gghhsize.1+ ggagehhhead.1+ gggenderhhhead.1+ propFemaleCocoaLabor.1+           
        ggliterate.1+ ggschoolinghhhead.1+ ggborninvillage.1+ 
        gghealthinsurance.1+ ggwouldlosefallow.1+ 
        # Income and Finances
        ggincometotalcap.1+ ggsavings.1+ ggreceivedloan.1+ 
        ggbank.1+ ggsusu.1+ ggSpouseAccount.1+
        # Cocoa Production and Losses
        ggfsizecocoaacres.1+ ggyieldall_cocarea.1+ 
        ggfraclost.1+ ggyoungtrees.1+
        # Certifications and Groups
        ggorggroup.1+ ggcertifiedfarmerorg.1+
        ggknowsft.1+ ggorganic.1+ ggOrgLeader.1+ 
        ggFarmGroupOrCoop.1+
        # Tools
       # ggmistblower.1 + ggknap.1 +
        # Training
        ggTrainingNum.1,
      family = "binomial",
      data=insect_data)

# mutliple comparison adjustments 
# need to add a penality to p-value with so many variables. 
pvals <- coef(summary(insect_model))[,4]
p.adjust(pvals, "holm")

# ------ FUNGICIDE ------
data <- 
  all_data_hh %>% 
  select(ggfungi.1,
         # Household Demographics 
         gghhsize.1, ggagehhhead.1, gggenderhhhead.1, propFemaleCocoaLabor.1,           
         ggliterate.1, ggschoolinghhhead.1, ggborninvillage.1, 
         gghealthinsurance.1, ggwouldlosefallow.1, 
         # Income and Finances
         ggincometotalcap.1, ggsavings.1, ggreceivedloan.1, 
         ggbank.1, ggsusu.1, ggSpouseAccount.1,
         # Cocoa Production and Losses
         ggfsizecocoaacres.1, ggyieldall_cocarea.1, 
         ggfraclost.1, ggyoungtrees.1,
         # Certifications and Groups
         ggorggroup.1, ggcertifiedfarmerorg.1,
         ggknowsft.1, ggorganic.1, ggOrgLeader.1, 
         ggFarmGroupOrCoop.1,
         # Tools
         #ggmistblower.1, ggknap.1,
         # Training
         ggTrainingNum.1)

# Select rows with data for insectice use
fungi_data <- data[complete.cases(data$ggfungi.1),]

# MICE to fill NAs
mice_data <- mice(fungi_data)
fungi_data <- complete(mice_data,1)

# insecticide model
fungi_model <- 
  glm(ggfungi.1 ~ 
        # Household Demographics 
        gghhsize.1+ ggagehhhead.1+ gggenderhhhead.1+ propFemaleCocoaLabor.1+           
        ggliterate.1+ ggschoolinghhhead.1+ ggborninvillage.1+ 
        gghealthinsurance.1+ ggwouldlosefallow.1+ 
        # Income and Finances
        ggincometotalcap.1+ ggsavings.1+ ggreceivedloan.1+ 
        ggbank.1+ ggsusu.1+ ggSpouseAccount.1+
        # Cocoa Production and Losses
        ggfsizecocoaacres.1+ ggyieldall_cocarea.1+ 
        ggfraclost.1+ ggyoungtrees.1+
        # Certifications and Groups
        ggorggroup.1+ ggcertifiedfarmerorg.1+
        ggknowsft.1+ ggorganic.1+ ggOrgLeader.1+ 
        ggFarmGroupOrCoop.1+
        # Tools
        # ggmistblower.1 + ggknap.1 +
        # Training
        ggTrainingNum.1,
      family = "binomial",
      data=fungi_data)

# mutliple comparison adjustments 
# need to add a penality to p-value with so many variables. 
pvals <- coef(summary(fungi_model))[,4]
p.adjust(pvals, "holm")

# ------ WEEDING ------
# Select variables for this analysis
data <- 
  all_data_hh %>% 
  select(weed_farm,
         # Household Demographics 
         gghhsize.1, ggagehhhead.1, gggenderhhhead.1, propFemaleCocoaLabor.1,           
         ggliterate.1, ggschoolinghhhead.1, ggborninvillage.1, 
         gghealthinsurance.1, ggwouldlosefallow.1, 
         # Income and Finances
         ggincometotalcap.1, ggsavings.1, ggreceivedloan.1, 
         ggbank.1, ggsusu.1, ggSpouseAccount.1,
         # Cocoa Production and Losses
         ggfsizecocoaacres.1, ggyieldall_cocarea.1, 
         ggfraclost.1, ggyoungtrees.1,
         # Equipment
         c41cutlass.1, c41pruner.1,
         # Inputs
         ggdumfert.1, gginsect.1, ggherb.1, ggfungi.1,
         # Certifications and Groups
         ggorggroup.1, ggcertifiedfarmerorg.1,
         ggknowsft.1, ggorganic.1, ggOrgLeader.1, 
         ggFarmGroupOrCoop.1,
         # Training
         ggTrainingNum.1)

# Select rows with data for farm pruning
weed_data <- data[complete.cases(data$weed_farm),]

# MICE to fill NAs
mice_data <- mice(weed_data)
weed_data <- complete(mice_data,1)

# Weeding model
weed_model <- 
  glm(as.factor(weed_farm) ~ 
        # Household Demographics 
        gghhsize.1+ ggagehhhead.1+ gggenderhhhead.1+ propFemaleCocoaLabor.1+           
        ggliterate.1+ ggschoolinghhhead.1+ ggborninvillage.1+ 
        gghealthinsurance.1+ ggwouldlosefallow.1+ 
        # Income and Finances
        ggincometotalcap.1+ ggsavings.1+ ggreceivedloan.1+ 
        ggbank.1+ ggsusu.1+ ggSpouseAccount.1+
        # Cocoa Production and Losses
        ggfsizecocoaacres.1+ ggyieldall_cocarea.1+ 
        ggfraclost.1+ ggyoungtrees.1+
        # Equipment
        c41cutlass.1+ c41pruner.1+
        # Inputs
        ggdumfert.1+ gginsect.1+ ggherb.1+ ggfungi.1+
        # Certifications and Groups
        ggorggroup.1+ ggcertifiedfarmerorg.1+
        ggknowsft.1+ ggorganic.1+ ggOrgLeader.1+ 
        ggFarmGroupOrCoop.1+
        # Training
        ggTrainingNum.1,
      family = "binomial",
      data=weed_data)

# mutliple comparison adjustments 
# need to add a penality to p-value with so many variables. 
pvals <- coef(summary(weed_model))[,4]
p.adjust(pvals, "holm")
