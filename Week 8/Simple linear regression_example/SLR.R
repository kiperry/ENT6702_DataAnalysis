############################################################################
# 
# ENT6702 Entomological Techniques and Data Analysis
#
# Simple linear regression example
#
# Rohner, Patrick T.; Moczek, Armin (2023). Evolutionary and plastic
# variation in larval growth and digestion reveal the complex underpinnings
# of size and age at maturation in dung beetles [Dataset].
# Dryad. https://doi.org/10.5061/dryad.j9kd51cdc
#
# Rohner, P. T., & Moczek, A. P. (2021). Evolutionary and plastic
# variation in larval growth and digestion reveal the complex underpinnings
# of size and age at maturation in dung beetles. Ecology and Evolution,
# 11, 15098–15110. https://doi.org/10.1002/ece3.8192
#
# 10 October 2023
#
############################################################################

# Load the data
growth <- read.csv("./Week 8/Simple linear regression_example/data_larval_growth.csv")

nrow(growth)
str(growth)
head(growth)  
tail(growth)

summary(growth)

# how many species are in the data set?
growth$species <- as.factor(growth$species)
levels(growth$species) # 4 species

# how many levels of the nutritional treatment?
growth$nutritional.treatment <- as.factor(growth$nutritional.treatment)
levels(growth$nutritional.treatment) # two: high and low quality

str(growth)
summary(growth)

# for the purposes of this example, we will compare adult weight as a function of 
# larval weight of dung beetles fed a high-quality nutritional diet
# SO, we need to subset the data

library(tidyverse)

# pull out data from the dung beetle species Liatongus militaris because it has the
# most observations in the dataset (i.e., 50)
beetle <- growth %>% filter(species == "L. militaris") %>% droplevels()
levels(beetle$species)
plot(weight.adult ~ weight.L3, data = beetle)
summary(beetle)

# now let's subset the data again to pull out data associated with the high-quality
# nutritional treatment
beetle_high <- beetle %>% filter(nutritional.treatment == "high-quality") %>% droplevels()
levels(beetle_high$nutritional.treatment)

str(beetle_high)

plot(weight.adult ~ weight.L3, data = beetle_high, pch = 19)

beetle_mod <- lm(weight.adult ~ weight.L3, data = beetle_high)
summary(beetle_mod)
anova(beetle_mod)

par(mfrow = c(2, 2))
plot(beetle_mod, pch = 19)
plot(beetle_mod, which = 1:4, pch = 19)

par(mfrow = c(1, 1))
plot(weight.adult ~ weight.L3, data = beetle_high, pch = 19, cex = 1.3,
     xlab = "Weight of Third Larval Instar (g)",
     ylab = "Weight of Adult (g)")
abline(beetle_mod)

fitted(beetle_mod) # returns the fitted values (the Y values that you would expect
# for the given X values according to the best fit line)

resid(beetle_mod) # returns the residuals


