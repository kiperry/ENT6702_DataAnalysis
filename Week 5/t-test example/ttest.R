############################################################################
# 
# ENT6702 Entomological Techniques and Data Analysis
#
# t-test example
#
# Lemoine, Nathan P.; Burkepile, Deron E.; Parker, John D. (2015).
# Data from: Variable effects of temperature on insect herbivory [Dataset].
# Dryad. https://doi.org/10.5061/dryad.9fd75
#
# Lemoine NP, Burkepile DE, Parker JD. 2014.
# Variable effects of temperature on insect herbivory.
# PeerJ 2:e376 https://doi.org/10.7717/peerj.376
#
# 19 September 2023
#
############################################################################

# We are going to use a data set deposited on dryad to run some example t-tests
# The citation for the article and data are above

# Study looks at the effects of temperature on the herbivory rates of different
# insect species using no-choice bioassays. Insects were placed singly in a rearing
# cup and then randomly assigned to one of four temperatures (20, 25, 30, and 35 C).
# Consumption of plant material was assessed after 24 hrs

# Load the data
herb <- read.csv("./Week 5/t-test example/Feeding_Assays.csv")

nrow(herb)
str(herb)
head(herb)  
tail(herb)  

summary(herb)

# how many insect orders in the data set?
herb$Herb_Order <- as.factor(herb$Herb_Order)
levels(herb$Herb_Order)

# how many temperatures were measured in the bioassays?
herb$Temperature <- as.factor(herb$Temperature)
levels(herb$Temperature)

str(herb)
summary(herb)

# for the purposes of this example, we will compare relative growth rate of
# Lepidoptera among two temperatures, 20 C and 30 C
# SO, we need to subset the data

library(tidyverse)

# pull out data from Lepidoptera only
leps <- herb %>% filter(Herb_Order == "Lepidoptera") %>% droplevels()
levels(leps$Herb_Order)
boxplot(Herb_RGR ~ Temperature, data = leps)
summary(leps)

# now let's subset the data again to pull out data associated with 20 and 30 C treatments
t20 <- leps[which(leps$Temperature == "20"),]
t30 <- leps[which(leps$Temperature == "30"),]

leps.rgr <- rbind(t20, t30)
boxplot(Herb_RGR ~ Temperature, data = leps.rgr)
leps.rgr <- droplevels(leps.rgr)

str(leps.rgr)
summary(leps.rgr)

boxplot(Herb_RGR ~ Temperature, data = leps.rgr)
stripchart(Herb_RGR ~ Temperature, data = leps.rgr, pch = 19, add = TRUE,
           vertical = TRUE, method = "jitter", jitter = 0.2)


#### Checking assumptions: 1. Response is continuous
plot(leps.rgr$Herb_RGR)


#### Checking assumptions: 2. Data are independent
# Since we did not collect the data from this experiment, we will have to assume this is true.
# We can review the methods of the article to support our assumption.


#### Checking assumptions: 3. Normality
hist(leps.rgr$Herb_RGR)
qqnorm(leps.rgr$Herb_RGR)

# There are formal tests to assess normality (e.g., Shapiro-Wilk Test)
# but they are usually very sensitive
shapiro.test(leps.rgr$Herb_RGR)
?shapiro.test()


#### Checking assumptions: 4. Homogeneity of variances
boxplot(Herb_RGR ~ Temperature, data = leps.rgr)
stripchart(Herb_RGR ~ Temperature, data = leps.rgr, pch = 19, add = TRUE,
           vertical = TRUE, method = "jitter", jitter = 0.2)

dotchart(leps.rgr$Herb_RGR, groups = leps.rgr$Temperature, pch = 19)
?dotchart()

# There are formal tests to assess variance as well (e.g., Levene's Test; Bartlett Test)
bartlett.test(leps.rgr$Herb_RGR ~ leps.rgr$Temperature)
?bartlett.test()


#### T-test
?t.test()
t.test(Herb_RGR ~ Temperature, data = leps.rgr, alternative = "two.sided")

t.test(Herb_RGR ~ Temperature, data = leps.rgr, alternative = "two.sided", var.equal = TRUE)

#### Non-parametric version (Mann-Whitney test) if your data violate the assumptions
?wilcox.test
wilcox.test(Herb_RGR ~ Temperature, data = leps.rgr, alternative = "two.sided")


