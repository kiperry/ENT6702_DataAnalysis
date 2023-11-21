############################################################################
# ENT6702 Entomological Techniques and Data Analysis
#
# Multivariate analyses
#
# National Science Foundation (NSF)
# Long-term Ecological Research (LTER)
# Harvard Forest
#
# 16 November 2023
############################################################################

##### ACTIVITY
# https://jamboard.google.com/d/1fg5nOmTKiIPZojm20V45MbB2KMXGudR5snPIu0LLUQA/viewer?f=0


# Harvard Forest LTER centers on deciduous forest ecosystem and was established in 1988,
# although this site has been used for research and educational purposes since 1907.
# Research focuses on ecological dynamics from natural disturbances, environmental change, and
# human impacts
# https://lternet.edu/site/harvard-forest-lter/ for more information

# We are going to use a data set with ant species collected in the hemlock removal experiment
# Metadata descriptions here: https://harvardforest1.fas.harvard.edu/exist/apps/datasets/showData.html?id=HF118

#LTER Package ID: knb-lter-hfr.118.34

# data are available online, so we can pull from the website
ants <- read.csv(file="https://pasta.lternet.edu/package/data/eml/knb-lter-hfr/118/34/90ca76917fe458ee78390c30de46afe5", 
                 header=T, na.strings=c("",".","NA"))

str(ants)

ants$genus <- as.factor(ants$genus)
levels(ants$genus) # 13 ant genera

ants$species <- as.factor(ants$species)
levels(ants$species) # 49 ant species

# Notice the data are in 'long format'. All ant species are in one column and there is 
# one column for abundance of those species. Each row represents the abundance of one species
# at a site. We want the data represented in 'wide format'. So, we want each species to
# have a separate column that has the abundance of that species at different sites, which are
# represented in rows.

library(reshape2)

ant.matrix <- dcast(ants, year + block + plot + treatment + trap.type ~ code, value.var = "abundance", length)
str(ant.matrix)

# change variables to factors
ant.matrix$year <- as.factor(ant.matrix$year)
ant.matrix$block <- as.factor(ant.matrix$block)
ant.matrix$plot <- as.factor(ant.matrix$plot)
ant.matrix$treatment <- as.factor(ant.matrix$treatment)
ant.matrix$trap.type <- as.factor(ant.matrix$trap.type)

summary(ant.matrix)
# data from 2003-2018
# two blocks
# four treatments (Hemlock girdled, hemlock logged, hemlock control, and hardwood control)
# established in 90 x 90 m plots, each treatment replicated twice across 8 plots (2 per treatment)
# four collection methods (pitfall trap, litter sample, bait trap, and hand collection)

# Let's subset the data to focus on pitfall trap data from years 2015 and 2018
library(tidyverse)

yr1 <- ant.matrix %>% filter(year == "2015") %>% droplevels()
yr2 <- ant.matrix %>% filter(year == "2018") %>% droplevels()

ants2 <- rbind(yr1, yr2) # combine the two data sets

# remove columns of species that were not collected
ants3 <- ants2[, colSums(ants2 !=0) > 0]


### Nonmetric multidimensional scaling (NMDS)

library(vegan)

# compute a dissimilarity matrix
# the method option let's you indicate which dissimilarity metric to calculate
# here, we are using ant abundance data and calculating the a bray-curtis dissimilarity matrix
dis.matrix <- vegdist(ants3[,6:38], method = "bray")
dis.matrix

# run the nonmetric multidimensional scaling model
nmds.ants <- metaMDS(dis.matrix, trymax = 500, autotransform = TRUE, k = 2)
nmds.ants # stress is quality of fit
stressplot(nmds.ants)
plot(nmds.ants) # basic plot with no treatment distinctions

# plot the NMDS model
ordiplot(nmds.ants, disp = "sites", type = "n", xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
points(nmds.ants, dis = "sites", select = which(ants3$treatment=="HemlockControl"), pch = 17, cex = 2, col = "#73D055FF")
points(nmds.ants, dis = "sites", select = which(ants3$treatment=="Girdled"), pch = 18, cex = 2, col = "#481567FF")
points(nmds.ants, dis = "sites", select = which(ants3$treatment=="Logged"), pch = 15, cex = 2, col = "#2D708EFF")
points(nmds.ants, dis = "sites", select = which(ants3$treatment=="HardwoodControl"), pch = 16, cex = 2, col = "#FDE725FF")
levels(ants3$treatment)
ordiellipse(nmds.ants, ants3$treatment, draw = "lines", col = c("#481567FF", "#FDE725FF", "#73D055FF", "#2D708EFF"), 
            lwd = 3, kind = "sd", conf = 0.90, label = FALSE)

legend("bottomright", legend = c("Hemlock", "Girdled", "Logged", "Hardwood"),
       pch = c(17, 18, 15, 16), cex = 1.5, bty = "n", col = c("#73D055FF", "#481567FF", "#2D708EFF", "#FDE725FF"))

## Test for differences in ant composition among treatments

# PERMANOVA tests whether the group centroid of communities differs among groups
# in multivariate space (e.g. different community composition)
adonis2(dis.matrix ~ ants3$treatment, permutations = 999)

#install.packages("devtools")
#library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
pairwise.adonis(dis.matrix, ants3$treatment)

# BETADISPER tests whether the dispersion of a group from its spatial median is different
# between groups (i.e. species redundancy across space)
# analysis of multivariate homogeneity of group dispersions (variances)
# multivariate analogue of Levene's test for homogeneity of variances
ants.beta <- betadisper(dis.matrix, ants3$treatment, type = c("median"))
anova(ants.beta)
plot(ants.beta)
boxplot(ants.beta, ylab = "Distance to median")
TukeyHSD(ants.beta, which = "group", conf.level = 0.95)


### Beta-diversity with presence/absence data

# change dataset to presence/absence
ants4 <- ants3[,6:38] # save as a new object
ants4[ants4 > 0] <- 1 # if a value is greater than zero, replace it with 1
ants4$treatment <- ants3$treatment # add the treatments back into the data set
str(ants4)

# compute a dissimilarity matrix
# for presence/absence data, we are going to use the jaccard dissimilarity metric
dis.matrix.pa <- vegdist(ants4[,1:33], method = "jaccard")
dis.matrix.pa

# run the nonmetric multidimensional scaling model
nmds.ants.pa <- metaMDS(dis.matrix.pa, trymax = 500, autotransform = TRUE, k = 2)
nmds.ants.pa # stress is quality of fit
stressplot(nmds.ants.pa)
plot(nmds.ants.pa) # basic plot with no treatment distinctions
nmds.ants.pa$points

# plot the NMDS model
ordiplot(nmds.ants.pa, disp = "sites", type = "n", xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
points(nmds.ants.pa, dis = "sites", select = which(ants4$treatment=="HemlockControl"), pch = 17, cex = 2, col = "#73D055FF")
points(nmds.ants.pa, dis = "sites", select = which(ants4$treatment=="Girdled"), pch = 18, cex = 2, col = "#481567FF")
points(nmds.ants.pa, dis = "sites", select = which(ants4$treatment=="Logged"), pch = 15, cex = 2, col = "#2D708EFF")
points(nmds.ants.pa, dis = "sites", select = which(ants4$treatment=="HardwoodControl"), pch = 16, cex = 2, col = "#FDE725FF")
ordiellipse(nmds.ants.pa, ants3$treatment, draw = "lines", col = c("#481567FF", "#FDE725FF", "#73D055FF", "#2D708EFF"), 
            lwd = 3, kind = "sd", conf = 0.90, label = FALSE)

legend("topright", legend = c("Hemlock", "Girdled", "Logged", "Hardwood"),
       pch = c(17, 18, 15, 16), cex = 1.5, bty = "n", col = c("#73D055FF", "#481567FF", "#2D708EFF", "#FDE725FF"))

## Test for differences in ant composition among treatments
# PERMANOVA tests whether the group centroid of communities differs among groups
# in multivariate space (e.g. different community composition)
adonis2(dis.matrix.pa ~ ants4$treatment, permutations = 999)
pairwise.adonis(dis.matrix.pa, ants4$treatment)

# BETADISPER tests whether the dispersion of a group from its spatial median is different
# between groups (i.e. species redundancy across space)
# analysis of multivariate homogeneity of group dispersions (variances)
# multivariate analogue of Levene's test for homogeneity of variances
ants.beta.pa <- betadisper(dis.matrix.pa, ants4$treatment, type = c("median"))
anova(ants.beta.pa)
plot(ants.beta.pa)
boxplot(ants.beta.pa, ylab = "Distance to median")
TukeyHSD(ants.beta.pa, which = "group", conf.level = 0.95)


##########################################################################################################################
# For the environmental data, We are going to use a data set with shrub and herbaceous plant cover
# collected in the hemlock removal experiment
# Metadata descriptions here: https://harvardforest1.fas.harvard.edu/exist/apps/datasets/showData.html?id=HF106

# LTER Package ID: knb-lter-hfr.106.33

herb <- read.csv(file="https://pasta.lternet.edu/package/data/eml/knb-lter-hfr/106/33/417bcd2b9b90c63460d893c43d53ec8c", 
                 header=T, na.strings=c("",".","NA"))

summary(herb)
herb$species <- as.factor(herb$species)
levels(herb$species)
# species codes are provided in the csv file hf106-01-species-codes

herb.matrix <- dcast(herb, year + block + trt + plot ~ species, value.var = "cover", sum)
str(herb.matrix)

# Let's subset the data to focus on data from years 2015 and 2018, just like the ants
yr1.herb <- herb.matrix %>% filter(year == "2015") %>% droplevels()
yr2.herb <- herb.matrix %>% filter(year == "2018") %>% droplevels()

herb2 <- rbind(yr1.herb, yr2.herb)

# remove columns of species that were not observed or observed at low frequencies
herb3 <- herb2[, colSums(herb2 !=0) > 2]
colSums(herb3[,5:23])

# Let's see if any of the plant species are correlated to one another

# two ways to do this
# option 1
plot(herb3[,5:23], pch = 19)
cor(herb3[,5:23], method = c("pearson"), use = "complete.obs")
# not the easiest to interpret since we have so many plant species

library(ggplot2)
library(GGally)
cp <- ggpairs(herb3[,5:23], upper = list(continuous = wrap("cor", size = 2.5, color = "black")))
cp + theme(strip.text.x = element_text(size = 10), strip.text.y = element_text(size = 7))
# it might take a minute to generate, but it is worth the wait

# Multivariate methods are generally robust to highly correlated data, but it is a good idea to assess
# before running any analyses


### Principal Components Analysis

# using package vegan

herb.pca <- rda(herb3[,5:23], scale = FALSE)
# if RDA function is used without explanatory variables, then it calculates a PCA
plot(herb.pca)
summary(herb.pca)

# evaluate the importance of the ordination axes
scores(herb.pca, display = 'species', scaling = 0)
scores(herb.pca, display = 'sites', scaling = 0)

# to see complete list of scores for all axes
scores(herb.pca$CA$u) # site scores
scores(herb.pca$CA$v) # species scores

library(BiodiversityR)
sig <- PCAsignificance(herb.pca, axes = 15)
sig

barplot(sig[c('percentage of variance', 'broken-stick percentage'), ], beside = T, 
         xlab = 'PCA axis', ylab = 'explained variation [%]', col = c('grey', 'black'), 
         legend = TRUE)

# change treatment to a factor
herb3$trt <- as.factor(herb3$trt)
levels(herb3$trt)

# create a plot displaying sites
ordiplot (herb.pca, display = 'sites', type = 'n')
points(herb.pca, dis = "sites", select = which(herb3$trt=="hemlock"), pch = 17, cex = 2, col = "#73D055FF")
points(herb.pca, dis = "sites", select = which(herb3$trt=="girdled"), pch = 18, cex = 2, col = "#481567FF")
points(herb.pca, dis = "sites", select = which(herb3$trt=="logged"), pch = 15, cex = 2, col = "#2D708EFF")
points(herb.pca, dis = "sites", select = which(herb3$trt=="hardwood"), pch = 16, cex = 2, col = "#FDE725FF")
orditorp(herb.pca, display = 'sp')
legend("topright", legend = c("Hemlock", "Girdled", "Logged", "Hardwood"),
       pch = c(17, 18, 15, 16), cex = 1.5, bty = "n", col = c("#73D055FF", "#481567FF", "#2D708EFF", "#FDE725FF"))

biplot(herb.pca, dis = 'species', scaling = 'species')
biplot(herb.pca, dis = 'sites', scaling = 'sites')

# it is possible that total vegetation cover is skewing our results
# let's run the PCA model again without total cover

herb.pca2 <- rda(herb3[,5:22], scale = FALSE)
plot(herb.pca2)
summary(herb.pca2)

# evaluate the importance of the ordination axes
scores(herb.pca2, display = 'species', scaling = 0)
scores(herb.pca2, display = 'sites', scaling = 0)

plot(herb.pca2$CA$eig)
sig2 <- PCAsignificance(herb.pca2, axes = 15)
sig2
barplot(sig2[c('percentage of variance', 'broken-stick percentage'), ], beside = T, 
        xlab = 'PCA axis', ylab = 'explained variation [%]', col = c('grey', 'black'), 
        legend = TRUE)

# create a plot displaying sites
ordiplot (herb.pca2, display = 'sites', type = 'n')
points(herb.pca2, dis = "sites", select = which(herb3$trt=="hemlock"), pch = 17, cex = 2, col = "#73D055FF")
points(herb.pca2, dis = "sites", select = which(herb3$trt=="girdled"), pch = 18, cex = 2, col = "#481567FF")
points(herb.pca2, dis = "sites", select = which(herb3$trt=="logged"), pch = 15, cex = 2, col = "#2D708EFF")
points(herb.pca2, dis = "sites", select = which(herb3$trt=="hardwood"), pch = 16, cex = 2, col = "#FDE725FF")
orditorp(herb.pca2, display = 'sp')
legend("topright", legend = c("Hemlock", "Girdled", "Logged", "Hardwood"),
       pch = c(17, 18, 15, 16), cex = 1.5, bty = "n", col = c("#73D055FF", "#481567FF", "#2D708EFF", "#FDE725FF"))

biplot(herb.pca2, dis = 'species', scaling = 'species')
biplot(herb.pca2, dis = 'sites', scaling = 'sites')

# main take home message is the same - the majority of the variance is explained by PCA axis 1
# this axis ranges from hemlock dominated forests to hardwood dominated forests, suggesting that there is a 
# change in the understory plant community along this gradient

site.scores <- as.data.frame(scores(herb.pca$CA$u))
# can use PC axis 1 as a predictor variable in a model

############################################################################################################################
### Redundancy analysis (RDA)
# using package vegan

ants.rda <- rda(ants3[,6:38] ~ ., herb3[,5:23])
ants.rda
summary(ants.rda)
plot(ants.rda)

# model selection procedure
mod0.rda <- rda(ants3[,6:38] ~ 1, herb3[,5:23])
ants.rda.red <- ordistep(mod0.rda, scope = formula(ants.rda), direction = "both",
                         permutations = how(nperm = 199))
summary(ants.rda.red)
plot(ants.rda.red)
anova(ants.rda.red, by = 'axis') # both axes are significant

ordiplot(ants.rda.red, display = c('si', 'cn'), type = 'n')
points(ants.rda.red, dis = "sites", select = which(herb3$trt=="hemlock"), pch = 17, cex = 2, col = "#73D055FF")
points(ants.rda.red, dis = "sites", select = which(herb3$trt=="girdled"), pch = 18, cex = 2, col = "#481567FF")
points(ants.rda.red, dis = "sites", select = which(herb3$trt=="logged"), pch = 15, cex = 2, col = "#2D708EFF")
points(ants.rda.red, dis = "sites", select = which(herb3$trt=="hardwood"), pch = 16, cex = 2, col = "#FDE725FF")
text(ants.rda.red, display = 'cn', col = 'navy', cex = 1)
orditorp(ants.rda.red, display = 'sp')
legend("topleft", legend = c("Hemlock", "Girdled", "Logged", "Hardwood"),
       pch = c(17, 18, 15, 16), cex = 1.5, bty = "n", col = c("#73D055FF", "#481567FF", "#2D708EFF", "#FDE725FF"))


############################################################################################################################
### Canonical Correspondence Analysis (CCA)
# using package vegan

ants.cca <- cca(ants3[,6:38] ~ ., herb3[,5:23])
ants.cca
summary(ants.cca)
plot(ants.cca)

# model selection procedure
mod0.cca <- cca(ants3[,6:38] ~ 1, herb3[,5:23])
ants.cca.red <- ordistep(mod0.cca, scope = formula(ants.cca), direction = "both",
                         permutations = how(nperm = 199))
summary(ants.cca.red)
plot(ants.cca.red)
anova(ants.cca.red, by = 'axis')

ordiplot(ants.cca.red, display = c('si', 'cn'), type = 'n')
points(ants.cca.red, dis = "sites", select = which(herb3$trt=="hemlock"), pch = 17, cex = 2, col = "#73D055FF")
points(ants.cca.red, dis = "sites", select = which(herb3$trt=="girdled"), pch = 18, cex = 2, col = "#481567FF")
points(ants.cca.red, dis = "sites", select = which(herb3$trt=="logged"), pch = 15, cex = 2, col = "#2D708EFF")
points(ants.cca.red, dis = "sites", select = which(herb3$trt=="hardwood"), pch = 16, cex = 2, col = "#FDE725FF")
text(ants.cca.red, display = 'cn', col = 'navy', cex = 1)
orditorp (ants.cca.red, display = 'sp')
legend("bottomright", legend = c("Hemlock", "Girdled", "Logged", "Hardwood"),
       pch = c(17, 18, 15, 16), cex = 1.5, bty = "n", col = c("#73D055FF", "#481567FF", "#2D708EFF", "#FDE725FF"))

# can use a DCA to help decide which analysis (RDA or CCA) is more appropriate for the data
# Detrended Correspondence analysis

DCA <- decorana(ants3[,6:38])
DCA # DCA1 axis length is less than 2, so RDA is the more appropriate analysis
