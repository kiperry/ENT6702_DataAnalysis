############################################################################
# ENT6702 Entomological Techniques and Data Analysis
#
# Diversity metrics
#
# National Science Foundation (NSF)
# Long-term Ecological Research (LTER)
# Harvard Forest
#
# 31 October 2023
############################################################################

# Harvard Forest LTER centers on deciduous forest ecosystem and was established in 1988,
# although this site has been used for research and educational purposes since 1907
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

# Notice the data in are 'long format'. All ant species are in one column and there is 
# one column for abundance of those species. Each row represented a species abundance
# at a site. We want the data represented in 'wide format'. So, we want each species to
# have a separate column that has the abundance of the species at different sites
# represented in rows.

library(reshape2) # can also use functions in the tidyr package

ant.matrix <- dcast(ants, year + block + plot + treatment + trap.type ~ code, length)
str(ant.matrix)

# change variables to factors
ant.matrix$year <- as.factor(ant.matrix$year)
ant.matrix$block <- as.factor(ant.matrix$block)
ant.matrix$plot <- as.factor(ant.matrix$plot)
ant.matrix$treatment <- as.factor(ant.matrix$treatment)
ant.matrix$trap.type <- as.factor(ant.matrix$trap.type)

summary(ant.matrix)
# data from 2003-2007
# two blocks
# four treatments (Hemlock girdled, hemlock logged, hemlock control, and hardwood control)
# established in 90 x 90 m plots, each treatment replicated twice across 8 plots (2 per treatment)
# four collection methods (pitfall trap, litter sample, bait trap, and hand collection)

# Let's subset the data to focus on data from year 2006
library(tidyverse)

ants2 <- ant.matrix %>% filter(year == "2006") %>% droplevels()
levels(ants2$year)

### Abundance
# let's look at species abundance and dominance overall (across all treatments)
ant.dom <- colSums(ants2[,6:54]) # sum abundances across sites
ant.dom <- as.data.frame(ant.dom) # change to data frame
names(ant.dom)[1] <- "count" # rename the abundance column
ant.dom$species <- rownames(ant.dom) # make a column of species
str(ant.dom)
ant.dom$species <- as.factor(ant.dom$species) # change species to a factor

# graph it
ant.dom %>% mutate(species = fct_reorder(species, desc(count)))  %>%
  ggplot( aes(x = species, y = count)) +
  geom_bar(stat="identity", fill="seagreen", alpha=.6, width=.4) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("Ant Species") +
  ylab("Abundance")

# we have many species that were not collected during 2006
# we need to remove those columns before proceeding
ants3 <- ants2[, colSums(ants2 !=0) > 0]
str(ants3)

### Indicator species analysis
library(indicspecies)
# https://cran.r-project.org/web/packages/indicspecies/indicspecies.pdf
# https://cran.r-project.org/web/packages/indicspecies/vignettes/IndicatorSpeciesAnalysis.html

indval <- multipatt(ants3[,6:20], ants2[,4], duleg = TRUE, control = how(nperm=999)) # alpha = 0.05 is the default
summary(indval, indvalcomp = TRUE)
summary(indval, alpha = 1) # provides results for all species by changing the significance level

### Calculate species richness
library(vegan)
# https://cran.r-project.org/web/packages/vegan/vegan.pdf

specnumber(ants3[,6:20]) # calculate species richness
ants3$sp.rich <- specnumber(ants3[,6:20]) # add it as a column in the data set
str(ants3)
specnumber(ants3[,6:20], groups = ants3$treatment)
dotchart(ants3$sp.rich, group = ants3$treatment, pch = 19)
hist(ants3$sp.rich)
boxplot(sp.rich ~ treatment, data = ants3,
        xlab = "", ylab = "Ant species richness")
stripchart(sp.rich ~ treatment, data = ants3, pch = 19, add = TRUE,
           vertical = TRUE, method = "jitter", jitter = 0.2)

### Estimate species richness with accumulation curves

# individual-based rarefaction by treatment, jackknife estimates by treatment

# we are separating out each treatment so we can get a species accumulation curve for each, and then we will graph it
levels(ants3$treatment)
girdled <- ants3[which(ants3$treatment == "Girdled"),]
hardwood <- ants3[which(ants3$treatment == "HardwoodControl"),]
hemlock <- ants3[which(ants3$treatment == "HemlockControl"),]
logged <- ants3[which(ants3$treatment == "Logged"),]

# still using the R package vegan
# the rarefaction method standardizes the sample sizes so that we are comparing species richness
# at equivalent abundances

sp.girdled <- specaccum(girdled[6:20], method = "rarefaction", permutations = 100, gamma = "jack2")
sp.hardwood <- specaccum(hardwood[6:20], method = "rarefaction", permutations = 100, gamma = "jack2")
sp.hemlock <- specaccum(hemlock[6:20], method = "rarefaction", permutations = 100, gamma = "jack2")
sp.logged <- specaccum(logged[6:20], method = "rarefaction", permutations = 100, gamma = "jack2")

# make the plot

plot(sp.girdled, pch = 19, col = "#481567FF", xvar = c("individuals"), lty = 4, lwd = 4,
     ylab = "Species Richness", xlab = "Number of Individuals", xlim = c(0, 110), ylim = c(0, 15))
plot(sp.hardwood, add = TRUE, pch = 15, xvar = c("individuals"), lty = 1, lwd = 4, col = "#FDE725FF")
plot(sp.hemlock, add = TRUE, pch = 4, xvar = c("individuals"), lty = 2, lwd = 4, col = "#73D055FF")
plot(sp.logged, add = TRUE, pch = 9, xvar = c("individuals"), lty = 3, lwd = 4, col = "#2D708EFF")

legend("bottomright", legend = c("Hemlock", "Girdled", "Logged", "Hardwood"),
       pch = c(16, 17, 15, 18), lty = c(1,2,3,4), cex = 1.5, bty = "n", lwd = 4,
       col = c("#73D055FF", "#481567FF", "#2D708EFF", "#FDE725FF"))


# calculates the estimated species richness of a population using first- and second-order jackknife estimators
# first-order jackknife estimates are based on the number of singletons
# second-order jackknife estimates are based on the number of singletons and doubletons

library(fossil)
# https://cran.r-project.org/web/packages/fossil/fossil.pdf

# start with the entire data set
jack1(ants3[,6:20], taxa.row = FALSE, abund = TRUE)# estimated gamma diversity
jack2(ants3[,6:20], taxa.row = FALSE, abund = TRUE)

# girdled
jack1(girdled[6:20], taxa.row = FALSE, abund = TRUE)
jack2(girdled[6:20], taxa.row = FALSE, abund = TRUE)

# hardwood
jack1(hardwood[6:20], taxa.row = FALSE, abund = TRUE)
jack2(hardwood[6:20], taxa.row = FALSE, abund = TRUE)

# hemlock
jack1(hemlock[6:20], taxa.row = FALSE, abund = TRUE)
jack2(hemlock[6:20], taxa.row = FALSE, abund = TRUE)

# logged
jack1(logged[6:20], taxa.row = FALSE, abund = TRUE)
jack2(logged[6:20], taxa.row = FALSE, abund = TRUE)

### Species diversity metrics

# Shannon index
diversity(ants3[,6:20], index = "shannon")
ants3$sh.div <- diversity(ants3[,6:20], index = "shannon")
str(ants3)
dotchart(ants3$sh.div, group = ants3$treatment, pch = 19)
hist(ants3$sh.div)
boxplot(sh.div ~ treatment, data = ants3)
stripchart(sh.div ~ treatment, data = ants3, pch = 19, add = TRUE,
           vertical = TRUE, method = "jitter", jitter = 0.2)

# Simpson index
diversity(ants3[,6:20], index = "simpson")
ants3$sp.div <- diversity(ants3[,6:20], index = "simpson")
str(ants3)
dotchart(ants3$sp.div, group = ants3$treatment, pch = 19)
hist(ants3$sp.div)
boxplot(sp.div ~ treatment, data = ants3)
stripchart(sp.div ~ treatment, data = ants3, pch = 19, add = TRUE,
           vertical = TRUE, method = "jitter", jitter = 0.2)

# Effective numbers of species - Inverse Simpson index
diversity(ants3[,6:20], index = "invsimpson")
ants3$isp.div <- diversity(ants3[,6:20], index = "invsimpson")
str(ants3)
dotchart(ants3$isp.div, group = ants3$treatment, pch = 19)
hist(ants3$isp.div)
boxplot(isp.div ~ treatment, data = ants3)
stripchart(isp.div ~ treatment, data = ants3, pch = 19, add = TRUE,
           vertical = TRUE, method = "jitter", jitter = 0.2)

# Hill numbers
library(hillR)
# https://cran.r-project.org/web/packages/hillR/hillR.pdf

# Hill number, q = 0 (default) to get species richness, q = 1 to get shannon entropy,
# q = 2 will give inverse Simpson.

hill_taxa(ants3[,6:20], q = 0, MARGIN = 1)
hill_taxa(ants3[,6:20], q = 1, MARGIN = 1) 
hill_taxa(ants3[,6:20], q = 2, MARGIN = 1)
# margin 1 is the default, indicates that sites are rows
# if sites are columns, then set margin = 2

### Species evenness
library(chemodiv)
# https://cran.r-project.org/web/packages/chemodiv/chemodiv.pdf

calcDiv(ants3[,6:20], type = "PielouEven")
calcDiv(ants3[,6:20], type = "HillEven", q = 1)
calcDiv(ants3[,6:20], type = "HillEven", q = 2)

ants3$sp.eve <- calcDiv(ants3[,6:20], type = "HillEven", q = 2)
str(ants3)
dotchart(ants3$sp.eve$HillEven, group = ants3$treatment, pch = 19)
hist(ants3$sp.eve$HillEven)
boxplot(sp.eve$HillEven ~ treatment, data = ants3)
stripchart(sp.eve$HillEven ~ treatment, data = ants3, pch = 19, add = TRUE,
           vertical = TRUE, method = "jitter", jitter = 0.2)

