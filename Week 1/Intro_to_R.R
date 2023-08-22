############################################################################
# ENT6702 Entomological Techniques and Data Analysis

# start R & RStudio

# elements ('panes') of RStudio:
# - top left:     Script Editor
# - bottom left:  Console (where you give instructions to R)
# - top right:    Environment, Command History, Connections, GIT
# - bottom right: File Browser, Plots, Packages, Help, Viewer

# Check the version of R - shown in the Console

############################################################################

# Benefits of R: base functions/packages & additional add-on packages
# RStudio: an interface that makes it easier to interact with R

# create a script
# permanent, repeatable, annotated, shareable archive of you analysis

# R is a calculator
1 + 4

2 * 4

3 / 8

12.2 - 7.88

10^2

7 < 10

8 > 19

8 == 8
# are two things equal?

7 == 8

7 != 8
# are two things not equal?

7 != 7

# so far, R is printing answers into the console, but not saving them
# can assign to an object
x <- 4 / 2
# to see the contents of an object, type the name
x
print(x)

# create a numeric vector
# Vector: a group of numbers together
a <- c(4, 2, 3, 6) # uses c() which is a function that combines
a

mean(a)
sd(a)
boxplot(a)
plot(a)

# character/string vectors
a <- c("beetle", "bee", "cricket", "butterfly")
a # note, we have overwritten a

# quickly create a vector of consecutive numbers
# tells R to make a sequence
1:50
c(1:50)
1:10

# using functions to perform tasks
# arguments are separated by commas
# function(argument)
b <- seq(from = 1, to = 10, by = 1)
mean(b)
sd(b)
median(b)
boxplot(b)

# can use a function to clear the environment in R
ls() # list objects in the environment

rm(a) # remove object a

rm(list = ls()) # remove everything in the environment

# let's create a dataframe
id  <- 1:8
count <- c(2, 15, 25, 8, 11, 9, 21, 30)
trmt <- c("T", "C", "T", "C", "T", "C", "T", "C")

dat <- data.frame(id, count, trmt)
dat

summary(dat)
str(dat)

# view individual variables
dat$id
dat$count
dat$trmt

# subset a row
dat[2,]

# subset a column
dat[,2]

# add a new variable
dat$plant <- c(1, 1, 2, 2, 3, 3, 4, 4)

# rename a variable
names(dat)[2] <- "aphids"

# graph the data
plot(dat$aphids, pch = 19)
boxplot(dat$aphids)
boxplot(aphids ~ trmt, data = dat)

