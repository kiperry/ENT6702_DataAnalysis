
# create vector of class roster
class_roster <- c("Amponsah, Maaame Grace",
                  "Bai, Ningzhu",
                  "Donzelli, Brooke",
                  "Foster, Adam",
                  "Johnson, Lily",
                  "Moran, Shane",
                  "Park, Yeaeun",
                  "Sabet, Afsoon",
                  "Stiller, Amber")

# create vector for the number of people per group (three groups, each with three people)
length(class_roster) # how many students are there
rep(1,3) # just to show you what rep() does... repeats 1, three times
groups <- c(rep(1,3), rep(2,3), rep(3,3))
length(class_roster)==length(groups) # quality control check

# Randomize!
# If you don't run set.seed() each time, you will get different groups
set.seed(123)
class_shuffled <- sample(class_roster)
groups_for_project <- cbind(class_shuffled, groups)
groups_for_project
