library(plyr)
library(tidyverse)
library(magrittr)
library(randomForest)
library(ROCR)

### Load in Data, Filter to cpw, and Select columns ### 

sqf <- read_csv("sqf_08_16.csv")

sqf <- sqf %>%
  filter(suspected.crime == "cpw") %>%
  select(id, year, found.weapon, precinct, location.housing, stopped.bc.bulge,
         stopped.bc.object, stopped.bc.desc, stopped.bc.casing, stopped.bc.clothing,
         stopped.bc.drugs, stopped.bc.furtive, stopped.bc.lookout,
         stopped.bc.other, stopped.bc.violent, additional.associating, additional.direction,
         additional.evasive, additional.highcrime, additional.investigation, additional.report,
         additional.proximity, additional.other, additional.time, additional.sights,
         suspect.age, suspect.build, suspect.sex, suspect.height, suspect.weight,
         inside, radio.run, officer.uniform, stop.length, day, month, time.period)

#Hint 2 
# randomForest likes variables to be factors, so convert them if necessary. 
# outcome found.weapon should be coded as a factor, so that randomForest performs
# classification and not regression.

cols <- c("found.weapon", "location.housing", "precinct","stopped.bc.bulge",
          "stopped.bc.object", "stopped.bc.desc", "stopped.bc.casing", "stopped.bc.clothing",
          "stopped.bc.drugs", "stopped.bc.furtive", "stopped.bc.lookout",
          "stopped.bc.other", "stopped.bc.violent", "additional.associating", "additional.direction",
          "additional.evasive", "additional.highcrime", "additional.investigation", "additional.report",
          "additional.proximity", "additional.other", "additional.time", "additional.sights",
         "suspect.build", "suspect.sex", "inside", "radio.run", "officer.uniform", "day", "month", "time.period")

#the only ones not keeping as categorical are stop.length, suspect.age, suspect.height, 
#suspect.weight
sqf <- sqf %>% mutate_at(cols, funs(factor(.)))
str(sqf)

levels(sqf$found.weapon)
levels(sqf$found.weapon) <- c(0,1)
levels(sqf$found.weapon)

#Hint 3 
# You can deal with missing values by just restricting to complete cases. Don’t worry about
# standardizing real-valued variables.

sqf <- sqf %>% filter(complete.cases(.))

#Hint 4
# randomForest has trouble dealing with categorical features with many values (e.g.,
# precinct). You should deal with this by spreading your data (using the spread()
# command), creating a binary indicator variable for each precinct.

sqf <- sqf %>% mutate(i = 1) %>% spread(precinct, i, fill = 0)

names(sqf)[37:113] <- c("one", "five", "six", "seven", "nine", "ten", "thirteen", "fourteen",
                         "seventeen", "eighteen", "nineteen", "twenty","twentytwo", "twentythree",
                         "twentyfour", "twentyfive", "twentysix", "twentyeight", "thirty", "thirtytwo",
                         "thirtythree", "thirtyfour", "fourty", "fourtyone", "fourtytwo", "fourtythree",
                         "fourtyfour", "fourtyfive", "fourtysix", "fourtyseven", "fourtyeight", "fourtynine",
                         "fifty", "fiftytwo", "sixty", "sixtyone", "sixtytwo", "sixtythree", "sixtysix",
                         "sixtyseven", "sixtyeight", "sixtynine", "seventy", "seventyone", "seventytwo", 
                         "seventythree", "seventyfive", "seventysix", "seventyseven", "seventyeight", "seventynine",
                         "eightyone", "eightythree", "eightyfour", "eightyeight", "ninety", "ninetyfour", "onehundred",
                         "onehundredone", "onehundredtwo", "onehundredthree", "onehundredfour", "onehundredfive",
                         "onehundredsix", "onehundredseven", "onehundredeight", "onehundrednine", "onehundredten", 
                         "onehundredeleven", "onehundredtwelve", "onehundredthirteen", "onehundredfourteen", 
                         "onehundredfifteen", "onehundredtwenty", "onehundredtwentyone", "onehundredtwentytwo",
                         "onehundredtwentythree")

### A ### 
 
  #Create one training set and two validation sets in the following manner. Restrict
  # sqf to 2013-2014, randomly shuffle data, and split in half. Call one half train_half
  # the other half test_half. Next restrict sqf to year 2015 and call this test_later.
  # Remove stop id and year columns from train_half, test_half, and test_later. 

# train_half & test_half 
set.seed(1234)
smp_size <- floor(0.50 * nrow(filter(sqf, year == 2013 | year == 2014)))
sqf_13_14 <- filter(sqf, year == 2013 | year == 2014)

train_ind <- sample(seq_len(nrow(sqf_13_14)), size = smp_size)
train_half <-sqf_13_14[train_ind,]
test_half <- sqf_13_14[-train_ind,]

# test_later 
test_later <- sqf %>%
  filter(year==2015)

#removing stop id and year columns from train_half, test_half, and test_later 
train_half <- train_half %>%
  select(-id, -year)

test_half <- test_half %>%
  select(-id, -year)

test_later <- test_later %>%
  select(-id, -year)

### B ###

  #Fit a randomforestmodel on train_half using the randomForest package in R,
  #predicting found.weapon as a function of all features. Use 200 trees, 
  #but all other options for the model can be the default options.

#fit model 
mod1 <- randomForest(found.weapon ~ ., ntree = 200 , data = train_half)
mod1
  
### C ### 

  #Generate predicted probabilities using the model from partB for both test_half
  #and test_later. Compute the AUC of the model on each test set.

#predicted probabilities 
test_half$predicted.probability <- predict(mod1, newdata = test_half, type = "prob")
test_later$predicted.probability <-predict(mod1, newdata = test_later, type = "prob")

#AUC 

#test_half 
test.pred <- prediction(test_half$predicted.probability[,2], test_half$found.weapon)
test.perf <- performance(test.pred, "auc")
cat('the auc score is', 100*test.perf@y.values[[1]], "\n")

#test_later
test.pred <- prediction(test_later$predicted.probability[,2], test_later$found.weapon)
test.perf <- performance(test.pred, "auc")
cat('the auc score is', 100*test.perf@y.values[[1]], "\n")


