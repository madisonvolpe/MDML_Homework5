library(tidyverse)
library(lubridate)
library(ROCR)
library(randomForest)
library(data.table)

## A- Import and Clean entire data set as tibble called all_data ## 
  
  # read in data 
  all_data <- read_csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
  
  #drop recommended columns 
  all_data <- select(all_data, -BUILDING, -STREET, -PHONE, - DBA, - ZIPCODE, -`RECORD DATE`, 
                     -`VIOLATION DESCRIPTION`, -`GRADE DATE`)

  #renaming variable names 
  names(all_data) <- c("id", "borough", "cuisine","inspection_date", "action", "code", 
                       "critical", "score", "grade", "inspection_type")
  
  #Make INSPECTION DATE a date object called inspection_date 
  #all_data[all_data$inspection_date == "1/1/00",4] <- "1/1/1900"
  all_data$inspection_date <- mdy(all_data$inspection_date)
  str(all_data$inspection_date)
  
  #extract the year as inspection_year
  all_data$inspection_year <- year(all_data$inspection_date)
  
  #rename the values in the ‘action’‘inspection_type’ column with shorter, simpler values
  all_data$action <- factor(all_data$action)
  levels(all_data$action) <- c("closed", "re_closed", "re-opened", "no violations", "violations")
  
  #remove inspection types: 'Calorie Posting / Re-inspection',
  #'Inter-Agency Task Force / Re-inspection', 
  #'Smoke-Free Air Act / Re-inspection','Administrative Miscellaneous / Re-inspection',
  #'Trans Fat / Re-inspection','Inter-Agency Task Force / Initial Inspection'
  
all_data <- all_data %>%
    filter(!inspection_type %in% c("Calorie Posting / Re-inspection", "Inter-Agency Task Force / Re-inspection",
                                   "Smoke-Free Air Act / Re-inspection",
                                   "Administrative Miscellaneous / Re-inspection",
                                   "Trans Fat / Re-inspection",
                                   "Inter-Agency Task Force / Initial Inspection"))
  
  #rename the values in the ‘inspection_type’ column with shorter, simpler values
  all_data$inspection_type <- factor(all_data$inspection_type)
  levels(all_data$inspection_type) <- c("AM.CI", "AM.II", "AM.RI", "AM.SCI",
                                        "CP.CI", "CP.II", "CI.CI", "CI.II", "CI.RE-I",
                                        "CI.REOI", "CI.SCI", "PPNO.CI", "PPNO.II",
                                        "PPNO.RE-I", "PPNO.SCI", "PPO.CI", "PPO.II", 
                                        "PPO.RE-I","PPO.REOI", "PPO.SCI", "SFAA.CI",
                                        "SFAA.II", "SFAA.LI", "SFAA.SCI",
                                        "TF.CI", "TF.II", "TF.LI", "TF.SCI")
  
  #Deal with ‘Missing’ borough information
  all_data <- all_data[all_data$borough != "Missing",]
  
  #remove restaurants that have not been inspected (year is 1900)
  all_data <- filter(all_data, inspection_year != 1900)
  range(all_data$inspection_year)

  #remove rows without a score or with a negative score
  all_data <- all_data[!is.na(all_data$score),]
  all_data <- filter(all_data, score >= 0)
  
  #Some restaurants received different scores for the same inspection on the same day; 
  #replace all scores for any inspection for a given restaurant on a given day by the 
  #maximum score.
  
    #make a UID that combines, id of restaurant, inspection.date and inspection.type
    all_data$uid <- paste(all_data$id, all_data$inspection_date, all_data$inspection_type)
                                    
    #find ids that have more than one score for a particular inspection
    #on a particular date 
    ids <-all_data %>%
      group_by(uid) %>%
      summarise(distinctscores = n_distinct(score)) %>%
      filter(distinctscores > 1) %>%
      select(uid)
    
    #get duplicates data
    duplicates <- all_data %>%
      filter(uid %in% ids$uid) %>%
      arrange(uid)
    
    #assign max score
    duplicates <- duplicates %>% group_by(uid) %>% mutate(score = max(score))
    
    #put in new scores into dataset
    all_data <- all_data %>% arrange(uid)
    all_data[all_data$uid %in% duplicates$uid, ]$score <- duplicates$score
  
    #remove extra tibbles
    rm(duplicates, ids)


## B- Create the sample of data that you will use for prediction as a tibble called 
## restaurant_data. We will restrict our attention to all initial cycle inspections that took
## place in 2015, 2016, 2017 
 
    ATEST <- filter(all_data, id == 30112340)
    #in our case, those inspection_type with .II indicates initial inspection 
    #In other words, filter on initial inspections
    ATEST <- filter(ATEST, grepl(pattern = ".II$", ATEST$inspection_type))
    
    #filter on years 2015, 2016, 2017 
    ATEST <- filter(ATEST, inspection_year %in% c(2015, 2016, 2017))
    
    #only want one row for each unique initial inspection, can use uid column to remove
    #duplicates
    ATEST <- ATEST[!duplicated(ATEST$uid),]
    
    #a.Create a binary outcome variable called ‘outcome’, 
    #defined by whether the score for that initial cycle inspection was 28 or higher, or not.
    ATEST$outcome <- ifelse(ATEST$score >= 28, 1, 0)
    
    #b.For each initial cycle inspection, just keep the following features: borough, 
    #cuisine, outcome, and inspection_year.
    ATEST <- select(ATEST, borough, cuisine, outcome, inspection_year)
   
    #in our case, those inspection_type with .II indicates initial inspection 
    #In other words, filter on initial inspections
    restaurant_data <- filter(all_data,grepl(pattern = ".II$",all_data$inspection_type))
    
    #filter on years 2015, 2016, 2017 
    restaurant_data <- filter(restaurant_data, inspection_year %in% c(2015, 2016, 2017))
    
    #only want one row for each unique initial inspection, can use uid column to remove
    #duplicates
    restaurant_data <- restaurant_data[!duplicated(restaurant_data$uid),]
    length(unique(restaurant_data$uid)) == nrow(restaurant_data)
    
    #a.Create a binary outcome variable called ‘outcome’, 
    #defined by whether the score for that initial cycle inspection was 28 or higher, or not.
    restaurant_data$outcome <- ifelse(restaurant_data$score >= 28, 1, 0)
    
    #b.For each initial cycle inspection, just keep the following features: borough, 
    #cuisine, outcome, and inspection_year.
    #side note we need to keep in id for the merge to work!!!! 
    restaurant_data <- select(restaurant_data, id, borough, cuisine, outcome, inspection_date, inspection_year) 
    rm(ATEST)
    
## C Perform some feature engineering.We will only create features that could be known before
## a given initial cycle inspection takes place. 
   
    #a.  Add month and weekday to restaurant_data
      restaurant_data$month <- month(restaurant_data$inspection_date)
      restaurant_data$weekday <- weekdays(restaurant_data$inspection_date)
      
    #b. Add four features constructed from historical inspection records:
      all_data_res <- select(all_data, id, score, action, inspection_date)
      historical <-   merge(x= restaurant_data, y= all_data_res, by = "id")
    
      #looking at historical investigations
      historical <- filter(historical, inspection_date.y < inspection_date.x)
      sum(historical$inspection_date.y < historical$inspection_date.x)
    
      #drop inspection.x date from historical
      historical <- historical[-5]
    
      #create uid for historical 
      historical$uid <- paste(historical$id, historical$inspection_date.y)
  
      #The number of previous inspections with score < 14 
      low <- historical %>%
        filter(score < 14) %>%
        group_by(id) %>%
        summarise(count = n_distinct(uid))
   
      restaurant_data$num_previous_low_inspections <-low$count[match(restaurant_data$id, 
                                                                     low$id)]
      
      #The number of previous inspections with score >= 14 and < 28
      medium <- historical %>%
        filter(score >=14 & score < 28) %>%
        group_by(id) %>%
        summarise(count = n_distinct(uid))
      
      restaurant_data$num_previous_med_inspections <-medium$count[match(restaurant_data$id, 
                                                                     medium$id)]
    
      #The number of previous inspections with score >= 28
      high <- historical %>%
        filter(score >= 28) %>%
        group_by(id) %>%
        summarise(count = n_distinct(uid))
     
      restaurant_data$num_previous_high_inspections <-high$count[match(restaurant_data$id, 
                                                                        high$id)]
      
      #The number of previous inspections which resulted in closing or re-closing 
      #(call this num_previous_closings)
      closings <-historical %>%
        filter(action %in% c("closed", "re_closed")) %>%
        group_by(id) %>%
        summarise(count = n_distinct(uid))
      
      restaurant_data$num_previous_closings <-closings$count[match(restaurant_data$id, 
                                                                       closings$id)]
    
      #Hint: Make sure to replace NA values with zeros for the historical features for 
      #restaurants that have no prior inspections.
      
      restaurant_data[is.na(restaurant_data)] <- 0
      
    #c. Restrict restaurant_data to only the top 50 most common cuisines 
      
      top50 <- restaurant_data %>%
        count(cuisine) %>%
        arrange(desc(n)) %>%
        slice(1:50)
      
      restaurant_data <- filter(restaurant_data, cuisine %in% top50$cuisine)
      
      length(unique(restaurant_data$cuisine))
    
    rm(all_data_res, closings, high, historical, low, medium, top50)

## Create a training set of all initial cycle inspections in 2015 and 2016 (train),and
## a testing set of all initial cycle inspections in 2017 (test). Fit a standard 
## logistic regression model on the training set, predicting outcome as a function of 
## only cuisine, borough, month, and weekday. 
## Compute the AUC of this model on the test dataset.
    
    #create training set 
    train <- filter(restaurant_data, inspection_year %in% c(2015,2016))
    
    #create test set 
    test <- filter(restaurant_data, inspection_year == 2017)
    
    #fit standard logistic regression model 
    str(train)
    
      
    
    
  