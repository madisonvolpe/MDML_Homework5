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

    #### TEST OF PROCEDURE TO CREATE restaurant_data ####
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
   
    #clean up
    rm(ATEST)
    
    #### ACTUAL CREATION OF restaurant_data ####
    #in our case, those inspection_type with .II indicates initial inspection 
    #In other words, filter on initial inspections
    restaurant_data <- filter(all_data, grepl(pattern = ".II$", all_data$inspection_type))
    
    #filter on years 2015, 2016, 2017 
    restaurant_data <- filter(restaurant_data, inspection_year %in% c(2015, 2016, 2017))
    
    #only want one row for each unique initial inspection, can use uid column to remove
    #duplicates
    restaurant_data <- restaurant_data[!duplicated(restaurant_data$uid), ]
    length(unique(restaurant_data$uid)) == nrow(restaurant_data)
    
    #a.Create a binary outcome variable called ‘outcome’, 
    #defined by whether the score for that initial cycle inspection was 28 or higher, or not.
    restaurant_data$outcome <- ifelse(restaurant_data$score >= 28, 1, 0)
    
    #b.For each initial cycle inspection, just keep the following features: borough, 
    #cuisine, outcome, and inspection_year.
    #side note we need to keep in id for the merge to work!!!! 
    restaurant_data <- select(restaurant_data, id, borough, cuisine, 
                              outcome, inspection_date, inspection_year) 
    
    
## C Perform some feature engineering.We will only create features that could be known before
## a given initial cycle inspection takes place. 
   
    #a.  Add month and weekday to restaurant_data
      restaurant_data$month <- month(restaurant_data$inspection_date)
      restaurant_data$weekday <- weekdays(restaurant_data$inspection_date)
      
    #b. Add four features constructed from historical inspection records:
      all_data_res <- select(all_data, id, score, action, inspection_date)
      historical <-   merge(x = restaurant_data, y = all_data_res, by = "id", 
                            all.x = T, allow.cartesian = T)
    
      #looking at historical investigations
      historical <- filter(historical, inspection_date.y < inspection_date.x)
      sum(historical$inspection_date.y < historical$inspection_date.x)
    
      #drop inspection.x date from historical
      #historical <- historical[-5]
    
      #create uid for historical and restaurant_data 
      historical$uid <- paste(historical$id, historical$inspection_date.x)
      restaurant_data$uid <- paste(restaurant_data$id, restaurant_data$inspection_date)
      

      # create low, medium, and high counts and closings counts
       low <- historical %>% 
         filter(score < 14) %>% 
         group_by(uid) %>% 
         summarise('num_previous_low_inspections' = n_distinct(inspection_date.y))

       med <- historical %>% 
         filter(score >= 14 & score < 28) %>% 
         group_by(uid) %>% 
         summarise('num_previous_med_inspections' = n_distinct(inspection_date.y))

       high <- historical %>% 
         filter(score >= 28) %>% 
         group_by(uid) %>% 
         summarise('num_previous_high_inspections' = n_distinct(inspection_date.y))
       
       closings <- historical %>% 
         filter(action %in% c('closed', 're_closed')) %>% 
         group_by(uid) %>% 
         summarise('num_previous_closings' = n_distinct(inspection_date.y))
       
       
       
       #merge all into restaurant_data
       restaurant_data <- 
         merge(x = restaurant_data, y = low, by = "uid", all.x = T) %>% 
         merge(y = med, by = "uid", all.x = T) %>% 
         merge(y = high, by = "uid", all.x = T) %>% 
         merge(y = closings, by = "uid", all.x = T)
      
      
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
      
    #clean up
    rm(all_data_res, closings, high, historical, low, medium, top50)

## Create a training set of all initial cycle inspections in 2015 and 2016 (train),and
## a testing set of all initial cycle inspections in 2017 (test). Fit a standard 
## logistic regression model on the training set, predicting outcome as a function of 
## only cuisine, borough, month, and weekday. 
## Compute the AUC of this model on the test dataset.
    
    # change to factor variables
    cols <- c('cuisine', 'borough', 'month', 'weekday', 'outcome')
    restaurant_data <- restaurant_data %>% mutate_at(cols, funs(factor(.)))
    
    #create training set 
    train <- filter(restaurant_data, inspection_year %in% c(2015,2016))
    
    #create test set 
    test <- filter(restaurant_data, inspection_year == 2017)
    
    #fit standard logistic regression model 
    str(train)
    
    #remove cuisines in test that are not in train
    test <- test %>% filter(cuisine %in% train$cuisine)
    
    # run logistic regression
    log_model <- glm(outcome ~ cuisine + borough + month + weekday, data = train, family = 'binomial')
    
    # find predicted probabilities on test
    test$predicted.probability.log <- predict(log_model, newdata = test, type = "response")
    
    # compute AUC
    test.pred <- prediction(test$predicted.probability.log, test$outcome)
    test.perf <- performance(test.pred, "auc")
    cat('the auc score is', 100*test.perf@y.values[[1]], "\n")
    
## Fit a random forest model on train, predicting outcome as a function of cuisine,
## borough, month, weekday, and the four historical features created in Step C. Use 1000
## trees, but other settings can have default values. Compute the AUC of this model on the
## 4 test dataset. How does the AUC of the random forest compare with the AUC of the
## logistic regression model?
    
    # set seed
    set.seed(1234)
    
    # run random forest model
    rf_model <- randomForest(outcome ~ cuisine + borough + month +
                               weekday + num_previous_low_inspections + 
                               num_previous_med_inspections + num_previous_high_inspections + 
                               num_previous_closings, ntree = 1000 , data = train)
    
    # find predicted probabilities on test
    test$predicted.probability.rf <- predict(rf_model, newdata = test, type = "prob")[,2]
    
    # compute AUC
    test.pred <- prediction(test$predicted.probability.rf, test$outcome)
    test.perf <- performance(test.pred, "auc")
    cat('the auc score is', 100*test.perf@y.values[[1]], "\n")

##  Generate a precision plot that compares the performance of the logistic
##  regression and random forest models on just the highest ranked inspections.
##  Specifically,
##    a. create a plot where the x-axis is the number of restaurants, ranked from highest
##    model-estimated probability of the outcome to lowest model-estimated probability of the outcome, ##    and the y-axis displays the corresponding model precision (e.g.,
##    if you were to use the random forest model to rank all restaurants from most
##    likely to have the outcome to least likely to have the outcome, then a point like
##    (100, 0.3) would indicate that among the 100 highest-ranked restaurants, 30 of
##    them had the outcome). This is just like the performance plot you made in
##    Homework 3, except the x-axis should be on the absolute scale (not percent
##    scale), and the y-axis should display precision instead of recall.
  
    ## x axis the number of rows 
    
   xaxis <-  test %>%
      group_by(predicted.probability.rf) %>%
      summarise(n=n())
    
    
    
    
    
    
    
    
    
    
    
    
    
    # create prediction variable
    test <- test %>% mutate(prediction.log = case_when(
      predicted.probability.log < 0.5 ~ F,
      predicted.probability.log >= 0.5 ~ T), prediction.rf = case_when(
        predicted.probability.rf < 0.5 ~ F,
        predicted.probability.rf >= 0.5 ~ T)
      )
    
    # confusion table
    confusion <- table(test$prediction.log, test$outcome)
    
    
    plot.data <- test %>% arrange(desc(predicted.probability.log)) %>% 
      dplyr::mutate(numrests = row_number(), percent.outcome = cumsum(arrested)/sum(arrested),
                    highscore = numstops/n()) %>% select(stops, percent.outcome)
    
    # create and save plot
    theme_set(theme_bw())
    p <- ggplot(data=plot.data, aes(x=stops, y=percent.outcome)) 
    p <- p + geom_line()
    p <- p + scale_x_log10('\nPercent of stops', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1), 
                           labels=c('0.3%','1%','3%','10%','30%','100%'))
    p <- p + scale_y_continuous("Percent of arrested individuals", limits=c(0, 1), labels=scales::percent)
    p
    
    # precision plot data
    plot.data <- test %>% arrange(desc(predicted.probability)) %>% 
      mutate(numrests = row_number(), 
             precision = confusion[2,2] / sum(confusion[2,2], confusion[2,1])) %>% 
      select(numrests, precision)
    
    # precision plot
    p <- ggplot(data = plot.data, aes(x = stops, y = precision)) +
      geom_line() + 
      scale_x_log10('\nPercent of stops', 
                    limits=c(0.003, 1), 
                    breaks=c(.003,.01,.03,.1,.3,1), 
                    labels=c('0.3%','1%','3%','10%','30%','100%')) +
      scale_y_continuous("Percent of stops w/ frisk", limits=c(0, 1), labels=scales::percent) +
      theme_bw()
    
  