library(tidyverse)
library(readxl)
library(rsample)
library(recipes)
library(PerformanceAnalytics)
library(h2o)

product_backorders_tbl <- read_csv("product_backorders.csv")

# Split into test and train
set.seed(seed = 1113)
split_obj <- rsample::initial_split(product_backorders_tbl, prop = 0.85)


# Assign training and test data
train_readable_tbl<- training(split_obj)
test_readable_tbl <- testing(split_obj)


  recipe_obj <- recipe( went_on_backorder~., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>%
  prep()
  
train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

 #set the predictor names
 predictors <- c("national_inv", "lead_time", "forecast_3_month", "sales_3_month")
  
# #specify the response
 response <- "went_on_backorder"

 

 h2o.init()
 # Split data into a training and a validation data frame
 # Setting the seed is just for reproducability
 split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)
 train_h2o <- split_h2o[[1]]
 valid_h2o <- split_h2o[[2]]
 test_h2o  <- as.h2o(test_tbl)
 
 # Set the target and predictors
 y <- response
 x <- setdiff(names(train_h2o), y)
 
 ?h2o.automl
 
 automl_models_h2o <- h2o.automl(
   x = x,
   y = y,
   training_frame    = train_h2o,
   validation_frame  = valid_h2o,
   leaderboard_frame = test_h2o,
   max_runtime_secs  = 15,
   nfolds            = 5 
 )
 automl_models_h2o@leaderboard

 
 Model<-automl_models_h2o@leader
 
 
h2o.saveModel(Model,path = "ml_journal-MoamenElbahy")