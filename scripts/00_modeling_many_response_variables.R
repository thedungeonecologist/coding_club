#'---
#' title: "Modeling Many Response Variables"
#' author: "Darren Shoemaker"
#' ---

# Welcome!

#' This is a summary of my workflow for managing a high number of dependent variables. Other approaches are also perfectly valid so please do not feel that you have to use mine. This script does assume some basic knowledge of R programming, but I am happy to add clarifying points if necessary. If you have suggestions or feedback for how I can improve my own process, please let me know! 

#' In ecology we often are interested in many response variables. Creating individual model scripts for each quickly becomes impractical, but most books, courses, and blogs only present examples of fitting a single model at a time. This is sensible for teaching models themselves, but not make the day-to-day tasks any easier for students of ecology. I have prepared this document to present my techniques, developed during my PhD program, for handling an extensive set of response variables. 

#' I like to call my research philosophy OBJECTIVE ORIENTED. This means that my overarching objectives (what I am trying to accomplish or what question I am trying to answer) is the first consideration. Be sure you know well what you want to accomplish before spending a lot of time on coding. The computer will always give you an answer, but it is YOUR RESPONSIBILITY to ensure that answer is useful and appropriate. This will help you decide what model, what tuning parameters, and which performance metrics you will want.

#' This approach leverages a series of for loops and the list object type to manage many models simultaneously without excessively cluttering the environment. While loops not the fastest approach, this provides a high level of control over each stage and is relatively intuitive. 

#' I follow a general syntax of MODEL-TYPE-ABBREVIATION-OBJECT-TYPE-ABBREVIATION for each list. For example, a list of support vector classification models will be abbreviated svm_mod. Each model can then be accessed either by the index value `svm_mod[[x]]` or $ operator `svm_mod$MODEL`

#[1] Libraries ----

#' *This script will not install packages!

#' Here is necessary for collaboration. This will let anyone open data files from their local resository 

library(here)
library(tidymodels)

#' Do parallel may or may not be necessary depending on processing needs

library(doParallel)

#[2] Read Data ---- 

#' 'dat' is a generic name. .csv is a common data extension but replace with whichever data structure you need. If you need to encode any data as factors/dates/etc., this is a good time to do that.I personally use dplyr functions for this but base R is easier to show.  

dat <- read.csv(here('data', 'YOUR_DATA.csv'))
dat[1:10] <- lapply(dat[1:10], factor) #Replace the indexes with whatever variables need to be factors. The same syntax can be used for other data classes. 

#[3] Split Test-Train Data ----

#' This script is written for classification tasks but is applicable to continous data as well. Spliting test-train is good practice for both classification and regresssion. 

#' Setting a seed should create the same split each time. This is good practice for reproducible results. It can be challenging to explain to your advisor why your results are slightly different each time you present them. 

#' This will split the data into testing- and training sets. The strata argument enforces approximately even distribution of the response variable in the testing and training sets. The prop argument defines the ratio of training to testing data. 0.6-0.9 are common depending on objectives. 

set.seed(500)
train <- rsample::initial_split(dat, strata = YOUR_CLASS, prop = 0.8) 
train_set <- rsample::training(train)
test_set <- rsample::testing(train)

#' Define the dependent variables 

dep_var_train <- train_set[1:10] 
dep_var_test <- test_set[1:10]

#[4] Arrange data into lists ----

#' Here is where things will get complicated. This is not difficult, but may be different from R syntax which you are used to. I want to remind everyone this is my personal workflow, but other approaches are equally valid. If you find a more efficient approach, by all means do that.

#' Begin by making an empty list of your model type followed by the suffix _train. This indicates the training dataframe for the model type you want to use. 

#' We will make unique dataframe for each response variable, with the first column of each dataframe as the Response and the rest of the data in the remaining columns. This step will make our models easier to work with later. This code will create a dataframe as the i-th item of the blank list. This code is written for 10 response variables, but you can run this for any number of response variables 1:n, where n is the number of response variables. 

MODEL_TYPE_train <- list()
for(i in 1:10) {
  MODEL_TYPE_train[[i]] <- data.frame(dep_var_train[,i],
                                      train_set[c(11:30)])
}

#' Do the same with the testing data.

MODEL_TYPE_test <- list()
for(i in 1:10) {
  MODEL_TYPE_train[[i]] <- data.frame(dep_var_train[,i],
                                      train_set[c(11:30)])
}

#[5] Building the Models ----

#OPTIONAL! Parallel Processing

#' Depending on the type of model, number of tuning parameters, and size of your data, this process can be slow. Parallel processing can take advantage of multiple cores in your CPU. R is typically single-threaded, which just means R does not usually care about how powerful your processor is - it will still run slowly! This lets you take advantage if you have a more powerful processor. If your data/models are simple and this reads like Greek, just skip this step. 

cores = detectCores() # Detect the number of available cores.
cl <- makeCluster(cores[1] - 1) # Make a cluster of all but one of your cores. Always leave at least one open for background tasks.
registerDoParallel(cl) # Tell R to use the cluster you just made.

#' Make some empty lists. You knew this was coming.

MODEL_TYPE_final_wf <- list() # Workflow objects
MODEL_TYPE_final_fit <- list() # Fit objects
MODEL_TYPE_mod <- list() # Model objects

#' Using 1:length() instead of a raw number means you can update the number of response variables (number of training dataframes you made earlier) without modifying this part of the code.

for(i in 1:length(MODEL_TYPE_train)) {
  
  tryCatch({ #tryCatch will tell you which part of the loop failed. This can be useful for diagnostics
    
temp <- MODEL_TYPE_train[[i]] # Write the i_th training dataframe to a temporary data.frame object.

set.seed(500) # Set seed again for reproducibility. 
train_folds <- vfold_cv(temp, v = 5, strata = paste(colnames(dep_var[i]))) # Create folds of temp for cross validation.

#' Here is where the training dataframes from earlier come in handy. Because each dataframe only has one response variable, we can write the model formula as paste(colnames(dep_var[i])) ~ ., where . indicates "all other columns." This is much easier than writing out Response_1 ~ x11 + x12 ... x30. The paste function takes the name of the response variable for that iteration of the model to build the formula. We will reuse this trick in the recipe.

#' Make a recipe object with the model formula

MODEL_TYPE_rec <- 
  recipe(as.formula(paste(colnames(dep_var_train[i]), '~ .')), data = temp) 

#' Create a specification object for the model type. 

MODEL_TYPE_spec <- MODEL_TYPE( 
  
# Set the model type and tuning parameters. Think of the tuning parameters as empty placeholders.  

  tune_1 = tune(),
  tune_2 = tune()
) %>% 

# Set the engine (package) from the model you're using. Many models want to use scaled predictors, but do due diligence ahead of time to determine if this is advisable. Set prob.model to True if you want to output probabilities.   
  
  set_engine('MODEL_PACKAGE', 
             scaled = T,
             prob.model = T) %>% 

# Describe the task you want to perform. Typically classification or regression.
  
  set_mode('classification') 



#' Create a workflow object to integrate the recipe and specification. Technically these steps can be combined, but keeping the objects separate helps for tweaking things.

MODEL_TYPE_wf <- 
  workflow() %>% 
  add_model(MODEL_TYPE_spec) %>% #Add the specification
  add_recipe(MODEL_TYPE_rec) # Add the recipe

set.seed(500) # Pop quiz! Why do we set seeds?
param_grid <- dials::grid_regular(tune_1,
                                  tune_2,
                                  levels = 10) # This will make a 10-level grid of each tuning parameter we set when we made the model specification. 

# Decide what metrics you want to use to evaluate models. This will vary based on your research objectives. 

MODEL_TYPE_metrics <- metric_set(roc_auc, accuracy, kap) 

#' Bring everything together into a results object. 

MODEL_TYPE_tune_res <- 
  MODEL_TYPE_wf %>% 
  tune_grid(
    resamples = train_folds,
    metrics = MODEL_TYPE_metrics,
    grid = param_grid)

#' Select the best model based on a performance metric. Accuracy is just an example. Be sure to think critically about your research objectives and what type of error is the most important for you to minimize. 

MODEL_TYPE_best <- MODEL_TYPE_tune_res %>% 
  select_best(metric = 'accuracy') 
 
#' Here will start writing back to lists. The objects above will be overwritten with each iteration of the loop. The following will be stored to lists which we will use to evaluate models. You can store as many objects as you want in lists, even temp, but I like to keep my environment as clean as possible.

MODEL_TYPE_final_wf[[i]] <- 
  MODEL_TYPE_wf %>% 
  finalize_workflow(MODEL_TYPE_best) # Save the workflow for the best model

MODEL_TYPE_final_fit[[i]] <- 
  MODEL_TYPE_final_wf[[i]] %>% 
  last_fit(train) # Save the fit object for the best model

MODEL_TYPE_mod[[i]] <- extract_fit_engine(MODEL_TYPE_final_fit[[i]]) # Save the model object. 

#' Note: If you're going to be using tidymodels packages, the workflow objects may be more useful than the model objects, but I like saving both just in case. 

  }) # End of tryCatch function
  
} # End of for loop

stopCluster(cl) # Stop parallel processing

#' I recommend saving all the objects you made as .rds files. This will let you reload the models later without having to remake them, which as we described earlier can take a long time. 

saveRDS(MODEL_TYPE_mod, here('output', 'MODEL_TYPE_mod.rds'))
saveRDS(MODEL_TYPE_wf, here('output', 'MODEL_TYPE_final_wf.rds'))
saveRDS(MODEL_TYPE_final_fit, here('output', 'MODEL_TYPE_final_fit.rds'))