
## code to prepare `DATASET` dataset goes here

library(riskRegression)
library(survival)
library(ranger)
library(gbm)

dataset <- flchain
dataset$chapter <- NULL
dataset <- na.omit(dataset)

n_total <- nrow(dataset)
n_train <- round(n_total * 1/2)

set.seed(329)

train_index <- sample(nrow(dataset), size = n_train)

dataset_train <- dataset[train_index, ]
dataset_test <- dataset[-train_index, ]

cph <- coxph(Surv(futime, death) ~ .,
               data = dataset_train,
               x = TRUE)

rf <- ranger(Surv(futime, death) ~ .,
             data = dataset_train)

bst_cv <- gbm(Surv(futime, death) ~ .,
              data = dataset_train,
              interaction.depth = 1,
              shrinkage = 0.025,
              n.trees = 500,
              cv.folds = 10)

bst_final <- gbm(Surv(futime, death) ~ .,
                 data = dataset_train,
                 interaction.depth = 1,
                 shrinkage = 0.025,
                 n.trees = which.min(bst_cv$cv.error))


models <- list(prop_hazard = cph,
               random_forest = rf,
               gradient_booster = bst_final)

pred_horizon <- 2500

predrisk <- lapply(
 models,
 predictRisk,
 newdata = dataset_test,
 times = pred_horizon
)

data_predrisk <- data.frame(
 prop_hazard = predrisk$prop_hazard,
 random_forest = predrisk$random_forest,
 gradient_booster = predrisk$gradient_booster
)

flc_train <- dataset_train
flc_test <- dataset_test
flc_predrisk <- data_predrisk

usethis::use_data(flc_train,
                  flc_test,
                  flc_predrisk,
                  overwrite = TRUE)
