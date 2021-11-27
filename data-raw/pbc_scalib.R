
## code to prepare `DATASET` dataset goes here

library(riskRegression)
library(survival)
library(randomForestSRC)
library(gbm)
library(orsf2)

dataset <- pbc[complete.cases(pbc), ]
dataset$status[dataset$status > 0] <- dataset$status[dataset$status > 0] - 1
dataset$stage <- as.integer(dataset$stage)
dataset$id <- NULL

n_total <- nrow(dataset)
n_train <- round(n_total * 1/2)

set.seed(32987)

train_index <- sample(nrow(dataset), size = n_train)

dataset_train <- dataset[train_index, ]
dataset_test <- dataset[-train_index, ]

cph <- coxph(Surv(time, status) ~ .,
             data = dataset_train,
             x = TRUE)

rf <- rfsrc(Surv(time, status) ~ .,
          data = dataset_train,
          nodesize = 15,
          ntree = 1000)

bst_cv <- gbm(Surv(time, status) ~ .,
              data = dataset_train,
              interaction.depth = 1,
              shrinkage = 0.025,
              n.trees = 500,
              cv.folds = 10)

bst_final <- gbm(Surv(time, status) ~ .,
                 data = dataset_train,
                 interaction.depth = 1,
                 shrinkage = 0.025,
                 n.trees = 150)

aorsf <- orsf(data = dataset_train,
              formula = Surv(time, status) ~ .,
              n_tree = 1000)

predictRisk.aorsf <- function(object, newdata, times, ...){
 predict(object, new_data = newdata, times = times, risk = TRUE)
}

models <- list(prop_hazard = cph,
               rsf_axis = rf,
               gradient_booster = bst_final,
               rsf_oblique = aorsf)

pred_horizon <- 2500

data_predrisk <- as.data.frame(
 list_predrisk <- lapply(models,
                         predictRisk,
                         newdata = dataset_test,
                         times = pred_horizon)
)

data_predrisk$prop_hazard[data_predrisk$prop_hazard==1] <- 0.999

pbc_scalib <- list(train = dataset_train,
                   test = dataset_test,
                   predrisk = data_predrisk)

usethis::use_data(pbc_scalib,
                  overwrite = TRUE)

# sc <- scalib(pred_risk    = data_predrisk,
#              pred_horizon = pred_horizon,
#              event_status = pbc_test$status,
#              event_time   = pbc_test$time)
#
# scalib_gnd(sc)
# scalib_hare(sc)








