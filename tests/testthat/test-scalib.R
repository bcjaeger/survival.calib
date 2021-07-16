
suppressPackageStartupMessages(
 expr = {
  library(survival)
  library(riskRegression)
 }
)

# drop rows with missing values for simplicity
data_init <- na.omit(flchain)

data_init$chapter <- NULL

n_obs_total <- nrow(data_init)
n_obs_train <- round(n_obs_total * 2/3)

set.seed(32987)
train_index <- sample(n_obs_total, size = n_obs_train)

data_train <- data_init[train_index, ]
data_test <- data_init[-train_index, ]

model_1 <- coxph(Surv(futime, death) ~ .,
                 data = data_train,
                 x = TRUE)
model_2 <- update(model_1, . ~ . - flc.grp)
model_3 <- update(model_2, . ~ . -mgus)

pred_horizon = 1000


preds <- lapply(
 X = list(model_1,
          model_2,
          model_3),
 FUN = predictRisk,
 newdata = data_test,
 times = pred_horizon
)

x_list <- scalib_initiate(
 pred_risk = preds,
 pred_horizon = pred_horizon,
 event_status = data_test$death,
 event_time = data_test$futime
)


# preds is a data frame
preds_df <- data.frame(
 model_1 = as.numeric(preds[[1]]),
 model_2 = as.numeric(preds[[2]]),
 model_3 = as.numeric(preds[[3]])
)

x_df <- scalib_initiate(
 pred_risk = preds_df,
 pred_horizon = pred_horizon,
 event_status = data_test$death,
 event_time = data_test$futime
)


# preds is a list of matrices
test_that(
 desc = 'scalib_new constructs as expected',
 code = {

  expect_equal(x_list$data_inputs[[3]], as.numeric(preds[[1]]))
  expect_equal(x_list$data_inputs[[4]], as.numeric(preds[[2]]))
  expect_equal(x_list$data_inputs[[5]], as.numeric(preds[[3]]))

  expect_equal(x_df$data_inputs[[3]], as.numeric(preds[[1]]))
  expect_equal(x_df$data_inputs[[4]], as.numeric(preds[[2]]))
  expect_equal(x_df$data_inputs[[5]], as.numeric(preds[[3]]))

 }
)
