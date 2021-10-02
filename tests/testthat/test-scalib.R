
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

preds_unnamed <- lapply(
 X = list(model_1,
          model_2,
          model_3),
 FUN = predictRisk,
 newdata = data_test,
 times = pred_horizon
)

preds_named <- lapply(
  X = list(a = model_1,
           b = model_2,
           c = model_3),
  FUN = predictRisk,
  newdata = data_test,
  times = pred_horizon
)

x_list_unnamed <- scalib(
 pred_risk = preds_unnamed,
 pred_horizon = pred_horizon,
 event_status = data_test$death,
 event_time = data_test$futime
)

x_list_named <- scalib(
  pred_risk = preds_named,
  pred_horizon = pred_horizon,
  event_status = data_test$death,
  event_time = data_test$futime
)


# preds is a data frame
preds_df <- data.frame(
 model_1 = as.numeric(preds_unnamed[[1]]),
 model_2 = as.numeric(preds_unnamed[[2]]),
 model_3 = as.numeric(preds_unnamed[[3]])
)

x_df <- scalib(
 pred_risk = preds_df,
 pred_horizon = pred_horizon,
 event_status = data_test$death,
 event_time = data_test$futime
)


# preds is a list of matrices
test_that(
 desc = 'scalib_new constructs as expected',
 code = {

  expect_equal(x_list_named$data_inputs$a, as.numeric(preds_named$a))
  expect_equal(x_list_named$data_inputs$b, as.numeric(preds_named$b))
  expect_equal(x_list_named$data_inputs$c, as.numeric(preds_named$c))

  expect_equal(x_list_unnamed$data_inputs$pred_risk_1,
               as.numeric(preds_unnamed[[1]]))
  expect_equal(x_list_unnamed$data_inputs$pred_risk_2,
               as.numeric(preds_unnamed[[2]]))
  expect_equal(x_list_unnamed$data_inputs$pred_risk_3,
               as.numeric(preds_unnamed[[3]]))

  expect_equal(x_df$data_inputs$model_1, as.numeric(preds_df$model_1))
  expect_equal(x_df$data_inputs$model_2, as.numeric(preds_df$model_2))
  expect_equal(x_df$data_inputs$model_3, as.numeric(preds_df$model_3))

 }
)
