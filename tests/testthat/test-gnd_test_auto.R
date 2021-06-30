

library(riskRegression)
library(survival)

train_index <- 1:3000

risk_mdl <- coxph(data = flchain[train_index, ],
                  x = TRUE,
                  formula = Surv(futime, death)~age+sex+flc.grp+lambda)

risk_times <- c(4000)

risk_pred <- predictRisk(risk_mdl,
                         newdata = flchain[-train_index, ],
                         times = risk_times)

# check on error messages

gnd_test_auto(predicted_risk = risk_pred,
              event_time = flchain$futime[-train_index],
              event_status = flchain$death[-train_index],
              time_predict = risk_times,
              group_count_init = 50,
              verbose = 0)

# group_count_init (lwr bound)
test_that(
 desc = 'group_count_init min is caught',
 code = {
  expect_error(
   object = gnd_test_auto(predicted_risk = risk_pred,
                          event_time = flchain$futime[-train_index],
                          event_status = flchain$death[-train_index],
                          time_predict = risk_times,
                          group_count_init = 1,
                          verbose = 0),
   regexp = 'group_count_init = 1 should be'
  )
 }
)

# time_predict (length)
test_that(
 desc = 'time_predict length is caught',
 code = {
  expect_error(
   object = gnd_test_auto(predicted_risk = risk_pred,
                          event_time = flchain$futime[-train_index],
                          event_status = flchain$death[-train_index],
                          time_predict = c(1, 2),
                          group_count_init = 1,
                          verbose = 0),
   regexp = 'time_predict should have length <1>'
  )
 }
)

# event_status is caught (part 1)
test_that(
 desc = 'group_count_init min is caught',
 code = {
  expect_error(
   object = gnd_test_auto(predicted_risk = risk_pred,
                          event_time = flchain$futime[-train_index],
                          event_status = 1+flchain$death[-train_index],
                          time_predict = risk_times,
                          group_count_init = 5,
                          verbose = 0),
   regexp = 'event_status should contain values of'
  )
 }
)

# event_status is caught (part 2)
test_that(
 desc = 'group_count_init min is caught',
 code = {
  expect_error(
   object = gnd_test_auto(predicted_risk = risk_pred,
                          event_time = flchain$futime[-train_index],
                          event_status = rep(0, length(risk_pred)),
                          time_predict = risk_times,
                          group_count_init = 5,
                          verbose = 0),
   regexp = 'but has no 1 values'
  )
 }
)


