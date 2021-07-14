

library(riskRegression, quietly = TRUE)
library(survival)

train_index <- 1:3000

risk_mdl <- coxph(data = flchain[train_index, ],
                  x = TRUE,
                  formula = Surv(futime, death)~age+sex+flc.grp+lambda)

risk_times <- c(4000)

risk_pred <- predictRisk(risk_mdl,
                         newdata = flchain[-train_index, ],
                         times = risk_times)

test_that(
   desc = "event count error is triggered",
   code = {
      expect_error(
         calib_test_gnd(
            predicted_risk = risk_pred,
            event_time = flchain$futime[-train_index],
            event_status = flchain$death[-train_index],
            time_predict = risk_times,
            group_count_init = 50,
            group_count_min = 49,
            verbose = 2
         ),
         regexp = 'too few events'
      )
   }
)


# check printed output
test_that(
   desc = 'printed output has not changed',
   code = {
      expect_snapshot(
         calib_test_gnd(
            predicted_risk = risk_pred,
            event_time = flchain$futime[-train_index],
            event_status = flchain$death[-train_index],
            time_predict = risk_times,
            group_count_init = 50,
            verbose = 0
         ),
         cran = FALSE,
         error = FALSE
      )
   }
)

# check on error messages
# group_count_init (lwr bound)
test_that(
   desc = 'group_count_init min is caught',
   code = {
      expect_error(
         object = calib_test_gnd(
            predicted_risk = risk_pred,
            event_time = flchain$futime[-train_index],
            event_status = flchain$death[-train_index],
            time_predict = risk_times,
            group_count_init = 1,
            verbose = 0
         ),
         regexp = 'group_count_init = 1 should be'
      )
   }
)

test_that(
   desc = 'group_method incorrect entry is caught',
   code = {
      expect_error(
         object = calib_test_gnd(
            predicted_risk = risk_pred,
            event_time = flchain$futime[-train_index],
            event_status = flchain$death[-train_index],
            group_method = 'clump',
            time_predict = risk_times,
            group_count_init = 1,
            verbose = 0
         ),
         regexp = 'group_method should be'
      )
   }
)

# time_predict (length)
test_that(
   desc = 'time_predict length is caught',
   code = {
      expect_error(
         object = calib_test_gnd(
            predicted_risk = risk_pred,
            event_time = flchain$futime[-train_index],
            event_status = flchain$death[-train_index],
            time_predict = c(1, 2),
            group_count_init = 5,
            verbose = 0
         ),
         regexp = 'time_predict should have length <1>'
      )
   }
)

# time_predict (type)
test_that(
   desc = 'time_predict length is caught',
   code = {
      expect_error(
         object = calib_test_gnd(
            predicted_risk = risk_pred,
            event_time = flchain$futime[-train_index],
            event_status = flchain$death[-train_index],
            time_predict = "4000",
            group_count_init = 5,
            verbose = 0
         ),
         regexp = 'should have type <double or integer>'
      )
   }
)

# event_status is caught (part 1)
test_that(
   desc = 'status values are corrected, part 1',
   code = {
      expect_error(
         object = calib_test_gnd(
            predicted_risk = risk_pred,
            event_time = flchain$futime[-train_index],
            event_status = sample(c(0, 1, 2),
                                  size = length(risk_pred),
                                  replace = TRUE),
            time_predict = risk_times,
            group_count_init = 5,
            verbose = 0
         ),
         regexp = 'event_status should contain values of'
      )
   }
)

# event_status is caught (part 2)
test_that(
   desc = 'less than 2 events hard stop',
   code = {
      expect_error(
         object = calib_test_gnd(
            predicted_risk = risk_pred,
            event_time = flchain$futime[-train_index],
            event_status = rep(0, length(risk_pred)),
            time_predict = risk_times,
            group_count_init = 5,
            verbose = 0
         ),
         regexp = 'At least 1 group contains <'
      )
   }
)


