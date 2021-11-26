
library(riskRegression, quietly = TRUE)
library(survival)
library(polspline)

cc <- complete.cases(flchain)

.flchain <- flchain[cc, ]

set.seed(329)

train_index <- sample(nrow(.flchain), size = 1000)

effect1.df <- .flchain[train_index, ]

effect2.df <- .flchain[-train_index, ]

cox1 <- coxph(Surv(futime,death) ~ age+sex, x=TRUE, data = effect1.df)

times <- 3000

predict.cox <- predictRisk(cox1, newdata=effect2.df, times=times)

effect2.df$cox.1yr <- predict.cox[,1]

effect2.df$cox.1yr <- ifelse(effect2.df$cox.1yr==1,0.9999,effect2.df$cox.1yr)

effect2.df$cox.1yr.cll <- log(-log(1-effect2.df$cox.1yr))

calibrate.cox <- hare(data=effect2.df$futime,
                      delta=effect2.df$death,
                      cov=as.matrix(effect2.df$cox.1yr.cll))

predict.grid.cox <- seq(
        quantile(effect2.df$cox.1yr, probs = 0.01),
        quantile(effect2.df$cox.1yr, probs = 0.99),
        length = 500
)

predict.grid.cox.cll <- log(-log(1-predict.grid.cox))

predict.calibrate.cox <- phare(times,predict.grid.cox.cll,calibrate.cox)

.scalib <-
 scalib(pred_risk = predict.cox,
        pred_horizon = times,
        event_status = effect2.df$death,
        event_time = effect2.df$futime) |>
 scalib_hare()

test_that(
 desc = 'survival.calib matches the original calib slope hare code',
 code = {

  expect_equal(.scalib$data_outputs$hare_data_plot[[1]]$observed,
               predict.calibrate.cox)

  expect_equal(
   log_neg_log_complement(.scalib$data_outputs$hare_data_plot[[1]]$predicted),
   predict.grid.cox.cll
  )

 }
)








