
library(riskRegression, quietly = TRUE)
library(survival)
library(polspline)

cc <- complete.cases(flchain)

.flchain <- flchain[cc, ]

set.seed(329)

train_index <- sample(nrow(.flchain), size = 1000)

effect1.df <- .flchain[train_index, ]

effect2.df <- .flchain[-train_index, ]

cox1 <- coxph(Surv(futime,death) ~ . - chapter, x=TRUE, data = effect1.df)

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
        length = 100
)

predict.grid.cox.cll <- log(-log(1-predict.grid.cox))

predict.calibrate.cox <- phare(times,predict.grid.cox.cll,calibrate.cox)


hare <- calib_slope_hare(
        predicted_risk = predict.cox,
        event_status = effect2.df$death,
        event_time = effect2.df$futime,
        time_predict = times
)

check_obs <- hare$data$observed == predict.calibrate.cox
check_prd <- log_neg_log_complement(hare$data$predicted)==predict.grid.cox.cll

test_that(
        desc = 'survival.calib matches the original calib slope hare code',
        code = {
                expect_true(all(check_obs))
                expect_true(all(check_prd))
        }
)








