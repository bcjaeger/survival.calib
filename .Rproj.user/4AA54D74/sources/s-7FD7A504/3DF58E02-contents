

library(survival)

library(polspline)

cc <- complete.cases(flchain)

.flchain <- flchain[cc, ]

set.seed(329)

train_index <- sample(nrow(.flchain), size = 1000)

effect1.df <- .flchain[train_index, ]

effect2.df <- .flchain[-train_index, ]

################################################################################

# Fit Cox PH model to model hazard of death. Use all baseline covariates.

################################################################################

cox1 <- coxph(Surv(futime,death) ~ . - chapter, x=TRUE, data = effect1.df)

times <- 3000

predict.cox <- 1 - predictSurvProb(cox1, newdata=effect2.df, times=times)

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

# Predicted probability of death within 1 year.

plot(predict.grid.cox,predict.calibrate.cox,type="l",lty=1,col="red",

     xlim=c(0,1),ylim=c(0,1),

     xlab = "Predicted probability of 1-year mortality",

     ylab = "Observed probability of 1-year mortality")

abline(0,1)


################################################################################

# ICI for 1-year probabilities.

################################################################################

predict.calibrate.cox <- phare(times,effect2.df$cox.1yr.cll,calibrate.cox)

# Predicted probability of death within 1 year for all subjects in

# validation sample.

ICI.1yr.cox <- mean(abs(effect2.df$cox.1yr - predict.calibrate.cox))

ICI.1yr.forest <- mean(abs(effect2.df$forest.1yr - predict.calibrate.forest))

E50.1yr.cox <- median(abs(effect2.df$cox.1yr - predict.calibrate.cox))

E50.1yr.forest <- median(abs(effect2.df$forest.1yr - predict.calibrate.forest))

E90.1yr.cox <- quantile(abs(effect2.df$cox.1yr - predict.calibrate.cox),probs=0.9)

E90.1yr.forest <- quantile(abs(effect2.df$forest.1yr - predict.calibrate.forest),probs=0.9)

cat(1,ICI.1yr.cox,ICI.1yr.forest,E50.1yr.cox,E50.1yr.forest,

    E90.1yr.cox,E90.1yr.forest,file="ICI.out",fill=T,append=T)
