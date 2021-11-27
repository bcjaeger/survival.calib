#' Mayo Clinic Primary Biliary Cholangitis Data
#'
#' @description
#' `pbc_scalib` is a list with three datasets: `train`, `test`, and
#'   `predrisk`.
#'
#'   - `train` is the data used to train A proportional hazards model,
#'   a gradient boosting tree ensemble, and a random forest with axis
#'   based and oblique splits.
#'
#'   - `test` is the data that the trained models computed predictions for.
#'
#'   - `predrisk` is the predicted risk values for the models listed above.
#'      The prediction horizon for these predictions is 2,500 days after
#'      baseline assessment. Put another way, predicted values are the
#'      predicted probability that a person will have an event within
#'      2,500 of baseline.
#'
#' The `train` and `test` data are a light modification of the
#'  [survival::pbc] data. The modifications are:
#'
#'  1. removed rows with missing data
#'
#'  2. converted `status` into 0 for censor or transplant, 1 for dead
#'
#'  3. removed the `id` column.
#'
#' @format __train and test__ are random subsets of roughly equal size from a
#'   dataset with 276 rows and 20 variables. Those variables are:
#' \describe{
#'   \item{time}{number of days between registration and the earlier of death, transplantion, or study analysis in July, 1986}
#'   \item{status}{ status at endpoint, 0 for censored or transplant, 1 for dead}
#'   \item{trt}{1/2/NA for D-penicillmain, placebo, not randomised}
#'   \item{age}{in years}
#'   \item{sex}{m/f}
#'   \item{ascites}{presence of ascites }
#'   \item{hepato}{presence of hepatomegaly or enlarged liver}
#'   \item{spiders}{blood vessel malformations in the skin}
#'   \item{edema}{0 no edema, 0.5 untreated or successfully treated 1 edema despite diuretic therapy}
#'   \item{bili}{serum bilirunbin (mg/dl)}
#'   \item{chol}{serum cholesterol (mg/dl)}
#'   \item{albumin}{serum albumin (g/dl)}
#'   \item{copper}{urine copper (ug/day)}
#'   \item{alk.phos}{alkaline phosphotase (U/liter)}
#'   \item{ast}{aspartate aminotransferase, once called SGOT (U/ml)}
#'   \item{trig}{triglycerides (mg/dl)}
#'   \item{platelet}{platelet count}
#'   \item{protime}{standardised blood clotting time}
#'   \item{stage}{histologic stage of disease (needs biopsy)}
#' }
#'
#' __predrisk__ is a dataset with 138 rows and 4 variables:
#' \describe{
#'   \item{prop_hazard}{predictions from a proportional hazards model}
#'   \item{rsf_axis}{predictions from a gradient boosting tree ensemble}
#'   \item{gradient_booster}{predictions from a random forest with axis based splits}
#'   \item{rsf_oblique}{predictions from a random forest with oblique splits}
#' }
#'
#' @details See `example` for code to generate the data and fit the models
#'
#' @examples
#' \dontrun{
#'
#' library(riskRegression)
#' library(survival)
#' library(randomForestSRC)
#' library(gbm)
#' library(orsf2)
#'
#' dataset <- pbc[complete.cases(pbc), ]
#' dataset$status[dataset$status > 0] <- dataset$status[dataset$status > 0] - 1
#' dataset$id <- NULL
#' dataset$stage <- as.integer(dataset$stage)
#'
#' n_total <- nrow(dataset)
#' n_train <- round(n_total * 1/2)
#'
#' set.seed(32987)
#'
#' train_index <- sample(nrow(dataset), size = n_train)
#'
#' dataset_train <- dataset[train_index, ]
#' dataset_test <- dataset[-train_index, ]
#'
#' cph <- coxph(Surv(time, status) ~ .,
#'              data = dataset_train,
#'              x = TRUE)
#'
#' rf <- rfsrc(Surv(time, status) ~ .,
#'             data = dataset_train,
#'             nodesize = 15,
#'             ntree = 1000)
#'
#' bst_cv <- gbm(Surv(time, status) ~ .,
#'               data = dataset_train,
#'               interaction.depth = 1,
#'               shrinkage = 0.025,
#'               n.trees = 500,
#'               cv.folds = 10)
#'
#' bst_final <- gbm(Surv(time, status) ~ .,
#'                  data = dataset_train,
#'                  interaction.depth = 1,
#'                  shrinkage = 0.025,
#'                  n.trees = 150)
#'
#' aorsf <- orsf(data = dataset_train,
#'               formula = Surv(time, status) ~ .,
#'               n_tree = 1000)
#'
#' predictRisk.aorsf <- function(object, newdata, times, ...){
#'  predict(object, new_data = newdata, times = times, risk = TRUE)
#' }
#'
#' models <- list(prop_hazard = cph,
#'                rsf_axis = rf,
#'                gradient_booster = bst_final,
#'                rsf_oblique = aorsf)
#'
#' pred_horizon <- 2500
#'
#' data_predrisk <- as.data.frame(
#' lapply(models,
#'        predictRisk,
#'        newdata = dataset_test,
#'        times = pred_horizon)
#' )
#'
#' data_predrisk$prop_hazard[data_predrisk$prop_hazard==1] <- 0.999
#' }
#'
#'
#' @source T Therneau and P Grambsch (2000), Modeling Survival Data: Extending the Cox Model, Springer-Verlag, New York. ISBN: 0-387-98784-3.
"pbc_scalib"


